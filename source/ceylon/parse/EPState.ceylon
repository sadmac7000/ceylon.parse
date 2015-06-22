"An Earley parser state"
shared class EPState {
    "Whether we've matched once already"
    Boolean matchedOnce;

    "Token we processed before parsing this rule"
    shared Token lastToken;

    "Tokens processed before we began to parse this state"
    Integer tokensProcessedBefore;

    "Starting position for the rule match"
    shared Integer start;

    "Tokens"
    shared [Token|EPState?*] children;

    "Position this state belongs to"
    shared Integer pos;

    "Position within the rule we are matching"
    shared Integer matchPos;

    "The rule we are matching"
    shared Rule rule;

    "Levenshtein distance between what we matched after error correction and
     the real string"
    shared Integer lsd;

    "Number of tokens matched. It is important that this does not count error
     tokens."
    shared Integer tokensProcessed;

    "Create a new initial EPState for the given start rule"
    shared new (Rule rule, SOSToken start) {
        this.pos = 0;
        this.rule = rule;
        this.matchPos = 0;
        this.start = 0;
        this.children = [];
        this.lsd = 0;
        this.tokensProcessedBefore = 0;
        this.lastToken = start;
        this.matchedOnce = false;
        this.tokensProcessed = tokensProcessedBefore;
    }

    "Create a new EPState predicted from the given rule"
    shared new Predicted(Integer pos, Rule rule, Integer tokensProcessedBefore,
            Token lastToken) {
        this.pos = pos;
        this.rule = rule;
        this.matchPos = 0;
        this.start = pos;
        this.children = [];
        this.lsd = 0;
        this.tokensProcessedBefore = tokensProcessedBefore;
        this.lastToken = lastToken;
        this.matchedOnce = false;
        this.tokensProcessed = tokensProcessedBefore;
    }

    "Produce a new EPState derived from a previous state"
    new Derived(EPState original, Integer newPos, Token|EPState? newChild) {
        this.pos = newPos;
        variable value lsd_mod = 0;

        if (is Token newChild) {
            this.lastToken = newChild;
            lsd_mod = newChild.lsd;
        } else if (is EPState newChild) {
            this.lastToken = newChild.lastToken;
        } else {
            this.lastToken = original.lastToken;
        }

        assert(exists currentConsumes =
                original.rule.consumes[original.matchPos]);

        if (currentConsumes.variadic) {
            if (this.pos != original.pos) {
                this.matchPos = original.matchPos;
            } else {
                this.matchPos = original.matchPos + 1;
            }

            this.matchedOnce = true;
        } else {
            this.matchPos = original.matchPos + 1;
            this.matchedOnce = false;
        }

        this.lsd = original.lsd + lsd_mod;
        this.children = original.children.withTrailing(newChild);
        this.rule = original.rule;
        this.start = original.start;
        this.tokensProcessedBefore = original.tokensProcessedBefore;
        this.tokensProcessed = original.tokensProcessed + (if (is Token
                newChild) then 1 else if (is EPState newChild) then
            newChild.tokensProcessed else 0);
    }

    "Produce a new EPState skipping the current production clause"
    new Advanced(EPState original) {
        this.pos = original.pos;
        this.rule = original.rule;
        this.matchPos = original.matchPos + 1;
        this.start = original.start;
        this.children = original.children;
        this.lsd = original.lsd;
        this.tokensProcessedBefore = original.tokensProcessedBefore;
        this.lastToken = original.lastToken;
        this.matchedOnce = original.matchedOnce;
        this.tokensProcessed = original.tokensProcessed;
    }

    shared actual Integer hash => start ^ 4 +
        pos ^ 3 + matchPos ^ 2 + rule.hash;

    "Whether this state is complete"
    shared Boolean complete => rule.consumes.size == matchPos;

    "The AST node for this state"
    shared Object astNode {
        assert(complete);

        variable Object?[] sym = [];

        for (c in children) {
            if (is Token c) {
                sym = sym.withTrailing(c.node);
            } else if (is EPState c) {
                sym = sym.withTrailing(c.astNode);
            } else {
                sym = sym.withTrailing(null);
            }
        }

        return rule.consume(*sym);
    }

    "Stop matching the current variadic (if the current production clause is a
     variadic) and move to the next clause."
    EPState? breakVariadic() {
        if (complete) { return null; }
        assert(exists currentConsumes = rule.consumes[matchPos]);
        if (! currentConsumes.variadic) { return null; }
        if (currentConsumes.once && ! matchedOnce) { return null; }

        return EPState.Advanced(this);
    }

    "S-Expression view of this state"
    shared String sexp {
        variable String ret = "( ``rule.produces``";

        for (c in children) {
            ret += " ";
            if (is EPState c) {
                ret += c.sexp;
            } else if (is Token c) {
                ret += c.string;
            } else {
                ret += "_";
            }
        }
        ret += " )";
        return ret;
    }

    "Offer a symbol to this state for scanning or completion"
    shared EPState? feed(Token|EPState? other) {
        value want = rule.consumes[matchPos];
        if (! exists want) { return null; }
        assert(exists want);

        if (is Token other) {
            return other.type.subtypeOf(want.atom) then
                    EPState.Derived(this, other.position, other);
        } else if (! exists other){
            if (! nullAtom.subtypeOf(want.atom)) { return null; }

            return EPState.Derived(this, pos, null);
        } else {
            if (! other.rule.produces.subtypeOf(want.atom)) {
                return null;
            }

            if (rule.precedenceConflict(other.rule)) { return null; }
            if (exists p = rule.forbidPosition(other.rule),
                p == matchPos) { return null; }

            return EPState.Derived(this, other.pos, other);
        }
    }

    "Generate a prediction set for this state"
    shared {EPState *} predicted(RuleBitmap prev) {
        value c = rule.consumes[matchPos];
        if (! exists c) { return {}; }
        assert(exists c);

        return prev.addGetNew(c.predicted).states(pos, tokensProcessed,
                lastToken);
    }

    "Scan for the next desired object"
    shared {EPState *} scan =>
        if (exists c = rule.consumes[matchPos])
        then
            lastToken.next(c.atom).map(feed).chain({breakVariadic(),feed(null)}).narrow<EPState>()
        else {};

    "Scan for the next desired object"
    shared {EPState *} forceScan =>
        if (exists c = rule.consumes[matchPos])
        then
            lastToken.forceNext(c.atom).map(feed).chain({breakVariadic(),feed(null)}).narrow<EPState>()
        else {};

    shared actual Boolean equals(Object other) {
        if (is EPState other) {
            if (other === this) { return true; }

            if (other.start != start) { return false; }
            if (other.pos != pos) { return false; }
            if (other.rule != rule) { return false; }
            if (other.matchPos != matchPos) { return false; }

            if (children nonempty != other.children nonempty) { return false; }
            if (! nonempty children) { return true; }
            if (other.children.size != children.size) { return false; }
            if (other.children != children) { return false; }

            return true;
        } else {
            return false;
        }
    }

    "Checks which of two states (this and another) would make the best recovery
     token. The least state, by the returned comparison, is the winner"
    shared Comparison compareRecovery(EPState other) {
        if (other.lsd != lsd) { return lsd.compare(other.lsd); }
        if (other.tokensProcessed != tokensProcessed) { return
            other.tokensProcessed.compare(tokensProcessed); }

        /* Most of the important comparison is done now. */

        value otherToGo = other.rule.consumes.size - other.matchPos;
        value toGo = rule.consumes.size - matchPos;

        return toGo.compare(otherToGo);
    }

    shared actual String string {
        String produces = rule.produces.string;
        variable value ret = "``pos``: ``produces`` =>";
        variable value loc = 0;

        for (i in rule.consumes) {
            ret += " ";
            if (loc++ == matchPos) { ret += "*"; }
            ret += i.atom.string;
        }

        if (complete) { ret +=" *"; }

        return ret + " ``lsd``";
    }
}

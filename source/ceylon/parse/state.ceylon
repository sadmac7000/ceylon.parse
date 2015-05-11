import ceylon.language.meta { _type = type }
import ceylon.language.meta.model { Type }

"Exception thrown when we need an error constructor but one isn't defined"
class ErrorConstructorException(Type token)
        extends Exception("Could not construct error of type ``token``") {}

"An error in parsing"
abstract class Error() {
    shared actual Boolean equals(Object that) {
        return _type(this) == _type(that);
    }
}

"Interface for errors that delete a token"
interface DeletingError {
    shared formal Object original;
}

"Interface for errors that insert a token"
interface InsertingError {
    shared formal Object newSym;
}

"Error resolved by replacing a token"
class ErrorReplace(shared actual Object newSym,
                   shared actual Object original)
        extends Error()
        satisfies DeletingError, InsertingError {}

"Error resolved by deleting a token"
class ErrorDelete(shared actual Object original)
        extends Error()
        satisfies DeletingError {}

"Error resolved by inserting a new token"
class ErrorInsert(shared actual Object newSym)
        extends Error()
        satisfies InsertingError {}

"An Earley parser state"
class EPState {
    "Starting Levenshtein distance"
    Integer baseLsd;

    "Whether we've matched once already"
    Boolean matchedOnce;

    "Token we processed before parsing this rule"
    shared Object? lastToken;

    "Tokens processed before we began to parse this state"
    Integer tokensProcessedBefore;

    "Starting position for the rule match"
    shared Integer start;

    "Tokens"
    shared [Symbol|EPState|Error?*] children;

    "Position this state belongs to"
    shared Integer pos;

    "Position within the rule we are matching"
    shared Integer matchPos;

    "The rule we are matching"
    shared Rule rule;

    "Memoization for lsd attribute"
    variable Integer? _lsd = null;

    "Number of tokens matched. It is important that this does not count error
     tokens."
    shared Integer tokensProcessed;

    "Create a new initial EPState for the given start rule"
    shared new (Rule rule) {
        this.pos = 0;
        this.rule = rule;
        this.matchPos = 0;
        this.start = 0;
        this.children = [];
        this.baseLsd = 0;
        this.tokensProcessedBefore = 0;
        this.lastToken = null;
        this.matchedOnce = false;
        this.tokensProcessed = tokensProcessedBefore;
    }

    "Create a new EPState predicted from the given rule"
    new Predicted(Integer pos, Rule rule, Integer tokensProcessedBefore,
            Object? lastToken) {
        this.pos = pos;
        this.rule = rule;
        this.matchPos = 0;
        this.start = pos;
        this.children = [];
        this.baseLsd = 0;
        this.tokensProcessedBefore = tokensProcessedBefore;
        this.lastToken = lastToken;
        this.matchedOnce = false;
        this.tokensProcessed = tokensProcessedBefore;
    }

    "Produce a new EPState derived from a previous state"
    new Derived(EPState original, Integer newPos, Symbol|EPState|Error? newChild) {
        this.pos = newPos;

        if (is Symbol newChild) {
            this.lastToken = newChild.sym;
        } else if (is EPState newChild) {
            this.lastToken = newChild.lastToken;
        } else if (is ErrorReplace newChild) {
            this.lastToken = newChild.newSym;
        } else if (is DeletingError newChild) {
            this.lastToken = newChild.original;
        } else {
            this.lastToken = original.lastToken;
        }

        assert(exists currentConsumes =
                original.rule.consumes[original.matchPos]);

        if (is ErrorDelete newChild) {
            this.matchPos = original.matchPos;
            this.matchedOnce = false;
        } else if (currentConsumes.variadic) {
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

        if (is Error newChild) {
            this.baseLsd = original.baseLsd + 1;
        } else {
            this.baseLsd = original.baseLsd;
        }

        this.children = original.children.withTrailing(newChild);
        this.rule = original.rule;
        this.start = original.start;
        this.tokensProcessedBefore = original.tokensProcessedBefore;
        this.tokensProcessed = original.tokensProcessed + (if (is Symbol
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
        this.baseLsd = original.baseLsd;
        this.tokensProcessedBefore = original.tokensProcessedBefore;
        this.lastToken = original.lastToken;
        this.matchedOnce = original.matchedOnce;
        this.tokensProcessed = original.tokensProcessed;
    }

    "Levenshtein distance between what we matched after error correction and
     the real string"
    shared Integer lsd {
        if (exists l=_lsd) { return l; }

        variable value ret = baseLsd;

        for (c in children) {
            if (is EPState c) {
                ret += c.lsd;
            }
        }

        _lsd = ret;
        return ret;
    }

    shared actual Integer hash => start ^ 4 +
        pos ^ 3 + matchPos ^ 2 + rule.hash;

    "Whether this state is complete"
    shared Boolean complete => rule.consumes.size == matchPos;

    "The AST node for this state"
    shared Symbol astNode {
        assert(complete);

        variable Object?[] sym = [];

        for (c in children) {
            if (is Symbol c) {
                sym = sym.withTrailing(c.sym);
            } else if (is EPState c) {
                sym = sym.withTrailing(c.astNode.sym);
            } else if (! exists c) {
                sym = sym.withTrailing(null);
            } else if (is ErrorInsert c) {
                sym = sym.withTrailing(c.newSym);
            } else if (is ErrorReplace c) {
                sym = sym.withTrailing(c.newSym);
            }
        }

        value s = Symbol(rule.produces, rule.consume(*sym), pos - start);
        return s;
    }

    "Stop matching the current variadic (if the current production clause is a
     variadic) and move to the next clause."
    shared EPState? breakVariadic() {
        if (complete) { return null; }
        assert(exists currentConsumes = rule.consumes[matchPos]);
        if (! currentConsumes.variadic) { return null; }
        if (currentConsumes.once && ! matchedOnce) { return null; }

        return EPState.Advanced(this);
    }

    "Propagate this state with a trailing error."
    shared {EPState *} failPropagate({Token *} skip,
            Boolean badToken,
            Map<Atom,Object(Object?, Object?)> errorConstructors) {
        assert(exists nextSet = rule.consumes[matchPos]);

        value delete = { for (s in skip) EPState.Derived(this, pos + s.length,
                ErrorDelete(s.sym))
        };

        variable value ret = delete;

        if (nextSet.variadic && (!nextSet.once || matchedOnce)) { return ret; }

        for (next in nextSet) {
            value inscons = errorConstructors[next];

            if (! exists inscons) {
                throw ErrorConstructorException(next.type);
            }

            assert(exists inscons);

            value replace = { for (s in skip) EPState.Derived(this, pos + s.length,
                    ErrorReplace(inscons(s.sym, lastToken), s.sym))
            };

            ret = ret.chain(replace);

            if (badToken) { continue; }

            value newToken = inscons(null, lastToken);
            value insert = EPState.Derived(this, pos, ErrorInsert(newToken));

            ret = ret.chain({insert});
        }

        return ret;
    }

    "S-Expression view of this state"
    shared String sexp {
        variable String ret = "( ``rule.produces``";

        for (c in children) {
            ret += " ";
            if (is EPState c) {
                ret += c.sexp;
            } else if (is Symbol c) {
                ret += c.string;
            } else if (! exists c) {
                ret += "_";
            } else {
                ret += "<ERROR>";
            }
        }
        ret += " )";
        return ret;
    }

    "Offer a symbol to this state for scanning or completion"
    shared EPState? feed(Symbol|EPState? other) {
        assert(exists want = rule.consumes[matchPos]);

        if (is Symbol other) {
            value intersects = want.select(other.type.subtypeOf);
            if (intersects.size == 0) { return null; }

            return EPState.Derived(this, pos + other.length, other);
        } else if (! exists other){
            if (! want.contains(nullAtom)) { return null; }

            return EPState.Derived(this, pos, null);
        } else {
            value intersects = want.select(other.rule.produces.subtypeOf);
            if (intersects.size == 0) { return null; }
            if (rule.precedenceConflict(other.rule)) { return null; }
            if (exists p = rule.forbidPosition(other.rule),
                p == matchPos) { return null; }

            return EPState.Derived(this, other.pos, other);
        }
    }

    "Generate a prediction set for this state"
    shared {EPState *} predicted =>
            if (exists c = rule.consumes[matchPos])
            then c.predicted.map((r) => EPState.Predicted(pos, r,
                        tokensProcessed, lastToken))
            else {};

    shared actual Boolean equals(Object other) {
        if (is EPState other) {
            if (other.start != start) { return false; }
            if (other.pos != pos) { return false; }
            if (other.rule != rule) { return false; }
            if (other.matchPos != matchPos) { return false; }

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

            variable value sep = "";

            for (t in i) {
                ret += sep;
                ret += t.string;
                sep = "|";
            }
        }

        if (complete) { ret +=" *"; }

        return ret + " ``lsd``";
    }
}

import ceylon.language.meta { _type = type }
import ceylon.language.meta.model { Type }
import ceylon.collection { HashSet }

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
class EPState(pos, rule, matchPos, start, children, baseLsd,
        errorConstructors, tokensProcessedBefore, lastToken = null) {
    "Starting Levenshtein distance"
    Integer baseLsd;

    "Token we processed before parsing this rule"
    shared Object? lastToken;

    "Tokens processed before we began to parse this state"
    Integer tokensProcessedBefore;

    "Starting position for the rule match"
    shared Integer start;

    "Error constructors"
    shared Map<Integer,Object(Object?, Object?)> errorConstructors;

    "Tokens"
    shared [Symbol|EPState|Error*] children;

    assert(matchPos <= children.size);

    "Number of tokens matched. It is important that this does not count error
     tokens."
    shared Integer tokensProcessed = sum({ for (c in children) if (is EPState c)
        c.tokensProcessed }.chain({ for (c in children) if (is Symbol c) 1
        }).chain({0})) + tokensProcessedBefore;

    "Position this state belongs to"
    shared Integer pos;

    "Position within the rule we are matching"
    shared Integer matchPos;

    "The rule we are matching"
    shared Rule rule;

    variable Integer? _lsd = null;

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

    shared actual Integer hash = start ^ 4 +
        pos ^ 3 + matchPos ^ 2 + rule.hash;

    "Whether this state is complete"
    shared Boolean complete = rule.consumes.size == matchPos;

    "The AST node for this state"
    shared Symbol astNode {
        assert(complete);

        variable Object[] sym = [];

        for (c in children) {
            if (is Symbol c) {
                sym = sym.withTrailing(c.sym);
            } else if (is EPState c) {
                sym = sym.withTrailing(c.astNode.sym);
            } else if (is ErrorInsert c) {
                sym = sym.withTrailing(c.newSym);
            } else if (is ErrorReplace c) {
                sym = sym.withTrailing(c.newSym);
            }
        }

        value s = Symbol(rule.produces, rule.consume(*sym), pos - start);
        return s;
    }

    "Derive a new state"
    EPState derive(Integer newPos,
            Symbol|EPState|Error newChild) {
        Integer newBaseLsd;
        Integer newMatchPos;
        [Symbol|EPState|Error*] newChildren;

        Object? last;

        if (is Symbol newChild) {
            last = newChild.sym;
        } else if (is EPState newChild) {
            last = newChild.lastToken;
        } else if (is ErrorReplace newChild) {
            last = newChild.newSym;
        } else if (is DeletingError newChild) {
            last = newChild.original;
        } else {
            last = lastToken;
        }

        if (is ErrorDelete newChild) {
            newMatchPos = matchPos;
        } else {
            newMatchPos = matchPos + 1;
        }

        if (is Error newChild) {
            newBaseLsd = baseLsd + 1;
        } else {
            newBaseLsd = baseLsd;
        }

        newChildren = children.withTrailing(newChild);

        return EPState(newPos, rule, newMatchPos, start, newChildren,
                newBaseLsd, errorConstructors, tokensProcessedBefore,
                last);
    }

    "Propagate this state with a trailing error."
    shared {EPState *} failPropagate({Token *} skip,
            Boolean badToken) {
        assert(exists next = rule.consumes[matchPos]);
        value inscons = errorConstructors[next];

        if (! inscons exists) {
            throw ErrorConstructorException(typeAtomCache.resolve(next));
        }

        assert(exists inscons);

        value delete = { for (s in skip) derive(pos + s.length,
                ErrorDelete(s.sym))
        };

        value replace = { for (s in skip) derive(pos + s.length,
                ErrorReplace(inscons(s.sym, lastToken), s.sym))
        };

        if (badToken) {
            return delete.chain(replace);
        }

        value newToken = inscons(null, lastToken);
        value insert = derive(pos, ErrorInsert(newToken));

        return delete.chain(replace).chain({insert});
    }

    "Offer a symbol to this state for scanning or completion"
    shared EPState? feed(Symbol|EPState other) {
        assert(exists want = rule.consumes[matchPos]);

        if (is Symbol other) {
            if (want != other.type) { return null; }

            return derive(pos + other.length, other);
        } else {
            if (want != other.rule.produces) { return null; }

            return derive(other.pos, other);
        }
    }

    "Generate a prediction set for this state"
    shared {EPState *} propagate({Rule *} rules) {
        {EPState *} predict = {
            for (other in rules)
                if (exists c=rule.consumes[matchPos], other.produces == c)
                    EPState(pos, other, 0, pos, [], 0, errorConstructors,
                            tokensProcessed, lastToken)
        };

        return predict;
    }

    shared actual Boolean equals(Object other) {
        if (is EPState other) {
            if (other.start != start) { return false; }
            if (other.pos != pos) { return false; }
            if (other.rule != rule) { return false; }
            if (other.matchPos != matchPos) { return false; }

            if (other.children.size != children.size) { return false;  }

            for (a -> b in zipEntries(other.children, children)) {
                if (a != b) { return false; }
            }

            return true;
        } else {
            return false;
        }
    }

    "Checks which of two states (this and another) would make the best recovery
     token. The least state, by the returned comparison, is the winner"
    shared Comparison compareRecovery(EPState other, {Rule *} rules) {
        HashSet<Integer> productions = HashSet<Integer>{elements={for (r in
                rules) r.produces};};

        assert(exists otherNext = other.rule.consumes[other.matchPos]);
        assert(exists next = rule.consumes[matchPos]);
        value otherStrictlyTerminal = !productions.contains(otherNext);
        value thisStrictlyTerminal = !productions.contains(next);

        if (otherStrictlyTerminal != thisStrictlyTerminal) {
            if (otherStrictlyTerminal) { return larger; }
            return smaller;
        }

        if (other.lsd != lsd) { return lsd.compare(other.lsd); }
        if (other.tokensProcessed != tokensProcessed) { return
            other.tokensProcessed.compare(tokensProcessed); }

        /* Most of the important comparison is done now. */

        value otherToGo = other.rule.consumes.size - other.matchPos;
        value toGo = rule.consumes.size - matchPos;

        return toGo.compare(otherToGo);
    }

    shared actual String string {
        String produces = typeAtomCache.resolve(rule.produces).string;
        variable value ret = "``pos``: ``produces`` =>";
        variable value loc = 0;

        for (i in rule.consumes) {
            ret += " ";
            if (loc++ == matchPos) { ret += "*"; }

            ret += typeAtomCache.resolve(i).string;
        }

        if (complete) { ret +=" *"; }

        return ret + " ``lsd``";
    }
}

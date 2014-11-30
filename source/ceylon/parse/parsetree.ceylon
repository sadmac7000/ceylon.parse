import ceylon.language.meta { type }
import ceylon.language.meta.model { Method, Type }
import ceylon.collection {
    HashSet,
    HashMap,
    ArrayList,
    unlinked
}

"A single token result returned by a TokenArray"
shared class Token(typeIn, sym, length) {
    shared Object sym;
    shared Integer length;
    Integer|Type typeIn;

    shared Integer type;

    if (is Integer typeIn) {
        type = typeIn;
    } else {
        assert(is Type typeIn);
        type = typeAtomCache.getAlias(typeIn);
    }
}

"The type we need for a token input"
shared alias TokenArray => Correspondence<Integer, Set<Token>>;

"A result to represent the end of a stream."
shared Token eos = Token(type(eosObject), eosObject, 0);
object eosObject {}

"We have to convert type objects to integers to pass them around, otherwise we
 encounter weird performance issues."
object typeAtomCache {
    value from = HashMap<Type, Integer>();
    value to = HashMap<Integer, Type>();
    variable value next = 0;

    "Get an alias for a type"
    shared Integer getAlias(Type t) {
        if (from.defines(t)) {
            value ret = from[t];
            assert(exists ret);
            return ret;
        }

        from.put(t, next);
        to.put(next, t);
        return next++;
    }

    "Resolve a type"
    shared Type resolve(Integer i) {
        value ret = to[i];
        assert(exists ret);
        return ret;
    }
}

"A rule. Specifies produced and consumed symbols and a method to execute them"
shared class Rule(Method<Nothing,Object> meth, ParseTree<Object> tree) {
    "Sequence of symbols consumed by this production"
    shared Integer[] consumes = [ for (x in meth.parameterTypes)
        typeAtomCache.getAlias(x) ];

    "Symbol produced by this production"
    shared Integer produces = typeAtomCache.getAlias(meth.type);

    "Declaration object for the method we call"
    value declaration = meth.declaration;

    shared actual Integer hash = consumes.hash ^ 2 + produces.hash;

    "Run the production-handling code for this method."
    shared Object consume(Object[] syms) {
        value result = declaration.memberInvoke{container=tree;
            typeArguments=[]; arguments=syms;};
        assert(is Object result);
        return result;
    }

    shared actual Boolean equals(Object other) {
        if (is Rule other) {
            return other.consumes == consumes && other.produces == produces;
        } else {
            return false;
        }
    }
}

"An Earley parser state"
class EPState(pos, rule, matchPos, start, children = []) {
    "Starting position for the rule match"
    shared Integer start;

    "Tokens"
    shared [Token|EPState*] children;

    "Position this state belongs to"
    shared Integer pos;

    "Position within the rule we are matching"
    Integer matchPos;

    "The rule we are matching"
    shared Rule rule;

    shared actual Integer hash = start ^ 4 + pos ^ 3 +
        matchPos ^ 2 + rule.hash;

    "Whether this state is complete"
    shared Boolean complete = rule.consumes.size == matchPos;

    "Whether this state has propagated from its position"
    variable Boolean propagated = complete;

    shared Token result {
        assert(complete);

        variable Object[] sym = [];

        for (c in children) {
            if (is Token c) {
                sym = sym.withTrailing(c.sym);
            } else if (is EPState c) {
                sym = sym.withTrailing(c.result.sym);
            }
        }

        return Token(rule.produces, rule.consume(sym), pos - start);
    }

    "Offer a symbol to this state for scanning or completion"
    shared EPState? feed(Token|EPState other) {
        value want = rule.consumes[matchPos];
        assert(exists want);

        if (is Token other) {
            if (want != other.type) { return null; }

            return EPState(pos + other.length, rule, matchPos + 1, start,
                    children.withTrailing(other));
        } else if (is EPState other) {
            if (want != other.rule.produces) { return null; }

            return EPState(other.pos, rule, matchPos + 1, start,
                    children.withTrailing(other));
        } else {
            /* Unreachable */
            assert(false);
        }
    }

    "Generate a prediction set for this state"
    shared {EPState *} propagate({Rule *} rules, {Token *} newtoks) {
        if (propagated) {
            return {};
        }

        propagated = true;

        {EPState *} predict = {
            for (other in rules)
                if (exists c=rule.consumes[matchPos], other.produces == c)
                    EPState(pos, other, 0, pos)
        };

        {EPState *} scan = {
            for (token in newtoks) if (exists x = feed(token)) x
        };

        return predict.chain(scan);
    }

    shared actual Boolean equals(Object other) {
        if (is EPState other) {
            return other.start == start &&
                other.pos == pos &&
                other.rule == rule &&
                other.matchPos == matchPos;
        } else {
            return false;
        }
    }
}

"A do-nothing annotation class for the `rule` annotation"
shared final annotation class GrammarRule()
        satisfies OptionalAnnotation<GrammarRule, Annotated> {}

"We annotate methods of a `ParseTree` object to indicate that those methods
 correspond to production rules"
shared annotation GrammarRule rule() => GrammarRule();

"Insert a new EPState into a hash of sets of states."
Boolean insertEPState(EPState state, HashMap<Integer,HashSet<EPState>> map)
{
    if (! map.defines(state.pos)) {
        map.put(state.pos, HashSet<EPState>());
    }

    value target = map[state.pos];
    assert(exists target);

    if (target.contains(state)) { return false; }

    target.add(state);
    return true;
}

"Exception thrown when we get a null value from a [[TokenArray]]. This should
 never happen for properly-implemented [[TokenArrays|TokenArray]], as we only
 consume from values right after tokens we've gotten back, and the zero length
 [[end of stream token|eos]] should always be the last token we get."
shared class TokenException() extends Exception("TokenArray must be contiguously
                                                 defined") {}

"Exception thrown when a [[ParseTree]] is ambiguous. [[ParseTree]] subtypes
 which override [[resolveAmbiguity]] may choose not to throw this exception."
shared class AmbiguityException() extends Exception("Parser generated ambiguous
                                                     results") {}

"A `ParseTree` is defined by a series of BNF-style production rules. The rules
 are specifed by defining methods with the `rule` annotation.  The parser will
 create an appropriate production rule and call the annotated method in order
 to reduce the value."
shared abstract class ParseTree<out RootTerminal>(TokenArray tokens)
        given RootTerminal satisfies Object {
    "A list of rules for this object"
    shared variable Rule[] rules = [];

    "The result symbol we expect from this tree"
    shared Integer result = typeAtomCache.getAlias(`RootTerminal`);

    "The root node of the parse tree"
    shared RootTerminal root {
        value states = HashMap<Integer,HashSet<EPState>>();
        value stateQueue = ArrayList<EPState>();

        if (rules.size == 0) { populateRules(); }

        for (rule in rules) {
            if (rule.produces != result) { continue; }

            value newState = EPState(0, rule, 0, 0);
            stateQueue.offer(newState);

            assert(insertEPState(newState, states));
        }

        while(stateQueue.size > 0) {
            value next = stateQueue.accept();
            assert(exists next);

            if (next.complete) {
                value prev = states[next.start];
                assert(exists prev);
                for (s in prev) {
                    value n = s.feed(next);

                    if (exists n) {
                        if (insertEPState(n, states)) { stateQueue.offer(n); }
                    }
                }
            } else {
                value token = tokens[next.pos];
                if (exists token) {
                    for (s in next.propagate(rules, token)) {
                        if (insertEPState(s, states)) { stateQueue.offer(s); }
                    }
                } else {
                    throw TokenException();
                }
            }
        }

        value endsPair = states.last;
        assert(exists endsPair);

        value ends = endsPair.item;

        value resultNodes = ArrayList<RootTerminal>();

        for (i in endsPair.item) {
            if (! i.complete) { continue; }
            if (i.rule.produces != result) { continue; }

            assert(is RootTerminal t = i.result.sym);
            resultNodes.add(t);
        }

        if (resultNodes.size == 1) {
            assert(exists ret = resultNodes[0]);
            return ret;
        }

        if (resultNodes.size > 1) {
            return resolveAmbiguity(resultNodes);
        }

        /* TODO: Error recovery */
        assert(false);
    }

    "Method to resolve parse ambiguities. The default implementation simply
     throws [[AmbiguityException]]. Child classes may override this behavior.
     If the child class would like to recover the error, it should return
     a single root node which will be used as the resolved root."
    shared default RootTerminal resolveAmbiguity({Object *} roots) {
        throw AmbiguityException();
    }

    "Set up the list of rules"
    void populateRules() {
        value meths = type(this).getMethods<Nothing,Object>(`GrammarRule`);

        for (r in meths) {
            rules = rules.withTrailing(Rule(r, this));
        }
    }
}

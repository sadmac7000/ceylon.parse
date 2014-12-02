import ceylon.language.meta { type }
import ceylon.language.meta.model { Method, Type }
import ceylon.language.meta.declaration { FunctionDeclaration }
import ceylon.collection {
    HashSet,
    HashMap,
    ArrayList,
    PriorityQueue,
    unlinked
}

"A single token result returned by a tokenizer"
shared class Token(sym, length) {
    shared Object sym;
    shared Integer length;
}

"A parsed symbol."
class Symbol(shared Integer type, Object symIn, Integer lenIn)
        extends Token(symIn, lenIn) {}

"A result to represent the end of a stream."
shared Token eos = Token(eosObject, 0);
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
shared class Rule(shared Object(Object*) consume, shared Integer[] consumes,
        shared Integer produces) {
    shared actual Integer hash = consumes.hash ^ 2 + produces.hash;

    shared actual Boolean equals(Object other) {
        if (is Rule other) {
            return other.consumes == consumes && other.produces == produces;
        } else {
            return false;
        }
    }
}

"Exception thrown when we need an error constructor but one isn't defined"
class ErrorConstructorException(Type token)
        extends Exception("Could not construct error of type ``token``") {}

"An error in parsing"
class Error(errorConstructors, replaces = null) {
    shared Integer? replaces;
    Map<Integer, Object()> errorConstructors;

    shared Object()? construct;

    if (exists replaces) {
        if (exists func = errorConstructors[replaces]) {
            construct = func;
        } else {
            "Default error constructor"
            Object defaultErrorConstructor() {
                throw ErrorConstructorException(typeAtomCache.resolve(replaces));
            }
            construct = defaultErrorConstructor;
        }
    } else {
        construct = null;
    }
}

"An Earley parser state"
class EPState(pos, rule, matchPos, start, children, baseLsd,
        errorConstructors, tokensProcessedBefore) {
    "Starting Levenshtein distance"
    Integer baseLsd;

    "Tokens processed before we began to parse this state"
    Integer tokensProcessedBefore;

    "Starting position for the rule match"
    shared Integer start;

    "Error constructors"
    shared Map<Integer,Object()> errorConstructors;

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

        variable value prev = start;
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

    "Whether this state has propagated from its position"
    variable Boolean propagated = complete;

    "The AST node for this state"
    shared Symbol astNode {
        assert(complete);

        variable Object[] sym = [];

        for (c in children) {
            if (is Symbol c) {
                sym = sym.withTrailing(c.sym);
            } else if (is EPState c) {
                sym = sym.withTrailing(c.astNode.sym);
            } else if (is Error c) {
                if (exists cons = c.construct) {
                    sym = sym.withTrailing(cons());
                }
            }
        }

        value s = Symbol(rule.produces, rule.consume(*sym), pos - start);
        return s;
    }

    "Derive a new state"
    EPState derive(Integer newPos=pos, Integer newMatchPos=matchPos, Symbol|EPState|Error?
            newChild=null, Boolean error=false) {
        Integer newBaseLsd;
        [Symbol|EPState|Error*] newChildren;

        if (error) {
            newBaseLsd = baseLsd + 1;
        } else {
            newBaseLsd = baseLsd;
        }

        if (exists newChild) {
            newChildren = children.withTrailing(newChild);
        } else {
            newChildren = children;
        }

        return EPState(newPos, rule, newMatchPos, start, newChildren,
                newBaseLsd, errorConstructors, tokensProcessedBefore);
    }

    "Propagate this state with a trailing error. If badToken is set, we stopped
     at a position where there was no parseable token."
    shared {EPState *} failPropagate({Integer *} skipBoundaries,
            Boolean badToken = false) {

        value delete = { for (s in skipBoundaries) derive(s,
                matchPos, Error(errorConstructors), true)
        };

        value replace = { for (s in skipBoundaries) derive(s,
                matchPos + 1, Error(errorConstructors, rule.consumes[matchPos]), true)
        };

        if (! badToken) {
            value insert = derive(pos, matchPos + 1, Error(errorConstructors,
                        rule.consumes[matchPos]), true);

            return delete.chain(replace).chain({insert});
        } else {
            return delete.chain(replace);
        }
    }

    "Offer a symbol to this state for scanning or completion"
    shared EPState? feed(Symbol|EPState other) {
        value want = rule.consumes[matchPos];
        assert(exists want);

        if (is Symbol other) {
            if (want != other.type) { return null; }

            return derive(pos + other.length, matchPos + 1, other);
        } else if (is EPState other) {
            if (want != other.rule.produces) { return null; }

            return derive(other.pos, matchPos + 1, other);
        } else {
            /* Unreachable */
            assert(false);
        }
    }

    "Generate a prediction set for this state"
    shared {EPState *} propagate({Rule *} rules, {Symbol *} newTokens) {
        if (propagated) {
            return {};
        }

        propagated = true;

        {EPState *} predict = {
            for (other in rules)
                if (exists c=rule.consumes[matchPos], other.produces == c)
                    EPState(pos, other, 0, pos, [], 0, errorConstructors,
                            tokensProcessed)
        };

        {EPState *} scan = {
            for (token in newTokens) if (exists x = feed(token)) x
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

    "Checks which of two states (this and another) would make the best recovery
     token. The least state is the winner"
    shared Comparison compareRecovery(EPState other, {Rule *} rules) {
        HashSet<Integer> productions = HashSet<Integer>{elements={for (r in
                rules) r.produces};};

        /* Error tokens don't count in tokensProcessed, so this is penalized
         * slightly for errors as compared to just comparing position.
         */
        if (other.tokensProcessed != tokensProcessed) { return
            other.tokensProcessed.compare(tokensProcessed); }
        if (other.lsd != lsd) { return lsd.compare(other.lsd); }

        /* Most of the important comparison is done now. */

        assert(exists otherNext = other.rule.consumes[other.matchPos]);
        assert(exists next = rule.consumes[matchPos]);
        value otherStrictlyTerminal = productions.contains(otherNext);
        value thisStrictlyTerminal = productions.contains(next);

        if (otherStrictlyTerminal != thisStrictlyTerminal) {
            if (otherStrictlyTerminal) { return larger; }
            return smaller;
        }

        value otherToGo = other.rule.consumes.size - other.matchPos;
        value toGo = rule.consumes.size - matchPos;

        return toGo.compare(otherToGo);
    }
}

"A do-nothing annotation class for the `error` annotation"
shared final annotation class GrammarErrorConstructor()
        satisfies OptionalAnnotation<GrammarErrorConstructor, Annotated> {}

"We annotate some methods of a `ParseTree` object to indicate that those
 methods can construct an error version of symbols so we can build error
 reporting into the parse tree."
shared annotation GrammarErrorConstructor errorConstructor() =>
        GrammarErrorConstructor();

"A do-nothing annotation class for the `rule` annotation"
shared final annotation class GrammarRule()
        satisfies OptionalAnnotation<GrammarRule, Annotated> {}

"We annotate methods of a `ParseTree` object to indicate that those methods
 correspond to production rules"
shared annotation GrammarRule rule() => GrammarRule();

"A do-nothing annotation class for the `tokenizer` annotation."
shared final annotation class Tokenizer()
        satisfies OptionalAnnotation<Tokenizer, Annotated> {}

"Methods annotated with `tokenizer` take a sequence and return a token."
shared annotation Tokenizer tokenizer() => Tokenizer();

"Exception thrown when a [[ParseTree]] is ambiguous. [[ParseTree]] subtypes
 which override [[resolveAmbiguity]] may choose not to throw this exception."
shared class AmbiguityException() extends Exception("Parser generated ambiguous
                                                     results") {}

"Bulk-add types to the atom cache and return Symbol objects for them"
{Symbol *} tokensToSymbols({Token *} tokens) {
    return {for (t in tokens) Symbol(typeAtomCache.getAlias(type(t.sym)),
            t.sym, t.length)};
}

"A queue of states, ordered and also prioritized by amount of error"
class StateQueue() {
    value queues = HashMap<Integer,ArrayList<EPState>>();
    value states = HashMap<Integer,HashSet<EPState>>();

    shared <Integer->HashSet<EPState>>? latest => states.last;

    variable PriorityQueue<EPState>? recoveryQueue = null;

    "Initialize recovery queue"
    shared void initRecovery(Rule[] rules) {
        if (recoveryQueue exists) { return; }
        recoveryQueue = PriorityQueue<EPState>((x,y) => x.compareRecovery(y,
                    rules));

        assert(exists r = recoveryQueue);

        for (set in states.items) {
            for (item in set) {
                if (! item.complete) { r.offer(item); }
            }
        }
    }

    "Offer an item to this queue"
    shared void offer(EPState state) {
        if (! states.defines(state.pos)) {
            states.put(state.pos, HashSet<EPState>());
        }

        value target = states[state.pos];
        assert(exists target);

        if (target.contains(state)) { return; }

        target.add(state);

        if (! queues.defines(state.lsd)) {
            queues.put(state.lsd, ArrayList<EPState>());
        }

        assert(exists queue = queues[state.lsd]);
        queue.offer(state);

        if (state.complete) { return; }

        if (exists r=recoveryQueue) {
            r.offer(state);
        }
    }

    "Accept an item from this queue"
    shared EPState? accept() {
        for (lsd in queues.keys.sort(uncurry(Integer.compare))) {
            assert(exists queue = queues[lsd]);
            value ret = queue.accept();

            if (exists ret) { return ret; }

            queues.remove(lsd);
        }

        return null;
    }

    "Get states for a given position"
    shared HashSet<EPState> at(Integer pos) {
        if (! states.defines(pos)) { return HashSet<EPState>(); }
        assert(exists ret = states[pos]);
        return ret;
    }

    "Accept a recovery state"
    shared EPState acceptRecoveryState() {
        assert(exists r=recoveryQueue);
        assert(exists ret=r.accept());
        return ret;
    }
}

"A `ParseTree` is defined by a series of BNF-style production rules. The rules
 are specifed by defining methods with the `rule` annotation.  The parser will
 create an appropriate production rule and call the annotated method in order
 to reduce the value."
shared abstract class ParseTree<out Root>(List<Object> data)
        given Root satisfies Object {
    "A list of rules for this object"
    shared variable Rule[] rules = [];

    "The result symbol we expect from this tree"
    shared Integer result = typeAtomCache.getAlias(`Root`);

    "Error constructors"
    value errorConstructors = HashMap<Integer, Object()>();

    value tokenCache = HashMap<Integer, Set<Symbol>>();

    "Tokenizers"
    variable Token?(List<Object>)[] tokenizers = [];

    "Queue of states to process"
    value stateQueue = StateQueue();

    "Process queued states"
    void pumpStateQueue() {
        while(exists next = stateQueue.accept()) {
            if (next.complete) {
                completeState(next);
            } else {
                propagateState(next);
            }
        }
    }

    "Process a complete state"
    void completeState(EPState state) {
        value prev = stateQueue.at(state.start);
        for (s in prev) {
            value n = s.feed(state);

            if (exists n) { stateQueue.offer(n); }
        }
    }

    "Get tokens at a given location"
    Set<Symbol> getTokens(Integer loc) {
        assert(loc <= data.size);

        if (loc == data.size) {
            return HashSet{elements=tokensToSymbols({eos});};
        }

        if (tokenCache.defines(loc)) {
            assert(exists ret = tokenCache[loc]);
            return ret;
        }

        value ret = HashSet{elements=tokensToSymbols({ for (t in tokenizers)
            if (exists r = t(data[loc...])) r});};

        tokenCache.put(loc, ret);
        return ret;
    }

    "Propagate a state"
    void propagateState(EPState state) {
        value symbols = tokensToSymbols(getTokens(state.pos));
        for (s in state.propagate(rules, symbols)) {
            stateQueue.offer(s);
        }
    }

    "Recover an error"
    void recoverError() {
        stateQueue.initRecovery(rules);
        value state = stateQueue.acceptRecoveryState();
        value tokens = getTokens(state.pos);
        value badToken = tokens.size == 0;
        {Integer *} resetPos;

        if (badToken) {
            variable value i = state.pos + 1;

            while (getTokens(i).size == 0) { continue; }

            resetPos = {i};
        } else {
            value posSet = HashSet<Integer>{elements={ for (t in tokens)
                t.length + state.pos };};
            assert(exists maxPos = max(posSet));

            for (i in (state.pos + 1)..(maxPos - 1)) {
                if (posSet.contains(i)) { continue; }
                if (getTokens(i).size == 0) { continue; }
                posSet.add(i);
            }

            resetPos = posSet;
        }

        for (s in state.failPropagate(resetPos, badToken)) {
            stateQueue.offer(s);
        }
    }

    "Confirm that we have successfully parsed."
    Root? validate() {
        assert(exists endsPair = stateQueue.latest);

        value eosTokens = getTokens(endsPair.key);

        if (eosTokens.size != 1) {
            recoverError();
            return null;
        }

        /* TODO: Error handling (trailing tokens) */
        assert(exists eosToken = eosTokens.first);

        if (eosToken.sym != eosObject) {
            recoverError();
            return null;
        }

        value ends = endsPair.item;

        value resultNodes = ArrayList<Root>();

        for (i in endsPair.item) {
            if (! i.complete) { continue; }
            if (i.rule.produces != result) { continue; }

            assert(is Root t = i.astNode.sym);
            resultNodes.add(t);
        }

        if (resultNodes.size == 1) {
            assert(exists ret = resultNodes[0]);
            return ret;
        }

        if (resultNodes.size > 1) {
            return resolveAmbiguity(resultNodes);
        }

        recoverError();
        return null;
    }

    "The root node of the parse tree"
    shared Root ast {
        if (rules.size == 0) { populateRules(); }
        variable Root? ret = null;

        while (! ret exists) {
            pumpStateQueue();
            ret = validate();
        }

        assert(exists v=ret);
        return v;
    }

    "Method to resolve parse ambiguities. The default implementation simply
     throws [[AmbiguityException]]. Child classes may override this behavior.
     If the child class would like to recover the error, it should return
     a single root node which will be used as the resolved root."
    shared default Root resolveAmbiguity({Object *} roots) {
        throw AmbiguityException();
    }

    "Set up the list of rules"
    void populateRules() {
        value meths = type(this).getMethods<Nothing>(`GrammarRule`);
        value errConMeths =
            type(this).getMethods<Nothing>(`GrammarErrorConstructor`);
        value tokenizerMeths = type(this).getMethods<Nothing>(`Tokenizer`);

        for (t in tokenizerMeths) {
            Token? tokenizer(List<Object> s) {
                assert(is Token? ret = t.declaration.memberInvoke(this, [],
                            s));
                return ret;
            }

            tokenizers = tokenizers.withTrailing(tokenizer);
        }

        for (c in errConMeths) {
            value type = typeAtomCache.getAlias(c.type);

            Object construct() {
                assert(is Object ret = c.declaration.memberInvoke(this));
                return ret;
            }

            errorConstructors.put(type, construct);
        }

        for (r in meths) {
            Object consume(Object *o) {
                assert(is Object ret = r.declaration.memberInvoke(this, [], *o));
                return ret;
            }

            value consumes = [ for (p in r.parameterTypes)
                typeAtomCache.getAlias(p) ];
            value produces = typeAtomCache.getAlias(r.type);
            value rule = Rule(consume, consumes, produces);

            rules = rules.withTrailing(rule);

            if (rule.produces != result) { continue; }

            value newState = EPState(0, rule, 0, 0, [], 0,
                    errorConstructors, 0);
            stateQueue.offer(newState);
        }
    }
}

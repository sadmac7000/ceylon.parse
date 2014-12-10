import ceylon.language.meta {
    _type = type
}
import ceylon.language.meta.model {
    Generic,
    UnionType
}
import ceylon.collection {
    HashSet,
    HashMap,
    ArrayList,
    PriorityQueue
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
 which override [[ParseTree.resolveAmbiguity]] may choose not to throw this
 exception."
class AmbiguityException()
        extends Exception("Parser generated ambiguous results") {}

"Exception thrown when we need a bad token constructor but one isn't defined"
class BadTokenConstructorException()
        extends Exception("Could not construct invalid token") {}

"A queue of states"
class StateQueue() {
    value queue = ArrayList<EPState>();
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

        queue.offer(state);

        if (state.complete) { return; }

        if (exists r=recoveryQueue) {
            r.offer(state);
        }
    }

    "Accept an item from this queue"
    shared EPState? accept() => queue.accept();

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
shared abstract class ParseTree<out Root, in Data>(Data data)
        given Data satisfies List<Object>
        given Root satisfies Object {
    "A list of rules for this object"
    shared variable Rule[] rules = [];

    "The result symbol we expect from this tree"
    shared Integer result = typeAtomCache.getAlias(`Root`);

    "Error constructors"
    value errorConstructors = HashMap<Integer, Object(Object?)>();

    value tokenCache = HashMap<Integer, Set<Token>>();

    "Tokenizers"
    variable HashMap<Integer, Token?(Data, Object?)> tokenizers =
    HashMap<Integer, Token?(Data, Object?)>();

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
            if (s.complete) { continue; }
            value n = s.feed(state);

            if (exists n) { stateQueue.offer(n); }
        }
    }

    "Get tokens at a given location"
    Set<Token> getTokens(Integer loc, Object? last) {
        assert(loc <= data.size);

        if (loc == data.size) {
            return HashSet{elements={eos};};
        }

        if (tokenCache.defines(loc)) {
            assert(exists cached = tokenCache[loc]);
            return cached;
        }

        assert(is Data tail = data[loc...]);
        value ret = HashSet{elements={ for (t in tokenizers.items)
            if (exists r = t(tail, last)) r};};

        tokenCache.put(loc, ret);
        return ret;
    }

    "Propagate a state"
    void propagateState(EPState state) {
        Symbol? symbol;

        assert(exists want = state.rule.consumes[state.matchPos]);

        if (exists t = tokenizers[want],
            is Data tail = data[state.pos...],
            exists sym = t(tail, state.lastToken)) {
            symbol = sym;
        } else {
            symbol = null;
        }

        for (s in state.propagate(rules, symbol)) {
            stateQueue.offer(s);
        }
    }

    "Recover an error"
    void recoverError() {
        stateQueue.initRecovery(rules);
        value state = stateQueue.acceptRecoveryState();
        value tokens = getTokens(state.pos, state.lastToken);
        value badToken = tokens.size == 0;

        if (badToken) {
            variable value i = state.pos + 1;

            while (getTokens(i, state.lastToken).size == 0) { i++; }

            assert(is Data tokenData = data[state.pos..(i - 1)]);
            value tok = constructBadToken(tokenData, state.lastToken);

            for (s in state.failPropagate({tok}, true)) {
                stateQueue.offer(s);
            }
        } else {
            value posSet = HashSet<Integer>{elements={ for (t in tokens)
                t.length + state.pos };};
            assert(exists maxPos = max(posSet));
            value resultSet = HashSet<Token>{elements={ for (t in
                    tokens) t };};

            for (i in (state.pos + 1)..(maxPos - 1)) {
                if (posSet.contains(i)) { continue; }

                value toks = getTokens(i, state.lastToken);

                for (tok in toks) {
                    if (posSet.contains(tok.length + state.pos)) {
                        continue;
                    }

                    assert(is Data tokenData = data[state.pos..(i - 1)]);
                    value bad = constructBadToken(tokenData, state.lastToken);

                    resultSet.add(bad);
                    posSet.add(i);
                    break;
                }
            }

            for (s in state.failPropagate(resultSet, false)) {
                stateQueue.offer(s);
            }
        }
    }

    "Confirm that we have successfully parsed."
    Root? validate() {
        assert(exists endsPair = stateQueue.latest);

        value eosTokens = getTokens(endsPair.key, null);

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
        value meths = _type(this).getMethods<Nothing>(`GrammarRule`);
        value errConMeths =
            _type(this).getMethods<Nothing>(`GrammarErrorConstructor`);
        value tokenizerMeths = _type(this).getMethods<Nothing>(`Tokenizer`);

        for (t in tokenizerMeths) {
            Token? tokenizer(Data s, Object? last) {
                assert(is Token? ret = t.declaration.memberInvoke(this, [],
                            s, last));
                return ret;
            }

            assert(is UnionType retType = t.type);
            value caseTypes =  retType.caseTypes;
            assert(caseTypes.size == 2);
            assert(is Generic tokenType = {for (r in caseTypes) if (
                        !r.typeOf(null)) r}.first);

            value typeArgs = tokenType.typeArguments.items;
            assert(typeArgs.size == 1);
            assert(exists type = typeArgs.first);

            tokenizers.put(typeAtomCache.getAlias(type), tokenizer);
        }

        for (c in errConMeths) {
            value type = typeAtomCache.getAlias(c.type);

            Object construct(Object? o) {
                assert(is Object ret = c.declaration.memberInvoke(this, [],
                            o));
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

    Token constructBadToken(Data data, Object? previous) {
        return Token(badTokenConstructor(data, previous), data.size);
    }

    shared default Object badTokenConstructor(Data data, Object? previous) {
        throw BadTokenConstructorException();
    }
}

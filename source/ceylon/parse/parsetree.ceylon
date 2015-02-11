import ceylon.collection {
    HashSet,
    HashMap,
    ArrayList,
    PriorityQueue
}

"A queue of states"
class StateQueue() {
    value queue = ArrayList<EPState>();
    value states = HashMap<Integer,HashSet<EPState>>();
    value completeStates = HashMap<Integer,HashSet<EPState>>();

    shared Integer size => queue.size;

    shared <Integer->HashSet<EPState>>? latest {
        Integer? key = max(states.keys);

        if (! exists key) { return null; }
        assert(exists key);
        assert(exists val = states[key]);
        return key->val;
    }

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

        assert(exists target = states[state.pos]);

        if (target.contains(state)) { return; }

        target.add(state);

        queue.offer(state);

        if (state.complete) {
            if (! completeStates.defines(state.start)) {
                completeStates.put(state.start, HashSet<EPState>());
            }

            assert(exists ctarget = completeStates[state.start]);
            ctarget.add(state);

            return;
        } else if (exists comp = completeStates[state.pos]) {
            for (c in comp) {
                if (exists s = state.feed(c)) {
                    offer(s);
                }
            }
        }

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
shared class ParseTree<out Root, in Data>(Grammar<Root,Data> g,
                                                   Data data)
        given Data satisfies List<Object>
        given Root satisfies Object {
    g.populateRules();

    "A list of rules for this object"
    shared Rule[] rules = g.rules;

    "The result symbol we expect from this tree"
    shared Atom result = g.result;

    "Error constructors"
    shared Map<Atom, Object(Object?, Object?)> errorConstructors =
        g.errorConstructors;

    "Tokenizers"
    value tokenizers = g.tokenizers;

    value tokenCache = HashMap<Integer, Set<Token>>();

    "Queue of states to process"
    value stateQueue = StateQueue();

    for (rule in rules) {
        if (! result.supertypeOf(rule.produces)) { continue; }

        value newState = EPState(0, rule, 0, 0, [], 0, errorConstructors, 0);
        stateQueue.offer(newState);
    }

    "Process queued states"
    void pumpStateQueue() {
        while(exists next = stateQueue.accept()) {
            if (next.complete) {
                completeState(next);
            } else {
                predictState(next);
            }
        }
    }

    "Process a complete state"
    void completeState(EPState state) {
        for (s in stateQueue.at(state.start)) {
            if (! s.complete,
                exists n = s.feed(state)) {
                stateQueue.offer(n);
            }
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

    "Run prediction for a state"
    void predictState(EPState state) {
        assert(exists wants = state.rule.consumes[state.matchPos]);

        for (want in wants ) {
            for (k->t in tokenizers) {
                if (! k.subtypeOf(want)) { continue; }
                if (is Data tail = data[state.pos...],
                    exists sym = t(tail, state.lastToken),
                    exists s = state.feed(sym)) {
                    stateQueue.offer(s);
                }
            }
        }

        if (exists s = state.feed(null)) {
            stateQueue.offer(s);
        }

        if (exists s = state.breakVariadic()) {
            stateQueue.offer(s);
        }

        for (s in state.predicted) {
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

            while (i <= data.size && getTokens(i, state.lastToken).size == 0) { i++; }

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
                if (i > data.size) { break; }

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

        variable Integer? minLsd = null;

        for (i in endsPair.item) {
            if (! i.complete) { continue; }
            if (! result.supertypeOf(i.rule.produces)) { continue; }
            if (i.start != 0) { continue; }

            if (! exists k = minLsd) {
                minLsd = i.lsd;
            } else if (exists k = minLsd, i.lsd > k) {
                continue;
            } else if (exists k = minLsd, i.lsd < k) {
                minLsd = i.lsd;
                resultNodes.clear();
            }

            assert(is Root t = i.astNode.sym);
            resultNodes.add(t);
        }

        if (resultNodes.size == 0) {
            recoverError();
            return null;
        }

        if (resultNodes.size > 1, exists m = minLsd, m == 0) {
            return g.resolveAmbiguity(resultNodes);
        }

        assert(exists ret = resultNodes[0]);
        return ret;
    }

    "The root node of the parse tree"
    shared Root ast {
        variable Root? ret = null;

        while (! ret exists) {
            pumpStateQueue();
            ret = validate();
        }


        assert(exists v=ret);
        return v;
    }

    Token constructBadToken(Data data, Object? previous) {
        return Token(g.badTokenConstructor(data, previous), data.size);
    }
}

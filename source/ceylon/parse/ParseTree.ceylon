import ceylon.collection {
    HashSet,
    HashMap,
    ArrayList,
    PriorityQueue
}

"A queue of states"
class StateQueue<Root>(AnyGrammar g)
    given Root satisfies Object {
    g.populateRules();

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
        recoveryQueue = PriorityQueue<EPState>((x,y) => x.compareRecovery(y));

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

    for (rule in g.startRules<Root>()) {
        value newState = EPState(rule);
        offer(newState);
    }

    "Accept an item from this queue"
    shared EPState? accept() {
        variable value state = queue.accept();

        while (exists st = state, st.complete) {
            for (s in at(st.start)) {
                if (! s.complete,
                    exists n = s.feed(st)) {
                    offer(n);
                }
            }

            state = queue.accept();
        }

        return state;
    }

    "Get states for a given position"
    shared HashSet<EPState> at(Integer pos) {
        if (! states.defines(pos)) { return HashSet<EPState>(); }
        assert(exists ret = states[pos]);
        return ret;
    }

    "Accept a recovery state"
    shared EPState? acceptRecoveryState() {
        assert(exists r=recoveryQueue);
        return r.accept();
    }
}

"A `ParseTree` is defined by a series of BNF-style production rules. The rules
 are specifed by defining methods with the `rule` annotation.  The parser will
 create an appropriate production rule and call the annotated method in order
 to reduce the value."
shared class ParseTree<out Root, in Char>(Grammar<Char> g, List<Char> data)
        given Char satisfies Object
        given Root satisfies Object {

    value tokenCache = HashMap<Integer, Set<Token>>();

    "Queue of states to process"
    value stateQueue = StateQueue<Root>(g);

    "Process queued states"
    void pumpStateQueue() {
        while(exists next = stateQueue.accept()) {
            predictState(next);
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

        value tail = data[loc...];
        value ret = HashSet{elements={ for (t in g.tokenizers.items)
            if (exists r = t(tail, last)) r};};

        tokenCache.put(loc, ret);
        return ret;
    }

    "Run prediction for a state"
    void predictState(EPState state) {
        /* FIXME: This method is the performance bottleneck */
        assert(exists wants = state.rule.consumes[state.matchPos]);

        value tail = data[state.pos...];

        for (t in g.scannersFor(wants.atom)) {
            if (exists sym = t(tail, state.lastToken),
                exists s = state.feed(sym)) {
                stateQueue.offer(s);
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
        stateQueue.initRecovery(g.rules);
        value state = stateQueue.acceptRecoveryState();
        if (! exists state) {
            /* TODO: Getting here means the grammar is malformed such that none
             * of the rules actually produce the AST root. It's a complex error
             * to handle better, and it won't come up very often, but we should
             * probably try.
             */
        }

        assert(exists state);

        value tokens = getTokens(state.pos, state.lastToken);
        value badToken = tokens.size == 0;

        if (badToken) {
            variable value i = state.pos + 1;

            while (i <= data.size && getTokens(i, state.lastToken).size == 0) { i++; }

            value tokenData = data[state.pos..(i - 1)];
            value tok = constructBadToken(tokenData, state.lastToken);

            for (s in state.failPropagate({tok}, true, g.errorConstructors)) {
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

                    value tokenData = data[state.pos..(i - 1)];
                    value bad = constructBadToken(tokenData, state.lastToken);

                    resultSet.add(bad);
                    posSet.add(i);
                    break;
                }
            }

            for (s in state.failPropagate(resultSet, false, g.errorConstructors)) {
                stateQueue.offer(s);
            }
        }
    }

    "Confirm that we have successfully parsed."
    Set<Root>? validate() {
        if (! stateQueue.latest exists) {
            recoverError();
            return null;
        }

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
            if (! Atom(`Root`).supertypeOf(i.rule.produces)) { continue; }
            if (i.start != 0) { continue; }

            if (! exists k = minLsd) {
                minLsd = i.lsd;
            } else if (exists k = minLsd, i.lsd > k) {
                continue;
            } else if (exists k = minLsd, i.lsd < k) {
                minLsd = i.lsd;
                resultNodes.clear();
            }

            assert(is Root t = i.astNode);
            resultNodes.add(t);
        }

        if (resultNodes.size == 0) {
            recoverError();
            return null;
        }

        assert(exists ret = resultNodes[0]);
        return HashSet{*resultNodes};
    }

    "The root node of the parse tree"
    shared Set<Root> ast {
        variable Set<Root>? ret = null;

        while (! ret exists) {
            pumpStateQueue();
            ret = validate();
        }


        assert(exists v=ret);
        return v;
    }

    Token constructBadToken(List<Char> data, Object? previous) {
        return Token(g.badTokenConstructor(data, previous), data.size);
    }
}

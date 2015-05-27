import ceylon.collection {
    HashSet,
    HashMap,
    ArrayList,
    PriorityQueue
}

"A queue of states"
class StateQueue<Root>(Grammar g, SOSToken start)
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
        value newState = EPState(rule, start);
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
shared class ParseTree<out Root>(Grammar g, SOSToken data)
        given Root satisfies Object {
    "Queue of states to process"
    value stateQueue = StateQueue<Root>(g, data);

    "Process queued states"
    void pumpStateQueue() {
        while(exists next = stateQueue.accept()) {
            predictState(next);
        }
    }

    "Run prediction for a state"
    void predictState(EPState state) {
        if (exists s = state.feed(null)) {
            stateQueue.offer(s);
        }

        if (exists s = state.breakVariadic()) {
            stateQueue.offer(s);
        }

        for (s in state.predicted) {
            stateQueue.offer(s);
        }

        for (s in state.scan) {
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

        for (s in state.forceScan) {
            stateQueue.offer(s);
        }
    }

    "Confirm that we have successfully parsed."
    Set<Root>? validate() {
        if (! stateQueue.latest exists) {
            recoverError();
            return null;
        }

        assert(exists endsPair = stateQueue.latest);
        value resultNodes = ArrayList<Root>();

        variable Integer? minLsd = null;

        for (i in endsPair.item) {
            if (! i.complete) { continue; }
            if (! Atom(`Root`).supertypeOf(i.rule.produces)) { continue; }
            if (i.start != 0) { continue; }
            if (i.lastToken.next(`EOS`).empty) { continue; }

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
}

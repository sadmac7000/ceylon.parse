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

    Integer size => queue.size;

    <Integer->HashSet<EPState>>? latest {
        Integer? key = max(states.keys);

        if (! exists key) { return null; }
        assert(exists key);
        assert(exists val = states[key]);
        return key->val;
    }

    variable PriorityQueue<EPState>? recoveryQueue = null;

    "Initialize recovery queue"
    void initRecovery(Rule[] rules) {
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
    void offer(EPState state) {
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
    EPState? accept() {
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
    HashSet<EPState> at(Integer pos) {
        if (! states.defines(pos)) { return HashSet<EPState>(); }
        assert(exists ret = states[pos]);
        return ret;
    }

    "Accept a recovery state"
    EPState? acceptRecoveryState() {
        assert(exists r=recoveryQueue);
        return r.accept();
    }

    "Pump the queue until we are out of work to do"
    void pump() {
        while(exists next = accept()) {
            if (exists s = next.feed(null)) {
                offer(s);
            }

            if (exists s = next.breakVariadic()) {
                offer(s);
            }

            for (s in next.predicted) {
                offer(s);
            }

            for (s in next.scan) {
                offer(s);
            }
        }
    }

    "Recover an error"
    Boolean recoverError() {
        initRecovery(g.rules);
        value state = acceptRecoveryState();

        if (! exists state) {
            return false;
        }

        assert(exists state);

        for (s in state.forceScan) {
            offer(s);
        }

        return true;
    }

    "Validate"
    Set<Root>? validate() {
        if (! latest exists) {
            return if (recoverError()) then null else HashSet<Root>();
        }

        assert(exists endsPair = latest);
        value resultNodes = ArrayList<Root>();

        variable Integer? minLsd = null;

        for (i in endsPair.item) {
            if (! i.complete) { continue; }
            if (! Atom(`Root`).supertypeOf(i.rule.produces)) { continue; }
            if (i.start != 0) { continue; }
            if (i.lastToken.next(Atom(`EOS`)).empty) { continue; }

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

        if (resultNodes.empty) {
            return if (recoverError()) then null else HashSet<Root>();
        }

        assert(exists ret = resultNodes[0]);
        return HashSet{*resultNodes};
    }

    "The root node of the parse tree"
    shared Set<Root> ast {
        variable Set<Root>? ret = null;

        while (! ret exists) {
            pump();
            ret = validate();
        }

        assert(exists v=ret);
        return v;
    }
}

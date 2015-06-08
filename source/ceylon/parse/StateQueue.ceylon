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

    value sets = ArrayList<EPState>{*g.startRules<Root>().collect(
                (x) => EPState(x, start))};
    value incomming = PriorityQueue<EPState>((x,y) => x.pos <=> y.pos);
    value localComplete = ArrayList<EPState>();
    value streamToSetPos = HashMap<Integer,Integer>{0->0};

    variable Integer setStart = 0;
    variable Integer scanStart = 0;
    variable Integer predictStart = 0;
    variable Integer setStreamPos = 0;

    variable PriorityQueue<EPState>? recoveryQueue = null;

    value currentPrediction = RuleBitmap(g);

    "Pump the queue until we are out of work to do"
    void pump() {
        while (predictStart < sets.size || ! incomming.empty) {
            predict();
            scan();
            advance();
        }
    }

    "Move states from incomming to the sets list. Complete along the way."
    void advance() {
        if (exists f = incomming.front, f.pos != setStreamPos) {
            setStreamPos = f.pos;
            setStart = predictStart; // == scanStart == sets.size
            currentPrediction.clear();
            streamToSetPos.put(setStreamPos, setStart);
            localComplete.clear();
        }

        while (exists f = incomming.front,
               f.pos == setStreamPos,
               exists next = incomming.accept()) {
            completeFromLocal(next);
            complete(next);
            sets.add(next);
        }
    }

    "Run completion for a given state"
    void complete(EPState e) {
        if (! e.complete) { return; }
        value startPoint = streamToSetPos[e.start];
        assert(exists startPoint);

        for (other in sets.sublistFrom(startPoint)) {
            if (other.pos != e.start) { return; }
            if (exists n = other.feed(e)) { incomming.offer(n); }
        }

        if (e.start == setStreamPos) {
            localComplete.add(e);
        }
    }

    "Get a list of states in the current set"
    value currentSet => sets.sublistFrom(setStart);

    "Try to complete the given state with the states in localComplete"
    void completeFromLocal(EPState e) {
        for (l in localComplete) {
            if (exists n = e.feed(l),
                ! currentSet.any((x) => x == n)) {
                incomming.offer(n);
            }
        }
    }

    "Run prediction for the current set"
    void predict() {
        while (exists e = sets[predictStart]) {
            for (next in e.predicted(currentPrediction)) {
                completeFromLocal(next);
                complete(next);
                sets.add(next);
            }

            predictStart++;
        }
    }

    "Run scanning for the current set"
    void scan() {
        while (exists e = sets[scanStart]) {
            for (s in e.scan) {
                if (! currentSet.any((x) => x == s)) {
                    incomming.offer(s);
                }
            }

            scanStart++;
        }
    }

    "Validate"
    Set<Root>? validate() {
        if (sets.empty) {
            return if (recoverError()) then null else HashSet<Root>();
        }

        assert(exists lastPos = sets.last?.pos);
        value resultNodes = ArrayList<Root>();

        variable Integer? minLsd = null;
        value rootAtom = Atom(`Root`);

        for (i in sets.reversed) {
            if (i.pos != lastPos) { break; }
            if (! i.complete) { continue; }
            if (! rootAtom.supertypeOf(i.rule.produces)) { continue; }
            if (i.start != 0) { continue; }
            if (i.lastToken.next(eosAtom).empty) { continue; }

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

    "Recover a stalled queue"
    Boolean recoverError() => false;
}

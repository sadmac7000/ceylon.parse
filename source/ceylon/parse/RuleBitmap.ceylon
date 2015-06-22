import ceylon.collection { ArrayList }

"A bitmap for storing a set of rules"
shared class RuleBitmap(Grammar g) extends ArrayList<Integer>() {
    Integer bpi = runtime.integerAddressableSize;

    "Add a rule to this bitmap"
    shared void addRule(Rule r) {
        assert(r.g === g);
        Integer bucket = r.identifier / bpi;

        while (size <= bucket) { add(0); }
        assert (exists old = this[bucket]);

        set(bucket, old.set(r.identifier % bpi, true));
    }

    "Add the items from another bitmap to this one. Return a third bitmap that
     has only the items that we added that were not present to begin with."
    shared RuleBitmap addGetNew(RuleBitmap other) {
        RuleBitmap ret = RuleBitmap(g);
        assert(g == other.g);

        variable Integer i = 0;
        while (i < size, i < other.size) {
            assert(exists a = this[i]);
            assert(exists b = other[i]);
            ret.add(a.not.and(b));
            set(i, a.or(b));
            i++;
        }

        while (i < other.size) {
            assert(exists a = other[i]);
            add(a);
            ret.add(a);
            i++;
        }

        return ret;
    }

    shared {EPState *} states(Integer pos, Integer tp, Token t){
        variable Integer bucket = 0;
        value ret = ArrayList<EPState>();

        for (i in this) {
            variable Integer offset = 0;

            while (offset < bpi, i != 0) {
                if (i.get(offset),
                        exists r = g.getRuleByIdentifier(offset + bucket * bpi)) {
                    ret.add(EPState.Predicted(pos, r, tp, t));
                }
                offset++;
            }

            bucket++;
        }

        return ret;
    }
}

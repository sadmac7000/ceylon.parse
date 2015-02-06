import ceylon.parse { ... }
import ceylon.test { test, assertEquals }

object tupleGrammar extends ABGrammar<S>() {
    rule
    shared S tupleMatcher([ATerm,BTerm]+ terms) {
        variable [ATerm|BTerm*] vals = [];
        for (term in terms) {
            vals = vals.withTrailing(term[0]);
            vals = vals.withTrailing(term[1]);
        }

        return S(terms.first[0].position, *vals);
    }
}

test
void tuple() {
    value root = tupleGrammar.parse("abababab");
    value expect = S (0,
                ATerm(0),
                BTerm(1),
                ATerm(2),
                BTerm(3),
                ATerm(4),
                BTerm(5),
                ATerm(6),
                BTerm(7)
            );
    assertEquals(root, expect);
}

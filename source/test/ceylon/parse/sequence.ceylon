import ceylon.parse { ... }
import ceylon.test { test, assertEquals }

"A parse tree that accepts any string of at least one A followed by zero or
 more B's using sequence matching"
object sequenceGrammar extends Grammar() {
    rule
    shared S rule1([ATerm+] a, [BTerm *] b) {
        return S(a.first.position, *a.chain(b));
    }
}

test
shared void sequence() {
    value root = sequenceGrammar.unambiguousParse<S>(ABStartToken("aaabbb"));
    value expect = S (0,
                ATerm(0),
                ATerm(1),
                ATerm(2),
                BTerm(3),
                BTerm(4),
                BTerm(5)
            );
    assertEquals(root, expect);
}

import ceylon.parse { ... }
import ceylon.test { test, assertEquals }

class ASubtype(Integer pos = 0, Sym* children) extends A(pos, *children) {}

"A parse tree that accepts a string of a's and uses inheriting symbols"
object inheritingGrammar extends ABGrammar<S>() {
    rule
    shared S rule1(A* a) {
        assert(exists first = a.first);
        return S(first.position, *a);
    }

    rule
    shared ASubtype rule2(ATerm a) {
        return ASubtype(a.position, a);
    }
}

test
shared void inheritance() {
    value root = inheritingGrammar.unambiguousParse("aaaaa");
    value expect = S (0,
                ASubtype(0, ATerm(0)),
                ASubtype(1, ATerm(1)),
                ASubtype(2, ATerm(2)),
                ASubtype(3, ATerm(3)),
                ASubtype(4, ATerm(4))
            );
    assertEquals(root, expect);
}

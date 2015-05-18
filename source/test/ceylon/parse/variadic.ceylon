import ceylon.parse { ... }
import ceylon.test { test, assertEquals }

"A parse tree that accepts any string of A's and B's using variadic matching."
object variadicGrammar extends ABGrammar() {
    rule
    shared S rule1(ATerm|BTerm* a) {
        assert(exists first=a.first);
        return S(first.position, *a);
    }
}

test
shared void variadic() {
    value root = variadicGrammar.unambiguousParse<S>("abababaa");
    value expect = S (0,
                ATerm(0),
                BTerm(1),
                ATerm(2),
                BTerm(3),
                ATerm(4),
                BTerm(5),
                ATerm(6),
                ATerm(7)
            );
    assertEquals(root, expect);
}

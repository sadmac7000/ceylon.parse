import ceylon.parse { ... }
import ceylon.test { test, assertEquals }

"A parse tree that accepts any string of A's and AB's using option matching."
object optionGrammar extends ABGrammar<S>() {
    rule
    shared S rule1(A a) => S(a.position, a);

    rule
    shared S rule2(S s, A a) => S(s.position, s, a);

    rule
    shared A rule3(ATerm a, BTerm? b) {
        if (exists b) {
            return A(a.position, a, b);
        } else {
            return A(a.position, a);
        }
    }
}

test
shared void option() {
    value root = optionGrammar.parse("abababaa");
    value expect = S (0,
        S (0,
            S(0,
                S(0,
                    S(0, A(0, ATerm(0), BTerm(1))),
                A(2, ATerm(2), BTerm(3))),
            A(4, ATerm(4), BTerm(5))),
        A(6, ATerm(6))),
    A(7, ATerm(7)));

    assertEquals(root, expect);
}

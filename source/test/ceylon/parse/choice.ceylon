import ceylon.parse { ... }
import ceylon.test { test, assertEquals }

"A parse tree that accepts any string of A's and B's using union matching."
object choiceGrammar extends ABGrammar<S>() {
    rule
    shared S rule1(A a) => S(a.position, a);

    rule
    shared S rule2(S s, A a) => S(s.position, s, a);

    rule
    shared A rule3(ATerm|BTerm t) => A(t.position, t);
}

test
shared void choice1() {
    value root = choiceGrammar.parse("abababab");
    value expect = S (0,
        S (0,
            S(0,
                S(0,
                    S(0,
                        S(0,
                            S(0,
                                S(0,
                                    A(0, ATerm(0))),
                                A(1, BTerm(1))),
                            A(2, ATerm(2))),
                        A(3, BTerm(3))),
                    A(4, ATerm(4))),
                A(5, BTerm(5))),
            A(6, ATerm(6))),
        A(7, BTerm(7))
    );

    assertEquals(root, expect);
}

test
shared void choice2() {
    value root = choiceGrammar.parse("abbbabbb");
    value expect = S (0,
        S (0,
            S(0,
                S(0,
                    S(0,
                        S(0,
                            S(0,
                                S(0,
                                    A(0, ATerm(0))),
                                A(1, BTerm(1))),
                            A(2, BTerm(2))),
                        A(3, BTerm(3))),
                    A(4, ATerm(4))),
                A(5, BTerm(5))),
            A(6, BTerm(6))),
        A(7, BTerm(7))
    );

    assertEquals(root, expect);
}

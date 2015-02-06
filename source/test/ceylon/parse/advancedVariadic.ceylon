import ceylon.parse { ... }
import ceylon.test { test, assertEquals }

"A parse tree that accepts the string babab with the first a repeating one or
 more times and the second a repeating zero or more times"
object advancedVariadicGrammar extends ABGrammar<S>() {
    rule
    shared S rule1(A a, B b, BTerm t) {
        return S(a.position, a, b, t);
    }

    rule
    shared A rule2(BTerm b, ATerm+ a) {
        return A(b.position, b, *a);
    }

    rule
    shared B rule3(BTerm b, ATerm* a) {
        return B(b.position, b, *a);
    }
}

test
shared void advancedVariadic() {
    value root = advancedVariadicGrammar.parse("bbb");
    value expect = S (0,
                A(0,
                    BTerm(0),
                    ATermError(null, 1)
                ),
                B(1,
                    BTerm(1)
                ),
                BTerm(2)
            );
    assertEquals(root, expect);
}

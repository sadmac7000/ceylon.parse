import ceylon.parse { ... }
import ceylon.test { test, assertEquals }

"A parse tree that accepts a very, very simple grammar. There are only 4 words
 in it (aaa, aaaa, baab, bab)."
object simpleGrammar extends ABGrammar() {
    rule
    shared S rule1(ATerm at, A a, ATerm at2) => S(at.position, at, a, at2);

    rule
    shared S rule2(BTerm bt, A a, BTerm bt2) => S(bt.position, bt, a, bt2);

    rule
    shared A rule3(ATerm at) => A(at.position, at);

    rule
    shared A rule4(ATerm at, ATerm at2) => A(at.position, at, at2);
}

test
shared void simple_word1() {
    value root = simpleGrammar.unambiguousParse<S>("baab");
    value expect = S (0,
        BTerm(0),
        A (1,
            ATerm(1),
            ATerm(2)
        ),
        BTerm(3)
    );

    assertEquals(root, expect);
}

test
shared void simple_word2() {
    value root = simpleGrammar.unambiguousParse<S>("bab");
    value expect = S (0,
        BTerm(0),
        A (1,
            ATerm(1)
        ),
        BTerm(2)
    );

    assertEquals(root, expect);
}

test
shared void simple_word3() {
    value root = simpleGrammar.unambiguousParse<S>("aaa");
    value expect = S (0,
        ATerm(0),
        A (1,
            ATerm(1)
        ),
        ATerm(2)
    );

    assertEquals(root, expect);
}

test
shared void simple_word4() {
    value root = simpleGrammar.unambiguousParse<S>("aaaa");
    value expect = S (0,
        ATerm(0),
        A (1,
            ATerm(1),
            ATerm(2)
        ),
        ATerm(3)
    );

    assertEquals(root, expect);
}

test
shared void simple_word4_bad() {
    value root = simpleGrammar.unambiguousParse<S>("aaqaa");
    value expect = S (0,
        ATerm(0),
        A (1,
            ATerm(1),
            ATerm(3, Crap("q", 2))
        ),
        ATerm(4)
    );

    assertEquals(root, expect);
}

test
shared void simple_word2_bad() {
    value root = simpleGrammar.unambiguousParse<S>("bqb");
    value expect = S (0,
        BTerm(0),
        A (1,
            ATermError(Crap("q"), 1)
        ),
        BTerm(2)
    );

    assertEquals(root, expect);
}

test
shared void simple_word2_bad2() {
    value root = simpleGrammar.unambiguousParse<S>("bb");
    value expect = S (0,
        BTerm(0),
        A (1,
            ATermError(null, 1)
        ),
        BTerm(1)
    );

    assertEquals(root, expect);
}

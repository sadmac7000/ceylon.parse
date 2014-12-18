import ceylon.test { test, assertEquals }

"A base class for symbols in the test that defines a few handy features."
class Sym(shared variable Integer position = 0, Sym* children) {
    shared default Object? prevError = null;

    shared actual Integer hash {
        return string.hash;
    }

    shared actual Boolean equals(Object other) {
        return string.equals(other.string);
    }

    shared String sexp {
        String prefix;

        if (exists p = prevError) {
            prefix = "(``p``)";
        } else {
            prefix = "";
        }

        if (children.size == 0) {
            return prefix + shortName;
        }

        return "``prefix + shortName`` ``[for (x in children) x.sexp]``";
    }

    shared actual String string => "[``sexp``]";

    shared default String shortName {
        value start = className(this);
        assert(exists properIdx = start.lastOccurrence('.'));
        return start[(properIdx+1)...] + "@``position``";
    }
}

class S(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}
class A(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}
class B(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}
class ATerm(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class BTerm(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class ATermError(Object? replaces = null, Integer pos = 0)
        extends ATerm(pos) {
    shared actual String shortName {
        if (! replaces exists) { return super.shortName + "(Missing 'a')"; }

        if (is Crap replaces) {
            return "``super.shortName``(Bad token: '``replaces.data``')";
        } else {
            assert(exists replaces);
            return "``super.shortName``(Replaced: '``replaces``')";
        }
    }
}

"A string of bad data"
class Crap(shared String data, shared Integer position = 0) {
    shared actual Integer hash = data.hash;
    shared actual Boolean equals(Object that) {
        if (is Crap that) {
            return that.data == this.data;
        }

        return false;
    }
    shared variable Boolean consumed = false;

    shared actual String string {
        return "``position``-``data``";
    }
}

"A grammar on the alphabet of 'a' and 'b'"
class ABGrammar<K>() extends Grammar<K, String>()
        given K satisfies Object {
    tokenizer
    shared Token<ATerm>? aTerm(String input, Object? last) {
        Integer position;
        Crap? prevError;

        if (is Sym last) {
            position = last.position + 1;
            prevError = null;
        } else if (is Crap last) {
            position = last.position + last.data.size;
            prevError = last;
        } else {
            position = 0;
            prevError = null;
        }
        if (input.startsWith("a")) { return Token(ATerm(position, prevError),1); }
        return null;
    }

    tokenizer
    shared Token<BTerm>? bTerm(String input, Object? last) {
        Integer position;
        Crap? prevError;

        if (is Sym last) {
            position = last.position + 1;
            prevError = null;
        } else if (is Crap last) {
            position = last.position + last.data.size;
            prevError = last;
        } else {
            position = 0;
            prevError = null;
        }

        if (input.startsWith("b")) { return Token(BTerm(position, prevError),1); }
        return null;
    }

    errorConstructor
    shared ATerm error(Object? replaces, Object? last) {
        if (is Sym last) {
            return ATermError(replaces, last.position + 1);
        } else if (is Crap last) {
            return ATermError(replaces, last.position + last.data.size);
        } else {
            return ATermError(replaces);
        }
    }

    shared actual Crap badTokenConstructor(String data, Object? last) {
        if (is Sym last) {
            return Crap(data, last.position + 1);
        } else if (is Crap last) {
            return Crap(data, last.position + last.data.size);
        } else {
            return Crap(data);
        }
    }
}

"A parse tree that accepts a very, very simple grammar. There are only 4 words
 in it (aaa, aaaa, baab, bab)."
object simpleGrammar extends ABGrammar<S>() {
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
    value root = ParseTree(simpleGrammar, "baab").ast;
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
    value root = ParseTree(simpleGrammar, "bab").ast;
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
    value root = ParseTree(simpleGrammar, "aaa").ast;
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
    value root = ParseTree(simpleGrammar, "aaaa").ast;
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
    value root = ParseTree(simpleGrammar, "aaqaa").ast;
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
    value root = ParseTree(simpleGrammar, "bqb").ast;
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
    value root = ParseTree(simpleGrammar, "bb").ast;
    value expect = S (0,
        BTerm(0),
        A (1,
            ATermError(null, 1)
        ),
        BTerm(1)
    );

    assertEquals(root, expect);
}

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
    value root = ParseTree(choiceGrammar, "abababab").ast;
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
    value root = ParseTree(choiceGrammar, "abbbabbb").ast;
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
    value root = ParseTree(optionGrammar, "abababaa").ast;
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

"A parse tree that accepts any string of A's and B's using variadic matching."
object variadicGrammar extends ABGrammar<S>() {
    rule
    shared S rule1(ATerm|BTerm* a) {
        assert(exists first=a.first);
        return S(first.position, *a);
    }
}

test
shared void variadic() {
    value root = ParseTree(variadicGrammar, "abababaa").ast;
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
    value root = ParseTree(inheritingGrammar, "aaaaa").ast;
    value expect = S (0,
                ASubtype(0, ATerm(0)),
                ASubtype(1, ATerm(1)),
                ASubtype(2, ATerm(2)),
                ASubtype(3, ATerm(3)),
                ASubtype(4, ATerm(4))
            );
    assertEquals(root, expect);
}

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
    /* FIXME: We get a more complete test if we remove the 'a' from the test
     * string and remove the ATerm from the result and shift the last BTerm to
     * position 2. We can implement this as soon as ceylon.language bug 607 is
     * fixed.
     */
    value root = ParseTree(advancedVariadicGrammar, "bbab").ast;
    value expect = S (0,
                A(0,
                    BTerm(0),
                    ATermError(null, 1)
                ),
                B(1,
                    BTerm(1),
                    ATerm(2)
                ),
                BTerm(3)
            );
    assertEquals(root, expect);
}

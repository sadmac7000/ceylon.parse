import ceylon.test { test, assertEquals }

"A base class for symbols in the test that defines a few handy features."
class Sym(shared variable Integer position = 0, Sym* children) {
    shared actual Integer hash {
        return string.hash;
    }

    shared actual Boolean equals(Object other) {
        return string.equals(other.string);
    }

    shared actual String string {
        if (children.size == 0) {
            return shortName;
        }

        return "[``this.shortName`` ``[for (x in children) x.string]``]";
    }

    shared default String shortName {
        value start = className(this);
        value properIdx = start.lastOccurrence('.');
        assert(exists properIdx);
        return start[(properIdx+1)...] + "@``position``";
    }
}

class S(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}
class A(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}
class ATerm(Integer pos = 0) extends Sym(pos) {}
class BTerm(Integer pos = 0) extends Sym(pos) {}
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
}

"A parse tree that accepts a very, very simple grammar. There are only 4 words
 in it (aaa, aaaa, baab, bab)."
object simpleGrammar extends Grammar<S, String>() {
    rule
    shared S rule1(ATerm at, A a, ATerm at2) => S(at.position, at, a, at2);

    rule
    shared S rule2(BTerm bt, A a, BTerm bt2) => S(bt.position, bt, a, bt2);

    rule
    shared A rule3(ATerm at) => A(at.position, at);

    rule
    shared A rule4(ATerm at, ATerm at2) => A(at.position, at, at2);

    tokenizer
    shared Token<ATerm>? aTerm(String input, Object? last) {
        Integer position;

        if (is Sym last) {
            position = last.position + 1;
        } else if (is Crap last) {
            position = last.position + last.data.size;
        } else {
            position = 0;
        }
        if (input.startsWith("a")) { return Token(ATerm(position),1); }
        return null;
    }

    tokenizer
    shared Token<BTerm>? bTerm(String input, Object? last) {
        Integer position;

        if (is Sym last) {
            position = last.position + 1;
        } else if (is Crap last) {
            position = last.position + last.data.size;
        } else {
            position = 0;
        }

        if (input.startsWith("b")) { return Token(BTerm(position),1); }
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
    print(root);

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
            ATerm(3)
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

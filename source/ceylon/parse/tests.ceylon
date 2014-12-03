import ceylon.language.meta { type }
import ceylon.collection { HashSet, HashMap }
import ceylon.test { test, assertEquals }

"A base class for symbols in the test that defines a few handy features."
class Sym(Sym* children) {
    shared actual Integer hash {
        variable value ret = shortName.hash;

        for (i in 0:children.size) {
            assert(exists child = children[i]);
            ret += child.hash ^ (children.size + 1 - i);
        }

        return ret;
    }

    shared actual Boolean equals(Object other) {
        if (is Sym other) {
            if (type(other) != type(this)) { return false; }
            for (i in zipPairs(this.children, other.children)) {
                if (i[0] != i[1]) { return false; }
            }
            return true;
        } else {
            return false;
        }
    }

    shared actual String string {
        if (children.size == 0) {
            return shortName;
        }

        return "[``this.shortName`` ``[for (x in children) x.string]``]";
    }

    String shortName {
        value start = className(this);
        value properIdx = start.lastOccurrence('.');
        assert(exists properIdx);
        return start[(properIdx+1)...];
    }
}

class S(Sym* children) extends Sym(*children) {}
class A(Sym* children) extends Sym(*children) {}
class ATerm() extends Sym() {}
class BTerm() extends Sym() {}
class ATermError() extends ATerm() {}

"A parse tree that accepts a very, very simple grammar. There are only 4 words
 in it (aaa, aaaa, baab, bab)."
class SimpleTree(String input) extends ParseTree<S>(input) {
    rule
    shared S rule1(ATerm at, A a, ATerm at2) => S(at, a, at2);

    rule
    shared S rule2(BTerm bt, A a, BTerm bt2) => S(bt, a, bt2);

    rule
    shared A rule3(ATerm at) => A(at);

    rule
    shared A rule4(ATerm at, ATerm at2) => A(at, at2);

    tokenizer
    shared Token? aTerm(String input) {
        if (input.startsWith("a")) { return Token(ATerm(),1); }
        return null;
    }

    tokenizer
    shared Token? bTerm(String input) {
        if (input.startsWith("b")) { return Token(BTerm(),1); }
        return null;
    }

    errorConstructor
    shared ATerm error(Object? replaces) => ATermError();
}

test
shared void simple_word1() {
    value root = SimpleTree("baab").ast;
    value expect = S (
        BTerm(),
        A (
            ATerm(),
            ATerm()
        ),
        BTerm()
    );

    assertEquals(root, expect);
}

test
shared void simple_word2() {
    value root = SimpleTree("bab").ast;
    value expect = S (
        BTerm(),
        A (
            ATerm()
        ),
        BTerm()
    );

    assertEquals(root, expect);
}

test
shared void simple_word3() {
    value root = SimpleTree("aaa").ast;
    value expect = S (
        ATerm(),
        A (
            ATerm()
        ),
        ATerm()
    );

    assertEquals(root, expect);
}

test
shared void simple_word4() {
    value root = SimpleTree("aaaa").ast;
    value expect = S (
        ATerm(),
        A (
            ATerm(),
            ATerm()
        ),
        ATerm()
    );

    assertEquals(root, expect);
}

test
shared void simple_word4_bad() {
    value root = SimpleTree("aaqaa").ast;
    value expect = S (
        ATerm(),
        A (
            ATerm(),
            ATerm()
        ),
        ATerm()
    );

    assertEquals(root, expect);
}

test
shared void simple_word2_bad() {
    value root = SimpleTree("bqb").ast;
    value expect = S (
        BTerm(),
        A (
            ATermError()
        ),
        BTerm()
    );

    assertEquals(root, expect);
}

test
shared void simple_word2_bad2() {
    value root = SimpleTree("bb").ast;
    value expect = S (
        BTerm(),
        A (
            ATermError()
        ),
        BTerm()
    );

    assertEquals(root, expect);
}

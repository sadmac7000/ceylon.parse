import ceylon.test { test, assertEquals }

"A base class for symbols in the test that defines a few handy features."
class Sym(Sym* children) {
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
        return start[(properIdx+1)...];
    }
}

class S(Sym* children) extends Sym(*children) {}
class A(Sym* children) extends Sym(*children) {}
class ATerm() extends Sym() {}
class BTerm() extends Sym() {}
class ATermError(Object? replaces = null) extends ATerm() {
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
class Crap(shared String data) {
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
    shared S rule1(ATerm at, A a, ATerm at2) => S(at, a, at2);

    rule
    shared S rule2(BTerm bt, A a, BTerm bt2) => S(bt, a, bt2);

    rule
    shared A rule3(ATerm at) => A(at);

    rule
    shared A rule4(ATerm at, ATerm at2) => A(at, at2);

    tokenizer
    shared Token<ATerm>? aTerm(String input, Object? last) {
        if (input.startsWith("a")) { return Token(ATerm(),1); }
        return null;
    }

    tokenizer
    shared Token<BTerm>? bTerm(String input, Object? last) {
        if (input.startsWith("b")) { return Token(BTerm(),1); }
        return null;
    }

    errorConstructor
    shared ATerm error(Object? replaces) => ATermError(replaces);

    shared actual Crap badTokenConstructor(String data, Object? last) {
        return Crap(data);
    }
}

test
shared void simple_word1() {
    value root = ParseTree(simpleGrammar, "baab").ast;
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
    value root = ParseTree(simpleGrammar, "bab").ast;
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
    value root = ParseTree(simpleGrammar, "aaa").ast;
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
    value root = ParseTree(simpleGrammar, "aaaa").ast;
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
    value root = ParseTree(simpleGrammar, "aaqaa").ast;
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
    value root = ParseTree(simpleGrammar, "bqb").ast;
    value expect = S (
        BTerm(),
        A (
            ATermError(Crap("q"))
        ),
        BTerm()
    );

    assertEquals(root, expect);
}

test
shared void simple_word2_bad2() {
    value root = ParseTree(simpleGrammar, "bb").ast;
    value expect = S (
        BTerm(),
        A (
            ATermError()
        ),
        BTerm()
    );

    assertEquals(root, expect);
}

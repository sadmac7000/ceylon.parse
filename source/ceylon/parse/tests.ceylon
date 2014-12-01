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

"A tokenizer that tokenizes only 'a' and 'b' with no whitespace removal"
class SimpleTokenizer(String input)
        satisfies Correspondence<Integer, Set<Token>> {
    value cache = HashMap<Integer, Set<Token>>();

    shared actual Boolean defines(Integer i) {
        return i <= input.size;
    }

    shared actual Set<Token>? get(Integer i) {
        if (cache.defines(i)) {
            value x = cache[i];
            assert(exists x);
            return x;
        }

        value ret = HashSet<Token>();
        cache.put(i, ret);
        value char = input[i];

        if (! char exists) {
            if (i != input.size) { return null; }
            ret.add(eos);
            return ret;
        }

        assert(exists char);
        
        if (char == 'a') {
            ret.add(Token(`ATerm`, ATerm(), 1));
        } else if (char == 'b') {
            ret.add(Token(`BTerm`, BTerm(), 1));
        }

        return ret;
    }
}

"A parse tree that accepts a very, very simple grammar. There are only 4 words
 in it (aaa, aaaa, baab, bab)."
class SimpleTree(TokenArray tokens) extends ParseTree<S>(tokens) {
    rule
    shared S rule1(ATerm at, A a, ATerm at2) => S(at, a, at2);

    rule
    shared S rule2(BTerm bt, A a, BTerm bt2) => S(bt, a, bt2);

    rule
    shared A rule3(ATerm at) => A(at);

    rule
    shared A rule4(ATerm at, ATerm at2) => A(at, at2);

    errorConstructor
    shared ATerm error(Object? replaces) => ATerm();
}

test
shared void simple_word1() {
    value root = SimpleTree(SimpleTokenizer("baab")).ast;
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
    value root = SimpleTree(SimpleTokenizer("bab")).ast;
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
    value root = SimpleTree(SimpleTokenizer("aaa")).ast;
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
    value root = SimpleTree(SimpleTokenizer("aaaa")).ast;
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
    value root = SimpleTree(SimpleTokenizer("aaqaa")).ast;
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
    value root = SimpleTree(SimpleTokenizer("bqb")).ast;
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
shared void simple_word2_bad2() {
    value root = SimpleTree(SimpleTokenizer("bb")).ast;
    value expect = S (
        BTerm(),
        A (
            ATerm()
        ),
        BTerm()
    );

    assertEquals(root, expect);
}

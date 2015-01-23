import ceylon.test { test, assertEquals, assertThatException }

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
    value root = ParseTree(advancedVariadicGrammar, "bbb").ast;
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

/* Algebraic tests */

class Expr(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}

class Var(String name, Integer pos = 0, shared actual Object? prevError = null)
        extends Expr(pos) {
    shared actual String shortName => super.shortName + " \"``name``\"";
}
class Plus(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class Minus(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class Mul(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class Div(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class LParen(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class RParen(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}

class AlgebraGrammar() extends Grammar<Expr, String>() {
    tokenizer
    shared Token<Var>? var(String input, Object? last) {
        String varChars = "abcdefghijklmnopqrstuvwxyz";
        Integer position;
        Object? prevError;

        assert(exists chr = input.first);

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

        if (varChars.contains(chr)) {
            return Token(Var(chr.string, position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<Plus>? plus(String input, Object? last) {
        Integer position;
        Object? prevError;

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

        if (input.startsWith("+")) {
            return Token(Plus(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<Minus>? minus(String input, Object? last) {
        Integer position;
        Object? prevError;

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

        if (input.startsWith("-")) {
            return Token(Minus(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<Mul>? mull(String input, Object? last) {
        Integer position;
        Object? prevError;

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

        if (input.startsWith("*")) {
            return Token(Mul(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<Div>? divv(String input, Object? last) {
        Integer position;
        Object? prevError;

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

        if (input.startsWith("/")) {
            return Token(Div(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<LParen>? lparen(String input, Object? last) {
        Integer position;
        Object? prevError;

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

        if (input.startsWith("(")) {
            return Token(LParen(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<RParen>? rparen(String input, Object? last) {
        Integer position;
        Object? prevError;

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

        if (input.startsWith(")")) {
            return Token(RParen(position, prevError), 1);
        } else {
            return null;
        }
    }

    rule
    shared Expr parenExpression(LParen l, Expr e, RParen r) => e;

    shared actual Crap badTokenConstructor(String data, Object? last) {
        if (is Sym last) {
            return Crap(data, last.position + 1);
        } else if (is Crap last) {
            return Crap(data, last.position + last.data.size);
        } else {
            return Crap(data);
        }
    }

    errorConstructor
    shared Mul error(Object? replaces, Object? last) {
        return Mul(0);
    }
}

object ambiguousAlgebraGrammar extends AlgebraGrammar() {
    rule
    shared Expr add(Expr a, Plus o, Expr b) => Expr(a.position, a, o, b);

    rule
    shared Expr mul(Expr a, Mul o, Expr b) => Expr(a.position, a, o, b);
}

test
void verticalAmbiguity() {
    value tree = ParseTree(ambiguousAlgebraGrammar, "a+b*c");

    assertThatException(() => tree.ast).hasType(`AmbiguityException`);
}

test
void horizontalAmbiguity() {
    value tree = ParseTree(ambiguousAlgebraGrammar, "a+b+c");

    assertThatException(() => tree.ast).hasType(`AmbiguityException`);
}

test
void resolvedVerticalAmbiguity()
{
    value root = ParseTree(ambiguousAlgebraGrammar, "(a+b)*c").ast;
    value expect = Expr (1,
        Expr (1,
            Var("a", 1),
            Plus(2),
            Var("b", 3)
            ),
        Mul(5),
        Var("c", 6)
    );

    assertEquals(root, expect);
}

test
void resolvedHorizontalAmbiguity()
{
    value root = ParseTree(ambiguousAlgebraGrammar, "(a+b)+c").ast;
    value expect = Expr (1,
        Expr (1,
            Var("a", 1),
            Plus(2),
            Var("b", 3)
            ),
        Plus(5),
        Var("c", 6)
    );

    assertEquals(root, expect);
}

object precedencedAlgebraGrammar extends AlgebraGrammar() {
    rule(1)
    shared Expr mul(Expr a, Mul o, Expr b) => Expr(a.position, a, o, b);

    rule(2)
    shared Expr add(Expr a, Plus o, Expr b) => Expr(a.position, a, o, b);
}

test
void precedenceResolvedVerticalAmbiguity() {
    value root = ParseTree(precedencedAlgebraGrammar, "a+b*c").ast;
    value expect = Expr (0,
        Var("a", 0),
        Plus(1),
        Expr(2,
            Var("b", 2),
            Mul(3),
            Var("c", 4)
        )
    );

    assertEquals(root, expect);
}

test
void horizontalAmbiguityDespitePrecedence() {
    value tree = ParseTree(precedencedAlgebraGrammar, "a+b+c");

    assertThatException(() => tree.ast).hasType(`AmbiguityException`);
}

object fullAlgebraGrammar extends AlgebraGrammar() {
    rule(1, lassoc)
    shared Expr mul(Expr a, Mul o, Expr b) => Expr(a.position, a, o, b);

    rule(1, lassoc)
    shared Expr div(Expr a, Div o, Expr b) => Expr(a.position, a, o, b);

    rule(2, lassoc)
    shared Expr add(Expr a, Plus o, Expr b) => Expr(a.position, a, o, b);

    rule(2, lassoc)
    shared Expr sub(Expr a, Minus o, Expr b) => Expr(a.position, a, o, b);
}

test
shared void associativityResolvedHorizontalAmbiguity()
{
    value root = ParseTree(fullAlgebraGrammar, "a+b+c").ast;
    value expect = Expr (0,
        Expr (0,
            Var("a", 0),
            Plus(1),
            Var("b", 2)
            ),
        Plus(3),
        Var("c", 4)
    );

    assertEquals(root, expect);
}

test
void multipleOperatorAssociativity()
{
    value root = ParseTree(fullAlgebraGrammar, "a+b-c").ast;
    value expect = Expr (0,
        Expr (0,
            Var("a", 0),
            Plus(1),
            Var("b", 2)
            ),
        Minus(3),
        Var("c", 4)
    );

    assertEquals(root, expect);
}

"A parse tree that accepts any string of at least one A followed by zero or
 more B's using sequence matching"
object sequenceGrammar extends ABGrammar<S>() {
    rule
    shared S rule1({ATerm+} a, {BTerm *} b) {
        return S(a.first.position, *a.chain(b));
    }
}

test
shared void sequence() {
    value root = ParseTree(sequenceGrammar, "aaabbb").ast;
    value expect = S (0,
                ATerm(0),
                ATerm(1),
                ATerm(2),
                BTerm(3),
                BTerm(4),
                BTerm(5)
            );
    assertEquals(root, expect);
}

class Spc(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class MulA(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}
class MMulA(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}
class ParenBox(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}

"Grammar to test generics that apply broadly"
object broadGenericGrammar extends ABGrammar<[S*]>() {
    tokenizer
    shared Token<Spc>? spc(String input, Object? last) {
        Integer position;
        Object? prevError;

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

        if (input.startsWith(" ")) {
            return Token(Spc(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<LParen>? lparen(String input, Object? last) {
        Integer position;
        Object? prevError;

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

        if (input.startsWith("(")) {
            return Token(LParen(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<RParen>? rparen(String input, Object? last) {
        Integer position;
        Object? prevError;

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

        if (input.startsWith(")")) {
            return Token(RParen(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<Mul>? mull(String input, Object? last) {
        Integer position;
        Object? prevError;

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

        if (input.startsWith("*")) {
            return Token(Mul(position, prevError), 1);
        } else {
            return null;
        }
    }

    rule
    shared K whitespace<K>({Spc+} space, K k) given K of Mul|RParen|LParen|ATerm => k;

    rule
    shared A aparse(ATerm a) => A(a.position, a);

    rule
    shared MulA mulA(Mul m, A a) => MulA(m.position, m, a);

    rule
    shared MMulA mmulA(A a, {MulA *} list) => MMulA(a.position, a, *list);

    rule
    shared S stmnt(ATerm at, MMulA m, ParenBox p) => S(at.position, at, m, p);

    rule
    shared ParenBox parenBox(LParen o, A a, RParen c) => ParenBox(o.position, o, a, c);
}

test
void broadGenericTest() {
    value text = "a a*a ( a ) a a*a ( a ) a a*a ( a )";
    value root = ParseTree(broadGenericGrammar, text).ast;
    value expect = [S(0,
            ATerm(0),
            MMulA(2,
                A(2,
                    ATerm(2)),
                MulA(3,
                    Mul(3),
                    A(4,
                        ATerm(4)))),
            ParenBox(6,
                LParen(6),
                A(8,
                    ATerm(8)),
                RParen(10))),
        S(12,
            ATerm(12),
            MMulA(14,
                A(14,
                    ATerm(14)),
                MulA(15,
                    Mul(15),
                    A(16,
                        ATerm(16)))),
            ParenBox(18,
                LParen(18),
                A(20,
                    ATerm(20)),
                RParen(22))),
        S(24,
            ATerm(24),
            MMulA(26,
                A(26,
                    ATerm(26)),
                MulA(27,
                    Mul(27),
                    A(28,
                        ATerm(28)))),
            ParenBox(30,
                LParen(30),
                A(32,
                    ATerm(32)),
                RParen(34)))
    ];
    assertEquals(root, expect);
}

import ceylon.parse { ... }
import ceylon.test { test, assertEquals }

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

    omniRule
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
void broadGeneric() {
    value text = "a a*a ( a ) a a*a ( a ) a a*a ( a )";
    value root = broadGenericGrammar.unambiguousParse(text);
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

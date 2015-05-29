import ceylon.parse { ... }
import ceylon.test { test, assertEquals }
import ceylon.collection { ArrayList }
import ceylon.language.meta.model { Type }

{Token<Object> *} tokenizeBroad(String s, Integer pos, Atom k) {
    value results = ArrayList<Token<Object>>();

    if (eosAtom.subtypeOf(k),
        s.size <= pos) {
        object q satisfies BroadGenericGrammarToken<EOS>&EOSToken {
            shared actual String str = s;
            shared actual Integer position => pos;
        }
        results.add(q);
    }

    if (Atom(`ATerm`).subtypeOf(k),
        exists chr = s[pos],
        chr == 'a') {
        object q satisfies BroadGenericGrammarToken<ATerm> {
            shared actual String str = s;
            shared actual ATerm node => ATerm(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    if (Atom(`BTerm`).subtypeOf(k),
        exists chr = s[pos],
        chr == 'b') {
        object q satisfies BroadGenericGrammarToken<BTerm> {
            shared actual String str = s;
            shared actual BTerm node => BTerm(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    if (Atom(`Spc`).subtypeOf(k),
        exists chr = s[pos],
        chr == ' ') {
        object q satisfies BroadGenericGrammarToken<Spc> {
            shared actual String str = s;
            shared actual Spc node => Spc(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    if (Atom(`LParen`).subtypeOf(k),
        exists chr = s[pos],
        chr == '(') {
        object q satisfies BroadGenericGrammarToken<LParen> {
            shared actual String str = s;
            shared actual LParen node => LParen(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    if (Atom(`RParen`).subtypeOf(k),
        exists chr = s[pos],
        chr == ')') {
        object q satisfies BroadGenericGrammarToken<RParen> {
            shared actual String str = s;
            shared actual RParen node => RParen(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    if (Atom(`Mul`).subtypeOf(k),
        exists chr = s[pos],
        chr == '*') {
        object q satisfies BroadGenericGrammarToken<Mul> {
            shared actual String str = s;
            shared actual Mul node => Mul(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    return results;
}

interface BroadGenericGrammarToken<T>
        satisfies Token<T>
        given T satisfies Object {
    shared formal String str;
    shared actual {Token<Object> *} next(Atom k)
        => tokenizeBroad(str, position, k);
    shared actual {Token<Object> *} forceNext(Atom k) => {};
}

class BroadGenericStartToken(shared actual String str)
        satisfies SOSToken&BroadGenericGrammarToken<SOS> {}

"Grammar to test generics that apply broadly"
object broadGenericGrammar extends Grammar() {
    omniRule
    shared K whitespace<K>([Spc+] space, K k) given K of Mul|RParen|LParen|ATerm => k;

    rule
    shared A aparse(ATerm a) => A(a.position, a);

    rule
    shared MulA mulA(Mul m, A a) => MulA(m.position, m, a);

    rule
    shared MMulA mmulA(A a, [MulA *] list) => MMulA(a.position, a, *list);

    rule
    shared S stmnt(ATerm at, MMulA m, ParenBox p) => S(at.position, at, m, p);

    rule
    shared ParenBox parenBox(LParen o, A a, RParen c) => ParenBox(o.position, o, a, c);
}

test
void broadGeneric() {
    value text = "a a*a ( a ) a a*a ( a ) a a*a ( a )";
    value root = broadGenericGrammar.unambiguousParse<[S*]>(BroadGenericStartToken(text));
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

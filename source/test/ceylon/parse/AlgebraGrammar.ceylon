import ceylon.parse { ... }
import ceylon.language.meta { type }
import ceylon.language.meta.model { Type }
import ceylon.collection { ArrayList }

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

{Token<Object> *} tokenize(String s, Integer pos, Atom k) {
    value results = ArrayList<Token<Object>>();
    String varChars = "abcdefghijklmnopqrstuvwxyz";

    if (eosAtom.subtypeOf(k),
        s.size <= pos) {
        object q satisfies AlgebraGrammarToken<EOS>&EOSToken {
            shared actual String str = s;
            shared actual Integer position => pos;
        }
        results.add(q);
    }

    if (Atom(`Var`).subtypeOf(k),
        exists chr = s[pos],
        varChars.contains(chr)) {
        object q satisfies AlgebraGrammarToken<Var> {
            shared actual String str = s;
            shared actual Var node => Var(chr.string, pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    if (Atom(`Plus`).subtypeOf(k),
        exists chr = s[pos],
        chr == '+') {
        object q satisfies AlgebraGrammarToken<Plus> {
            shared actual String str = s;
            shared actual Plus node => Plus(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    if (Atom(`Minus`).subtypeOf(k),
        exists chr = s[pos],
        chr == '-') {
        object q satisfies AlgebraGrammarToken<Minus> {
            shared actual String str = s;
            shared actual Minus node => Minus(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    if (Atom(`Mul`).subtypeOf(k),
        exists chr = s[pos],
        chr == '*') {
        object q satisfies AlgebraGrammarToken<Mul> {
            shared actual String str = s;
            shared actual Mul node => Mul(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    if (Atom(`Div`).subtypeOf(k),
        exists chr = s[pos],
        chr == '/') {
        object q satisfies AlgebraGrammarToken<Div> {
            shared actual String str = s;
            shared actual Div node => Div(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    if (Atom(`LParen`).subtypeOf(k),
        exists chr = s[pos],
        chr == '(') {
        object q satisfies AlgebraGrammarToken<LParen> {
            shared actual String str = s;
            shared actual LParen node => LParen(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    if (Atom(`RParen`).subtypeOf(k),
        exists chr = s[pos],
        chr == ')') {
        object q satisfies AlgebraGrammarToken<RParen> {
            shared actual String str = s;
            shared actual RParen node => RParen(pos);
            shared actual Integer position => pos + 1;
        }
        results.add(q);
    }

    return results;
}

interface AlgebraGrammarToken<T>
        satisfies Token<T>
        given T satisfies Object {
    shared formal String str;
    shared actual {Token<Object> *} next(Atom k)
        => tokenize(str, position, k);
    shared actual {Token<Object> *} forceNext(Atom k) => {};
}

class AlgebraStartToken(shared actual String str)
        satisfies SOSToken&AlgebraGrammarToken<SOS> {}

class AlgebraGrammar() extends Grammar() {
    rule
    shared Expr parenExpression(LParen l, Expr e, RParen r) => e;
}

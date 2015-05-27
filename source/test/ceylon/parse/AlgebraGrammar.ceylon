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

{Token<K&Object> *} tokenize<K>(String s, Integer pos, Type<K> k) {
    value results = ArrayList<Token<K&Object>>();
    String varChars = "abcdefghijklmnopqrstuvwxyz";

    if (`EOS`.subtypeOf(k),
        s.size <= pos) {
        assert (is Token<K> q = object satisfies AlgebraGrammarToken<EOS>&EOSToken {
            shared actual String str = s;
            shared actual Integer position => pos;
        });
        results.add(q);
    }

    if (`Var`.subtypeOf(k),
        exists chr = s[pos],
        varChars.contains(chr)) {
        assert (is Token<K> q = object satisfies AlgebraGrammarToken<Var> {
            shared actual String str = s;
            shared actual Var node => Var(chr.string, pos);
            shared actual Integer position => pos + 1;
        });
        results.add(q);
    }

    if (`Plus`.subtypeOf(k),
        exists chr = s[pos],
        chr == '+') {
        assert (is Token<K> q = object satisfies AlgebraGrammarToken<Plus> {
            shared actual String str = s;
            shared actual Plus node => Plus(pos);
            shared actual Integer position => pos + 1;
        });
        results.add(q);
    }

    if (`Minus`.subtypeOf(k),
        exists chr = s[pos],
        chr == '-') {
        assert (is Token<K> q = object satisfies AlgebraGrammarToken<Minus> {
            shared actual String str = s;
            shared actual Minus node => Minus(pos);
            shared actual Integer position => pos + 1;
        });
        results.add(q);
    }

    if (`Mul`.subtypeOf(k),
        exists chr = s[pos],
        chr == '*') {
        assert (is Token<K> q = object satisfies AlgebraGrammarToken<Mul> {
            shared actual String str = s;
            shared actual Mul node => Mul(pos);
            shared actual Integer position => pos + 1;
        });
        results.add(q);
    }

    if (`Div`.subtypeOf(k),
        exists chr = s[pos],
        chr == '/') {
        assert (is Token<K> q = object satisfies AlgebraGrammarToken<Div> {
            shared actual String str = s;
            shared actual Div node => Div(pos);
            shared actual Integer position => pos + 1;
        });
        results.add(q);
    }

    if (`LParen`.subtypeOf(k),
        exists chr = s[pos],
        chr == '(') {
        assert (is Token<K> q = object satisfies AlgebraGrammarToken<LParen> {
            shared actual String str = s;
            shared actual LParen node => LParen(pos);
            shared actual Integer position => pos + 1;
        });
        results.add(q);
    }

    if (`RParen`.subtypeOf(k),
        exists chr = s[pos],
        chr == ')') {
        assert (is Token<K> q = object satisfies AlgebraGrammarToken<RParen> {
            shared actual String str = s;
            shared actual RParen node => RParen(pos);
            shared actual Integer position => pos + 1;
        });
        results.add(q);
    }

    return results;
}

interface AlgebraGrammarToken<T>
        satisfies Token<T>
        given T satisfies Object {
    shared formal String str;
    shared actual {Token<K&Object> *} next<K>(Type<K> k)
        => tokenize(str, position, k);
    shared actual {Token<K&Object> *} forceNext<K>(Type<K> k) => {};
}

class AlgebraStartToken(shared actual String str)
        satisfies SOSToken&AlgebraGrammarToken<SOS> {}

class AlgebraGrammar() extends Grammar() {
    rule
    shared Expr parenExpression(LParen l, Expr e, RParen r) => e;
}

import ceylon.parse { ... }
import ceylon.collection { ArrayList }
import ceylon.language.meta.model { Type }

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

class Spc(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class MulA(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}
class MMulA(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}
class ParenBox(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}

{Token<K&Object> *} tokenizeAB<K>(String s, Integer pos, Type<K> k) {
    value results = ArrayList<Token<K&Object>>();

    if (`EOS`.subtypeOf(k),
        s.size <= pos) {
        assert (is Token<K> q = object satisfies ABGrammarToken<EOS>&EOSToken {
            shared actual String str = s;
            shared actual Integer position => pos;
        });
        results.add(q);
    }

    if (`ATerm`.subtypeOf(k),
        exists chr = s[pos],
        chr == 'a') {
        assert (is Token<K> q = object satisfies ABGrammarToken<ATerm> {
            shared actual String str = s;
            shared actual ATerm node => ATerm(pos);
            shared actual Integer position => pos + 1;
        });
        results.add(q);
    }

    if (`BTerm`.subtypeOf(k),
        exists chr = s[pos],
        chr == 'b') {
        assert (is Token<K> q = object satisfies ABGrammarToken<BTerm> {
            shared actual String str = s;
            shared actual BTerm node => BTerm(pos);
            shared actual Integer position => pos + 1;
        });
        results.add(q);
    }

    return results;
}

{Token<K&Object> *} forceTokenizeAB<K>(String s, Integer pos, Type<K> k) {
    value results = ArrayList<Token<K&Object>>();
    if (`EOS`.subtypeOf(k)) {
        assert (is Token<K> q = object satisfies ABGrammarToken<EOS>&EOSToken {
            shared actual String str = s;
            shared actual Integer position = s.size;
            shared actual Integer lsd = s.size - pos;
        });
        results.add(q);
    }

    if (`ATerm`.subtypeOf(k)) {

        assert (is Token<K> q = object satisfies ABGrammarToken<ATerm> {
            shared actual String str = s;
            shared actual ATerm node => ATermError(null, pos);
            shared actual Integer position = pos;
            shared actual Integer lsd = 1;
        });
        results.add(q);

        assert (is Token<K> r = object satisfies ABGrammarToken<ATerm> {
            shared actual String str = s;
            shared actual ATerm node => ATermError(Crap(s[pos:1]), pos);
            shared actual Integer position = pos + 1;
            shared actual Integer lsd = 1;
        });

        if (s.longerThan(pos)) {
            results.add(r);
        }

        if (exists chr = s[pos + 1],
            chr == 'a') {

            assert (is Token<K> t = object satisfies ABGrammarToken<ATerm> {
                shared actual String str = s;
                shared actual ATerm node => ATerm(pos + 1, Crap(s[pos:1], pos));
                shared actual Integer position = pos + 2;
                shared actual Integer lsd = 1;
            });
            results.add(t);
        }
    }

    /*if (`BTerm`.subtypeOf(k),
        exists chr = s[pos],
        chr == 'b') {
        assert (is Token<K> q = object satisfies ABGrammarToken<BTerm> {
            shared actual String str = s;
            shared actual BTerm node => BTerm(pos);
            shared actual Integer position => pos + 1;
        });
        results.add(q);
    }*/

    return results;
}

interface ABGrammarToken<T>
        satisfies Token<T>
        given T satisfies Object {
    shared formal String str;
    shared actual {Token<K&Object> *} next<K>(Type<K> k)
        => tokenizeAB(str, position, k);
    shared actual {Token<K&Object> *} forceNext<K>(Type<K> k) =>
        forceTokenizeAB(str, position, k);
}

class ABStartToken(shared actual String str)
        satisfies SOSToken&ABGrammarToken<SOS> {}

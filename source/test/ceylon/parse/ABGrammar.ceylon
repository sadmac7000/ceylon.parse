import ceylon.parse { ... }

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

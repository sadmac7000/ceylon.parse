import ceylon.language.meta { _type = type }

"A single token result returned by a tokenizer"
shared class Token<out SymType = Object>(SymType sym, Integer length)
        extends Symbol(typeAtomCache.getAlias(`SymType`), sym, length)
        given SymType satisfies Object {}

"A parsed symbol."
shared class Symbol(shared Integer type, shared Object sym, shared Integer length) {
    shared actual Integer hash => type ^ 2 + length;

    shared actual Boolean equals(Object that) {
        if (! is Symbol that) {
            return false;
        } else {
            if (_type(this) != _type(that)) { return false; }

            return this.length == that.length;
        }
    }
}

"A result to represent the end of a stream."
shared Token eos = Token(eosObject, 0);
object eosObject {}

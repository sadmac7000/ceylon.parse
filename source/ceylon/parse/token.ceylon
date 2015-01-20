"A single token result returned by a tokenizer"
shared class Token<out SymType = Object>(SymType sym, Integer length)
        extends Symbol(Atom(`SymType`), sym, length)
        given SymType satisfies Object {}

"A parsed symbol."
shared class Symbol(shared Atom type, shared Object sym, shared Integer length) {
    shared actual Integer hash => type.hash ^ 2 + length;

    shared actual Boolean equals(Object that) {
        if (! is Symbol that) {
            return false;
        } else {
            if (type != that.type) { return false; }

            return this.length == that.length;
        }
    }
}

"A result to represent the end of a stream."
shared Token eos = Token(eosObject, 0);
object eosObject {}

"A single token result returned by a tokenizer"
shared class Token<out SymType = Object>(shared SymType sym, shared Integer length)
        given SymType satisfies Object {
    shared Atom type = Atom(`SymType`);

    shared actual Integer hash => type.hash ^ 2 + length;

    shared actual Boolean equals(Object that) {
        if (! is Token<SymType> that) {
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

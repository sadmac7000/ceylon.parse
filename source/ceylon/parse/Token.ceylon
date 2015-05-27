"A single token result returned by a tokenizer"
shared interface Token<out SymType = Object> satisfies Identifiable given SymType satisfies Object {
    shared Atom type => Atom(`SymType`);
    shared formal SymType sym;
    shared formal Integer length;

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

"Type returned by the end of a stream"
shared interface EOS of eosObject {}

"Object returned by the end-of-stream token"
object eosObject satisfies EOS {}

"A result to represent the end of a stream."
shared object eos satisfies Token<EOS> {
    shared actual EOS sym = eosObject;
    shared actual Integer length = 0;
}

"Type returned by the start of a stream"
shared interface SOS of sosObject {}

"Object returned by the start-of-stream token"
object sosObject satisfies SOS {}

"Token type which begins a stream"
shared interface SOSToken satisfies Token<SOS> {
    shared actual SOS sym => sosObject;
    shared actual Integer length => 0;
}

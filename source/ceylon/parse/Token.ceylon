"A single token result returned by a tokenizer"
shared interface Token<out SymType = Object> satisfies Identifiable given SymType satisfies Object {
    "Type atom for this token"
    shared Atom type => Atom(`SymType`);

    "Node value returned by this token"
    shared formal SymType sym;

    "Amount of text matched by this token"
    shared formal Integer length;

    "If this token was retrieved by a call to forceNext, then this is the
     amount of data that was added, removed, or replaced in the stream to allow
     this token to come in to being."
    shared default Integer lsd => 0;

    "Whether this token was created by `forceNext` to force us past an error"
    shared default Boolean isError => false;

    shared actual Integer hash => type.hash ^ 2 + length;

    shared actual Boolean equals(Object that) {
        if (! is Token<SymType> that) {
            return false;
        } else {
            if (type != that.type) { return false; }

            return this.length == that.length;
        }
    }

    "Get the next token(s) in the stream where those tokens are of a given
     type. We return a stream because we allow non-deterministic tokenization."
    shared formal {Token<K> *} next<K>() given K satisfies Object;

    "Try to force tokens of the given type to appear in the stream. The
     tokenizer may insert, delete, or replace data in the stream and record the
     resulting Levenshtein distance in the result token's `lsd` field."
    shared formal {Token<K> *} forceNext<K>() given K satisfies Object;
}

"Type returned by the end of a stream"
shared interface EOS of eosObject {}

"Object returned by the end-of-stream token"
object eosObject satisfies EOS {}

"A result to represent the end of a stream."
shared object eos satisfies Token<EOS> {
    shared actual EOS sym = eosObject;
    shared actual Integer length = 0;

    shared actual {Token<K> *} next<K>()
            given K satisfies Object
        => if (is Token<K> eos) then {eos} else {};

    shared actual {Token<K> *} forceNext<K>()
            given K satisfies Object
        => next<K>();
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

import ceylon.language.meta.model { Type }
import ceylon.language.meta { _type = type }

"A single token result returned by a tokenizer"
shared interface Token<out NodeType = Object> satisfies Identifiable given NodeType satisfies Object {
    "Type atom for this token"
    shared default Atom type => Atom(`NodeType`);

    "Node value returned by this token"
    shared formal NodeType node;

    "Position in the stream of this token"
    shared formal Integer position;

    "If this token was retrieved by a call to forceNext, then this is the
     amount of data that was added, removed, or replaced in the stream to allow
     this token to come in to being."
    shared default Integer lsd => 0;

    "Whether this token was created by `forceNext` to force us past an error"
    shared Boolean isError => lsd > 0;

    shared actual Integer hash => type.hash ^ 2 + position;

    shared actual Boolean equals(Object that) {
        if (! is Token<NodeType> that) {
            return false;
        } else {
            if (type != that.type) { return false; }

            return this.position == that.position;
        }
    }

    "Get the next token(s) in the stream where those tokens are of a given
     type. We return a stream because we allow non-deterministic tokenization."
    shared formal {Token<Object> *} next(Atom k);

    "Try to force tokens of the given type to appear in the stream. The
     tokenizer may insert, delete, or replace data in the stream and record the
     resulting Levenshtein distance in the result token's `lsd` field."
    shared formal {Token<Object> *} forceNext(Atom k);
}

"Type returned by the end of a stream"
shared interface EOS of eosObject {}
shared Atom eosAtom = Atom(`EOS`);

"Object returned by the end-of-stream token"
object eosObject satisfies EOS {}

"Token type which ends a stream"
shared interface EOSToken satisfies Token<EOS> {
    shared actual EOS node => eosObject;
}

"Type returned by the start of a stream"
shared interface SOS of sosObject {}
shared Atom sosAtom = Atom(`SOS`);

"Object returned by the start-of-stream token"
object sosObject satisfies SOS {}

"Token type which begins a stream"
shared interface SOSToken satisfies Token<SOS> {
    shared actual SOS node => sosObject;
    shared actual Integer position => 0;
}

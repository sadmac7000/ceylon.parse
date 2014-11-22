import ceylon.collection { ArrayList }
import ceylon.language.meta.model { Class }

"A token queue is a queue-like object which accepts strings and emits parser
 text. The strings are effectively concatenated; each string specifies more
 text for the tokenizer, until we call `finalize` to send `EOS`."
class TokenQueue([TerminalClass *] terminals) {
    "Text we have not yet processed"
    variable String buffer = "";

    "A stream of already-processed tokens ready to be accepted."
    ArrayList<Terminal> outStream = ArrayList<Terminal>();

    "Whether we have finalized this queue."
    variable value finalized = false;

    "Add text to the pool of text waiting to be tokenized."
    shared void offer(String text) {
        assert(! finalized);
        buffer += text;
        variable Integer matched = 1;

        while (matched > 0) {
            matched = 0;
            for (term in terminals) {
                assert(is Class<Terminal, [String]> term);
                Terminal t = term(buffer);
                matched++;
                outStream.add(t);
                buffer = buffer[t.s.size:-1];
            }
        }
    }

    "Declare the end of the stream to have been reached."
    shared void finalize() {
        assert(! finalized);
        assert(buffer.size == 0);
        finalized = true;
    }

    "Retrieve a token from the queue."
    shared Terminal? accept() {
        Terminal? ret = outStream.accept();

        if (exists ret) {
            return ret;
        }

        if (finalized) {
            return EOS();
        }

        return ret;
    }
}

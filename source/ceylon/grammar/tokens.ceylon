import ceylon.collection { ArrayList }
import ceylon.language.meta.model { Class }

class TokenQueue() {
    ArrayList<TerminalClass> terminals = ArrayList<TerminalClass>();
    variable String buffer = "";
    ArrayList<Terminal> outStream = ArrayList<Terminal>();

    object populating {}
    object tokenizing {}
    object finalized {}

    variable value state = populating;

    shared void addTerminals(TerminalClass *adding) {
        assert(state == populating);
        terminals.addAll(adding);
    }

    shared void offerText(String text) {
        assert(state != finalized);
        state = tokenizing;
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

    shared void finalize() {
        assert(state != finalized);
        assert(buffer.size == 0);
        state = finalized;
    }

    shared Terminal? accept() {
        Terminal? ret = outStream.accept();

        if (exists ret) {
            return ret;
        }

        if (state == finalized) {
            return EOS();
        }

        return ret;
    }
}

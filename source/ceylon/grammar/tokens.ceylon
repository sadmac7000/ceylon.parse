import ceylon.collection { ArrayList }

class TokenQueue() {
    ArrayList<TerminalClass> terminals = ArrayList<TerminalClass>();
    variable String buffer = "";
    ArrayList<TerminalNode> outStream = ArrayList<TerminalNode>();

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
                Terminal t = term();
                String match = buffer[0:t.size];

                if (! t.matches(buffer)) {
                    continue;
                }

                matched++;
                outStream.add(TerminalNode(term, match));
                buffer = buffer[t.size:-1];
            }
        }
    }

    shared void finalize() {
        assert(state != finalized);
        assert(buffer.size == 0);
        state = finalized;
    }

    shared TerminalNode? accept() {
        TerminalNode? ret = outStream.accept();

        if (exists ret) {
            return ret;
        }

        if (state == finalized) {
            return EOSNode();
        }

        return ret;
    }
}



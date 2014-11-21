import ceylon.collection { ArrayList }

class Tree(Grammar g) {
    variable Boolean _finalized = false;
    shared Boolean finalized => _finalized;

    ArrayList<Node> stack = ArrayList<Node>();

    TokenQueue tokens = TokenQueue();

    tokens.addTerminals{[for (x in g.terminals) x];};

    shared void addText(String text) {
        tokens.offerText(text);
        do_parse();
    }

    shared void finalize() {
        TerminalNode? last;

        tokens.finalize();
        do_parse();
        last = tokens.accept();
        assert(is EOSNode last);
    }

    shared void do_parse() {
    }
}


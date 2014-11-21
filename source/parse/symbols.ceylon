import ceylon.language.meta.model { Class, ClassModel }

abstract class Symbol() {}

alias TerminalClass => Class<Terminal,[]>;
alias NonterminalClass => ClassModel<Nonterminal>;
alias SymbolClass => ClassModel<Symbol>;

abstract class Nonterminal(shared {SymbolClass +} symbols) extends Symbol() {
    shared {TerminalClass *} terminals =
        { for (s in symbols) if (is TerminalClass s) s };
    shared {NonterminalClass *} nonterminals =
        { for (s in symbols) if (is NonterminalClass s) s };

    shared formal void consume({Symbol +} syms);
}

abstract class Terminal() extends Symbol() {
    shared formal String s;
    shared Integer size => s.size;
    shared Boolean matches(String input) => input[0:s.size] == s;
}

class EOS() extends Terminal() {
    s = "";
}

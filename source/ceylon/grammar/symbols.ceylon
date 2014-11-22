import ceylon.language.meta.model { Class, ClassModel }

abstract class Symbol() {}

alias TerminalClass => Class<Terminal, []>;
alias NonterminalClass => ClassModel<Nonterminal>;
alias SymbolClass => ClassModel<Symbol>;

abstract class Nonterminal() extends Symbol() {}

abstract class Terminal(String buf) extends Symbol() {
    shared formal String s;
}

class EOS() extends Terminal("") {
    s = "";
}

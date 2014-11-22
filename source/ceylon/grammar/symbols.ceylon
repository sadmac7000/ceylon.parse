import ceylon.language.meta.model { Class, ClassModel }

"`Symbol` is the supertype of all parse tree nodes produced by a `ParseTree`"
abstract class Symbol() {}

alias TerminalClass => Class<Terminal, []>;
alias NonterminalClass => ClassModel<Nonterminal>;
alias SymbolClass => ClassModel<Symbol>;

"The supertype of all non-terminal nodes in a parse tree."
abstract class Nonterminal() extends Symbol() {}

"The supertype of all terminal nodes in a parse tree."
abstract class Terminal(String buf) extends Symbol() {
    shared formal String s;
}

"The end-of-stream terminal. Used to match the end of the parse stream."
class EOS() extends Terminal("") {
    s = "";
}

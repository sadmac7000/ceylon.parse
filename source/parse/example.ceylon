class Expression({SymbolClass +} symbols) extends Nonterminal(symbols)
{
	shared actual void consume({Symbol +} syms) {}
}

class OParen() extends Terminal() { s = "("; }
class CParen() extends Terminal() { s = ")"; }

Grammar emptylisp = Grammar{
    start=`Expression`;
    Expression{`OParen`, `CParen`},
    Expression{`OParen`, `Expression`, `CParen`}
};

void run() {
}

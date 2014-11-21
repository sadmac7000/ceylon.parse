class Node(shared default SymbolClass s) {}

class TerminalNode(shared actual TerminalClass s, shared String data)
	extends Node(s) {}

class EOSNode() extends TerminalNode(`EOS`, "") {}


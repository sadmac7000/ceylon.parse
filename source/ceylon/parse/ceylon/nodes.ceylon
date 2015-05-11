import ceylon.ast.core { Node }
import ceylon.language.meta { type }

"Base class for Ceylon token objects."
shared class CeylonToken(shared String text, shared Integer line_start, shared
        Integer col_start, shared Integer line_end, shared Integer col_end) {

    shared actual Integer hash = text.hash * line_start ^ 2 * col_start ^ 3;

    shared actual Boolean equals(Object other) {
        if (type(other) != type(this)) { return false; }
        assert(is CeylonToken other);

        return text == other.text && line_start == other.line_start && line_end
            == other.line_end;
    }
}


"Token for keywords"
shared class Keyword(String t, Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken(t, ls, cs, le, ce) {}

"Token for keywords"
shared class Punctuation(String t, Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken(t, ls, cs, le, ce) {}

"A single-line comment"
shared class LineComment(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken("", ls, cs, le, ce) {}

"Some whitespace"
shared class Whitespace(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken("", ls, cs, le, ce) {}

"The start of a block comment"
shared class CommentStart(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken("/*", ls, cs, le, ce) {}

"The end of a block comment"
shared class CommentEnd(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken("*/", ls, cs, le, ce) {}

"Body text in a block comment"
shared class CommentBody(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken("", ls, cs, le, ce) {}

"Escape for a UIdentifier"
shared class UIdentStart(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken("\\I", ls, cs, le, ce) {}

"Escape for an LIdentifier"
shared class LIdentStart(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken("\\i", ls, cs, le, ce) {}

"Text of a UIdentifier"
shared class UIdentText(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(text, ls, cs, le, ce) {}

"A +"
shared class Plus(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("+", ls, cs, le, ce) {}

"A -"
shared class Minus(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("-", ls, cs, le, ce) {}

"A #"
shared class HashMark(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("#", ls, cs, le, ce) {}

"A $"
shared class DollarMark(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("$", ls, cs, le, ce) {}

"A _"
shared class Underscore(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("_", ls, cs, le, ce) {}

"A ."
shared class Dot(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation(".", ls, cs, le, ce) {}

"A |"
shared class Pipe(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("|", ls, cs, le, ce) {}

"A &"
shared class Ampersand(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("&", ls, cs, le, ce) {}

"A '"
shared class Quote(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("'", ls, cs, le, ce) {}

"A \""
shared class DoubleQuote(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("\"", ls, cs, le, ce) {}

"A <"
shared class LT(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("<", ls, cs, le, ce) {}

"A >"
shared class GT(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation(">", ls, cs, le, ce) {}

"A ?"
shared class Question(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("?", ls, cs, le, ce) {}

"A ["
shared class SqOpen(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("[", ls, cs, le, ce) {}

"A ]"
shared class SqClose(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("]", ls, cs, le, ce) {}

"A ("
shared class ParOpen(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("(", ls, cs, le, ce) {}

"A )"
shared class ParClose(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation(")", ls, cs, le, ce) {}

"A ,"
shared class Comma(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation(",", ls, cs, le, ce) {}

"A {"
shared class CurlOpen(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("{", ls, cs, le, ce) {}

"A }"
shared class CurlClose(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("}", ls, cs, le, ce) {}

"A *"
shared class Star(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("*", ls, cs, le, ce) {}

"A ="
shared class Eq(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("=", ls, cs, le, ce) {}

"A =>"
shared class DArrow(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("=>", ls, cs, le, ce) {}

"A ;"
shared class Semicolon(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation(";", ls, cs, le, ce) {}

"A ->"
shared class Arrow(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("->", ls, cs, le, ce) {}

"An ellipsis"
shared class Ellipsis(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("...", ls, cs, le, ce) {}

"A !"
shared class Bang(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("!", ls, cs, le, ce) {}

"A `"
shared class Tick(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("`", ls, cs, le, ce) {}

"A \``"
shared class TickTick(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("\`\`", ls, cs, le, ce) {}

"A ?."
shared class QDot(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("?.", ls, cs, le, ce) {}

"A *."
shared class SDot(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Punctuation("*.", ls, cs, le, ce) {}

"'super'"
shared class SuperTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("super", ls, cs, le, ce) {}

"'in'"
shared class In(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("in", ls, cs, le, ce) {}

"'out'"
shared class Out(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("out", ls, cs, le, ce) {}

"'extends'"
shared class Extends(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("extends", ls, cs, le, ce) {}

"'satisfies'"
shared class Satisfies(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("satisfies", ls, cs, le, ce) {}

"'of'"
shared class Of(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("of", ls, cs, le, ce) {}

"'given'"
shared class Given(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("given", ls, cs, le, ce) {}

"'import'"
shared class ImportTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("import", ls, cs, le, ce) {}

"'dynamic'"
shared class Dynamic(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("dynamic", ls, cs, le, ce) {}

"'value'"
shared class Value(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("value", ls, cs, le, ce) {}

"'new'"
shared class New(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("new", ls, cs, le, ce) {}

"'void'"
shared class Void(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("void", ls, cs, le, ce) {}

"'interface'"
shared class Interface(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("interface", ls, cs, le, ce) {}

"'class'"
shared class ClassTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("class", ls, cs, le, ce) {}

"'alias'"
shared class Alias(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("alias", ls, cs, le, ce) {}

"'return'"
shared class ReturnTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("return", ls, cs, le, ce) {}

"'throw'"
shared class ThrowTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("throw", ls, cs, le, ce) {}

"'break'"
shared class BreakTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("break", ls, cs, le, ce) {}

"'continue'"
shared class ContinueTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("continue", ls, cs, le, ce) {}

"'this'"
shared class ThisTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("this", ls, cs, le, ce) {}

"'is'"
shared class Is(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("is", ls, cs, le, ce) {}

"'exists'"
shared class Exists(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("exists", ls, cs, le, ce) {}

"'nonempty'"
shared class Nonempty(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("nonempty", ls, cs, le, ce) {}

"'if'"
shared class IfTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("if", ls, cs, le, ce) {}

"'else'"
shared class ElseTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("else", ls, cs, le, ce) {}

"'switch'"
shared class SwitchTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("switch", ls, cs, le, ce) {}

"'case'"
shared class CaseTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("case", ls, cs, le, ce) {}

"'for'"
shared class ForTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("for", ls, cs, le, ce) {}

"'while'"
shared class WhileTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("while", ls, cs, le, ce) {}

"'try'"
shared class TryTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("try", ls, cs, le, ce) {}

"'catch'"
shared class CatchTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("catch", ls, cs, le, ce) {}

"'finally'"
shared class FinallyTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("finally", ls, cs, le, ce) {}

"'assert'"
shared class AssertTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("assert", ls, cs, le, ce) {}

"'outer'"
shared class OuterTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("outer", ls, cs, le, ce) {}

"'package'"
shared class PackageTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("package", ls, cs, le, ce) {}

"'function'"
shared class FunctionTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("function", ls, cs, le, ce) {}

"'object'"
shared class ObjectTok(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends Keyword("object", ls, cs, le, ce) {}

"Text of an LIdentifier"
shared class LIdentText(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(text, ls, cs, le, ce) {}

"A decimal digit"
shared class Digit(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(text, ls, cs, le, ce) {}

"A hexadecimal digit"
shared class HexDigit(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(text, ls, cs, le, ce) {}

"A binary digit"
shared class BinDigit(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(text, ls, cs, le, ce) {}

"A magnitude suffix"
shared class Magnitude(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(text, ls, cs, le, ce) {}

"A fractional magnitude suffix"
shared class Minitude(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(text, ls, cs, le, ce) {}

"Exponent marker"
shared class ExpMarker(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken("e", ls, cs, le, ce) {}

"A character literal"
shared class CharacterLiteralTok(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(text, ls, cs, le, ce) {}

"A string literal"
shared class StringLiteralTok(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(text, ls, cs, le, ce) {}

"A 'token' that may contain other tokens"
shared class CeylonMetaToken(shared CeylonToken+ subtokens)
        extends CeylonToken(subtokens*.text.fold("")((x,y) => x + y), subtokens.first.line_start,
                        subtokens.first.col_start, subtokens.last.line_start,
                        subtokens.last.col_start) {}

"A block comment"
shared class BlockComment(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A separator"
shared class Separator(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A string of decimal digits"
shared class Digits(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A string of decimal digits, left-aligned"
shared class FracDigits(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A string of three decimal digits after a _"
shared class DigitCluster(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A string of three decimal digits before a _"
shared class FracDigitCluster(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A string of hexadecimal digits"
shared class HexDigits(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A string of four hexadecimal digits"
shared class HexDigitCluster(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A string of two hexadecimal digits"
shared class HexDigitTwoCluster(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A string of binary digits"
shared class BinDigits(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A string of four binary digits"
shared class BinDigitCluster(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"An exponent postfix to a float"
shared class Exponent(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A . followed by a base type"
shared class QualifiedTypeSuffix(CeylonToken+ tokens)
        extends CeylonMetaToken(*tokens) {}

"A node that isn't part of the AST but simplifies rules"
shared class MetaNode<out NodeType>(shared [NodeType+] nodes,
        shared CeylonToken* tokens)
        given NodeType satisfies Node {
    shared actual Integer hash = nodes.hash ^ 2 + tokens.hash;

    shared actual Boolean equals(Object other) {
        if (type(other) != type(this)) { return false; }
        assert(is MetaNode<NodeType> other);

        return other.nodes == nodes && other.tokens == tokens;
    }
}

"A comma-separated list"
shared class CommaSepList<out NodeType>([NodeType+] nodes, CeylonToken* tokens)
        extends MetaNode<NodeType>(nodes, *tokens)
        given NodeType satisfies Node {}

"The bad token. Used to box up unparseable input"
shared class Crap(String contents, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(contents, ls, cs, le, ce) {}

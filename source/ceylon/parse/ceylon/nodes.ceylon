"Base class for Ceylon token objects."
shared class CeylonToken(shared Integer line_start, shared Integer col_start,
        shared Integer line_end, shared Integer col_end) {}

"A single-line comment"
shared class LineComment(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken(ls, cs, le, ce) {}

"Some whitespace"
shared class Whitespace(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken(ls, cs, le, ce) {}

"The start of a block comment"
shared class CommentStart(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken(ls, cs, le, ce) {}

"The end of a block comment"
shared class CommentEnd(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken(ls, cs, le, ce) {}

"Body text in a block comment"
shared class CommentBody(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken(ls, cs, le, ce) {}

"Escape for a UIdentifier"
shared class UIdentStart(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken(ls, cs, le, ce) {}

"Escape for an LIdentifier"
shared class LIdentStart(Integer ls, Integer cs, Integer le, Integer ce)
        extends CeylonToken(ls, cs, le, ce) {}

"A token that we need to store the text of for processing"
shared class CeylonTextToken(shared String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(ls, cs, le, ce) {}

"Text of a UIdentifier"
shared class UIdentText(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A +"
shared class Plus(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A -"
shared class Minus(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A #"
shared class HashMark(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A $"
shared class DollarMark(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A _"
shared class Underscore(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A ."
shared class Dot(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A |"
shared class Pipe(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A '"
shared class Quote(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A \""
shared class DoubleQuote(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"Text of an LIdentifier"
shared class LIdentText(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A decimal digit"
shared class Digit(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A hexadecimal digit"
shared class HexDigit(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A binary digit"
shared class BinDigit(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A binary digit"
shared class Magnitude(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A binary digit"
shared class Minitude(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A binary digit"
shared class ExpMarker(Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonToken(ls, cs, le, ce) {}

"A character literal"
shared class CharacterLiteralTok(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A string literal"
shared class StringLiteralTok(String text, Integer ls, Integer cs,
        Integer le, Integer ce)
        extends CeylonTextToken(text, ls, cs, le, ce) {}

"A 'token' that may contain other tokens"
shared class CeylonMetaToken(shared CeylonToken+ subtokens)
        extends CeylonToken(subtokens.first.line_start,
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

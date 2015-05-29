import ceylon.parse { Token, SOS, EOS, SOSToken, Atom }
import ceylon.language.meta.model { Class, Type }
import ceylon.collection { ArrayList }
import ceylon.parse.regular { ... }

"List of whitespace characters"
Character[] whitespaceChars = [ ' ', '\{FORM FEED (FF)}',
       '\{LINE FEED (LF)}', '\{CHARACTER TABULATION}',
       '\{CARRIAGE RETURN (CR)}'];

"List of reserved words"
String[] reservedWords = ["assembly", "module", "package", "import", "alias",
    "class", "interface", "object", "given", "value", "assign", "void",
    "function", "new", "of", "extends", "satisfies", "abstracts", "in", "out",
    "return", "break", "continue", "throw", "assert", "dynamic", "if", "else",
    "switch", "case", "for", "while", "try", "catch", "finally", "then", "let",
    "this", "outer", "super", "is", "exists", "nonempty"];

[[Atom, String] +] keywords = [
    [Atom(`Extends`), "extends"],
    [Atom(`SuperTok`), "super"],
    [Atom(`Satisfies`), "satisfies"],
    [Atom(`Of`), "of"],
    [Atom(`In`), "in"],
    [Atom(`Out`), "out"],
    [Atom(`Given`), "given"],
    [Atom(`ImportTok`), "import"],
    [Atom(`Dynamic`), "dynamic"],
    [Atom(`Void`), "void"],
    [Atom(`Interface`), "interface"],
    [Atom(`ClassTok`), "class"],
    [Atom(`ObjectTok`), "object"],
    [Atom(`Alias`), "alias"],
    [Atom(`Value`), "value"],
    [Atom(`New`), "new"],
    [Atom(`ReturnTok`), "return"],
    [Atom(`ThrowTok`), "throw"],
    [Atom(`BreakTok`), "break"],
    [Atom(`ContinueTok`), "continue"],
    [Atom(`ThisTok`), "this"],
    [Atom(`Is`), "is"],
    [Atom(`Exists`), "exists"],
    [Atom(`Nonempty`), "nonempty"],
    [Atom(`IfTok`), "if"],
    [Atom(`ElseTok`), "else"],
    [Atom(`SwitchTok`), "switch"],
    [Atom(`CaseTok`), "case"],
    [Atom(`ForTok`), "for"],
    [Atom(`WhileTok`), "while"],
    [Atom(`TryTok`), "try"],
    [Atom(`CatchTok`), "catch"],
    [Atom(`FinallyTok`), "finally"],
    [Atom(`AssertTok`), "assert"],
    [Atom(`OuterTok`), "outer"],
    [Atom(`PackageTok`), "package"],
    [Atom(`FunctionTok`), "function"],
    [Atom(`ThenTok`), "then"],
    [Atom(`LetTok`), "let"],
    [Atom(`ModuleTok`), "module"]
];

[[Atom, String] +] literals = [
    [Atom(`ParOpen`), "("],
    [Atom(`ParClose`), ")"],
    [Atom(`Comma`), ","],
    [Atom(`CurlOpen`), "{"],
    [Atom(`CurlClose`), "}"],
    [Atom(`Star`), "*"],
    [Atom(`Eq`), "="],
    [Atom(`Arrow`), "->"],
    [Atom(`Ellipsis`), "..."],
    [Atom(`DArrow`), "=>"],
    [Atom(`Semicolon`), ";"],
    [Atom(`TickTick`), "\``"],
    [Atom(`QDot`), "?."],
    [Atom(`SDot`), "*."],
    [Atom(`DotDot`), ".."],
    [Atom(`Colon`), ":"],
    [Atom(`PlusPlus`), "++"],
    [Atom(`MinusMinus`), "--"],
    [Atom(`Caret`), "^"],
    [Atom(`Tilde`), "~"],
    [Atom(`Slash`), "/"],
    [Atom(`Percent`), "%"],
    [Atom(`StarStar`), "**"],
    [Atom(`LTE`), "<="],
    [Atom(`GTE`), ">="],
    [Atom(`Spaceship`), "<=>"],
    [Atom(`AbsEq`), "=="],
    [Atom(`NEq`), "!="],
    [Atom(`Identical`), "==="],
    [Atom(`AndOp`), "&&"],
    [Atom(`OrOp`), "||"],
    [Atom(`PlusEq`), "+="],
    [Atom(`MinusEq`), "-="],
    [Atom(`StarEq`), "*="],
    [Atom(`SlashEq`), "/="],
    [Atom(`PercentEq`), "%="],
    [Atom(`AmpersandEq`), "&="],
    [Atom(`PipeEq`), "|="],
    [Atom(`TildeEq`), "~="],
    [Atom(`AndEq`), "&&="],
    [Atom(`OrEq`), "||="],
    [Atom(`Tick`), "`"],
    [Atom(`CommentStart`), "/*"],
    [Atom(`CommentEnd`), "*/"],
    [Atom(`UIdentStart`), "\\I"],
    [Atom(`LIdentStart`), "\\i"],
    [Atom(`HashMark`), "#"],
    [Atom(`DollarMark`), "$"],
    [Atom(`Underscore`), "_"],
    [Atom(`Plus`), "+"],
    [Atom(`Minus`), "-"],
    [Atom(`Dot`), "."],
    [Atom(`Quote`), "'"],
    [Atom(`DoubleQuote`), "\""],
    [Atom(`Pipe`), "|"],
    [Atom(`Ampersand`), "&"],
    [Atom(`LT`), "<"],
    [Atom(`GT`), ">"],
    [Atom(`Question`), "?"],
    [Atom(`SqOpen`), "["],
    [Atom(`SqClose`), "]"]
];

[[Atom, Boolean(Character&Object)] +] singles = [
    [Atom(`Digit`), (Character x) => x.digit],
    [Atom(`HexDigit`), (Character x) => x.digit || "abcdefABCDEF".contains(x)],
    [Atom(`BinDigit`), "01".contains],
    [Atom(`Magnitude`), "kMGTP".contains],
    [Atom(`Minitude`), "munpf".contains],
    [Atom(`ExpMarker`), "eE".contains]
];

[[Atom, Regular] +] expressions = [
    [Atom(`Whitespace`), any(whitespaceChars).atLeast(1)],
    [Atom(`LineComment`), lit("//").or(lit("#!")) + (not(any("\r\n")) + anyChar).zeroPlus ],
    [Atom(`CommentBody`), (not(lit("/*").or(lit("*/"))) + anyChar).zeroPlus ],
    [Atom(`UIdentText`), anyUpper + anyLetter.or(anyDigit).or(lit("_")).zeroPlus ],
    [Atom(`LIdentText`), anyLower + anyLetter.or(anyDigit).or(lit("_")).zeroPlus ],
    [Atom(`CharacterLiteralTok`), (not(lit("'")) + anyChar).or(lit("\\") + anyChar)],
    [Atom(`StringLiteralTok`), (not(lit("\"")) + anyChar).or(lit("\\") + anyChar).zeroPlus]
];

"Construct a TokenizerToken given a metamodel type argument"
Token<CeylonToken> getTokenizerToken(Atom t, String text, Integer position,
        Integer length, [Integer,Integer] startPos, Integer lsd=0) {
    assert(is Callable<Token<CeylonToken>, [String, Integer,
            Integer, [Integer,Integer], Integer]> cls = `class
            TokenizerToken`.apply<TokenizerToken<out CeylonToken>>(t.type));
    return cls(text, position, length, startPos, lsd);
}

"Our starting tokenizer"
shared class Tokenizer(String t)
        extends BaseTokenizerToken<SOS>(t, 0, [1, 0])
        satisfies SOSToken {}

"A token from the parser's point of view"
shared abstract class BaseTokenizerToken<K>(shared String text, shared Integer dataLength,
        shared [Integer,Integer] startPosition, shared actual Integer lsd = 0)
        satisfies Token<K> given K of CeylonToken|SOS|EOS satisfies Object {

    shared [Integer,Integer] endPosition
        => [startPosition[0], startPosition[1] + dataLength];

    shared actual {Token<Object> *} next(Atom k) {
        value results = ArrayList<Token<Object>>();

        if (! this is Token<Keyword>) {
            for ([t, s] in keywords) {
                if (t.subtypeOf(k), text.includesAt(position, s)) {
                    results.add(getTokenizerToken(t, text, position + s.size,
                                s.size, endPosition));
                }
            }
        }

        for ([t, s] in literals) {
            if (t.subtypeOf(k),
                text.includesAt(position, s)) {
                results.add(getTokenizerToken(t, text, position + s.size,
                            s.size, endPosition));
            }
        }

        for ([t, f] in singles) {
            if (t.subtypeOf(k),
                exists c = text[position], f(c)) {
                results.add(getTokenizerToken(t, text, position + 1, 1,
                            endPosition));
            }
        }

        for ([t, e] in expressions) {
            if (t.subtypeOf(k), exists m = e.matchAt(position, text)) {
                results.add(getTokenizerToken(t, text, position + m.length,
                            m.length, endPosition));
            }
        }

        if (Atom(`EOS`).subtypeOf(k), text.size <= position) {
            results.add(TokenizerToken<EOS>(text, 0, 0, endPosition));
        }

        return results;
    }

    shared actual {Token<Object> *} forceNext(Atom k)
        => {};
}

class TokenizerToken<K>(String t, Integer p, Integer d, [Integer,Integer] s, Integer l = 0)
        extends BaseTokenizerToken<K>(t, d, s, l)
        given K of CeylonToken|EOS satisfies Object {
    shared actual Integer position = p;
    shared actual default K node {
        if (is Callable<K, [Integer,Integer,Integer,Integer]> t = `K`) {
            return t(startPosition[0], startPosition[1],
                    endPosition[0], endPosition[1]);
        } else {
            assert(is Callable<K, [String,Integer,Integer,Integer,Integer]> t =
                    `K`);
            return t(text[(position - dataLength) : dataLength],
                    startPosition[0], startPosition[1],
                    endPosition[0], endPosition[1]);
        }
    }
}

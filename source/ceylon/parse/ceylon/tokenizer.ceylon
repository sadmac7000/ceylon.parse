import ceylon.parse { Token, SOS, EOS, SOSToken }
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

[[Type, String] +] keywords = [
    [`Extends`, "extends"],
    [`SuperTok`, "super"],
    [`Satisfies`, "satisfies"],
    [`Of`, "of"],
    [`In`, "in"],
    [`Out`, "out"],
    [`Given`, "given"],
    [`ImportTok`, "import"],
    [`Dynamic`, "dynamic"],
    [`Void`, "void"],
    [`Interface`, "interface"],
    [`ClassTok`, "class"],
    [`ObjectTok`, "object"],
    [`Alias`, "alias"],
    [`Value`, "value"],
    [`New`, "new"],
    [`ReturnTok`, "return"],
    [`ThrowTok`, "throw"],
    [`BreakTok`, "break"],
    [`ContinueTok`, "continue"],
    [`ThisTok`, "this"],
    [`Is`, "is"],
    [`Exists`, "exists"],
    [`Nonempty`, "nonempty"],
    [`IfTok`, "if"],
    [`ElseTok`, "else"],
    [`SwitchTok`, "switch"],
    [`CaseTok`, "case"],
    [`ForTok`, "for"],
    [`WhileTok`, "while"],
    [`TryTok`, "try"],
    [`CatchTok`, "catch"],
    [`FinallyTok`, "finally"],
    [`AssertTok`, "assert"],
    [`OuterTok`, "outer"],
    [`PackageTok`, "package"],
    [`FunctionTok`, "function"],
    [`ThenTok`, "then"],
    [`LetTok`, "let"],
    [`ModuleTok`, "module"]
];

[[Type, String] +] literals = [
    [`ParOpen`, "("],
    [`ParClose`, ")"],
    [`Comma`, ","],
    [`CurlOpen`, "{"],
    [`CurlClose`, "}"],
    [`Star`, "*"],
    [`Eq`, "="],
    [`Arrow`, "->"],
    [`Ellipsis`, "..."],
    [`Ellipsis`, "..."],
    [`DArrow`, "=>"],
    [`Semicolon`, ";"],
    [`TickTick`, "\``"],
    [`QDot`, "?."],
    [`SDot`, "*."],
    [`DotDot`, ".."],
    [`Colon`, ":"],
    [`PlusPlus`, "++"],
    [`MinusMinus`, "--"],
    [`Caret`, "^"],
    [`Tilde`, "~"],
    [`Slash`, "/"],
    [`Percent`, "%"],
    [`StarStar`, "**"],
    [`LTE`, "<="],
    [`GTE`, ">="],
    [`Spaceship`, "<=>"],
    [`AbsEq`, "=="],
    [`NEq`, "!="],
    [`Identical`, "==="],
    [`AndOp`, "&&"],
    [`OrOp`, "||"],
    [`PlusEq`, "+="],
    [`MinusEq`, "-="],
    [`StarEq`, "*="],
    [`SlashEq`, "/="],
    [`PercentEq`, "%="],
    [`AmpersandEq`, "&="],
    [`PipeEq`, "|="],
    [`TildeEq`, "~="],
    [`AndEq`, "&&="],
    [`OrEq`, "||="],
    [`Tick`, "`"],
    [`CommentStart`, "/*"],
    [`CommentEnd`, "*/"],
    [`UIdentStart`, "\\I"],
    [`LIdentStart`, "\\i"],
    [`HashMark`, "#"],
    [`DollarMark`, "$"],
    [`Underscore`, "_"],
    [`Plus`, "+"],
    [`Minus`, "-"],
    [`Dot`, "."],
    [`Quote`, "'"],
    [`DoubleQuote`, "\""],
    [`Pipe`, "|"],
    [`Ampersand`, "&"],
    [`LT`, "<"],
    [`GT`, ">"],
    [`Question`, "?"],
    [`SqOpen`, "["],
    [`SqClose`, "]"]
];

[[Type, Boolean(Character&Object)] +] singles = [
    [`Digit`, (Character x) => x.digit],
    [`HexDigit`, (Character x) => x.digit || "abcdefABCDEF".contains(x)],
    [`BinDigit`, "01".contains],
    [`Magnitude`, "kMGTP".contains],
    [`Minitude`, "munpf".contains],
    [`ExpMarker`, "eE".contains]
];

[[Type, Regular<Character>] +] expressions = [
    [`Whitespace`, any(whitespaceChars).atLeast(1)],
    [`LineComment`, lit("//").or(lit("#!")) + (not(any("\r\n")) + anyChar).zeroPlus ],
    [`CommentBody`, (not(lit("/*").or(lit("*/"))) + anyChar).zeroPlus ],
    [`UIdentText`, anyUpper + anyLetter.or(anyDigit).or(lit("_")).zeroPlus ],
    [`LIdentText`, anyLower + anyLetter.or(anyDigit).or(lit("_")).zeroPlus ],
    [`CharacterLiteralTok`, (not(lit("'")) + anyChar).or(lit("\\") + anyChar)],
    [`StringLiteralTok`, (not(lit("\"")) + anyChar).or(lit("\\") + anyChar).zeroPlus]
];

"Construct a TokenizerToken given a metamodel type argument"
Token<K&Object> getTokenizerToken<K>(Type<K> t, String text, Integer position,
        Integer length, [Integer,Integer] startPos, Integer lsd=0) {
    assert(is Callable<Token<K&Object>, [String, Integer,
            Integer, [Integer,Integer], Integer]> cls = `class
            TokenizerToken`.apply<TokenizerToken<out CeylonToken>>(t));
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

    shared actual {Token<K&Object> *} next<K>(Type<K> k) {
        value results = ArrayList<Token<K&Object>>();

        if (! this is Token<Keyword>) {
            for ([t, s] in keywords) {
                if (t.subtypeOf(k), text.includesAt(position, s)) {
                    assert(is Type<K> t);
                    results.add(getTokenizerToken<K>(t, text, position + s.size,
                                s.size, endPosition));
                }
            }
        }

        for ([t, s] in literals) {
            if (t.subtypeOf(k),
                text.includesAt(position, s)) {
                assert(is Type<K> t);
                results.add(getTokenizerToken<K>(t, text, position + s.size,
                            s.size, endPosition));
            }
        }

        for ([t, f] in singles) {
            if (t.subtypeOf(k),
                exists c = text[position], f(c)) {
                assert(is Type<K> t);
                results.add(getTokenizerToken<K>(t, text, position + 1, 1,
                            endPosition));
            }
        }

        for ([t, e] in expressions) {
            if (t.subtypeOf(k), exists m = e.match(text[position...])) {
                assert(is Type<K> t);
                results.add(getTokenizerToken<K>(t, text, position + m.length,
                            m.length, endPosition));
            }
        }

        if (`EOS`.subtypeOf(k), text.size <= position) {
            assert(is Token<K&Object> obj =
                    TokenizerToken<EOS>(text, 0, 0, endPosition));
            results.add(obj);
        }

        return results;
    }

    shared actual {Token<K&Object> *} forceNext<K>(Type<K> k)
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

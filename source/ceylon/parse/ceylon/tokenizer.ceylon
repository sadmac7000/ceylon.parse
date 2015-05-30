import ceylon.parse { Token, SOS, EOS, SOSToken, Atom, eosAtom }
import ceylon.language.meta.model { Class, Type }
import ceylon.collection { ArrayList, HashMap }
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

Map<Atom,
    [Boolean(Character&Object)|String|Regular,
     TokenizerToken<out CeylonToken>(String, Integer, Integer,
             [Integer,Integer], Integer)]> tokens = HashMap{
    Atom(`Extends`) -> ["extends", TokenizerToken<Extends>],
    Atom(`SuperTok`) -> ["super", TokenizerToken<SuperTok>],
    Atom(`Satisfies`) -> ["satisfies", TokenizerToken<Satisfies>],
    Atom(`Of`) -> ["of", TokenizerToken<Of>],
    Atom(`In`) -> ["in", TokenizerToken<In>],
    Atom(`Out`) -> ["out", TokenizerToken<Out>],
    Atom(`Given`) -> ["given", TokenizerToken<Given>],
    Atom(`ImportTok`) -> ["import", TokenizerToken<ImportTok>],
    Atom(`Dynamic`) -> ["dynamic", TokenizerToken<Dynamic>],
    Atom(`Void`) -> ["void", TokenizerToken<Void>],
    Atom(`Interface`) -> ["interface", TokenizerToken<Interface>],
    Atom(`ClassTok`) -> ["class", TokenizerToken<ClassTok>],
    Atom(`ObjectTok`) -> ["object", TokenizerToken<ObjectTok>],
    Atom(`Alias`) -> ["alias", TokenizerToken<Alias>],
    Atom(`Value`) -> ["value", TokenizerToken<Value>],
    Atom(`New`) -> ["new", TokenizerToken<New>],
    Atom(`ReturnTok`) -> ["return", TokenizerToken<ReturnTok>],
    Atom(`ThrowTok`) -> ["throw", TokenizerToken<ThrowTok>],
    Atom(`BreakTok`) -> ["break", TokenizerToken<BreakTok>],
    Atom(`ContinueTok`) -> ["continue", TokenizerToken<ContinueTok>],
    Atom(`ThisTok`) -> ["this", TokenizerToken<ThisTok>],
    Atom(`Is`) -> ["is", TokenizerToken<Is>],
    Atom(`Exists`) -> ["exists", TokenizerToken<Exists>],
    Atom(`Nonempty`) -> ["nonempty", TokenizerToken<Nonempty>],
    Atom(`IfTok`) -> ["if", TokenizerToken<IfTok>],
    Atom(`ElseTok`) -> ["else", TokenizerToken<ElseTok>],
    Atom(`SwitchTok`) -> ["switch", TokenizerToken<SwitchTok>],
    Atom(`CaseTok`) -> ["case", TokenizerToken<CaseTok>],
    Atom(`ForTok`) -> ["for", TokenizerToken<ForTok>],
    Atom(`WhileTok`) -> ["while", TokenizerToken<WhileTok>],
    Atom(`TryTok`) -> ["try", TokenizerToken<TryTok>],
    Atom(`CatchTok`) -> ["catch", TokenizerToken<CatchTok>],
    Atom(`FinallyTok`) -> ["finally", TokenizerToken<FinallyTok>],
    Atom(`AssertTok`) -> ["assert", TokenizerToken<AssertTok>],
    Atom(`OuterTok`) -> ["outer", TokenizerToken<OuterTok>],
    Atom(`PackageTok`) -> ["package", TokenizerToken<PackageTok>],
    Atom(`FunctionTok`) -> ["function", TokenizerToken<FunctionTok>],
    Atom(`ThenTok`) -> ["then", TokenizerToken<ThenTok>],
    Atom(`LetTok`) -> ["let", TokenizerToken<LetTok>],
    Atom(`ModuleTok`) -> ["module", TokenizerToken<ModuleTok>],
    Atom(`ParOpen`) -> ["(", TokenizerToken<ParOpen>],
    Atom(`ParClose`) -> [")", TokenizerToken<ParClose>],
    Atom(`Comma`) -> [",", TokenizerToken<Comma>],
    Atom(`CurlOpen`) -> ["{", TokenizerToken<CurlOpen>],
    Atom(`CurlClose`) -> ["}", TokenizerToken<CurlClose>],
    Atom(`Star`) -> ["*", TokenizerToken<Star>],
    Atom(`Eq`) -> ["=", TokenizerToken<Eq>],
    Atom(`Arrow`) -> ["->", TokenizerToken<Arrow>],
    Atom(`Ellipsis`) -> ["...", TokenizerToken<Ellipsis>],
    Atom(`DArrow`) -> ["=>", TokenizerToken<DArrow>],
    Atom(`Semicolon`) -> [";", TokenizerToken<Semicolon>],
    Atom(`TickTick`) -> ["\``", TokenizerToken<TickTick>],
    Atom(`QDot`) -> ["?.", TokenizerToken<QDot>],
    Atom(`SDot`) -> ["*.", TokenizerToken<SDot>],
    Atom(`DotDot`) -> ["..", TokenizerToken<DotDot>],
    Atom(`Colon`) -> [":", TokenizerToken<Colon>],
    Atom(`PlusPlus`) -> ["++", TokenizerToken<PlusPlus>],
    Atom(`MinusMinus`) -> ["--", TokenizerToken<MinusMinus>],
    Atom(`Caret`) -> ["^", TokenizerToken<Caret>],
    Atom(`Tilde`) -> ["~", TokenizerToken<Tilde>],
    Atom(`Slash`) -> ["/", TokenizerToken<Slash>],
    Atom(`Percent`) -> ["%", TokenizerToken<Percent>],
    Atom(`StarStar`) -> ["**", TokenizerToken<StarStar>],
    Atom(`LTE`) -> ["<=", TokenizerToken<LTE>],
    Atom(`GTE`) -> [">=", TokenizerToken<GTE>],
    Atom(`Spaceship`) -> ["<=>", TokenizerToken<Spaceship>],
    Atom(`AbsEq`) -> ["==", TokenizerToken<AbsEq>],
    Atom(`NEq`) -> ["!=", TokenizerToken<NEq>],
    Atom(`Identical`) -> ["===", TokenizerToken<Identical>],
    Atom(`AndOp`) -> ["&&", TokenizerToken<AndOp>],
    Atom(`OrOp`) -> ["||", TokenizerToken<OrOp>],
    Atom(`PlusEq`) -> ["+=", TokenizerToken<PlusEq>],
    Atom(`MinusEq`) -> ["-=", TokenizerToken<MinusEq>],
    Atom(`StarEq`) -> ["*=", TokenizerToken<StarEq>],
    Atom(`SlashEq`) -> ["/=", TokenizerToken<SlashEq>],
    Atom(`PercentEq`) -> ["%=", TokenizerToken<PercentEq>],
    Atom(`AmpersandEq`) -> ["&=", TokenizerToken<AmpersandEq>],
    Atom(`PipeEq`) -> ["|=", TokenizerToken<PipeEq>],
    Atom(`TildeEq`) -> ["~=", TokenizerToken<TildeEq>],
    Atom(`AndEq`) -> ["&&=", TokenizerToken<AndEq>],
    Atom(`OrEq`) -> ["||=", TokenizerToken<OrEq>],
    Atom(`Tick`) -> ["`", TokenizerToken<Tick>],
    Atom(`CommentStart`) -> ["/*", TokenizerToken<CommentStart>],
    Atom(`CommentEnd`) -> ["*/", TokenizerToken<CommentEnd>],
    Atom(`UIdentStart`) -> ["\\I", TokenizerToken<UIdentStart>],
    Atom(`LIdentStart`) -> ["\\i", TokenizerToken<LIdentStart>],
    Atom(`HashMark`) -> ["#", TokenizerToken<HashMark>],
    Atom(`DollarMark`) -> ["$", TokenizerToken<DollarMark>],
    Atom(`Underscore`) -> ["_", TokenizerToken<Underscore>],
    Atom(`Plus`) -> ["+", TokenizerToken<Plus>],
    Atom(`Minus`) -> ["-", TokenizerToken<Minus>],
    Atom(`Dot`) -> [".", TokenizerToken<Dot>],
    Atom(`Quote`) -> ["'", TokenizerToken<Quote>],
    Atom(`DoubleQuote`) -> ["\"", TokenizerToken<DoubleQuote>],
    Atom(`Pipe`) -> ["|", TokenizerToken<Pipe>],
    Atom(`Ampersand`) -> ["&", TokenizerToken<Ampersand>],
    Atom(`LT`) -> ["<", TokenizerToken<LT>],
    Atom(`GT`) -> [">", TokenizerToken<GT>],
    Atom(`Question`) -> ["?", TokenizerToken<Question>],
    Atom(`SqOpen`) -> ["[", TokenizerToken<SqOpen>],
    Atom(`SqClose`) -> ["]", TokenizerToken<SqClose>],
    Atom(`Digit`) -> [(Character x) => x.digit, TokenizerToken<Digit>],
    Atom(`HexDigit`) -> [(Character x) => x.digit || "abcdefABCDEF".contains(x), TokenizerToken<HexDigit>],
    Atom(`BinDigit`) -> ["01".contains, TokenizerToken<BinDigit>],
    Atom(`Magnitude`) -> ["kMGTP".contains, TokenizerToken<Magnitude>],
    Atom(`Minitude`) -> ["munpf".contains, TokenizerToken<Minitude>],
    Atom(`ExpMarker`) -> ["eE".contains, TokenizerToken<ExpMarker>],
    Atom(`Whitespace`) -> [any(whitespaceChars).atLeast(1), TokenizerToken<Whitespace>],
    Atom(`LineComment`) -> [lit("//").or(lit("#!")) + (not(any("\r\n")) + anyChar).zeroPlus, TokenizerToken<LineComment>],
    Atom(`CommentBody`) -> [(not(lit("/*").or(lit("*/"))) + anyChar).zeroPlus, TokenizerToken<CommentBody>],
    Atom(`UIdentText`) -> [anyUpper + anyLetter.or(anyDigit).or(lit("_")).zeroPlus, TokenizerToken<UIdentText>],
    Atom(`LIdentText`) -> [anyLower + anyLetter.or(anyDigit).or(lit("_")).zeroPlus, TokenizerToken<LIdentText>],
    Atom(`CharacterLiteralTok`) -> [(not(lit("'")) + anyChar).or(lit("\\") + anyChar), TokenizerToken<CharacterLiteralTok>],
    Atom(`StringLiteralTok`) -> [(not(lit("\"")) + anyChar).or(lit("\\") + anyChar).zeroPlus, TokenizerToken<StringLiteralTok>]
};

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

        for (t in k.subtypes) {
            value info = tokens[t];

            if (! exists info) { continue; }
            assert(exists info);
            value [s, make] = info;

            if (is String s, text.includesAt(position, s)) {
                results.add(make(text, position + s.size, s.size, endPosition, 0));
            } else if (is Boolean(Object) s, exists c = text[position], s(c)) {
                results.add(make(text, position + 1, 1, endPosition, 0));
            } else if (is Regular s, exists m = s.matchAt(position, text)) {
                results.add(make(text, position + m.length, m.length, endPosition, 0));
            }
        }

        if (eosAtom.subtypeOf(k), text.size <= position) {
            results.add(TokenizerToken<EOS>(text, 0, 0, endPosition, 0));
        }

        return results;
    }

    shared actual {Token<Object> *} forceNext(Atom k)
        => {};
}

class TokenizerToken<K>(String t, Integer p, Integer d, [Integer,Integer] s, Integer l)
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

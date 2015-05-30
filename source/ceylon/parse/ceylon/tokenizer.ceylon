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

"Shortcut function: Allows you to write `tokenizerToken<Given>()` instead of `TokenizerToken<Given>`.
 Since that is a fully applied function call, the compiler no longer has to generate an anonymous `AbstractCallable` subclass at use-site."
TokenizerToken<out CeylonToken>(String, Integer, Integer, [Integer,Integer], Integer) tokenizerToken<Tok>()
        given Tok satisfies CeylonToken
        => TokenizerToken<Tok>;

Map<Atom,
    [Boolean(Character&Object)|String|Regular,
     TokenizerToken<out CeylonToken>(String, Integer, Integer,
             [Integer,Integer], Integer)]> tokens = HashMap{
    Atom(`Extends`) -> ["extends", tokenizerToken<Extends>()],
    Atom(`SuperTok`) -> ["super", tokenizerToken<SuperTok>()],
    Atom(`Satisfies`) -> ["satisfies", tokenizerToken<Satisfies>()],
    Atom(`Of`) -> ["of", tokenizerToken<Of>()],
    Atom(`In`) -> ["in", tokenizerToken<In>()],
    Atom(`Out`) -> ["out", tokenizerToken<Out>()],
    Atom(`Given`) -> ["given", tokenizerToken<Given>()],
    Atom(`ImportTok`) -> ["import", tokenizerToken<ImportTok>()],
    Atom(`Dynamic`) -> ["dynamic", tokenizerToken<Dynamic>()],
    Atom(`Void`) -> ["void", tokenizerToken<Void>()],
    Atom(`Interface`) -> ["interface", tokenizerToken<Interface>()],
    Atom(`ClassTok`) -> ["class", tokenizerToken<ClassTok>()],
    Atom(`ObjectTok`) -> ["object", tokenizerToken<ObjectTok>()],
    Atom(`Alias`) -> ["alias", tokenizerToken<Alias>()],
    Atom(`Value`) -> ["value", tokenizerToken<Value>()],
    Atom(`New`) -> ["new", tokenizerToken<New>()],
    Atom(`ReturnTok`) -> ["return", tokenizerToken<ReturnTok>()],
    Atom(`ThrowTok`) -> ["throw", tokenizerToken<ThrowTok>()],
    Atom(`BreakTok`) -> ["break", tokenizerToken<BreakTok>()],
    Atom(`ContinueTok`) -> ["continue", tokenizerToken<ContinueTok>()],
    Atom(`ThisTok`) -> ["this", tokenizerToken<ThisTok>()],
    Atom(`Is`) -> ["is", tokenizerToken<Is>()],
    Atom(`Exists`) -> ["exists", tokenizerToken<Exists>()],
    Atom(`Nonempty`) -> ["nonempty", tokenizerToken<Nonempty>()],
    Atom(`IfTok`) -> ["if", tokenizerToken<IfTok>()],
    Atom(`ElseTok`) -> ["else", tokenizerToken<ElseTok>()],
    Atom(`SwitchTok`) -> ["switch", tokenizerToken<SwitchTok>()],
    Atom(`CaseTok`) -> ["case", tokenizerToken<CaseTok>()],
    Atom(`ForTok`) -> ["for", tokenizerToken<ForTok>()],
    Atom(`WhileTok`) -> ["while", tokenizerToken<WhileTok>()],
    Atom(`TryTok`) -> ["try", tokenizerToken<TryTok>()],
    Atom(`CatchTok`) -> ["catch", tokenizerToken<CatchTok>()],
    Atom(`FinallyTok`) -> ["finally", tokenizerToken<FinallyTok>()],
    Atom(`AssertTok`) -> ["assert", tokenizerToken<AssertTok>()],
    Atom(`OuterTok`) -> ["outer", tokenizerToken<OuterTok>()],
    Atom(`PackageTok`) -> ["package", tokenizerToken<PackageTok>()],
    Atom(`FunctionTok`) -> ["function", tokenizerToken<FunctionTok>()],
    Atom(`ThenTok`) -> ["then", tokenizerToken<ThenTok>()],
    Atom(`LetTok`) -> ["let", tokenizerToken<LetTok>()],
    Atom(`ModuleTok`) -> ["module", tokenizerToken<ModuleTok>()],
    Atom(`ParOpen`) -> ["(", tokenizerToken<ParOpen>()],
    Atom(`ParClose`) -> [")", tokenizerToken<ParClose>()],
    Atom(`Comma`) -> [",", tokenizerToken<Comma>()],
    Atom(`CurlOpen`) -> ["{", tokenizerToken<CurlOpen>()],
    Atom(`CurlClose`) -> ["}", tokenizerToken<CurlClose>()],
    Atom(`Star`) -> ["*", tokenizerToken<Star>()],
    Atom(`Eq`) -> ["=", tokenizerToken<Eq>()],
    Atom(`Arrow`) -> ["->", tokenizerToken<Arrow>()],
    Atom(`Ellipsis`) -> ["...", tokenizerToken<Ellipsis>()],
    Atom(`DArrow`) -> ["=>", tokenizerToken<DArrow>()],
    Atom(`Semicolon`) -> [";", tokenizerToken<Semicolon>()],
    Atom(`TickTick`) -> ["\``", tokenizerToken<TickTick>()],
    Atom(`QDot`) -> ["?.", tokenizerToken<QDot>()],
    Atom(`SDot`) -> ["*.", tokenizerToken<SDot>()],
    Atom(`DotDot`) -> ["..", tokenizerToken<DotDot>()],
    Atom(`Colon`) -> [":", tokenizerToken<Colon>()],
    Atom(`PlusPlus`) -> ["++", tokenizerToken<PlusPlus>()],
    Atom(`MinusMinus`) -> ["--", tokenizerToken<MinusMinus>()],
    Atom(`Caret`) -> ["^", tokenizerToken<Caret>()],
    Atom(`Tilde`) -> ["~", tokenizerToken<Tilde>()],
    Atom(`Slash`) -> ["/", tokenizerToken<Slash>()],
    Atom(`Percent`) -> ["%", tokenizerToken<Percent>()],
    Atom(`StarStar`) -> ["**", tokenizerToken<StarStar>()],
    Atom(`LTE`) -> ["<=", tokenizerToken<LTE>()],
    Atom(`GTE`) -> [">=", tokenizerToken<GTE>()],
    Atom(`Spaceship`) -> ["<=>", tokenizerToken<Spaceship>()],
    Atom(`AbsEq`) -> ["==", tokenizerToken<AbsEq>()],
    Atom(`NEq`) -> ["!=", tokenizerToken<NEq>()],
    Atom(`Identical`) -> ["===", tokenizerToken<Identical>()],
    Atom(`AndOp`) -> ["&&", tokenizerToken<AndOp>()],
    Atom(`OrOp`) -> ["||", tokenizerToken<OrOp>()],
    Atom(`PlusEq`) -> ["+=", tokenizerToken<PlusEq>()],
    Atom(`MinusEq`) -> ["-=", tokenizerToken<MinusEq>()],
    Atom(`StarEq`) -> ["*=", tokenizerToken<StarEq>()],
    Atom(`SlashEq`) -> ["/=", tokenizerToken<SlashEq>()],
    Atom(`PercentEq`) -> ["%=", tokenizerToken<PercentEq>()],
    Atom(`AmpersandEq`) -> ["&=", tokenizerToken<AmpersandEq>()],
    Atom(`PipeEq`) -> ["|=", tokenizerToken<PipeEq>()],
    Atom(`TildeEq`) -> ["~=", tokenizerToken<TildeEq>()],
    Atom(`AndEq`) -> ["&&=", tokenizerToken<AndEq>()],
    Atom(`OrEq`) -> ["||=", tokenizerToken<OrEq>()],
    Atom(`Tick`) -> ["`", tokenizerToken<Tick>()],
    Atom(`CommentStart`) -> ["/*", tokenizerToken<CommentStart>()],
    Atom(`CommentEnd`) -> ["*/", tokenizerToken<CommentEnd>()],
    Atom(`UIdentStart`) -> ["\\I", tokenizerToken<UIdentStart>()],
    Atom(`LIdentStart`) -> ["\\i", tokenizerToken<LIdentStart>()],
    Atom(`HashMark`) -> ["#", tokenizerToken<HashMark>()],
    Atom(`DollarMark`) -> ["$", tokenizerToken<DollarMark>()],
    Atom(`Underscore`) -> ["_", tokenizerToken<Underscore>()],
    Atom(`Plus`) -> ["+", tokenizerToken<Plus>()],
    Atom(`Minus`) -> ["-", tokenizerToken<Minus>()],
    Atom(`Dot`) -> [".", tokenizerToken<Dot>()],
    Atom(`Quote`) -> ["'", tokenizerToken<Quote>()],
    Atom(`DoubleQuote`) -> ["\"", tokenizerToken<DoubleQuote>()],
    Atom(`Pipe`) -> ["|", tokenizerToken<Pipe>()],
    Atom(`Ampersand`) -> ["&", tokenizerToken<Ampersand>()],
    Atom(`LT`) -> ["<", tokenizerToken<LT>()],
    Atom(`GT`) -> [">", tokenizerToken<GT>()],
    Atom(`Question`) -> ["?", tokenizerToken<Question>()],
    Atom(`SqOpen`) -> ["[", tokenizerToken<SqOpen>()],
    Atom(`SqClose`) -> ["]", tokenizerToken<SqClose>()],
    Atom(`Digit`) -> [(Character x) => x.digit, tokenizerToken<Digit>()],
    Atom(`HexDigit`) -> [(Character x) => x.digit || "abcdefABCDEF".contains(x), tokenizerToken<HexDigit>()],
    Atom(`BinDigit`) -> ["01".contains, tokenizerToken<BinDigit>()],
    Atom(`Magnitude`) -> ["kMGTP".contains, tokenizerToken<Magnitude>()],
    Atom(`Minitude`) -> ["munpf".contains, tokenizerToken<Minitude>()],
    Atom(`ExpMarker`) -> ["eE".contains, tokenizerToken<ExpMarker>()],
    Atom(`Whitespace`) -> [any(whitespaceChars).atLeast(1), tokenizerToken<Whitespace>()],
    Atom(`LineComment`) -> [lit("//").or(lit("#!")) + (not(any("\r\n")) + anyChar).zeroPlus, tokenizerToken<LineComment>()],
    Atom(`CommentBody`) -> [(not(lit("/*").or(lit("*/"))) + anyChar).zeroPlus, tokenizerToken<CommentBody>()],
    Atom(`UIdentText`) -> [anyUpper + anyLetter.or(anyDigit).or(lit("_")).zeroPlus, tokenizerToken<UIdentText>()],
    Atom(`LIdentText`) -> [anyLower + anyLetter.or(anyDigit).or(lit("_")).zeroPlus, tokenizerToken<LIdentText>()],
    Atom(`CharacterLiteralTok`) -> [(not(lit("'")) + anyChar).or(lit("\\") + anyChar), tokenizerToken<CharacterLiteralTok>()],
    Atom(`StringLiteralTok`) -> [(not(lit("\"")) + anyChar).or(lit("\\") + anyChar).zeroPlus, tokenizerToken<StringLiteralTok>()]
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

import ceylon.parse { Token, SOS, EOS, SOSToken, Atom, eosAtom, sosAtom }
import ceylon.language.meta.model { Class, Type }
import ceylon.collection { ArrayList, MutableMap, HashMap }
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
TokenizerToken<out CeylonToken>(MutableMap<[Atom,String,Integer], {Token<Object> *}>,
        Atom, String, Integer, Integer, [Integer,Integer], Integer) tokenizerToken<Tok>()
        given Tok satisfies CeylonToken
        => TokenizerToken<Tok>;

"Shortcut function: Allows you to write `tokenizerTuple<Given>(\"given\")` instead of `[\"given\", tokenizerToken<Given>()]`.
 This means that less code has to be generated at use-site."
[String,TokenizerToken<out CeylonToken>(MutableMap<[Atom,String,Integer], {Token<Object> *}>,
        Atom, String, Integer, Integer, [Integer,Integer], Integer)] tokenizerTuple<Tok>(String token)
        given Tok satisfies CeylonToken
        => [token, tokenizerToken<Tok>()];

"Shortcut function: Allows you to write `tokenizerEntry<Given>(\"given\")` instead of `\` Atom(`Given`) -> tokenizerTuple<Given>(\"given\") `\`,
 or, as full form:
 ~~~
 Atom(`Given`) -> [\"given\", TokenizerToken<Given>]
 ~~~
 This means that the entry does not have to be reified at use-site, removing a huge amount of type descriptor generation code."
Atom->[String,TokenizerToken<out CeylonToken>(MutableMap<[Atom,String,Integer], {Token<Object> *}>,
        Atom, String, Integer, Integer, [Integer,Integer], Integer)] tokenizerEntry<Tok>(String token)
        given Tok satisfies CeylonToken
        => Atom(`Tok`) -> [token, tokenizerToken<Tok>()];

Map<Atom,
    [Boolean(Character&Object)|String|Regular,
     TokenizerToken<out CeylonToken>(MutableMap<[Atom,String,Integer], {Token<Object> *}>,
             Atom, String, Integer, Integer,
             [Integer,Integer], Integer)]> tokens = HashMap{
    tokenizerEntry<Extends>("extends"),
    tokenizerEntry<SuperTok>("super"),
    tokenizerEntry<Satisfies>("satisfies"),
    tokenizerEntry<Of>("of"),
    tokenizerEntry<In>("in"),
    tokenizerEntry<Out>("out"),
    tokenizerEntry<Given>("given"),
    tokenizerEntry<ImportTok>("import"),
    tokenizerEntry<Dynamic>("dynamic"),
    tokenizerEntry<Void>("void"),
    tokenizerEntry<Interface>("interface"),
    tokenizerEntry<ClassTok>("class"),
    tokenizerEntry<ObjectTok>("object"),
    tokenizerEntry<Alias>("alias"),
    tokenizerEntry<Value>("value"),
    tokenizerEntry<New>("new"),
    tokenizerEntry<ReturnTok>("return"),
    tokenizerEntry<ThrowTok>("throw"),
    tokenizerEntry<BreakTok>("break"),
    tokenizerEntry<ContinueTok>("continue"),
    tokenizerEntry<ThisTok>("this"),
    tokenizerEntry<Is>("is"),
    tokenizerEntry<Exists>("exists"),
    tokenizerEntry<Nonempty>("nonempty"),
    tokenizerEntry<IfTok>("if"),
    tokenizerEntry<ElseTok>("else"),
    tokenizerEntry<SwitchTok>("switch"),
    tokenizerEntry<CaseTok>("case"),
    tokenizerEntry<ForTok>("for"),
    tokenizerEntry<WhileTok>("while"),
    tokenizerEntry<TryTok>("try"),
    tokenizerEntry<CatchTok>("catch"),
    tokenizerEntry<FinallyTok>("finally"),
    tokenizerEntry<AssertTok>("assert"),
    tokenizerEntry<OuterTok>("outer"),
    tokenizerEntry<PackageTok>("package"),
    tokenizerEntry<FunctionTok>("function"),
    tokenizerEntry<ThenTok>("then"),
    tokenizerEntry<LetTok>("let"),
    tokenizerEntry<ModuleTok>("module"),
    tokenizerEntry<ParOpen>("("),
    tokenizerEntry<ParClose>(")"),
    tokenizerEntry<Comma>(","),
    tokenizerEntry<CurlOpen>("{"),
    tokenizerEntry<CurlClose>("}"),
    tokenizerEntry<Star>("*"),
    tokenizerEntry<Eq>("="),
    tokenizerEntry<Arrow>("->"),
    tokenizerEntry<Ellipsis>("..."),
    tokenizerEntry<DArrow>("=>"),
    tokenizerEntry<Semicolon>(";"),
    tokenizerEntry<TickTick>("\``"),
    tokenizerEntry<QDot>("?."),
    tokenizerEntry<SDot>("*."),
    tokenizerEntry<DotDot>(".."),
    tokenizerEntry<Colon>(":"),
    tokenizerEntry<PlusPlus>("++"),
    tokenizerEntry<MinusMinus>("--"),
    tokenizerEntry<Caret>("^"),
    tokenizerEntry<Tilde>("~"),
    tokenizerEntry<Slash>("/"),
    tokenizerEntry<Percent>("%"),
    tokenizerEntry<StarStar>("**"),
    tokenizerEntry<LTE>("<="),
    tokenizerEntry<GTE>(">="),
    tokenizerEntry<Spaceship>("<=>"),
    tokenizerEntry<AbsEq>("=="),
    tokenizerEntry<NEq>("!="),
    tokenizerEntry<Identical>("==="),
    tokenizerEntry<AndOp>("&&"),
    tokenizerEntry<OrOp>("||"),
    tokenizerEntry<PlusEq>("+="),
    tokenizerEntry<MinusEq>("-="),
    tokenizerEntry<StarEq>("*="),
    tokenizerEntry<SlashEq>("/="),
    tokenizerEntry<PercentEq>("%="),
    tokenizerEntry<AmpersandEq>("&="),
    tokenizerEntry<PipeEq>("|="),
    tokenizerEntry<TildeEq>("~="),
    tokenizerEntry<AndEq>("&&="),
    tokenizerEntry<OrEq>("||="),
    tokenizerEntry<Tick>("`"),
    tokenizerEntry<CommentStart>("/*"),
    tokenizerEntry<CommentEnd>("*/"),
    tokenizerEntry<UIdentStart>("\\I"),
    tokenizerEntry<LIdentStart>("\\i"),
    tokenizerEntry<HashMark>("#"),
    tokenizerEntry<DollarMark>("$"),
    tokenizerEntry<Underscore>("_"),
    tokenizerEntry<Plus>("+"),
    tokenizerEntry<Minus>("-"),
    tokenizerEntry<Dot>("."),
    tokenizerEntry<Quote>("'"),
    tokenizerEntry<DoubleQuote>("\""),
    tokenizerEntry<Pipe>("|"),
    tokenizerEntry<Ampersand>("&"),
    tokenizerEntry<LT>("<"),
    tokenizerEntry<GT>(">"),
    tokenizerEntry<Question>("?"),
    tokenizerEntry<SqOpen>("["),
    tokenizerEntry<SqClose>("]"),
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
        extends BaseTokenizerToken<SOS>(sosAtom, t, 0, [1, 0])
        satisfies SOSToken {
        shared actual MutableMap<[Atom,String,Integer], {Token<Object> *}> resultsCache
            = HashMap<[Atom,String,Integer], {Token<Object> *}>();
}


"A token from the parser's point of view"
shared abstract class BaseTokenizerToken<K>(shared actual Atom type, shared
        String text, shared Integer dataLength, shared [Integer,Integer]
        startPosition, shared actual Integer lsd = 0)
        satisfies Token<K> given K of CeylonToken|SOS|EOS satisfies Object {

    shared formal MutableMap<[Atom,String,Integer], {Token<Object> *}> resultsCache;

    shared [Integer,Integer] endPosition
        => [startPosition[0], startPosition[1] + dataLength];

    shared actual {Token<Object> *} next(Atom k) {
        value key = [k, text, position];
        value cached = resultsCache[key];
        if (exists cached) { return cached; }

        value results = ArrayList<Token<Object>>();

        for (t in k.subtypes) {
            value info = tokens[t];

            if (! exists info) { continue; }
            value [s, make] = info;

            if (is String s, text.includesAt(position, s)) {
                results.add(make(resultsCache, t, text, position + s.size, s.size, endPosition, 0));
            } else if (is Boolean(Object) s, exists c = text[position], s(c)) {
                results.add(make(resultsCache, t, text, position + 1, 1, endPosition, 0));
            } else if (is Regular s, exists m = s.matchAt(position, text)) {
                results.add(make(resultsCache, t, text, position + m.length, m.length, endPosition, 0));
            }
        }

        if (eosAtom.subtypeOf(k), text.size <= position) {
            results.add(TokenizerToken<EOS>(resultsCache, eosAtom, text, 0, 0, endPosition, 0));
        }

        resultsCache.put(key, results);
        return results;
    }

    shared actual {Token<Object> *} forceNext(Atom k)
        => {};
}

class TokenizerToken<K>(shared actual MutableMap<[Atom,String,Integer], {Token<Object> *}> resultsCache,
        Atom a, String t, Integer p, Integer d, [Integer,Integer] s, Integer l)
        extends BaseTokenizerToken<K>(a, t, d, s, l)
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

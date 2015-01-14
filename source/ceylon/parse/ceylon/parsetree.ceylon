import ceylon.parse { Grammar, Token, rule, tokenizer, lassoc }
import ceylon.language.meta.model { Class }
import ceylon.ast.core {
    AnyCompilationUnit,
    LIdentifier,
    UIdentifier,
    Key,
    ScopedKey,
    IntegerLiteral,
    CharacterLiteral,
    StringLiteral,
    UnionType,
    UnionableType,
    MainType,
    IntersectionType,
    PrimaryType,
    FloatLiteral
}

"AST Node key to attach individual tokens"
shared Key<CeylonToken[]> tokensKey = ScopedKey<CeylonToken[]>(`package
        ceylon.parse.ceylon`, "tokens");

"List of reserved words"
String[] reservedWords = ["assembly", "module", "package", "import", "alias",
    "class", "interface", "object", "given", "value", "assign", "void",
    "function", "new", "of", "extends", "satisfies", "abstracts", "in", "out",
    "return", "break", "continue", "throw", "assert", "dynamic", "if", "else",
    "switch", "case", "for", "while", "try", "catch", "finally", "then", "let",
    "this", "outer", "super", "is", "exists", "nonempty"];

"List of whitespace characters"
Character[] whitespaceChars = [ ' ', '\{FORM FEED (FF)}',
       '\{LINE FEED (LF)}', '\{CHARACTER TABULATION}',
       '\{CARRIAGE RETURN (CR)}'];

"Extract the ending line and column from an object which is assumed to be a
 CeylonToken, for use as a starting position for the next token"
[Integer, Integer] extractStartPos(Object? tok) {
    if (is CeylonToken tok) {
        return [tok.line_end, tok.col_end];
    }

    return [0, 0];
}

"Literal token"
Token<Type>? literal<Type>(String want, String input, Object? prev)
        given Type satisfies Object {
    value [start_line, start_col] = extractStartPos(prev);
    value t = `Type`;

    assert(is Class<Type, [Integer, Integer, Integer, Integer]> t);

    if (input.startsWith(want)) {
        return Token(t(start_line, start_col, start_line,
                    start_col + want.size), want.size);
    }

    return null;
}

"Calculate the ending line and column given the starting line and column and
 the intervening text"
[Integer, Integer] calculateStopPos(Integer start_line, Integer start_col,
        String text) {
    variable value line = start_line;
    variable value col = start_col;
    variable value i = 0;

    while (exists c = text[i]) {
        if (text[i...].startsWith("\r\n")) {
            i++; // We'll detect the linebreak later, just note that it's long
        }

        if (c == '\r' || c == '\n') {
            line++;
            col = 0;
        } else {
            col++;
        }

        i++; // Possibly the second increment if we're skipping \r\n
    }

    return [line, col];
}

"A parse tree for the Ceylon language"
by("Casey Dahlin")
object ceylonGrammar extends Grammar<AnyCompilationUnit, String>() {

    "Section 2.2 of the specification"
    tokenizer
    shared Token<Whitespace>? whitespace(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);
        variable value i = 0;

        while (exists c = input[i],
                whitespaceChars.contains(c)) {
            i++;
        }

        if (i == 0) { return null; }
        value [end_line, end_col] = calculateStopPos(start_line, start_col,
                string[0:i]);

        return Token(Whitespace(start_line, start_col, end_line, end_col), i);
    }

    "Section 2.2 of the specification"
    tokenizer
    shared Token<LineComment>? lineComment(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);
        if (! (input.startsWith("//") || input.startsWith("#!"))) {
            return null;
        }

        variable value i = 2;

        while (exists c = input[i], c != '\r', c != '\n') { i++; }

        return Token(LineComment(start_line, start_col, start_line, start_col +
                    i), i);
    }

    "Section 2.2 of the specification"
    tokenizer
    shared Token<CommentStart>? commentStart(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);
        if (input.startsWith("/*")) {
            return Token(CommentStart(start_line, start_col, start_line, start_col
                        + 2), 2);
        }

        return null;
    }

    "Section 2.2 of the specification"
    tokenizer
    shared Token<CommentEnd>? commentEnd(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);
        if (input.startsWith("*/")) {
            return Token(CommentEnd(start_line, start_col, start_line, start_col
                        + 2), 2);
        }

        return null;
    }

    "Section 2.2 of the specification"
    tokenizer
    shared Token<CommentBody>? commentBody(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);
        variable value i = 0;

        while (i < input.size) {
            if (input[i...].startsWith("/*")) { break; }
            if (input[i...].startsWith("*/")) { break; }
            i++;
        }

        if (i == 0) { return null; }
        value [end_line, end_col] = calculateStopPos(start_line, start_col,
                string[0:i]);

        return Token(CommentBody(start_line, start_col, end_line, end_col), i);
    }

    "Section 2.2 of the specification"
    rule
    shared BlockComment blockComment(CommentStart start,
            {CommentBody|BlockComment*} body, CommentEnd end) {
        return BlockComment(start, *(body.chain({end})));
    }

    "Section 2.2 of the specification"
    rule
    shared Separator separator({BlockComment|LineComment|Whitespace+}
            separator) {
        return Separator(*separator);
    }

    "Section 2.3 of the specification"
    tokenizer
    shared Token<UIdentStart>? uIdentStart(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);
        if (input.startsWith("\\I")) {
            return Token(UIdentStart(start_line, start_col, start_line, start_col +
                        2), 2);
        }

        return null;
    }

    "Section 2.3 of the specification"
    tokenizer
    shared Token<LIdentStart>? lIdentStart(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);
        if (input.startsWith("\\i")) {
            return Token(LIdentStart(start_line, start_col, start_line, start_col +
                        2), 2);
        }

        return null;
    }

    "Section 2.3 of the specification"
    tokenizer
    shared Token<UIdentText>? uIdentText(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);
        variable value i = 0;

        while (exists c = input[i], c.letter || (i > 0 && c.digit) || c == '_') { i++; }

        if (i == 0) { return null; }

        assert(exists c = input[0]);
        if (! c.uppercase) { return null; }

        return Token(UIdentText(input[0:i], start_line, start_col, start_line,
                    start_col + i), i);
    }

    "Section 2.3 of the specification"
    tokenizer
    shared Token<LIdentText>? lIdentText(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);
        variable value i = 0;

        while (exists c = input[i], c.letter || (i > 0 && c.digit) || c == '_') { i++; }

        if (i == 0) { return null; }

        assert(exists c = input[0]);
        if (! c.lowercase) { return null; }
        if (reservedWords.contains(input[0:i])) { return null; }

        return Token(LIdentText(input[0:i], start_line, start_col, start_line,
                    start_col + i), i);
    }

    "Section 2.3 of the specification"
    rule
    shared UIdentifier uident(Separator? ws, UIdentStart? start,
            UIdentText text) {
        value ret = UIdentifier(text.text);
        ret.put(tokensKey, [*{ws, start, text}.coalesced]);
        return ret;
    }

    "Section 2.3 of the specification"
    rule
    shared UIdentifier uidentEsc(Separator? ws, UIdentStart start,
            LIdentText text) {
        value ret = UIdentifier{text.text; usePrefix = true;};
        ret.put(tokensKey, [*{ws, start, text}.coalesced]);
        return ret;
    }

    "Section 2.3 of the specification"
    rule
    shared LIdentifier lident(Separator? ws, LIdentStart? start,
            LIdentText text) {
        value ret = LIdentifier(text.text);
        ret.put(tokensKey, [*{ws, start, text}.coalesced]);
        return ret;
    }

    "Section 2.3 of the specification"
    rule
    shared LIdentifier lidentEsc(Separator? ws, LIdentStart start,
            UIdentText text) {
        value ret = LIdentifier{text.text; usePrefix = true;};
        ret.put(tokensKey, [*{ws, start, text}.coalesced]);
        return ret;
    }

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<HashMark>? hashMark(String input, Object? prev)
            => literal("#", input, prev);

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<DollarMark>? dollarMark(String input, Object? prev)
            => literal("$", input, prev);

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Underscore>? underscore(String input, Object? prev)
            => literal("_", input, prev);

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Digit>? digit(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);

        if (exists c = input[0], c.digit) {
            return Token(Digit(input[0:1], start_line, start_col,
                        start_line, start_col + 1), 1);
        }

        return null;
    }

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<HexDigit>? hexDigit(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);

        if (exists c = input[0],
            c.digit || "abcdefABCDEF".contains(c)) {
            return Token(HexDigit(input[0:1], start_line, start_col,
                        start_line, start_col + 1), 1);
        }

        return null;
    }

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<BinDigit>? binDigit(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);

        if (exists c = input[0], "01".contains(c)) {
            return Token(BinDigit(input[0:1], start_line, start_col,
                        start_line, start_col + 1), 1);
        }

        return null;
    }

    "Section 2.4.1 of the specification"
    rule
    shared Digits digits({Digit+} items) => Digits(*items);

    "Section 2.4.1 of the specification"
    rule
    shared DigitCluster digitCluster(Underscore u, Digit a, Digit b,
            Digit c) => DigitCluster(u,a,b,c);

    "Section 2.4.1 of the specification"
    rule
    shared Digits clusteredDigits(Digit? a, Digit? b, Digit c,
            {DigitCluster+} clusters) {
        value toks = (clusters*.subtokens).reduce<{CeylonToken+}>((x,y) =>
                x.chain(y));
        value start = {a,b,c}.coalesced;

        return Digits(*start.chain(toks));
    }

    "Section 2.4.1 of the specification"
    rule
    shared FracDigitCluster fracDigitCluster(Digit a, Digit b, Digit c,
            Underscore u) => FracDigitCluster(a,b,c,u);

    "Section 2.4.1 of the specification"
    rule
    shared FracDigits fracDigits({FracDigitCluster+} clusters,
            Digit a, Digit? b, Digit? c) {
        value toks = (clusters*.subtokens).reduce<{CeylonToken+}>((x,y) =>
                x.chain(y));
        value end = {a,b,c}.coalesced;

        return FracDigits(*toks.chain(end));
    }

    "Section 2.4.1 of the specification"
    rule
    shared FracDigits unmarkedFracDigits({Digits+} digits) => FracDigits(*digits);


    "Section 2.4.1 of the specification"
    rule
    shared HexDigitCluster hexFourCluster(Underscore u, HexDigit a, HexDigit b,
            HexDigit c, HexDigit d) => HexDigitCluster(u,a,b,c,d);

    "Section 2.4.1 of the specification"
    rule
    shared HexDigitTwoCluster hexTwoCluster(Underscore u, HexDigit a,
            HexDigit b) => HexDigitTwoCluster(u,a,b);

    "Section 2.4.1 of the specification"
    rule
    shared BinDigitCluster binCluster(Underscore u, BinDigit a, BinDigit b,
            BinDigit c, BinDigit d) => BinDigitCluster(u,a,b,c,d);

    "Section 2.4.1 of the specification"
    rule
    shared BinDigits binDigits({BinDigit+} digits) => BinDigits(*digits);

    "Section 2.4.1 of the specification"
    rule
    shared BinDigits clusteredBinDigits(BinDigit? a, BinDigit? b, BinDigit? c,
            BinDigit d, {BinDigitCluster+} clusters) {
        value start = {a,b,c,d}.coalesced;
        value toks = (clusters*.subtokens).reduce<{CeylonToken+}>((x,y) =>
                x.chain(y));
        return BinDigits(*start.chain(toks));
    }

    "Section 2.4.1 of the specification"
    rule
    shared HexDigits hexDigits({HexDigit+} digits) => HexDigits(*digits);

    "Section 2.4.1 of the specification"
    rule
    shared HexDigits clusteredHexDigits(HexDigit? a, HexDigit? b, HexDigit? c,
            HexDigit d, {HexDigitCluster+} clusters) {
        value start = {a,b,c,d}.coalesced;
        value toks = (clusters*.subtokens).reduce<{CeylonToken+}>((x,y) =>
                x.chain(y));
        return HexDigits(*start.chain(toks));
    }

    "Section 2.4.1 of the specification"
    rule
    shared HexDigits twoClusteredHexDigits(HexDigit? a, HexDigit b,
            {HexDigitTwoCluster+} clusters) {
        value start = {a,b}.coalesced;
        value toks = (clusters*.subtokens).reduce<{CeylonToken+}>((x,y) =>
                x.chain(y));
        return HexDigits(*start.chain(toks));
    }

    "Section 2.4.1 of the specification"
    rule
    shared IntegerLiteral hexLiteral(Separator? s, HashMark h,
            {HexDigits+} digits) {
        value digit_tokens = { for (d in digits) for (t in d.subtokens) t };
        value text_bits = { for (t in digit_tokens) if (is HexDigit t)
            t.text };
        value text = text_bits.reduce<String>((x,y) => x + y);
        assert(exists text);
        value ret = IntegerLiteral("#" + text);

        if (exists s) {
            ret.put(tokensKey, [s, h, *digit_tokens]);
        } else {
            ret.put(tokensKey, [h, *digit_tokens]);
        }
        return ret;
    }

    "Section 2.4.1 of the specification"
    rule
    shared IntegerLiteral binLiteral(Separator? s, DollarMark h,
            {BinDigits+} digits) {
        value digit_tokens = { for (d in digits) for (t in d.subtokens) t };
        value text_bits = { for (t in digit_tokens) if (is BinDigit t)
            t.text };
        value text = text_bits.reduce<String>((x,y) => x + y);
        assert(exists text);
        value ret = IntegerLiteral("$" + text);

        if (exists s) {
            ret.put(tokensKey, [s, h, *digit_tokens]);
        } else {
            ret.put(tokensKey, [h, *digit_tokens]);
        }
        return ret;
    }

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Magnitude>? magnitude(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);

        if (exists c = input[0], "kMGTP".contains(c)) {
            return Token(Magnitude(input[0:1], start_line, start_col,
                        start_line, start_col + 1), 1);
        }

        return null;
    }

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Minitude>? minitude(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);

        if (exists c = input[0], "munpf".contains(c)) {
            return Token(Minitude(input[0:1], start_line, start_col,
                        start_line, start_col + 1), 1);
        }

        return null;
    }

    "Section 2.4.1 of the specification"
    rule
    shared IntegerLiteral decLiteral(Separator? s, {Digits+} digits,
            Magnitude? m) {
        value digit_tokens = { for (d in digits) for (t in d.subtokens) t };
        value text_bits = { for (t in digit_tokens) if (is Digit t)
            t.text };
        value text = text_bits.reduce<String>((x,y) => x + y);
        assert(exists text);

        String end;

        if (exists m) {
            end = m.text;
        } else {
            end = "";
        }

        value ret = IntegerLiteral(text + end);

        ret.put(tokensKey, [*{s, *digits}.chain({m}).coalesced]);
        return ret;
    }

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<ExpMarker>? expMarker(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);

        if (exists c = input[0], "eE".contains(c)) {
            return Token(ExpMarker(start_line, start_col,
                        start_line, start_col + 1), 1);
        }

        return null;
    }

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Plus>? plus(String input, Object? prev)
            => literal("+", input, prev);

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Minus>? minus(String input, Object? prev)
            => literal("-", input, prev);

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Dot>? dot(String input, Object? prev)
            => literal(".", input, prev);

    "Section 2.4.1 of the specification"
    rule
    shared Exponent exponent(ExpMarker e, Plus|Minus? s, {Digit+} digits)
            => Exponent(e, *{s, *digits}.coalesced);

    "Section 2.4.1 of the specification"
    rule
    shared FloatLiteral floatLiteral(Separator? s, {Digits+} digits, Dot dot,
            {FracDigits+} fracs, Magnitude|Minitude|Exponent? m) {
        value digit_tokens = { for (d in digits) for (t in d.subtokens) t };
        value frac_tokens = { for (d in fracs) for (t in d.subtokens) t };
        value text_bits = { for (t in digit_tokens) if (is Digit t)
            t.text }.chain({"."}).chain({ for (t in frac_tokens) if (is Digit
                        t) t.text });
        value text = text_bits.reduce<String>((x,y) => x + y);

        String end;

        if (is Magnitude|Minitude m) {
            end = m.text;
        } else if (is Exponent m) {
            value etext_bits = { for (t in m.subtokens) if (is CeylonTextToken
                    t) t.text };
            assert(exists e = etext_bits.reduce<String>((x,y) => x + y));

            variable Boolean neg = false;

            for (t in m.subtokens) {
                if (is Minus t) {
                    neg = true;
                    break;
                }
            }

            if (neg) {
                end = "e-" + e;
            } else {
                end = "e" + e;
            }
        } else {
            end = "";
        }

        value ret = FloatLiteral(text + end);

        ret.put(tokensKey, [*{s, *digits}.chain({dot, *fracs}).chain({m}).coalesced]);
        return ret;
    }

    "Section 2.4.1 of the specification"
    rule
    shared FloatLiteral shortcutFloatLiteral(Separator? s, {Digits+} digits,
            Minitude m) {
        value digit_tokens = { for (d in digits) for (t in d.subtokens) t };
        value text_bits = { for (t in digit_tokens) if (is Digit t) t.text };
        value text = text_bits.reduce<String>((x,y) => x + y);
        assert(exists text);

        value ret = FloatLiteral(text + m.text);
        ret.put(tokensKey, [*{s, *digits}.chain({m}).coalesced]);
        return ret;
    }

    "Section 2.4.2 of the specification"
    tokenizer
    shared Token<Quote>? quote(String input, Object? prev)
            => literal("'", input, prev);

    "Section 2.4.2 of the specification"
    tokenizer
    shared Token<CharacterLiteralTok>? characterLiteralTok(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);

        if (! input[0] exists) { return null; }

        variable value i = 0;
        variable value skip = false;

        while (exists c = input[0], c != "'" || skip) {
            skip = c == '\\' && !skip;
            i++;
        }

        if (! input[i] exists) { return null; }
        if (exists c = input[i], c != "'") { return null; }

        value [end_line, end_col] = calculateStopPos(start_line, start_col,
                input[0:i]);
        return Token(CharacterLiteralTok(input[0:i], start_line, start_col,
                    end_line, end_col), i);
    }

    "Section 2.4.2 of the specification"
    rule
    shared CharacterLiteral characterLiteral(Separator? s, Quote a,
            CharacterLiteralTok t, Quote b) {
        value ret = CharacterLiteral(t.text);
        ret.put(tokensKey, [*{s, a, t, b}.coalesced]);
        return ret;
    }

    "Section 2.4.3 of the specification"
    tokenizer
    shared Token<DoubleQuote>? doubleQuote(String input, Object? prev)
            => literal("\"", input, prev);

    "Section 2.4.3 of the specification"
    tokenizer
    shared Token<StringLiteralTok>? stringLiteralTok(String input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);

        if (! input[0] exists) { return null; }

        variable value i = 0;
        variable value skip = false;

        while (exists c = input[0], c != "\"" || skip) {
            skip = c == '\\' && !skip;
            i++;
        }

        if (! input[i] exists) { return null; }
        if (exists c = input[i], c != "\"") { return null; }

        value [end_line, end_col] = calculateStopPos(start_line, start_col,
                input[0:i]);
        return Token(StringLiteralTok(input[0:i], start_line, start_col,
                    end_line, end_col), i);
    }

    "Section 2.4.2 of the specification"
    rule
    shared StringLiteral stringLiteral(Separator? s, DoubleQuote a,
            StringLiteralTok t, DoubleQuote b) {
        value ret = StringLiteral(t.text);
        ret.put(tokensKey, [*{s, a, t, b}.coalesced]);
        return ret;
    }

    "Section 3.2.3 of the specification"
    tokenizer
    shared Token<Pipe>? pipe(String input, Object? prev)
            => literal("|", input, prev);

    "Section 3.2.3 of the specification"
    rule(0, lassoc)
    shared UnionType unionType(MainType a, Pipe p, MainType b) {
        [IntersectionType|PrimaryType+] left_children;
        [IntersectionType|PrimaryType+] right_children;

        if (is UnionType a) {
            left_children = a.children;
        } else {
            left_children = [a];
        }

        if (is UnionType b) {
            right_children = b.children;
        } else {
            right_children = [b];
        }

        value ret = UnionType(left_children.append(right_children));
        value left_toks = a.get(tokensKey);
        value right_toks = b.get(tokensKey);

        [CeylonToken*] a_toks;
        [CeylonToken*] b_toks;

        if (exists left_toks) {
            a_toks = left_toks;
        } else {
            a_toks = [];
        }

        if (exists right_toks) {
            b_toks = right_toks;
        } else {
            b_toks = [];
        }

        ret.put(tokensKey, a_toks.withTrailing(p).append(b_toks));
        return ret;
    }

    "Section 3.2.4 of the specification"
    tokenizer
    shared Token<Ampersand>? ampersand(String input, Object? prev)
            => literal("&", input, prev);

    "Section 3.2.4 of the specification"
    rule(0, lassoc)
    shared IntersectionType intersectionType(UnionableType a, Ampersand p,
            UnionableType b) {
        [PrimaryType+] left_children;
        [PrimaryType+] right_children;

        if (is IntersectionType a) {
            left_children = a.children;
        } else {
            left_children = [a];
        }

        if (is IntersectionType b) {
            right_children = b.children;
        } else {
            right_children = [b];
        }

        value ret = IntersectionType(left_children.append(right_children));
        value left_toks = a.get(tokensKey);
        value right_toks = b.get(tokensKey);

        [CeylonToken*] a_toks;
        [CeylonToken*] b_toks;

        if (exists left_toks) {
            a_toks = left_toks;
        } else {
            a_toks = [];
        }

        if (exists right_toks) {
            b_toks = right_toks;
        } else {
            b_toks = [];
        }

        ret.put(tokensKey, a_toks.withTrailing(p).append(b_toks));
        return ret;
    }
}

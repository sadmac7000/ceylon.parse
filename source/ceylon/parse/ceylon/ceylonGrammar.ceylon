import ceylon.parse { Grammar, Token, rule, omniRule, genericRule,
    tokenizer, lassoc, rassoc }
import ceylon.ast.core { ... }
import ceylon.collection { ArrayList }

"AST Node key to attach individual tokens"
shared Key<List<CeylonToken>> tokensKey = ScopedKey<List<CeylonToken>>(`package
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
Token<TypeArg>? literal<TypeArg>(Callable<TypeArg, [Integer,Integer,Integer,Integer]> t,
        List<Character> input, Object? prev, String+ wants)
        given TypeArg satisfies Object {
    value [start_line, start_col] = extractStartPos(prev);

    for (want in wants) {
        if (input.startsWith(want)) {
            return Token(t(start_line, start_col, start_line,
                        start_col + want.size), want.size);
        }
    }

    return null;
}

"Keyword token"
Token<TypeArg>? keyword<TypeArg>(Callable<TypeArg, [Integer,Integer,Integer,Integer]> t,
        List<Character> input, Object? prev, String wants)
        given TypeArg satisfies Object {
        value c = input[wants.size];

        if (exists c, c.letter || c.digit || c == '_') { return null; }
        return literal(t, input, prev, wants);
}

"Parse a single-character token"
Token<TypeArg>? takeCharToken<TypeArg>(Callable<TypeArg, [Integer, Integer, Integer,
        Integer]>|Callable<TypeArg, [List<Character>, Integer, Integer, Integer,
        Integer]> t, List<Character> input, Object? prev, Boolean(Character) test)
        given TypeArg satisfies Object {
    value [start_line, start_col] = extractStartPos(prev);
    value char = input[0];

    if (! exists char) { return null; }
    assert(exists char);

    if (! test(char)) { return null; }

    if (is Callable<TypeArg, [Integer, Integer, Integer, Integer]> t) {
        return Token(t(start_line, start_col, start_line, start_col + 1), 1);
    } else {
        return Token(t(input[0:1], start_line, start_col, start_line, start_col
                    + 1), 1);
    }
}

"Parse a token that consists of all characters at the head of the string for
 which the test function returns true."
Token<TypeArg>? takeTokenWhile<TypeArg>(Callable<TypeArg, [Integer, Integer,
        Integer, Integer]>|Callable<TypeArg, [List<Character>, Integer, Integer, Integer,
        Integer]> t, List<Character> input, Object? prev, Boolean(List<Character>)|Boolean(Character) test)
        given TypeArg satisfies Object {
    value [start_line, start_col] = extractStartPos(prev);

    variable value length = 0;

    if (is Boolean(List<Character>) test) {
        while (test(input[length...])) { length++; }
    } else {
        while (exists c = input[length], test(c)) { length++; }
    }

    value [end_line, end_col] = calculateStopPos(start_line, start_col,
            input[0:length]);

    if (length == 0) { return null; }

    if (is Callable<TypeArg, [Integer, Integer, Integer, Integer]> t) {
        return Token(t(start_line, start_col, end_line, end_col), length);
    } else {
        return Token(t(input[0:length], start_line, start_col, end_line,
                    end_col), length);
    }
}

"Meta token"
TypeArg meta<TypeArg>(Callable<TypeArg, [CeylonToken+]> t,
        CeylonToken|{CeylonToken|Node*}|Node?* children) {

    assert( is [CeylonToken+] toks = tokenStream(*children));

    return t(*toks);
}

variable Integer called = 0;

"AST Node"
NodeType astNode<NodeType, Arguments>(Callable<NodeType, Arguments> t,
        Arguments args, CeylonToken|{CeylonToken|Node*}|Node?* children)
        given NodeType satisfies Node
        given Arguments satisfies [Anything*] {
    value ret = t(*args);
    ret.put(tokensKey, tokenStream(*children));
    return ret;
}

"AST Text Node"
NodeType astTextNode<NodeType>(Callable<NodeType, [String]> t,
        CeylonToken|{CeylonToken|Node*}|Node?* children)
        given NodeType satisfies Node {
    value tstream = tokenStream(*children);
    value ret = t(tokenText(*tstream));
    ret.put(tokensKey, tstream);
    return ret;
}

"Text from a stream of tokens"
String tokenText(CeylonToken* token)
        =>(token*.text).fold("")((x,y)=>x+asString(y));

"Convert a character list to a string"
String asString(List<Character> l)
        => if (is String l) then l else String{*l};

"Extract all tokens from a series of arguments to a production"
List<CeylonToken> tokenStream(CeylonToken|{CeylonToken|Node*}|Node?* args) {
    value ret = ArrayList<CeylonToken>();

    for (arg in args) {
        if (! exists arg) {
            continue;
        } else if (is CeylonMetaToken arg) {
            ret.addAll(tokenStream(*arg.subtokens));
        } else if (is CeylonToken arg) {
            ret.add(arg);
        } else if (is {CeylonToken|Node*} arg) {
            ret.addAll(tokenStream(*arg));
        } else {
            assert(exists k = arg.get(tokensKey));
            ret.addAll(k);
        }
    }

    return ret;
}

"Calculate the ending line and column given the starting line and column and
 the intervening text"
[Integer, Integer] calculateStopPos(Integer start_line, Integer start_col,
        List<Character> text) {
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
shared object ceylonGrammar extends Grammar<Character>() {
    shared actual Crap badTokenConstructor(List<Character> data, Object? prev) {
        assert(is String data);
        value [start_line, start_col] = extractStartPos(prev);
        value [end_line, end_col] = calculateStopPos(start_line, start_col,
                data);

        return Crap(data, start_line, start_col, end_line, end_col);
    }

    "Section 2.2 of the specification"
    tokenizer
    shared Token<Whitespace>? whitespace(List<Character> input, Object? prev)
            => takeTokenWhile(Whitespace, input, prev,
                    (Character x) => whitespaceChars.contains(x));

    "Section 2.2 of the specification"
    tokenizer
    shared Token<LineComment>? lineComment(List<Character> input, Object? prev) {
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
    shared Token<CommentStart>? commentStart(List<Character> input, Object? prev)
            => literal(CommentStart, input, prev, "/*");

    "Section 2.2 of the specification"
    tokenizer
    shared Token<CommentEnd>? commentEnd(List<Character> input, Object? prev)
            => literal(CommentEnd, input, prev, "*/");

    "Section 2.2 of the specification"
    tokenizer
    shared Token<CommentBody>? commentBody(List<Character> input, Object? prev)
            => takeTokenWhile(CommentBody, input, prev,
                    (List<Character> x) => ! (x.startsWith("/*") || x.startsWith(
                            "*/") || x == ""));

    "Section 2.2 of the specification"
    rule
    shared BlockComment blockComment(CommentStart start,
            [CommentBody|BlockComment*] body, CommentEnd end)
            => meta(BlockComment, start, body, end);

    "Section 2.2 of the specification"
    omniRule(0, lassoc)
    shared AnySym separator<AnySym>(
            [BlockComment|LineComment|Whitespace+] before,
            AnySym sym)
            given AnySym of CeylonMetaToken|Keyword|Punctuation|LIdentifier|UIdentifier
            => sym;

    "Section 2.2 of the specification"
    rule
    shared AnyCompilationUnit trailingWs(AnyCompilationUnit ret,
            [BlockComment|LineComment|Whitespace+] after)
            => ret;

    "Section 2.3 of the specification"
    tokenizer
    shared Token<UIdentStart>? uIdentStart(List<Character> input, Object? prev)
            => literal(UIdentStart, input, prev, "\\I");

    "Section 2.3 of the specification"
    tokenizer
    shared Token<LIdentStart>? lIdentStart(List<Character> input, Object? prev)
            => literal(LIdentStart, input, prev, "\\i");

    "Section 2.3 of the specification"
    tokenizer
    shared Token<UIdentText>? uIdentText(List<Character> input, Object? prev) {
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
    shared Token<LIdentText>? lIdentText(List<Character> input, Object? prev) {
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
    shared UIdentifier uident(UIdentStart? start, UIdentText text)
            => astNode(UIdentifier, [text.text], start, text);

    "Section 2.3 of the specification"
    rule
    shared UIdentifier uidentEsc(UIdentStart start, LIdentText text)
            => astNode(UIdentifier, [text.text, true], start, text);

    "Section 2.3 of the specification"
    rule
    shared LIdentifier lident(LIdentStart? start, LIdentText text)
            => astNode(LIdentifier, [text.text], start, text);

    "Section 2.3 of the specification"
    rule
    shared LIdentifier lidentEsc(LIdentStart start,
            UIdentText text)
            => astNode(LIdentifier, [text.text, true], start, text);

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<HashMark>? hashMark(List<Character> input, Object? prev)
            => literal(HashMark, input, prev, "#");

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<DollarMark>? dollarMark(List<Character> input, Object? prev)
            => literal(DollarMark, input, prev, "$");

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Underscore>? underscore(List<Character> input, Object? prev)
            => literal(Underscore, input, prev, "_");

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Digit>? digit(List<Character> input, Object? prev)
            => takeCharToken(Digit, input, prev, (Character x) => x.digit);

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<HexDigit>? hexDigit(List<Character> input, Object? prev)
            => takeCharToken(HexDigit, input, prev, (x) => x.digit ||
                    "abcdefABCDEF".contains(x));

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<BinDigit>? binDigit(List<Character> input, Object? prev)
            => takeCharToken(BinDigit, input, prev, "01".contains);

    "Section 2.4.1 of the specification"
    rule
    shared Digits digits([Digit+] items) => Digits(*items);

    "Section 2.4.1 of the specification"
    rule
    shared DigitCluster digitCluster(Underscore u, Digit a, Digit b,
            Digit c) => DigitCluster(u,a,b,c);

    "Section 2.4.1 of the specification"
    rule
    shared Digits clusteredDigits(Digit? a, Digit? b, Digit c,
            [DigitCluster+] clusters)
            => meta(Digits, a, b, c, clusters);

    "Section 2.4.1 of the specification"
    rule
    shared FracDigitCluster fracDigitCluster(Digit a, Digit b, Digit c,
            Underscore u) => FracDigitCluster(a,b,c,u);

    "Section 2.4.1 of the specification"
    rule
    shared FracDigits fracDigits([FracDigitCluster+] clusters,
            Digit a, Digit? b, Digit? c)
            => meta(FracDigits, clusters, a, b, c);

    "Section 2.4.1 of the specification"
    rule
    shared FracDigits unmarkedFracDigits([Digits+] digits) => FracDigits(*digits);


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
    shared BinDigits binDigits([BinDigit+] digits) => BinDigits(*digits);

    "Section 2.4.1 of the specification"
    rule
    shared BinDigits clusteredBinDigits(BinDigit? a, BinDigit? b, BinDigit? c,
            BinDigit d, [BinDigitCluster+] clusters)
            => meta(BinDigits, a, b, c, d, clusters);

    "Section 2.4.1 of the specification"
    rule
    shared HexDigits hexDigits([HexDigit+] digits) => HexDigits(*digits);

    "Section 2.4.1 of the specification"
    rule
    shared HexDigits clusteredHexDigits(HexDigit? a, HexDigit? b, HexDigit? c,
            HexDigit d, [HexDigitCluster+] clusters)
            => meta(HexDigits, a, b, c, d, clusters);

    "Section 2.4.1 of the specification"
    rule
    shared HexDigits twoClusteredHexDigits(HexDigit? a, HexDigit b,
            [HexDigitTwoCluster+] clusters)
            => meta(HexDigits, a, b, clusters);

    "Section 2.4.1 of the specification"
    rule
    shared IntegerLiteral hexLiteral(HashMark h, [HexDigits+] digits)
            => astTextNode(IntegerLiteral, h, digits);

    "Section 2.4.1 of the specification"
    rule
    shared IntegerLiteral binLiteral(DollarMark h, [BinDigits+] digits)
            => astTextNode(IntegerLiteral, h, digits);

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Magnitude>? magnitude(List<Character> input, Object? prev)
            => takeCharToken(Magnitude, input, prev, "kMGTP".contains);

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Minitude>? minitude(List<Character> input, Object? prev)
            => takeCharToken(Minitude, input, prev, "munpf".contains);

    "Section 2.4.1 of the specification"
    rule
    shared IntegerLiteral decLiteral([Digits+] digits,
            Magnitude? m)
            => astTextNode(IntegerLiteral, digits, m);

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<ExpMarker>? expMarker(List<Character> input, Object? prev)
            => takeCharToken(ExpMarker, input, prev, "eE".contains);

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Plus>? plus(List<Character> input, Object? prev)
            => literal(Plus, input, prev, "+");

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Minus>? minus(List<Character> input, Object? prev)
            => literal(Minus, input, prev, "-");

    "Section 2.4.1 of the specification"
    tokenizer
    shared Token<Dot>? dot(List<Character> input, Object? prev)
            => literal(Dot, input, prev, ".");

    "Section 2.4.1 of the specification"
    rule
    shared Exponent exponent(ExpMarker e, Plus|Minus? s, [Digit+] digits)
            => meta(Exponent, e, s, digits);

    "Section 2.4.1 of the specification"
    rule
    shared FloatLiteral floatLiteral([Digits+] digits, Dot dot,
            [FracDigits+] fracs, Magnitude|Minitude|Exponent? m)
            => astTextNode(FloatLiteral, digits, dot, fracs, m);

    "Section 2.4.1 of the specification"
    rule
    shared FloatLiteral shortcutFloatLiteral([Digits+] digits, Minitude m)
            => astTextNode(FloatLiteral, digits, m);

    "Section 2.4.2 of the specification"
    tokenizer
    shared Token<Quote>? quote(List<Character> input, Object? prev)
            => literal(Quote, input, prev, "'");

    "Section 2.4.2 of the specification"
    tokenizer
    shared Token<CharacterLiteralTok>? characterLiteralTok(List<Character> input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);

        if (! input[0] exists) { return null; }

        variable value i = 0;
        variable value skip = false;

        while (exists c = input[i], c != "'" || skip) {
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
    shared CharacterLiteral characterLiteral(Quote a,
            CharacterLiteralTok t, Quote b)
            => astNode(CharacterLiteral, [t.text], a, t, b);

    "Section 2.4.3 of the specification"
    tokenizer
    shared Token<DoubleQuote>? doubleQuote(List<Character> input, Object? prev)
            => literal(DoubleQuote, input, prev, "\"");

    "Section 2.4.3 of the specification"
    tokenizer
    shared Token<StringLiteralTok>? stringLiteralTok(List<Character> input, Object? prev) {
        value [start_line, start_col] = extractStartPos(prev);

        if (! input[0] exists) { return null; }

        variable value i = 0;
        variable value skip = false;

        while (exists c = input[i], (c != '"' && ! input[i...].startsWith("\``")) || skip) {
            skip = c == '\\' && !skip;
            i++;
        }

        if (! input[i] exists) { return null; }

        value [end_line, end_col] = calculateStopPos(start_line, start_col,
                input[0:i]);
        return Token(StringLiteralTok(input[0:i], start_line, start_col,
                    end_line, end_col), i);
    }

    "Section 2.4.2 of the specification"
    rule
    shared StringLiteral stringLiteral(DoubleQuote a,
            StringLiteralTok t, DoubleQuote b)
            => astNode(StringLiteral, [t.text], a, t, b);

    "Section 3.2.3 of the specification"
    tokenizer
    shared Token<Pipe>? pipe(List<Character> input, Object? prev)
            => literal(Pipe, input, prev, "|");

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

        return astNode(UnionType, [left_children.append(right_children)], a, p, b);
    }

    "Section 3.2.4 of the specification"
    tokenizer
    shared Token<Ampersand>? ampersand(List<Character> input, Object? prev)
            => literal(Ampersand, input, prev, "&");

    "Section 3.2.4 of the specification"
    rule(1, lassoc)
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

        return astNode(IntersectionType, [left_children.append(right_children)], a, p, b);
    }

    "Section 3.2.7 of the specification"
    tokenizer
    shared Token<LT>? lessThan(List<Character> input, Object? prev)
            => literal(LT, input, prev, "<");

    "Section 3.2.7 of the specification"
    tokenizer
    shared Token<GT>? greaterThan(List<Character> input, Object? prev)
            => literal(GT, input, prev, ">");

    "Section 3.2.7 of the specification"
    rule
    shared GroupedType groupedType(LT a, Type t, GT b)
            => astNode(GroupedType, [t], a, t, b);

    "Section 3.2.7 of the specification"
    rule
    shared TypeNameWithTypeArguments typeNameWithArguments(TypeName name,
            TypeArguments? args)
            => astNode(TypeNameWithTypeArguments, [name, args], name, args);

    "Section 3.2.7 of the specification"
    rule
    shared BaseType baseType(TypeNameWithTypeArguments type)
            => astNode(BaseType, [type], type);

    "Section 3.2.7 of the specification"
    rule
    shared QualifiedType qualifiedType(SimpleType|GroupedType base,
            TypeNameWithTypeArguments type)
            => astNode(QualifiedType, [base, type], base, type);

    "Section 3.2.8 of the specification"
    tokenizer
    shared Token<Question>? question(List<Character> input, Object? prev)
            => literal(Question, input, prev, "?");

    "Section 3.2.8 of the specification"
    rule
    shared OptionalType optionalType(PrimaryType type, Question q)
            => astNode(OptionalType, [type], type, q);

    "Section 3.2.8 of the specification"
    tokenizer
    shared Token<SqOpen>? sqOpen(List<Character> input, Object? prev)
            => literal(SqOpen, input, prev, "[");

    "Section 3.2.8 of the specification"
    tokenizer
    shared Token<SqClose>? sqClose(List<Character> input, Object? prev)
            => literal(SqClose, input, prev, "]");

    "Section 3.2.8 of the specification"
    tokenizer
    shared Token<ParOpen>? parOpen(List<Character> input, Object? prev)
            => literal(ParOpen, input, prev, "(");

    "Section 3.2.8 of the specification"
    tokenizer
    shared Token<ParClose>? parClose(List<Character> input, Object? prev)
            => literal(ParClose, input, prev, ")");

    "Section 3.2.8 of the specification"
    tokenizer
    shared Token<Comma>? comma(List<Character> input, Object? prev)
            => literal(Comma, input, prev, ",");

    "Section 3.2.8 of the specification"
    rule
    shared SequentialType sequentialType(PrimaryType type,
            SqOpen a, SqClose b) => astNode(SequentialType, [type], a, b);

    "Section 3.2.8 of the specification"
    genericRule(`class CommaSepList`)
    shared CommaSepList<ItemType> commaSepList<ItemType>(ItemType t,
            [Comma, ItemType]* subsequent)
            given ItemType satisfies Node
            => CommaSepList<ItemType>([t, *subsequent.map((x) => x[1])],
                    *tokenStream(t, *subsequent));

    "Section 3.2.8 of the specification"
    rule
    shared TypeList typeList(CommaSepList<Type|DefaultedType> items)
            => astNode(TypeList, [items.nodes, null], *items.tokens);

    "Section 3.2.8 of the specification"
    rule
    shared TypeList typeListVar(CommaSepList<Type|DefaultedType> items,
            Comma c, VariadicType v)
            => astNode(TypeList, [items.nodes, v], *items.tokens.chain({c, v}));

    "Section 3.2.8 of the specification"
    rule
    shared TypeList emptyTypeList()
            => astNode(TypeList, [[], null]);

    "Section 3.2.8 of the specification"
    rule
    shared CallableType callableType(PrimaryType ret, ParOpen a,
            TypeList types, ParClose b)
            => astNode(CallableType, [ret, types], ret, a, types, b);

    "Section 3.2.8 of the specification"
    tokenizer
    shared Token<CurlOpen>? curlOpen(List<Character> input, Object? prev)
            => literal(CurlOpen, input, prev, "{");

    "Section 3.2.8 of the specification"
    tokenizer
    shared Token<CurlClose>? curlClose(List<Character> input, Object? prev)
            => literal(CurlClose, input, prev, "}");

    "Section 3.2.8 of the specification"
    tokenizer
    shared Token<Star>? star(List<Character> input, Object? prev)
            => literal(Star, input, prev, "*");

    "Section 3.2.8 of the specification"
    rule
    shared IterableType iterableType(CurlOpen a, VariadicType type,
            CurlClose b)
            => astNode(IterableType, [type], a, type, b);

    "Section 3.2.8 of the specification"
    rule
    shared TupleType tupleType(SqOpen a, TypeList types, SqClose b)
            => astNode(TupleType, [types], a, types, b);

    "Section 3.2.8 of the specification"
    rule
    shared VariadicType variadicType(MainType type, Plus|Star quality)
            => astNode(VariadicType, [type, quality is Plus], type, quality);

    "Section 3.2.8 of the specification"
    tokenizer
    shared Token<Eq>? eq(List<Character> input, Object? prev)
            => literal(Eq, input, prev, "=");

    "Section 3.2.8 of the specification"
    rule
    shared DefaultedType defaultedType(Type type, Eq e)
            => astNode(DefaultedType, [type], type, e);

    "Section 3.2.8 of the specification"
    tokenizer
    shared Token<Arrow>? arrow(List<Character> input, Object? prev)
            => literal(Arrow, input, prev, "->");

    "Section 3.2.8 of the specification"
    rule
    shared EntryType entryType(MainType key, Arrow a, MainType item)
            => astNode(EntryType, [key, item], key, a, item);

    "Section 3.3.2 of the specification"
    tokenizer
    shared Token<Extends>? extends_(List<Character> input, Object? prev)
            => keyword(Extends, input, prev, "extends");

    "Section 3.3.2 of the specification"
    tokenizer
    shared Token<SuperTok>? superTok(List<Character> input, Object? prev)
            => keyword(SuperTok, input, prev, "super");

    "Section 3.3.2 of the specification"
    rule
    shared ClassInstantiation classInstantiation([SuperTok, Dot]? sup,
            TypeNameWithTypeArguments type, PositionalArguments args)
            => astNode(ClassInstantiation, [type, args, if (exists sup) then
            Super() else null], sup, type, args);

    "Section 3.3.2 of the specification"
    rule
    shared ExtendedType extendedType(Extends e, ClassInstantiation inst)
            => astNode(ExtendedType, [inst], e, inst);

    "Section 3.3.3 of the specification"
    tokenizer
    shared Token<Satisfies>? satisfies_(List<Character> input, Object? prev)
            => keyword(Satisfies, input, prev, "satisfies");

    "Section 3.3.3 of the specification"
    rule
    shared SatisfiedTypes satisfiedTypes(Satisfies s, PrimaryType p,
            [Ampersand,PrimaryType]* more)
            => astNode(SatisfiedTypes, [[p, *more.map((x) => x[1])]], s, p,
                    *more);

    "Section 3.4.2 of the specification"
    tokenizer
    shared Token<Of>? of_(List<Character> input, Object? prev)
            => keyword(Of, input, prev, "of");

    "Section 3.4.2 of the specification"
    rule
    shared CaseTypes caseTypes(Of o, PrimaryType|MemberName p,
            [Pipe,PrimaryType|MemberName]* more)
            => astNode(CaseTypes, [[p, *more.map((x) => x[1])]], p, *more);

    "Section 3.5 of the specification"
    rule
    shared TypeParameters typeParameters(LT a, CommaSepList<TypeParameter>
            list, GT b)
            => astNode(TypeParameters, [list.nodes], a, list.tokens, b);

    "Section 3.5.1 of the specification"
    rule
    shared TypeParameter typeParameter(Variance? var, TypeName name,
            [Eq,Type]? eq)
            => astNode(TypeParameter, [name, var, if (exists eq) then
                    eq[1] else null], var, name, eq);

    "Section 3.5.1 of the specification"
    tokenizer
    shared Token<In>? in_(List<Character> input, Object? prev)
            => keyword(In, input, prev, "in");

    "Section 3.5.1 of the specification"
    tokenizer
    shared Token<Out>? out_(List<Character> input, Object? prev)
            => keyword(Out, input, prev, "out");

    "Section 3.5.1 of the specification"
    rule
    shared InModifier inModifier(In t)
            => astNode(InModifier, [], t);

    "Section 3.5.1 of the specification"
    rule
    shared OutModifier outModifier(Out t)
            => astNode(OutModifier, [], t);

    "Section 3.5.3 of the specification"
    tokenizer
    shared Token<Given>? given_(List<Character> input, Object? prev)
            => keyword(Given, input, prev, "given");

    "Section 3.5.3 of the specification"
    rule
    shared TypeConstraint typeConstraint(Given g, TypeName name,
            CaseTypes? cases, SatisfiedTypes? satisfieds)
            => astNode(TypeConstraint, [name, cases, satisfieds], g, name,
                    cases, satisfieds);

    "Section 3.6 of the specification"
    rule
    shared TypeArguments typeArguments(LT a, CommaSepList<TypeArgument> types,
            GT b)
            => astNode(TypeArguments, [types.nodes], a,
                    *types.tokens.chain({b}));

    "Section 3.6 of the specification"
    rule
    shared TypeArgument typeArgument(Variance? var, Type type)
            => astNode(TypeArgument, [type, var], var, type);

    "Section 4.1 of the specification"
    rule
    shared CompilationUnit compilationUnit([Import *] imports,
            [Declaration *] declarations)
            => astNode(CompilationUnit, [declarations, imports], imports,
                    declarations);

    "Section 4.1 of the specification"
    rule
    shared ModuleCompilationUnit moduleCompilationUnit([Import *] imports,
            ModuleDescriptor m)
            => astNode(ModuleCompilationUnit, [m, imports], imports, m);

    "Section 4.1 of the specification"
    rule
    shared PackageCompilationUnit packageCompilationUnit([Import *] imports,
            PackageDescriptor m)
            => astNode(PackageCompilationUnit, [m, imports], imports, m);

    "Section 4.1.2 of the specification"
    rule
    shared FullPackageName fullPackageName(PackageName name,
            [Dot,PackageName]* dotNames)
            => astNode(FullPackageName,
                    [[name, *dotNames.map((x) => x[1])]], name, *dotNames);

    "Section 4.2 of the specification"
    tokenizer
    shared Token<ImportTok>? importTok(List<Character> input, Object? prev)
            => keyword(ImportTok, input, prev, "import");

    "Section 4.2 of the specification"
    rule
    shared Import import_(ImportTok imp, FullPackageName name,
            ImportElements elements)
            => astNode(Import, [name, elements], imp, name, elements);

    "Section 4.2 of the specification"
    rule
    shared ImportElements importElements(CurlOpen a,
            CommaSepList<ImportElement> elements, ImportWildcard? wild,
            CurlClose b)
            => astNode(ImportElements, [elements.nodes, wild], a,
                    elements.tokens, wild, b);

    "Section 4.2.1 of the specification"
    rule
    shared ImportTypeElement importTypeElement(ImportTypeAlias? alias_,
            TypeName name, ImportElements? nested)
            => astNode(ImportTypeElement, [name, alias_, nested], alias_,
                    name, nested);

    "Section 4.2.2 of the specification"
    rule
    shared ImportFunctionValueElement
    importFunctionValueElement(ImportFunctionValueAlias? alias_,
            MemberName name, ImportElements? nested)
            => astNode(ImportFunctionValueElement, [name, alias_, nested],
                    alias_, name);

    "Section 4.2.3 of the specification"
    rule
    shared ImportTypeAlias importTypeAlias(TypeName type, Eq e)
            => astNode(ImportTypeAlias, [type], type, e);

    "Section 4.2.3 of the specification"
    rule
    shared ImportFunctionValueAlias importFunctionValueAlias(MemberName m,
            Eq e)
            => astNode(ImportFunctionValueAlias, [m], m, e);

    "Section 4.2.4 of the specification"
    tokenizer
    shared Token<Ellipsis>? ellipsis(List<Character> input, Object? prev)
            => literal(Ellipsis, input, prev, "...");

    "Section 4.2.5 of the specification"
    rule
    shared ImportWildcard importWildcard(Ellipsis e)
            => astNode(ImportWildcard, [], e);

    "Section 4.3.1 of the specification"
    rule
    shared Parameters parametersEmpty(ParOpen o, ParClose c)
            => astNode(Parameters, [[]], o, c);

    "Section 4.3.1 of the specification"
    rule
    shared Parameters parameters(ParOpen o, CommaSepList<Parameter> p,
            ParClose c)
            => astNode(Parameters, [p.nodes], o, p.tokens, c);

    "Section 4.3.3 of the specification"
    rule
    shared DefaultedValueParameter defaultedValue(ValueParameter v,
            Specifier p)
            => astNode(DefaultedValueParameter, [v, p], v, p);

    "Section 4.3.3 of the specification"
    rule
    shared DefaultedCallableParameter defaultedCallable(CallableParameter v,
            LazySpecifier p)
            => astNode(DefaultedCallableParameter, [v, p], v, p);

    "Section 4.3.3 of the specification"
    rule
    shared DefaultedParameterReference defaultedParameter(ParameterReference v,
            Specifier p)
            => astNode(DefaultedParameterReference, [v, p], v, p);

    "Section 4.3.3 of the specification"
    rule
    shared ParameterReference parameterReference(MemberName m)
            => astNode(ParameterReference, [m], m);

    "Section 4.3.3 of the specification"
    rule
    shared Specifier specifier(Eq e, Expression expr)
            => astNode(Specifier, [expr], e, expr);

    "Section 4.3.3 of the specification"
    tokenizer
    shared Token<DArrow>? dArrow(List<Character> input, Object? prev)
            => literal(DArrow, input, prev, "=>");

    "Section 4.3.3 of the specification"
    rule
    shared LazySpecifier lazySpecifier(DArrow d, Expression e)
            => astNode(LazySpecifier, [e], d, e);

    "Section 4.3.4 of the specification"
    tokenizer
    shared Token<Dynamic>? dynamic_(List<Character> input, Object? prev)
            => keyword(Dynamic, input, prev, "dynamic");

    "Section 4.3.4 of the specification"
    rule
    shared DynamicModifier dynamicModifier(Dynamic d)
            => astNode(DynamicModifier, [], d);

    "Section 4.3.4 of the specification"
    rule
    shared ValueParameter valueParameter(Annotations a, Type|DynamicModifier d,
            MemberName m)
            => astNode(ValueParameter, [d, m, a], a, d, m);

    "Section 4.3.5 of the specification"
    tokenizer
    shared Token<Void>? void_(List<Character> input, Object? prev)
            => keyword(Void, input, prev, "void");

    "Section 4.3.5 of the specification"
    rule
    shared VoidModifier voidModifier(Void d)
            => astNode(VoidModifier, [], d);

    "Section 4.3.5 of the specification"
    rule
    shared CallableParameter callableParameter(Annotations a,
            Type|VoidModifier v, MemberName m, Parameters+ p)
            => astNode(CallableParameter, [v, m, p, a], a, v, m, p);

    "Section 4.3.6 of the specification"
    rule
    shared VariadicParameter variadicParameter(Annotations a, VariadicType t, MemberName m)
            => astNode(VariadicParameter, [t, m, a], a, t, m);

    "Section 4.4 of the specification"
    tokenizer
    shared Token<Interface>? interface_(List<Character> input, Object? prev)
            => keyword(Interface, input, prev, "interface");

    "Section 4.4 of the specification"
    rule
    shared InterfaceDefinition interfaceDefinition(Annotations a, Interface i,
            TypeName n, TypeParameters? p, CaseTypes? c, SatisfiedTypes? s,
            [TypeConstraint *] t, InterfaceBody b)
            => astNode(InterfaceDefinition,
                    [n, b, c, s, p, t, a],
                    a, i, n, p, c, s, t, b);

    "Section 4.4 of the specification"
    tokenizer
    shared Token<Semicolon>? semicolon(List<Character> input, Object? prev)
            => literal(Semicolon, input, prev, ";");

    "Section 4.4 of the specification"
    rule
    shared InterfaceAliasDefinition interfaceAliasDefinition(Annotations a,
            Interface i, TypeName n, TypeParameters? p, CaseTypes? c,
            SatisfiedTypes? s, [TypeConstraint *] t, TypeSpecifier b,
            Semicolon end)
            => astNode(InterfaceAliasDefinition,
                    [n, b, c, s, p, t, a],
                    a, i, n, p, c, s, t, b, end);

    "Section 4.4 of the specification"
    rule
    shared DynamicInterfaceDefinition dynamicInterfaceDefinition(Annotations a, Dynamic i,
            TypeName n, TypeParameters? p, CaseTypes? c, SatisfiedTypes? s,
            [TypeConstraint *] t, InterfaceBody b)
            => astNode(DynamicInterfaceDefinition,
                    [n, b, c, s, p, t, a],
                    a, i, n, p, c, s, t, b);

    "Section 4.4 of the specification"
    rule
    shared InterfaceBody interfaceBody(CurlOpen a, [Declaration *] d,
            CurlClose b)
            => astNode(InterfaceBody, [d], a, d, b);

    "Section 4.4.5 of the specification"
    rule
    shared TypeSpecifier typeSpecifier(DArrow d, Type t)
            => astNode(TypeSpecifier, [t], d, t);

    "Section 4.5 of the specification"
    tokenizer
    shared Token<ClassTok>? class_(List<Character> input, Object? prev)
            => keyword(ClassTok, input, prev, "class");

    "Section 4.5 of the specification"
    rule
    shared ClassDefinition classDefinition(Annotations a, ClassTok i,
            TypeName n, TypeParameters? p, Parameters? pr, CaseTypes? c,
            ExtendedType? e, SatisfiedTypes? s, [TypeConstraint *] t,
            ClassBody b)
            => astNode(ClassDefinition,
                    [n, pr, b, c, e, s, p, t, a],
                    a, i, n, p, pr, c, e, s, t, b);

    "Section 4.5 of the specification"
    rule
    shared ClassAliasDefinition classAliasDefinition(Annotations a, ClassTok i,
            TypeName n, TypeParameters? p, Parameters pr, CaseTypes? c,
            ExtendedType? e, SatisfiedTypes? s,
            [TypeConstraint *] t, ClassSpecifier b, Semicolon end)
            => astNode(ClassAliasDefinition,
                    [n, pr, b, c, e, s, p, t, a],
                    a, i, n, p, pr, c, e, s, t, b, end);

    "Section 4.5 of the specification"
    rule
    shared ClassBody classBody(CurlOpen a,
            [Declaration|Statement *] d,
            CurlClose b)
            => astNode(ClassBody, [d], a, d, b);

    "Section 4.5.7 of the specification"
    tokenizer
    shared Token<ObjectTok>? object_(List<Character> input, Object? prev)
            => keyword(ObjectTok, input, prev, "object");

    "Section 4.5.7 of the specification"
    rule
    shared ObjectDefinition objectDefinition(Annotations a, ObjectTok i,
            MemberName n, ExtendedType? e, SatisfiedTypes? s, ClassBody b)
            => astNode(ObjectDefinition, [n, b, e, s, a], a, i, n, e, s, b);

    "Section 4.5.9 of the specification"
    rule
    shared ClassSpecifier classSpecifier(DArrow d, ClassInstantiation t)
            => astNode(ClassSpecifier, [t], d, t);

    "Section 4.6 of the specification"
    tokenizer
    shared Token<Alias>? alias_(List<Character> input, Object? prev)
            => keyword(Alias, input, prev, "alias");

    "Section 4.6 of the specification"
    rule
    shared TypeAliasDefinition typeAliasDefinition(Annotations a, Alias al,
            TypeName n, TypeParameters? tp, [TypeConstraint *] tc,
            TypeSpecifier s, Semicolon end)
            => astNode(TypeAliasDefinition, [n, s, tp, tc, a],
                    a, al, n, tp, tc, s, end);

    "Section 4.7 of the specification"
    rule
    shared FunctionDeclaration functionDeclaration(Annotations a, MemberName m,
            Type|DynamicModifier|VoidModifier t, TypeParameters? tp,
            [Parameters+] p, [TypeConstraint*] tc, Semicolon end)
            => astNode(FunctionDeclaration, [m, t, p, tp, tc, a],
                    a, m, t, tp, p, tc, end);

    "Section 4.7 of the specification"
    rule
    shared FunctionDefinition functionDefinition(Annotations a, MemberName m,
            Type|DynamicModifier|VoidModifier t, TypeParameters? tp,
            [Parameters+] p, [TypeConstraint*] tc, Block b)
            => astNode(FunctionDefinition, [m, t, p, b, tp, tc, a],
                    a, m, t, tp, p, tc, b);

    "Section 4.7 of the specification"
    rule
    shared FunctionShortcutDefinition functionShortcutDefinition(Annotations a,
            MemberName m, Type|DynamicModifier|VoidModifier t,
            TypeParameters? tp, [Parameters+] p, [TypeConstraint*] tc,
            LazySpecifier b, Semicolon end)
            => astNode(FunctionShortcutDefinition, [m, t, p, b, tp, tc, a],
                    a, m, t, tp, p, tc, b, end);

    "Section 4.8 of the specification"
    rule
    shared ValueDeclaration valueDeclaration(Annotations a,
            MemberName m, Type|VariadicType|DynamicModifier t, Semicolon end)
            => astNode(ValueDeclaration, [m, t, a],
                    a, m, t, end);

    "Section 4.8 of the specification"
    rule
    shared ValueDefinition valueDefinition(Annotations a,
            Type|ValueModifier|DynamicModifier t, MemberName m,
            AnySpecifier s, Semicolon end)
            => astNode(ValueDefinition, [m, t, s, a],
                    a, t, m, s, end);

    "Section 4.8 of the specification"
    tokenizer
    shared Token<Value>? value_(List<Character> input, Object? prev)
            => keyword(Value, input, prev, "value");

    "Section 4.8 of the specification"
    rule
    shared ValueModifier valueModifier(Value d)
            => astNode(ValueModifier, [], d);

    "Section 4.8 of the specification"
    rule
    shared ValueGetterDefinition valueGetterDefinition(Annotations a,
            Type|ValueModifier|DynamicModifier t, MemberName m,
            Block s)
            => astNode(ValueGetterDefinition, [m, t, s, a],
                    a, m, t, s);

    "Section 4.9 of the specification"
    tokenizer
    shared Token<New>? new_(List<Character> input, Object? prev)
            => keyword(New, input, prev, "new");

    "Section 4.9 of the specification"
    rule
    shared ConstructorDefinition constructorDefinition(Annotations a,
            New nk, TypeName? n, Parameters p, ExtendedType e, Block b)
            => astNode(ConstructorDefinition, [n, p, b, e, a],
                    a, nk, n, p, e, b);

    "Section 5.2.1 of the specification"
    rule
    shared TypedVariable typedVariable(Type t, MemberName m, Specifier? s)
            => astNode(TypedVariable, [m, t, s], t, m, s);

    "Section 5.2.1 of the specification"
    rule
    shared SpecifiedVariable specifiedVariable(Type|ValueModifier? t,
            MemberName m, Specifier s)
            => astNode(SpecifiedVariable, [m, s, t], t, m, s);

    "Section 5.2.3 of the specification"
    rule
    shared VariadicVariable variadicVariable(UnionType? u, Star s,
            MemberName m)
            => astNode(VariadicVariable, [m, u], u, s, m);

    "Section 5.2.4 of the specification"
    rule
    shared TuplePattern tuplePattern(SqOpen o, CommaSepList<Pattern> l,
            [Comma, VariadicVariable]? v, SqClose c)
            =>astNode(TuplePattern, [l.nodes, if (exists v) then v[1] else null],
                    o, *l.nodes.chain([v, c]));

    "Section 5.2.5 of the specification"
    rule
    shared EntryPattern entryPattern(VariablePattern|TuplePattern k, Arrow a,
            VariablePattern|TuplePattern v)
            =>astNode(EntryPattern, [k, v], k, a, v);

    "Unknown/Future revision"
    rule
    shared VariablePattern variablePattern(UnspecifiedVariable u)
            =>astNode(VariablePattern, [u], u);

    "Unknown/Future revision"
    rule
    shared UnspecifiedVariable unspecifiedVariable(Type|ValueModifier? t,
            MemberName m)
            => astNode(UnspecifiedVariable, [m, t], t, m);

    "Section 5.3 of the specification"
    rule
    shared Block block(CurlOpen o, [Declaration|Statement *] s, CurlClose c)
            => astNode(Block, [s], o, s, c);

    "Section 5.3.1 of the specificaton"
    rule
    shared AssignmentStatement assignmentStatement(AssignmentOperation a,
            Semicolon s)
            => astNode(AssignmentStatement, [a], a, s);

    "Section 5.3.1 of the specificaton"
    rule
    shared PrefixPostfixStatement
    prefixPostfixStatement(PrefixOperation|PostfixOperation a, Semicolon s)
            => astNode(PrefixPostfixStatement, [a], a, s);

    "Section 5.3.1 of the specificaton"
    rule
    shared InvocationStatement invocationStatement(Invocation a, Semicolon s)
            => astNode(InvocationStatement, [a], a, s);

    "Section 5.3.2 of the specification"
    tokenizer
    shared Token<ReturnTok>? returnTok(List<Character> input, Object? prev)
            => keyword(ReturnTok, input, prev, "return");

    "Section 5.3.2 of the specification"
    rule
    shared Return return_(ReturnTok t, Expression? e, Semicolon s)
            => astNode(Return, [e], t, e, s);

    "Section 5.3.2 of the specification"
    tokenizer
    shared Token<ThrowTok>? throwTok(List<Character> input, Object? prev)
            => keyword(ThrowTok, input, prev, "throw");

    "Section 5.3.2 of the specification"
    rule
    shared Throw throw_(ThrowTok t, Expression? e, Semicolon s)
            => astNode(Throw, [e], t, e, s);

    "Section 5.3.2 of the specification"
    tokenizer
    shared Token<BreakTok>? breakTok(List<Character> input, Object? prev)
            => keyword(BreakTok, input, prev, "break");

    "Section 5.3.2 of the specification"
    rule
    shared Break break_(BreakTok t, Semicolon s)
            => astNode(Break, [], t, s);

    "Section 5.3.2 of the specification"
    tokenizer
    shared Token<ContinueTok>? continueTok(List<Character> input, Object? prev)
            => keyword(ContinueTok, input, prev, "continue");

    "Section 5.3.2 of the specification"
    rule
    shared Continue continue_(ContinueTok t, Semicolon s)
            => astNode(Continue, [], t, s);

    "Section 5.3.3 of the specification"
    tokenizer
    shared Token<ThisTok>? thisTok(List<Character> input, Object? prev)
            => keyword(ThisTok, input, prev, "this");

    "Section 5.3.3 of the specification"
    rule
    shared This this_(ThisTok t)
            => astNode(This, [], t);

    "Section 5.3.3 of the specification"
    rule
    shared ValueSpecification valueSpecification([This, Dot]? t, MemberName m,
            Specifier s, Semicolon e)
            => astNode(ValueSpecification, [m, s, if (exists t) then t[0]
                    else null], t, m, s, e);

    "Section 5.3.3 of the specification"
    rule
    shared LazySpecification lazySpecification([This, Dot]? t,
            MemberName m, [Parameters*] p, LazySpecifier s, Semicolon e)
            => astNode(LazySpecification, [m, s, p, if (exists t) then t[0]
                    else null], t, m, p, s, e);

    "Section 5.3.4 of the specification"
    rule
    shared Destructure destructure(ValueModifier v, TuplePattern|EntryPattern p,
            Specifier s, Semicolon e)
            => astNode(Destructure, [p, s, v], v, p, s, e);

    "Section 5.3.5 of the specification"
    rule
    shared DynamicBlock dynamicBlock(Dynamic d, Block b)
            => astNode(DynamicBlock, [b], d, b);

    "Section 5.4 of the specification"
    rule
    shared Conditions conditions(ParOpen o, CommaSepList<Condition> l,
            ParClose c)
            => astNode(Conditions, [l.nodes], *l.nodes);

    "Section 5.4.1 of the specification"
    rule
    shared BooleanCondition booleanCondition(Expression e)
            => astNode(BooleanCondition, [e], e);

    "Section 5.4.2 of the specification"
    tokenizer
    shared Token<Is>? is_(List<Character> input, Object? prev)
            => keyword(Is, input, prev, "is");

    "Section 5.4.2 of the specification"
    tokenizer
    shared Token<Bang>? bang(List<Character> input, Object? prev)
            => literal(Bang, input, prev, "!");

    "Section 5.4.2 of the specification"
    rule
    shared IsCondition isCondition(Bang? b, Is i, TypedVariable tv)
            => astNode(IsCondition, [tv, b exists], b, i, tv);

    "Section 5.4.3 of the specification"
    tokenizer
    shared Token<Exists>? exists_(List<Character> input, Object? prev)
            => keyword(Exists, input, prev, "exists");

    "Section 5.4.3 of the specification"
    tokenizer
    shared Token<Nonempty>? nonempty_(List<Character> input, Object? prev)
            => keyword(Nonempty, input, prev, "nonempty");

    "Section 5.4.3 of the specification"
    rule
    shared SpecifiedPattern specifiedPattern(Pattern p, Specifier s)
            => astNode(SpecifiedPattern, [p, s], p, s);

    "Section 5.4.3 of the specification"
    rule
    shared ExistsCondition existsCondition(Bang? b, Exists i,
            SpecifiedPattern|LIdentifier tv)
            => astNode(ExistsCondition, [tv, b exists], b, i, tv);

    "Section 5.4.3 of the specification"
    rule
    shared NonemptyCondition nonemptyCondition(Bang? b, Nonempty i,
            SpecifiedPattern|LIdentifier tv)
            => astNode(NonemptyCondition, [tv, b exists], b, i, tv);

    "Section 5.4.4 of the specification"
    shared alias MatchCaseBase =>
    IntegerLiteral|CharacterLiteral|StringLiteral|NegationOperation|BaseExpression;

    "Section 5.4.4 of the specification"
    rule
    shared MatchCase matchCase(MatchCaseBase b, [[Pipe,MatchCaseBase] *] cont)
            => astNode(MatchCase, [ [b, *cont.narrow<MatchCaseBase>()] ], b,
                    *cont.map((x) => x[1]));

    "Section 5.4.4 of the specification"
    rule
    shared IsCase isCase(Is i, Type t)
            => astNode(IsCase, [t], i, t);

    "Section 5.5.1 of the specification"
    tokenizer
    shared Token<IfTok>? ifTok(List<Character> input, Object? prev)
            => keyword(IfTok, input, prev, "if");

    "Section 5.5.1 of the specification"
    tokenizer
    shared Token<ElseTok>? elseTok(List<Character> input, Object? prev)
            => keyword(ElseTok, input, prev, "else");

    "Section 5.5.1 of the specification"
    rule
    shared IfElse ifElse(IfClause i, ElseClause? e)
            => astNode(IfElse, [i, e], i, e);

    "Section 5.5.1 of the specification"
    rule
    shared IfClause ifClause(IfTok i, Conditions c, Block b)
            => astNode(IfClause, [c, b], i, c, b);

    "Section 5.5.1 of the specification"
    rule
    shared ElseClause elseClause(ElseTok i, Block|IfElse b)
            => astNode(ElseClause, [b], i, b);

    "Section 5.5.2 of the specification"
    tokenizer
    shared Token<SwitchTok>? switchTok(List<Character> input, Object? prev)
            => keyword(SwitchTok, input, prev, "switch");

    "Section 5.5.2 of the specification"
    tokenizer
    shared Token<CaseTok>? caseTok(List<Character> input, Object? prev)
            => keyword(CaseTok, input, prev, "case");

    "Section 5.5.2 of the specification"
    rule
    shared SwitchCaseElse switchCaseElse(SwitchClause c, SwitchCases s)
            => astNode(SwitchCaseElse, [c, s], c, s);

    "Section 5.5.2 of the specification"
    rule
    shared SwitchClause switchClause(SwitchTok t, ParOpen o,
            Expression|SpecifiedVariable e, ParClose p)
            => astNode(SwitchClause, [e], t, o, e, p);

    "Section 5.5.2 of the specification"
    rule
    shared SwitchCases switchCases([CaseClause +] c, ElseCaseClause? e)
            => astNode(SwitchCases, [c, e], c, e);

    "Section 5.5.2 of the specification"
    rule
    shared ElseCaseClause elseCaseClause(ElseTok i, Block b)
            => astNode(ElseCaseClause, [b], i, b);

    "Section 5.5.2 of the specification"
    rule
    shared CaseClause caseClause(CaseTok ct, ParOpen o, CaseItem c,
            ParClose cl, Block b)
            => astNode(CaseClause, [c, b], ct, o, c, cl, b);

    "Section 5.5.3 of the specification"
    tokenizer
    shared Token<ForTok>? forTok(List<Character> input, Object? prev)
            => keyword(ForTok, input, prev, "for");

    "Section 5.5.3 of the specification"
    rule
    shared ForFail forFail(ForClause f, FailClause? c)
            => astNode(ForFail, [f, c], f, c);

    "Section 5.5.3 of the specification"
    rule
    shared ForClause forClause(ForTok t, ForIterator f, Block b)
            => astNode(ForClause, [f, b], t, f, b);

    "Section 5.5.3 of the specification"
    rule
    shared ForIterator forIterator(ParOpen o, Pattern p, In i, Expression e,
            ParClose c)
            => astNode(ForIterator, [p, e], o, p, i, e, c);

    "Section 5.5.3 of the specification"
    rule
    shared FailClause failClause(ElseTok e, Block b)
            => astNode(FailClause, [b], e, b);

    "Section 5.5.4 of the specification"
    tokenizer
    shared Token<WhileTok>? whileTok(List<Character> input, Object? prev)
            => keyword(WhileTok, input, prev, "while");

    "Section 5.5.4 of the specification"
    rule
    shared While while_(WhileTok w, Conditions c, Block b)
            => astNode(While, [c, b], w, c, b);

    "Section 5.5.5 of the specification"
    tokenizer
    shared Token<TryTok>? tryTok(List<Character> input, Object? prev)
            => keyword(TryTok, input, prev, "try");

    "Section 5.5.5 of the specification"
    tokenizer
    shared Token<CatchTok>? catchTok(List<Character> input, Object? prev)
            => keyword(CatchTok, input, prev, "catch");

    "Section 5.5.5 of the specification"
    tokenizer
    shared Token<FinallyTok>? finallyTok(List<Character> input, Object? prev)
            => keyword(FinallyTok, input, prev, "finally");

    "Section 5.5.5 of the specification"
    rule
    shared TryCatchFinally tryCatchFinally(TryClause t, CatchClause[] c,
            FinallyClause? f)
            => astNode(TryCatchFinally, [t, c, f], t, c, f);

    "Section 5.5.5 of the specification"
    rule
    shared TryClause tryClause(TryTok t, Resources? r, Block b)
            => astNode(TryClause, [b, r], t, r, b);

    "Section 5.5.5 of the specification"
    rule
    shared Resources resources(ParOpen o, CommaSepList<Resource> r, ParClose c)
            => astNode(Resources, [r.nodes], o, *r.nodes.withTrailing(c));

    "Section 5.5.5 of the specification"
    rule
    shared Resource resource(Expression|SpecifiedVariable r)
            => astNode(Resource, [r], r);

    "Section 5.5.5 of the specification"
    rule
    shared CatchClause catchClause(CatchTok t, ParOpen o,
            UnspecifiedVariable u, ParClose c, Block b)
            => astNode(CatchClause, [u, b], t, o, u, c, b);

    "Section 5.5.5 of the specification"
    rule
    shared FinallyClause finallyClause(FinallyTok t, Block b)
            => astNode(FinallyClause, [b], t, b);

    "Section 5.5.6 of the specification"
    tokenizer
    shared Token<AssertTok>? assertTok(List<Character> input, Object? prev)
            => keyword(AssertTok, input, prev, "assert");

    "Section 5.5.6 of the specification"
    rule
    shared Assertion assertion(Annotations a, AssertTok t, Conditions c,
            Semicolon s)
            => astNode(Assertion, [c, a], a, t, c, s);

    "Section 6.2 of the specification"
    tokenizer
    shared Token<TickTick>? tickTick(List<Character> input, Object? prev)
            => literal(TickTick, input, prev, "\``");

    "Section 6.2 of the specification"
    rule
    shared StringTemplate stringTemplate(DoubleQuote s, StringLiteralTok a,
            [[TickTick,Expression,TickTick,StringLiteralTok] +] b, DoubleQuote e)
    {
        value stringLits = [*{StringLiteral(a.text)}.chain(b.map((x) => StringLiteral(x[3].text)))];
        value exprs = [*b.map((x) => x[1])];
        return astNode(StringTemplate, [stringLits, exprs], s, a,
                *b.withTrailing(e));
    }

    "Section 6.3 of the specification"
    tokenizer
    shared Token<OuterTok>? outerTok(List<Character> input, Object? prev)
            => keyword(OuterTok, input, prev, "outer");

    "Section 6.3 of the specification"
    tokenizer
    shared Token<PackageTok>? packageTok(List<Character> input, Object? prev)
            => keyword(PackageTok, input, prev, "package");

    "Section 6.3 of the specification"
    rule
    shared Super super_(SuperTok t) => astNode(Super, [], t);

    "Section 6.3 of the specification"
    rule
    shared Outer outer_(OuterTok t) => astNode(Outer, [], t);

    "Section 6.3 of the specification"
    rule
    shared Package package_(PackageTok t) => astNode(Package, [], t);

    "Section 6.4 of the specification"
    tokenizer
    shared Token<FunctionTok>? functionTok(List<Character> input, Object? prev)
            => keyword(FunctionTok, input, prev, "function");

    "Section 6.4 of the specification"
    rule
    shared FunctionModifier functionModifier(FunctionTok t)
            => astNode(FunctionModifier, [], t);

    "Section 6.4 of the specification"
    rule
    shared FunctionExpression functionExpression(FunctionModifier|VoidModifier? m,
            [Parameters+] p, Block|LazySpecifier l)
            => astNode(FunctionExpression, [p, l, m], m, p, l);

    "Section 6.5 of the specification"
    rule
    shared GroupedExpression groupedExpression(ParOpen o, Expression e,
            ParClose c)
            => astNode(GroupedExpression, [e], o, e, c);

    "Section 6.5.1 of the specification"
    rule
    shared BaseExpression baseExpression(NameWithTypeArguments n)
            => astNode(BaseExpression, [n], n);

    "Section 6.5.1 of the specification"
    rule
    shared MemberNameWithTypeArguments memberNameWithArguments(
            MemberName name, TypeArguments? args)
            => astNode(MemberNameWithTypeArguments, [name, args],
                    name, args);

    "Section 6.5.2 of the specification"
    rule
    shared QualifiedExpression qualifiedExpression(Primary p,
            AnyMemberOperator o, NameWithTypeArguments n)
            => astNode(QualifiedExpression, [p, n, o], p, o, n);

    "Section 6.5.2 of the specification"
    rule
    shared MemberOperator memberOperator(Dot d)
            => astNode(MemberOperator, [], d);

    "Section 6.5.2 of the specification"
    tokenizer
    shared Token<QDot>? qdot(List<Character> input, Object? prev)
            => literal(QDot, input, prev, "?.");

    "Section 6.5.2 of the specification"
    tokenizer
    shared Token<SDot>? sdot(List<Character> input, Object? prev)
            => literal(SDot, input, prev, "*.");

    "Section 6.5.2 of the specification"
    rule
    shared SafeMemberOperator safeMemberOperator(QDot d)
            => astNode(SafeMemberOperator, [], d);

    "Section 6.5.2 of the specification"
    rule
    shared SpreadMemberOperator spreadMemberOperator(SDot d)
            => astNode(SpreadMemberOperator, [], d);

    "Section 6.6 of the specification"
    rule
    shared Invocation invocation(Primary p, Arguments a)
            => astNode(Invocation, [p, a], p, a);

    "Section 6.6.3 of the specification"
    rule
    shared ArgumentList argumentList(CommaSepList<Expression> l,
            [Comma,SpreadArgument|Comprehension]? c)
            => astNode(ArgumentList, [l.nodes, if (exists c) then c[1] else
                    null],
                    *l.nodes.chain({c}));

    "Section 6.6.3 of the specification"
    rule
    shared ArgumentList argumentList2(SpreadArgument|Comprehension? c)
            => astNode(ArgumentList, [[], c], c);

    "Section 6.6.5 of the specification"
    rule
    shared SpreadArgument spreadArgument(Star s, UnioningExpression e)
            => astNode(SpreadArgument, [e], s, e);

    "Section 6.6.6 of the specification"
    rule
    shared Comprehension comprehension(InitialComprehensionClause i)
            => astNode(Comprehension, [i], i);

    "Section 6.6.6 of the specification"
    rule
    shared ForComprehensionClause forComprehensionClause(ForTok t,
            ForIterator f, ComprehensionClause c)
            => astNode(ForComprehensionClause, [f, c], t, f, c);

    "Section 6.6.6 of the specification"
    rule
    shared IfComprehensionClause ifComprehensionClause(IfTok t,
            Conditions f, ComprehensionClause c)
            => astNode(IfComprehensionClause, [f, c], t, f, c);

    "Section 6.6.7 of the specification"
    rule
    shared PositionalArguments positionalArguments(ParOpen p, ArgumentList a,
            ParClose c)
            => astNode(PositionalArguments, [a], p, a, c);

    "Section 6.6.8 of the specification"
    rule
    shared NamedArguments namedArguments(CurlOpen p,
            NamedArgument[] n, ArgumentList a, CurlClose c)
            => astNode(NamedArguments, [n, a], p, n, a, c);

    "Section 6.6.9 of the specification"
    rule
    shared AnonymousArgument anonymousArgument(Expression e, Semicolon s)
            => astNode(AnonymousArgument, [e], e, s);

    "Section 6.6.10 of the specification"
    rule
    shared SpecifiedArgument specifiedArgument(Specification s)
            => astNode(SpecifiedArgument, [s], s);

    "Section 6.6.11 of the specification"
    rule
    shared ValueArgument valueArgument(Type|ValueModifier|DynamicModifier t,
            LIdentifier n, [AnySpecifier,Semicolon]|[Block] b)
            => astNode(ValueArgument, [n,t,b[0]], t, n, b);

    "Section 6.6.11 of the specification"
    rule
    shared FunctionArgument functionArgument(
            Type|VoidModifier|FunctionModifier|DynamicModifier t,
            LIdentifier n, [Parameters+] p, [LazySpecifier,Semicolon]|[Block] b)
            => astNode(FunctionArgument, [n,t,p,b[0]], t, n, p, b);

    "Section 6.6.11 of the specification"
    rule
    shared ObjectArgument objectArgument(ObjectTok o, MemberName n,
            ExtendedType? e, SatisfiedTypes? s, ClassBody c)
            => astNode(ObjectArgument, [n, c, e, s], o, n, e, s, c);

    "Section 6.6.12 of the specification"
    rule
    shared Iterable iterable(CurlOpen o, ArgumentList a, CurlClose c)
            => astNode(Iterable, [a], o, a, c);

    "Section 6.6.12 of the specification"
    rule
    shared Tuple tuple(SqOpen o, ArgumentList a, SqClose c)
            => astNode(Tuple, [a], o, a, c);

    "Section 6.6.13 of the specification"
    rule
    shared DynamicValue dynamicValue(Dynamic d, SqOpen o,
            NamedArgument[] n, ArgumentList a, SqClose c)
            => astNode(DynamicValue, [n, a], d, o, n, a, c);

    "Section 6.7.1 of the specification"
    tokenizer
    shared Token<ThenTok>? thenTok(List<Character> input, Object? prev)
            => keyword(ThenTok, input, prev, "then");

    "Section 6.7.1 of the specification"
    rule
    shared IfElseExpression ifElseExpression(IfTok i, Conditions c,
            ThenTok t, DisjoiningExpression|IfElseExpression|LetExpression h,
            ElseTok e, DisjoiningExpression|IfElseExpression|LetExpression x)
            => astNode(IfElseExpression, [c, h, x], i, c, t, h, e, x);

    "Section 6.7.2 of the specification"
    rule
    shared SwitchCaseElseExpression switchCaseElseExpression(SwitchClause s,
            [CaseExpression +] c, [ElseTok,
            DisjoiningExpression|IfElseExpression|LetExpression]? e)
            => astNode(SwitchCaseElseExpression, [s, c, if (exists e) then e[1] else
            null], s, c, e);

    "Section 6.7.2 of the specification"
    rule
    shared CaseExpression caseExpression(CaseTok t, ParOpen o, CaseItem c,
            ParClose e,
            DisjoiningExpression|IfElseExpression|LetExpression x)
            => astNode(CaseExpression, [c, x], t, o, c, e, x);

    "Section 6.7.3 of the specification"
    tokenizer
    shared Token<LetTok>? letTok(List<Character> input, Object? prev)
            => keyword(LetTok, input, prev, "let");

    "Section 6.7.3 of the specification"
    rule
    shared LetExpression letExpression(LetTok l, PatternList p,
            DisjoiningExpression|IfElseExpression|LetExpression x)
            => astNode(LetExpression, [p, x], l, p, x);

    "Section 6.7.3 of the specification"
    rule
    shared PatternList patternList(ParOpen o, CommaSepList<SpecifiedPattern> p,
            ParClose c)
            => astNode(PatternList, [p.nodes], o, p.nodes.chain({c}));

    "Section 6.7.4 of the specification"
    rule
    shared ObjectExpression objectExpression(ObjectTok o, ExtendedType? e,
            SatisfiedTypes? s, ClassBody b)
            => astNode(ObjectExpression, [b, e, s], o, e, s, b);

    "Section 6.8.1 of the specification"
    rule
    shared ElementOrSubrangeExpression elementOrSubrangeExpression(Primary p,
            SqOpen o, Subscript s, SqClose c)
            => astNode(ElementOrSubrangeExpression, [p, s], p, o, s, c);

    "Section 6.8.1 of the specification"
    rule
    shared KeySubscript keySubscript(AddingExpression a)
            => astNode(KeySubscript, [a], a);

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<DotDot>? dotDot(List<Character> input, Object? prev)
            => literal(DotDot, input, prev, "..");

    "Section 6.8.1 of the specification"
    rule
    shared SpanSubscript spanSubscript(AddingExpression a, DotDot d,
            AddingExpression b)
            => astNode(SpanSubscript, [a, b], a, d, b);

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<Colon>? colon(List<Character> input, Object? prev)
            => literal(Colon, input, prev, ":");

    "Section 6.8.1 of the specification"
    rule
    shared MeasureSubscript measureSubscript(AddingExpression a, Colon d,
            AddingExpression b)
            => astNode(MeasureSubscript, [a, b], a, d, b);

    "Section 6.8.1 of the specification"
    rule
    shared SpanFromSubscript spanFromSubscript(AddingExpression a, Ellipsis d)
            => astNode(SpanFromSubscript, [a], a, d);

    "Section 6.8.1 of the specification"
    rule
    shared SpanToSubscript spanToSubscript(Ellipsis d, AddingExpression a)
            => astNode(SpanToSubscript, [a], d, a);

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<PlusPlus>? plusPlus(List<Character> input, Object? prev)
            => literal(PlusPlus, input, prev, "++");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<MinusMinus>? minusMinus(List<Character> input, Object? prev)
            => literal(MinusMinus, input, prev, "--");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<Caret>? caret(List<Character> input, Object? prev)
            => literal(Caret, input, prev, "^");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<Tilde>? tilde(List<Character> input, Object? prev)
            => literal(Tilde, input, prev, "~");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<Slash>? slash(List<Character> input, Object? prev)
            => literal(Slash, input, prev, "/");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<Percent>? percent(List<Character> input, Object? prev)
            => literal(Percent, input, prev, "%");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<StarStar>? starStar(List<Character> input, Object? prev)
            => literal(StarStar, input, prev, "**");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<LTE>? lte(List<Character> input, Object? prev)
            => literal(LTE, input, prev, "<=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<GTE>? gte(List<Character> input, Object? prev)
            => literal(GTE, input, prev, ">=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<Spaceship>? spaceship(List<Character> input, Object? prev)
            => literal(Spaceship, input, prev, "<=>");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<AbsEq>? absEq(List<Character> input, Object? prev)
            => literal(AbsEq, input, prev, "==");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<NEq>? neq(List<Character> input, Object? prev)
            => literal(NEq, input, prev, "!=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<Identical>? identical(List<Character> input, Object? prev)
            => literal(Identical, input, prev, "===");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<AndOp>? andOp(List<Character> input, Object? prev)
            => literal(AndOp, input, prev, "&&");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<OrOp>? orOp(List<Character> input, Object? prev)
            => literal(OrOp, input, prev, "||");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<PlusEq>? plusEq(List<Character> input, Object? prev)
            => literal(PlusEq, input, prev, "+=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<MinusEq>? minusEq(List<Character> input, Object? prev)
            => literal(MinusEq, input, prev, "-=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<StarEq>? starEq(List<Character> input, Object? prev)
            => literal(StarEq, input, prev, "*=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<SlashEq>? slashEq(List<Character> input, Object? prev)
            => literal(SlashEq, input, prev, "/=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<PercentEq>? percentEq(List<Character> input, Object? prev)
            => literal(PercentEq, input, prev, "%=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<AmpersandEq>? ampersandEq(List<Character> input, Object? prev)
            => literal(AmpersandEq, input, prev, "&=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<PipeEq>? pipeEq(List<Character> input, Object? prev)
            => literal(PipeEq, input, prev, "|=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<TildeEq>? tildeEq(List<Character> input, Object? prev)
            => literal(TildeEq, input, prev, "~=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<AndEq>? andEq(List<Character> input, Object? prev)
            => literal(AndEq, input, prev, "&&=");

    "Section 6.8.1 of the specification"
    tokenizer
    shared Token<OrEq>? orEq(List<Character> input, Object? prev)
            => literal(OrEq, input, prev, "||=");

    "Section 6.8.1 of the specification"
    rule
    shared PostfixIncrementOperation postfixIncrementOperation(Primary p,
            PlusPlus o)
            => astNode(PostfixIncrementOperation, [p], p, o);

    "Section 6.8.1 of the specification"
    rule
    shared PostfixDecrementOperation postfixDecrementOperation(Primary p,
            MinusMinus o)
            => astNode(PostfixDecrementOperation, [p], p, o);

    "Section 6.8.1 of the specification"
    rule
    shared PrefixIncrementOperation prefixIncrementOperation(PlusPlus o,
            Primary p)
            => astNode(PrefixIncrementOperation, [p], o, p);

    "Section 6.8.1 of the specification"
    rule
    shared PrefixDecrementOperation prefixDecrementOperation(MinusMinus o,
            Primary p)
            => astNode(PrefixDecrementOperation, [p], o, p);

    "Section 6.8.1 of the specification"
    rule
    shared IdentityOperation identityOperation(Plus o,
            Primary p)
            => astNode(IdentityOperation, [p], o, p);

    "Section 6.8.1 of the specification"
    rule
    shared NegationOperation negationOperation(Minus o,
            Primary p)
            => astNode(NegationOperation, [p], o, p);

    "Section 6.8.1 of the specification"
    rule
    shared ExistsOperation existsOperation(SpanningExpression p,
            Exists o)
            => astNode(ExistsOperation, [p], p, o);

    "Section 6.8.1 of the specification"
    rule
    shared NonemptyOperation nonemptyOperation(SpanningExpression p,
            Nonempty o)
            => astNode(NonemptyOperation, [p], p, o);

    "Section 6.8.1 of the specification"
    rule
    shared NotOperation notOperation(Bang o,
            NegatingExpression p)
            => astNode(NotOperation, [p], o, p);

    "Section 6.8.1 of the specification"
    rule
    shared ExponentiationOperation exponentiatingOperation(
            PrePostfixingExpression b, Caret c, ExponentiatingExpression e)
            => astNode(ExponentiationOperation, [b, e], b, c, e);

    "Section 6.8.1 of the specification"
    rule
    shared IntersectionOperation intersectionOperation(
            IntersectingExpression a, Ampersand o, InvertingExpression b)
            => astNode(IntersectionOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared UnionOperation unionOperation(
            UnioningExpression a, Pipe o, IntersectingExpression b)
            => astNode(UnionOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared ComplementOperation complementOperation(
            UnioningExpression a, Tilde o, IntersectingExpression b)
            => astNode(ComplementOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared ProductOperation productOperation(
            MultiplyingExpression a, Star o, UnioningExpression b)
            => astNode(ProductOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared QuotientOperation quotientOperation(
            MultiplyingExpression a, Slash o, UnioningExpression b)
            => astNode(QuotientOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared RemainderOperation remainderOperation(
            MultiplyingExpression a, Percent o, UnioningExpression b)
            => astNode(RemainderOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared ScaleOperation scaleOperation(
            MultiplyingExpression a, StarStar o, ScalingExpression b)
            => astNode(ScaleOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared SumOperation sumOperation(
            AddingExpression a, Plus o, ScalingExpression b)
            => astNode(SumOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared DifferenceOperation differenceOperation(
            AddingExpression a, Minus o, ScalingExpression b)
            => astNode(DifferenceOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared SpanOperation spanOperation(
            AddingExpression a, DotDot o, AddingExpression b)
            => astNode(SpanOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared MeasureOperation measureOperation(
            AddingExpression a, Colon o, AddingExpression b)
            => astNode(MeasureOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared EntryOperation entryOperation(
            AddingExpression a, Arrow o, AddingExpression b)
            => astNode(EntryOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared LargerOperation largerOperation(
            ExistsNonemptyExpression a, GT o, ExistsNonemptyExpression b)
            => astNode(LargerOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared SmallerOperation smallerOperation(
            ExistsNonemptyExpression a, LT o, ExistsNonemptyExpression b)
            => astNode(SmallerOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared LargeAsOperation largeAsOperation(
            ExistsNonemptyExpression a, GTE o, ExistsNonemptyExpression b)
            => astNode(LargeAsOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared SmallAsOperation smallAsOperation(
            ExistsNonemptyExpression a, LTE o, ExistsNonemptyExpression b)
            => astNode(SmallAsOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared InOperation inOperation(
            ExistsNonemptyExpression a, In o, ExistsNonemptyExpression b)
            => astNode(InOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared IsOperation isOperation(
            ExistsNonemptyExpression a, Is o, Type b)
            => astNode(IsOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared OfOperation ofOperation(
            ExistsNonemptyExpression a, Of o, Type b)
            => astNode(OfOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared CompareOperation compareOperation(
            ExistsNonemptyExpression a, Spaceship o, ExistsNonemptyExpression b)
            => astNode(CompareOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared EqualOperation equalOperation(
            ComparingExpression a, AbsEq o, ComparingExpression b)
            => astNode(EqualOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared NotEqualOperation notEqualOperation(
            ComparingExpression a, NEq o, ComparingExpression b)
            => astNode(NotEqualOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared IdenticalOperation identicalOperation(
            ComparingExpression a, Identical o, ComparingExpression b)
            => astNode(IdenticalOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared AndOperation andOperation(
            ConjoiningExpression a, AndOp o, NegatingExpression b)
            => astNode(AndOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared OrOperation orOperation(
            DisjoiningExpression a, OrOp o, ConjoiningExpression b)
            => astNode(OrOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared ThenOperation thenOperation(
            ThenElseExpression a, ThenTok o, DisjoiningExpression b)
            => astNode(ThenOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared ElseOperation elseOperation(
            ThenElseExpression a, ElseTok o, DisjoiningExpression b)
            => astNode(ElseOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared AddAssignmentOperation addAssignmentOperation(
            ThenElseExpression a, PlusEq o, AssigningExpression b)
            => astNode(AddAssignmentOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared SubtractAssignmentOperation subtractAssignmentOperation(
            ThenElseExpression a, MinusEq o, AssigningExpression b)
            => astNode(SubtractAssignmentOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared MultiplyAssignmentOperation multiplyAssignmentOperation(
            ThenElseExpression a, StarEq o, AssigningExpression b)
            => astNode(MultiplyAssignmentOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared DivideAssignmentOperation divideAssignmentOperation(
            ThenElseExpression a, SlashEq o, AssigningExpression b)
            => astNode(DivideAssignmentOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared RemainderAssignmentOperation remainderAssignmentOperation(
            ThenElseExpression a, PercentEq o, AssigningExpression b)
            => astNode(RemainderAssignmentOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared IntersectAssignmentOperation intersectAssignmentOperation(
            ThenElseExpression a, AmpersandEq o, AssigningExpression b)
            => astNode(IntersectAssignmentOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared UnionAssignmentOperation unionAssignmentOperation(
            ThenElseExpression a, PipeEq o, AssigningExpression b)
            => astNode(UnionAssignmentOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared ComplementAssignmentOperation complementAssignmentOperation(
            ThenElseExpression a, TildeEq o, AssigningExpression b)
            => astNode(ComplementAssignmentOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared AndAssignmentOperation andAssignmentOperation(
            ThenElseExpression a, AndEq o, AssigningExpression b)
            => astNode(AndAssignmentOperation, [a, b], a, o, b);

    "Section 6.8.1 of the specification"
    rule
    shared OrAssignmentOperation orAssignmentOperation(
            ThenElseExpression a, OrEq o, AssigningExpression b)
            => astNode(OrAssignmentOperation, [a, b], a, o, b);

    "Section 6.8.4 of the specification"
    rule
    shared WithinOperation withinOperator(ExistsNonemptyExpression a, LT|LTE b,
            ExistsNonemptyExpression c, LT|LTE d, ExistsNonemptyExpression e)
    {
        Bound aBound;
        Bound eBound;

        if (is LTE b) {
            aBound = ClosedBound(a);
        } else {
            aBound = OpenBound(a);
        }

        if (is LTE d) {
            eBound = ClosedBound(e);
        } else {
            eBound = OpenBound(e);
        }

        return astNode(WithinOperation, [c, aBound, eBound], a, b, c, d, e);
    }

    "Section 6.9 of the specification"
    tokenizer
    shared Token<Tick>? tick(List<Character> input, Object? prev)
            => literal(Tick, input, prev, "`");

    "Section 6.9 of the specification"
    rule
    shared TypeMeta typeMeta(Tick o, Type t, Tick c)
            => astNode(TypeMeta, [t], o, t, c);

    "Section 6.9 of the specification"
    rule
    shared BaseMeta baseMeta(Tick o, PackageQualifier? p,
            MemberNameWithTypeArguments m, Tick c)
            => astNode(BaseMeta, [m, p], o, p, m, c);

    "Section 6.9 of the specification"
    rule
    shared PackageQualifier packageQualifier(PackageTok p, Dot d)
            => astNode(PackageQualifier, [], p, d);

    "Section 6.9 of the specification"
    rule
    shared MemberMeta memberMeta(Tick o, PrimaryType p, Dot d,
            MemberNameWithTypeArguments m, Tick c)
            => astNode(MemberMeta, [p, m], o, p, d, m, c);

    "Section 7.1.1 of the specification"
    rule
    shared Annotations annotations(StringLiteral? s, [Annotation *] a)
            => astNode(Annotations, [s, a], s, a);

    "Section 7.1.1 of the specification"
    rule
    shared Annotation annotation(MemberName m, Arguments? a)
            => astNode(Annotation, [m, a], m, a);
}

import ceylon.parse { Grammar, Token, rule, tokenizer/*, errorConstructor*/ }
import ceylon.language.meta.model { Class }
import ceylon.ast.core {
    AnyCompilationUnit,
    LIdentifier,
    UIdentifier,
    Key,
    ScopedKey
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
        value ret = UIdentifier(text.text);
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
        value ret = LIdentifier(text.text);
        ret.put(tokensKey, [*{ws, start, text}.coalesced]);
        return ret;
    }
}

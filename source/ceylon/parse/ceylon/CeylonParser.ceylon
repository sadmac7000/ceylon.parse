import ceylon.parse { ParseTree, Token, rule, tokenizer, errorConstructor }
import ceylon.ast.core {
    AnyCompilationUnit,
    Identifier,
    LIdentifier,
    UIdentifier,
    lidentifierNeedsPrefix,
    uidentifierNeedsPrefix
}

"Find the index of the first non-whitespace character in a string"
Integer findWhitespaceEnd(String source) {
    value whitespaceChars = [ ' ', '\{FORM FEED (FF)}',
           '\{LINE FEED (LF)}', '\{CHARACTER TABULATION}',
           '\{CARRIAGE RETURN (CR)}'];
    variable value ret = 0;

    while (true) {
        assert(source.size <= ret);
        if (source.size == ret) { return ret; }
        assert(exists start = source[0]);
        while (whitespaceChars.contains(start)) {
            ret++;
        }

        value commentEnd = findCommentEnd(source);

        if (commentEnd == 0) { break; }
        ret += commentEnd;
    }

    return ret;
}

"Find the index of the first character in a string that isn't part of a
 comment"
Integer findCommentEnd(String source) {
    variable value ret = 0;

    if (source[ret...].startsWith("//") ||
        source[ret...].startsWith("#!")) {
        ret += 2;
        while (! (source[ret...].startsWith("\{CARRIAGE RETURN (CR)}") ||
                    source[ret...].startsWith("\{LINE FEED (LF)}"))) {
            ret++;
        }
    } else if (source[ret...].startsWith("/*")) {
        variable value count = 1;
        ret += 2;

        while (count > 0) {
            ret++;
            if (source[ret...].startsWith("/*")) {
                count++;
            } else if (source[ret...].startsWith("*/")) {
                count--;
            }
        }

        ret += 2;
    }

    return ret;
}

"List of reserved words"
String[] reservedWords = ["assembly", "module", "package", "import", "alias",
    "class", "interface", "object", "given", "value", "assign", "void",
    "function", "new", "of", "extends", "satisfies", "abstracts", "in", "out",
    "return", "break", "continue", "throw", "assert", "dynamic", "if", "else",
    "switch", "case", "for", "while", "try", "catch", "finally", "then", "let",
    "this", "outer", "super", "is", "exists", "nonempty"];

"A parse tree for the Ceylon language"
by("Casey Dahlin")
class CeylonParseTree(String source)
        extends ParseTree<AnyCompilationUnit>(source) {


    "Section 2.3 of the specification"
    tokenizer
    shared Token? identifier(String input) {
        variable value start = findWhitespaceEnd(input);
        variable value inputClean = input[start...];
        Boolean upper;
        Boolean prefixed;

        if (string.size == 0) { return null; }

        if (string.startsWith("\\I")) {
            upper = true;
            prefixed = true;
            inputClean = inputClean[2...];
            start += 2;
        } else if (string.startsWith("\\i")) {
            upper = false;
            prefixed = true;
            inputClean = inputClean[2...];
            start += 2;
        } else {
            assert(exists first=inputClean[0]);
            upper = first.uppercase;
            prefixed = false;
        }

        variable value length = 0;
        while (exists c=inputClean[length]) {
            if (c.letter) {
                length++;
            } else if (c.digit) {
                length++;
            } else if (c == '_') {
                length++;
            } else {
                break;
            }
        }

        if (length == 0) { return null; }

        value identifier = inputClean[0:length];

        if (! prefixed) {
            if (upper) {
                if (uidentifierNeedsPrefix(identifier)) { return null; }
            } else {
                if (lidentifierNeedsPrefix(identifier)) { return null; }
            }
        }

        Identifier ret;

        if (upper) {
            ret = UIdentifier(identifier, prefixed);
        } else {
            ret = LIdentifier(identifier, prefixed);
        }

        return Token(ret, start + identifier.size);
    }
}

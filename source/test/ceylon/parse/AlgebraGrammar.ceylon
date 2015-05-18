import ceylon.parse { ... }

class Expr(Integer pos = 0, Sym* children) extends Sym(pos, *children) {}

class Var(String name, Integer pos = 0, shared actual Object? prevError = null)
        extends Expr(pos) {
    shared actual String shortName => super.shortName + " \"``name``\"";
}
class Plus(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class Minus(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class Mul(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class Div(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class LParen(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}
class RParen(Integer pos = 0, shared actual Object? prevError = null) extends Sym(pos) {}

class AlgebraGrammar() extends Grammar<String>() {
    tokenizer
    shared Token<Var>? var(String input, Object? last) {
        String varChars = "abcdefghijklmnopqrstuvwxyz";
        Integer position;
        Object? prevError;

        assert(exists chr = input.first);

        if (is Sym last) {
            position = last.position + 1;
            prevError = null;
        } else if (is Crap last) {
            position = last.position + last.data.size;
            prevError = last;
        } else {
            position = 0;
            prevError = null;
        }

        if (varChars.contains(chr)) {
            return Token(Var(chr.string, position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<Plus>? plus(String input, Object? last) {
        Integer position;
        Object? prevError;

        if (is Sym last) {
            position = last.position + 1;
            prevError = null;
        } else if (is Crap last) {
            position = last.position + last.data.size;
            prevError = last;
        } else {
            position = 0;
            prevError = null;
        }

        if (input.startsWith("+")) {
            return Token(Plus(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<Minus>? minus(String input, Object? last) {
        Integer position;
        Object? prevError;

        if (is Sym last) {
            position = last.position + 1;
            prevError = null;
        } else if (is Crap last) {
            position = last.position + last.data.size;
            prevError = last;
        } else {
            position = 0;
            prevError = null;
        }

        if (input.startsWith("-")) {
            return Token(Minus(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<Mul>? mull(String input, Object? last) {
        Integer position;
        Object? prevError;

        if (is Sym last) {
            position = last.position + 1;
            prevError = null;
        } else if (is Crap last) {
            position = last.position + last.data.size;
            prevError = last;
        } else {
            position = 0;
            prevError = null;
        }

        if (input.startsWith("*")) {
            return Token(Mul(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<Div>? divv(String input, Object? last) {
        Integer position;
        Object? prevError;

        if (is Sym last) {
            position = last.position + 1;
            prevError = null;
        } else if (is Crap last) {
            position = last.position + last.data.size;
            prevError = last;
        } else {
            position = 0;
            prevError = null;
        }

        if (input.startsWith("/")) {
            return Token(Div(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<LParen>? lparen(String input, Object? last) {
        Integer position;
        Object? prevError;

        if (is Sym last) {
            position = last.position + 1;
            prevError = null;
        } else if (is Crap last) {
            position = last.position + last.data.size;
            prevError = last;
        } else {
            position = 0;
            prevError = null;
        }

        if (input.startsWith("(")) {
            return Token(LParen(position, prevError), 1);
        } else {
            return null;
        }
    }

    tokenizer
    shared Token<RParen>? rparen(String input, Object? last) {
        Integer position;
        Object? prevError;

        if (is Sym last) {
            position = last.position + 1;
            prevError = null;
        } else if (is Crap last) {
            position = last.position + last.data.size;
            prevError = last;
        } else {
            position = 0;
            prevError = null;
        }

        if (input.startsWith(")")) {
            return Token(RParen(position, prevError), 1);
        } else {
            return null;
        }
    }

    rule
    shared Expr parenExpression(LParen l, Expr e, RParen r) => e;

    shared actual Crap badTokenConstructor(String data, Object? last) {
        if (is Sym last) {
            return Crap(data, last.position + 1);
        } else if (is Crap last) {
            return Crap(data, last.position + last.data.size);
        } else {
            return Crap(data);
        }
    }

    errorConstructor
    shared Mul error(Object? replaces, Object? last) {
        return Mul(0);
    }
}

import ceylon.parse.regular { any, lit, not }
import ceylon.test { test }

test
shared void backtrack() {
    value exp  = lit("a").zeroPlus + lit("ab");
    assert(exp.match("aaaaaaab") exists);
}

test
shared void maybe() {
    value exp  = lit("a") + any("bcd").maybe + lit("e");
    assert(exp.match("ae") exists);
    assert(exp.match("abe") exists);
    assert(exp.match("ace") exists);
    assert(exp.match("ade") exists);
    assert(! exp.match("afe") exists);
    assert(! exp.match("abce") exists);
}

test
shared void andTest() {
    value exp = lit("abc").and(any("abcd").repeat(4));

    assert(exp.match("abca") exists);
    assert(exp.match("abcb") exists);
    assert(exp.match("abcd") exists);
    assert(! exp.match("abc") exists);
    assert(! exp.match("abdd") exists);
    assert(exists k = exp.match("abcd"), k == 4);

    value exp2 = lit("abc").and(any("abcd").repeat(2));

    assert(exists k2 = exp2.match("abc"), k2 == 2);
}

test
shared void orTest() {
    value exp = lit("abc").or(lit("def"));

    assert(exp.match("abc") exists);
    assert(exp.match("def") exists);
    assert(! exp.match("abf") exists);
}

test
shared void repeatTest() {
    value exp = lit("a").repeat(3, 5);

    assert(! exp.match("a") exists);
    assert(! exp.match("aa") exists);
    assert(exp.match("aaa") exists);
    assert(exp.match("aaaa") exists);
    assert(exp.match("aaaaa") exists);
    assert(exists k = exp.match("aaaaaa"), k == 5);
}

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

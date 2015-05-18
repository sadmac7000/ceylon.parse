import ceylon.collection { HashSet }

"Concatenate two lists of objects. If they are strings use string
 concatenation."
List<Char> cat<Char>(List<Char> a, List<Char> b) {
    if (is String a, b.size == 0) {
        return a;
    }

    if (is String b, a.size == 0) {
        return b;
    }

    if (is String a, is String b) {
        assert(is List<Char>  r = a + b);
        return r;
    }

    return concatenate(a, b);
}

"Result of a string match"
shared interface MatchResult<Char> of Res<Char> {
    shared formal Integer length;
    shared formal List<Char> matched;
}

"Our private MatchResult"
class Res<Char>(shared actual List<Char> matched,
        shared actual Integer length)
        satisfies MatchResult<Char> {
    shared default Res<Char>? backtrack = null;
}

"A regular language for string matching."
shared abstract class Regular<Char>() satisfies Summable<Regular<Char>>
        given Char satisfies Object {

    "Match the start of sequence [[s]] against this language. Return a
     [[MatchResult]] or `null` if no match. [[maxLength]] prevents the
     return of a match longer than the given length when specified, though we
     can still look ahead further for context to determine if the match is
     successful."
    shared formal MatchResult<Char>? match(List<Char> s, Integer? maxLength = null);

    "Get a new regular language that repeats this language [[min]] times, or
     between [[min]] and [[max]] times."
    shared Regular<Char> repeat(Integer min, Integer? max = null)
        => if (exists m = max) then Repeat(this, min, max)
        else Repeat(this, min, min);

    "Get a new regular language that matches this language or the empty string."
    shared Regular<Char> maybe => Repeat(this, 0, 1);

    "Get a new regular language that matches this language repeated [[m]] or
     more times."
    shared Regular<Char> atLeast(Integer m) => Repeat(this, m, null);

    "Get a new regular language that matches this language repeated [[m]] or
     fewer times."
    shared Regular<Char> atMost(Integer m) => Repeat(this, 0, m);

    "Get a new regular language that matches this language repeated zero or
     more times."
    shared Regular<Char> zeroPlus => atLeast(0);

    "Concatenate this and another regular language."
    shared actual Regular<Char> plus(Regular<Char> r) => Concat(this, r);

    "Get a new regular language that matches only the overlap of this and
     [[r]], that is, use this language as lookahead, and if successful, match
     [[r]]."
    shared Regular<Char> and(Regular<Char> r) => Concat(Lookahead(this, false), r);

    "Get a new regular language that matches this language or the language
     [[r]]."
    shared Regular<Char> or(Regular<Char> r) => Disjoin(this, r);
}

"Regular language that matches any of a set of characters"
class Any<Char>({Char *} valuesIn) extends Regular<Char>()
        given Char satisfies Object {
    "All characters that we match"
    value values = HashSet{ *valuesIn };

    shared actual MatchResult<Char>? match(List<Char> s, Integer? maxLength) {
        if (exists maxLength, maxLength < 1) { return null; }
        if (exists c = s[0], values.contains(c)) { return Res(s[0:1], 1); }
        return null;
    }
}

"Regular language that matches a literal string"
class Literal<Char>(List<Char> val) extends Regular<Char>()
        given Char satisfies Object {
    shared actual MatchResult<Char>? match(List<Char> s, Integer? maxLength) {
        if (exists maxLength, maxLength < val.size) { return null; }
        if (s.startsWith(val)) { return Res(s[0:val.size], val.size); }
        return null;
    }
}

"Regular language that matches another language, but always returns a
 zero-length match result. If [[invert]] is set, we return zero when [[r]]
 doesn't match, and `null` when it does."
class Lookahead<Char>(Regular<Char> r, Boolean invert) extends Regular<Char>()
        given Char satisfies Object {
    Boolean subMatch(List<Char> s)
        => if (r.match(s) exists) then !invert else invert;

    shared actual MatchResult<Char>? match(List<Char> s, Integer? maxLength)
        => if (subMatch(s)) then Res<Char>([], 0) else null;
}

"Regular language that concatenates two other languages"
class Concat<Char>(Regular<Char> a, Regular<Char> b) extends Regular<Char>()
        given Char satisfies Object {

    "Match result with appropriate backtracking"
    class CRes(List<Char> s, Integer? maxLength, Res<Char> aRes, Res<Char> bRes)
            extends Res<Char>(cat(aRes.matched, bRes.matched),
                              aRes.length + bRes.length) {
        shared actual Res<Char>? backtrack {
            if (exists b = bRes.backtrack) {
                return CRes(s, maxLength, aRes, b);
            }

            return outer.resultB(s, maxLength, aRes.backtrack);
        }
    }

    shared actual MatchResult<Char>? match(List<Char> s, Integer? maxLength)
            => resultB(s, maxLength, a.match(s, maxLength));

    "Get the second part of the match result"
    Res<Char>? resultB(List<Char> s, Integer? maxLength,
            variable MatchResult<Char>? aRes) {
        while (exists a = aRes) {
            value nextLength = if (exists maxLength) then maxLength - a.length
                else null;
            value next = b.match(s[a.length...], nextLength);

            if (exists n = next) { return CRes(s, maxLength, a of Res<Char>, n
                    of Res<Char>); }

            aRes = (a of Res<Char>).backtrack;
        }

        return null;
    }
}

"Regular language that repeats a given language several times."
class Repeat<Char>(Regular<Char> r, Integer min, Integer? max)
        extends Regular<Char>()
        given Char satisfies Object {
    "Local match result"
    class RRes(List<Char> s, Integer count, RRes? prev, Res<Char>? cur)
        extends Res<Char>(cat(prev?.matched else [], cur?.matched else []),
                    (prev?.length else 0) + (cur?.length else 0)) {

        shared actual Res<Char>? backtrack {
            if (exists c = cur?.backtrack) {
                return RRes(s, count, prev, c);
            } else {
                return prev?.advance(length - 1);
            }
        }

        shared Res<Char>? advance(Integer? maxLength) {
            if (exists maxLength, length > maxLength) {
                value b = backtrack;
                if (is RRes b) {
                    return b.advance(maxLength);
                }

                return b;
            }

            if (exists maxLength, length == maxLength) {
                return count >= outer.min then this;
            }

            if (exists m = outer.max, count == m) { return this; }
            if (exists c = cur?.length, c == 0) { return this; }

            value target = s[(cur?.length else 0)...];
            value next = outer.r.match(target, maxLength);

            if (exists next) {
                return RRes(target, count + 1, this, next of Res<Char>).advance(maxLength);
            }

            return count >= outer.min then this;
        }
    }

    shared actual MatchResult<Char>? match(List<Char> s, Integer? maxLength)
            => RRes(s, 0, null, null).advance(maxLength);
}

"Regular language that matches either of two other languages"
class Disjoin<Char>(Regular<Char> a, Regular<Char> b) extends Regular<Char>()
        given Char satisfies Object {

    "Special result for disjoins"
    class DRes<Char>(Res<Char> aRes, Res<Char> bRes)
        extends Res<Char>(aRes.length > bRes.length then aRes.matched else
                bRes.matched, max{aRes.length, bRes.length}) {
        shared actual Res<Char>? backtrack {
            if (aRes.length > bRes.length) {
                value back = aRes.backtrack;

                if (exists back) {
                    return DRes(back, bRes);
                }

                return bRes;
            }

            if (aRes.length < bRes.length) {
                value back = bRes.backtrack;

                if (exists back) {
                    return DRes(aRes, back);
                }

                return aRes;
            }

            value aBack = aRes.backtrack;
            value bBack = bRes.backtrack;

            if (exists aBack, exists bBack) {
                return DRes(aBack, bBack);
            }

            return aBack else bBack;
        }
    }

    shared actual MatchResult<Char>? match(List<Char> s, Integer? maxLength) {
        value aRes = a.match(s, maxLength);
        value bRes = b.match(s, maxLength);

        if (exists aRes, exists bRes) { return DRes(aRes of Res<Char>, bRes of
                Res<Char>); }

        return aRes else bRes;
    }
}

"Get a regular language that matches one of any of the characters in a given list."
shared Regular<Char> any<Char>({Char *} c)
        given Char satisfies Object
    => Any(c);

"Get a regular language that matches only the given literal string"
shared Regular<Char> lit<Char>(List<Char> c)
        given Char satisfies Object
    => Literal(c);

"Get a regular language that matches only when the given language does not
 match. The resulting match is always zero-length."
shared Regular<Char> not<Char>(Regular<Char> r)
        given Char satisfies Object
    => Lookahead(r, true);

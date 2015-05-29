import ceylon.collection { HashSet }

"Result of a string match"
shared interface MatchResult of Res {
    shared formal Integer length;
    shared formal String matched;
}

"Our private MatchResult"
class Res(shared actual String matched,
        shared actual Integer length)
        satisfies MatchResult {
    shared default Res? backtrack = null;
}

"A regular language for string matching."
shared abstract class Regular() satisfies Summable<Regular> {

    "Match the position in string [[s]] against this language. Return a
     [[MatchResult]] or `null` if no match. [[maxLength]] prevents the
     return of a match longer than the given length when specified, though we
     can still look ahead further for context to determine if the match is
     successful."
    shared formal MatchResult? matchAt(Integer start, String s,
            Integer? maxLength = null);

    "Match the start of the given string against this language"
    shared MatchResult? match(String s, Integer? maxLength = null)
            => matchAt(0, s, maxLength);

    "Get a new regular language that repeats this language [[min]] times, or
     between [[min]] and [[max]] times."
    shared Regular repeat(Integer min, Integer? max = null)
        => if (exists m = max) then Repeat(this, min, max)
        else Repeat(this, min, min);

    "Get a new regular language that matches this language or the empty string."
    shared Regular maybe => Repeat(this, 0, 1);

    "Get a new regular language that matches this language repeated [[m]] or
     more times."
    shared Regular atLeast(Integer m) => Repeat(this, m, null);

    "Get a new regular language that matches this language repeated [[m]] or
     fewer times."
    shared Regular atMost(Integer m) => Repeat(this, 0, m);

    "Get a new regular language that matches this language repeated zero or
     more times."
    shared Regular zeroPlus => atLeast(0);

    "Concatenate this and another regular language."
    shared actual Regular plus(Regular r) => Concat(this, r);

    "Get a new regular language that matches only the overlap of this and
     [[r]], that is, use this language as lookahead, and if successful, match
     [[r]]."
    shared Regular and(Regular r) => Concat(Lookahead(this, false), r);

    "Get a new regular language that matches this language or the language
     [[r]]."
    shared Regular or(Regular r) => Disjoin(this, r);
}

"Regular language that matches any of a set of characters"
class Any({Character *} valuesIn) extends Regular() {
    "All characters that we match"
    value values = HashSet{ *valuesIn };

    shared actual MatchResult? matchAt(Integer position, String s,
            Integer? maxLength) {
        if (exists maxLength, maxLength < 1) { return null; }
        if (exists c = s[position], values.contains(c)) { return
            Res(s[position:1], 1); }
        return null;
    }
}

"Regular language that matches a literal string"
class Literal(String val) extends Regular() {
    shared actual MatchResult? matchAt(Integer position,
            String s, Integer? maxLength) {
        if (exists maxLength, maxLength < val.size) { return null; }
        if (s.includesAt(position, val)) { return Res(s[position:val.size], val.size); }
        return null;
    }
}

"Regular language that matches another language, but always returns a
 zero-length match result. If [[invert]] is set, we return zero when [[r]]
 doesn't match, and `null` when it does."
class Lookahead(Regular r, Boolean invert) extends Regular() {
    Boolean subMatchAt(Integer position, String s)
        => if (r.matchAt(position, s) exists) then !invert else invert;

    shared actual MatchResult? matchAt(Integer position, String s,
            Integer? maxLength)
        => if (subMatchAt(position, s)) then Res("", 0) else null;
}

"Regular language that concatenates two other languages"
class Concat(Regular a, Regular b) extends Regular() {
    "Match result with appropriate backtracking"
    class CRes(Integer pos, String s, Integer? maxLength, Res aRes, Res bRes)
            extends Res(aRes.matched + bRes.matched,
                              aRes.length + bRes.length) {
        shared actual Res? backtrack {
            if (exists b = bRes.backtrack) {
                return CRes(pos, s, maxLength, aRes, b);
            }

            return outer.resultB(pos, s, maxLength, aRes.backtrack);
        }
    }

    shared actual MatchResult? matchAt(Integer position,
            String s, Integer? maxLength)
            => resultB(position, s, maxLength, a.matchAt(position, s, maxLength));

    "Get the second part of the match result"
    Res? resultB(Integer pos, String s, Integer? maxLength,
            variable MatchResult? aRes) {
        while (exists a = aRes) {
            value nextLength = if (exists maxLength) then maxLength - a.length
                else null;
            value next = b.matchAt(pos + a.length, s, nextLength);

            if (exists n = next) { return CRes(pos, s, maxLength, a of Res, n
                    of Res); }

            aRes = (a of Res).backtrack;
        }

        return null;
    }
}

"Regular language that repeats a given language several times."
class Repeat(Regular r, Integer min, Integer? max)
        extends Regular() {
    "Local match result"
    class RRes(Integer pos, String s, Integer count, RRes? prev, Res? cur)
        extends Res((prev?.matched else "") + (cur?.matched else ""),
                    (prev?.length else 0) + (cur?.length else 0)) {

        shared actual Res? backtrack {
            if (exists c = cur?.backtrack) {
                return RRes(pos, s, count, prev, c);
            } else {
                return prev?.advance(length - 1);
            }
        }

        shared Res? advance(Integer? maxLength) {
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

            value newPos = pos + (cur?.length else 0);
            value next = outer.r.matchAt(newPos, s,
                    maxLength);

            if (exists next) {
                return RRes(newPos, s, count + 1, this, next of Res).advance(maxLength);
            }

            return count >= outer.min then this;
        }
    }

    shared actual MatchResult? matchAt(Integer position, String s,
            Integer? maxLength)
            => RRes(position, s, 0, null, null).advance(maxLength);
}

"Regular language that matches either of two other languages"
class Disjoin(Regular a, Regular b) extends Regular() {

    "Special result for disjoins"
    class DRes(Res aRes, Res bRes)
        extends Res(aRes.length > bRes.length then aRes.matched else
                bRes.matched, max{aRes.length, bRes.length}) {
        shared actual Res? backtrack {
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

    shared actual MatchResult? matchAt(Integer position, String s, Integer? maxLength) {
        value aRes = a.matchAt(position, s, maxLength);
        value bRes = b.matchAt(position, s, maxLength);

        if (exists aRes, exists bRes) { return DRes(aRes of Res, bRes of
                Res); }

        return aRes else bRes;
    }
}

"Get a regular language that matches one of any of the characters in a given list."
shared Regular any({Character *} c)
    => Any(c);

"Get a regular language that matches only the given literal string"
shared Regular lit(String c)
    => Literal(c);

"Get a regular language that matches only when the given language does not
 match. The resulting match is always zero-length."
shared Regular not(Regular r)
    => Lookahead(r, true);

shared object anyChar extends Regular() {
    shared actual MatchResult? matchAt(Integer position, String s,
            Integer? maxLength) {
        if (exists maxLength, maxLength < 1) { return null; }
        if (exists c = s[position]) { return Res(s[0:1], 1); }
        return null;
    }
}

shared object anyLetter extends Regular() {
    shared actual MatchResult? matchAt(Integer position, String s,
            Integer? maxLength) {
        if (exists maxLength, maxLength < 1) { return null; }
        if (exists c = s[position], c.letter) { return Res(s[0:1], 1); }
        return null;
    }
}

shared object anyDigit extends Regular() {
    shared actual MatchResult? matchAt(Integer position, String s,
            Integer? maxLength) {
        if (exists maxLength, maxLength < 1) { return null; }
        if (exists c = s[position], c.digit) { return Res(s[0:1], 1); }
        return null;
    }
}

shared object anyUpper extends Regular() {
    shared actual MatchResult? matchAt(Integer position, String s,
            Integer? maxLength) {
        if (exists maxLength, maxLength < 1) { return null; }
        if (exists c = s[position], c.uppercase) { return Res(s[0:1], 1); }
        return null;
    }
}

shared object anyLower extends Regular() {
    shared actual MatchResult? matchAt(Integer position, String s,
            Integer? maxLength) {
        if (exists maxLength, maxLength < 1) { return null; }
        if (exists c = s[position], c.lowercase) { return Res(s[0:1], 1); }
        return null;
    }
}

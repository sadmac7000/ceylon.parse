import ceylon.collection { HashSet }

"A regular language for string matching."
shared abstract class Regular() satisfies Summable<Regular> {

    "Match the start of sequence [[s]] against this language. Return the match
     length or `null` if no match. [[maxLength]] prevents the return of a
     match longer than the given length when specified, though we can still
     look ahead further for context to determine if the match is successful."
    shared formal Integer? match(String s, Integer? maxLength = null);

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

    shared actual Integer? match(String s, Integer? maxLength) {
        if (exists maxLength, maxLength < 1) { return null; }
        if (exists c = s[0], values.contains(c)) { return 1; }
        return null;
    }
}

"Regular language that matches a literal string"
class Literal(String val) extends Regular() {
    shared actual Integer? match(String s, Integer? maxLength) {
        if (exists maxLength, maxLength < val.size) { return null; }
        if (s.startsWith(val)) { return 1; }
        return null;
    }
}

"Regular language that matches another language, but always returns a
 zero-length match result. If [[invert]] is set, we return zero when [[r]]
 doesn't match, and `null` when it does."
class Lookahead(Regular r, Boolean invert) extends Regular() {
    shared Boolean subMatch(String s)
        => if (r.match(s) exists) then !invert else invert;

    shared actual Integer? match(String s, Integer? maxLength)
        => if (subMatch(s)) then 0 else null;
}

"Regular language that concatenates two other languages"
class Concat(Regular a, Regular b) extends Regular() {
    shared actual Integer? match(String s, Integer? maxLength) {

        variable Integer? start = a.match(s, maxLength);

        while (exists st = start) {
            Integer? nextLength = if (exists maxLength) then maxLength - st
                else null;
            Integer? next = b.match(s[st...], nextLength);

            if (exists n = next) { return st + n; }

            start = if (st > 0) then a.match(s, st - 1) else null;
        }

        return null;
    }
}

"Regular language that repeats a given language several times."
class Repeat(Regular r, Integer min, Integer? max) extends Regular() {
    shared actual Integer? match(String s, Integer? maxLength) {
        variable Regular matching = r;
        variable Integer count  = 0;

        variable Integer? matched = r.match(s, maxLength);
        variable Integer last = 0;
        
        while (exists m = matched) {
            last = m;
            count++;
            if (exists max, count == max) { return m; }

            matching = Concat(matching, r);
            matched = matching.match(s, maxLength);
        }

        if (count >= min) { return last; }
        return null;
    }
}

"Regular language that matches either of two other languages"
class Disjoin(Regular a, Regular b) extends Regular() {
    shared actual Integer? match(String s, Integer? maxLength) {
        Integer? m = a.match(s, maxLength);

        if (exists m) { return m; }
        return b.match(s, maxLength);
    }
}

"Get a regular language that matches one of any of the characters in a given list."
shared Regular any({Character *} c) => Any(c);

"Get a regular language that matches only the given literal string"
shared Regular lit(String c) => Literal(c);

"Get a regular language that matches only when the given language does not
 match. The resulting match is always zero-length."
shared Regular not(Regular r) => Lookahead(r, true);

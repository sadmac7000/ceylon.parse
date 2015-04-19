import ceylon.collection { HashSet }

shared abstract class Regular() satisfies Summable<Regular> {
    shared formal Integer? match(String s, Integer? maxLength = null);

    shared Regular repeat(Integer min, Integer? max = null)
        => if (exists m = max) then Repeat(this, min, max)
        else Repeat(this, min, min);

    shared Regular maybe => Repeat(this, 0, 1);
    shared Regular atLeast(Integer m) => Repeat(this, m, null);
    shared Regular atMost(Integer m) => Repeat(this, 0, m);
    shared Regular zeroPlus => atLeast(0);

    shared actual Regular plus(Regular r) => Concat(this, r);

    shared Regular and(Regular r) => Concat(Lookahead(this, false), r);
    shared Regular or(Regular r) => Disjoin(this, r);
}

class Any({Character *} valuesIn) extends Regular() {
    value values = HashSet{ *valuesIn };

    shared actual Integer? match(String s, Integer? maxLength) {
        if (exists maxLength, maxLength < 1) { return null; }
        if (exists c = s[0], values.contains(c)) { return 1; }
        return null;
    }
}

class Literal(String val) extends Regular() {
    shared actual Integer? match(String s, Integer? maxLength) {
        if (exists maxLength, maxLength < val.size) { return null; }
        if (s.startsWith(val)) { return 1; }
        return null;
    }
}

class Lookahead(Regular r, Boolean invert) extends Regular() {
    shared Boolean subMatch(String s)
        => if (r.match(s) exists) then !invert else invert;

    shared actual Integer? match(String s, Integer? maxLength)
        => if (subMatch(s)) then null else 0;
}

class Concat(Regular a, Regular b) extends Regular() {
    shared actual Integer? match(String s, Integer? maxLength) {

        variable Integer? start = a.match(s, maxLength);

        while (exists st = start) {
            Integer? nextLength = if (exists maxLength) then maxLength - st
                else null;
            Integer? next = b.match(s[st...], nextLength);

            if (exists n = next) { return st + n; }

            start = a.match(s, st - 1);
        }

        return null;
    }
}

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

class Disjoin(Regular a, Regular b) extends Regular() {
    shared actual Integer? match(String s, Integer? maxLength) {
        Integer? m = a.match(s, maxLength);

        if (exists m) { return m; }
        return b.match(s, maxLength);
    }
}

shared Regular any({Character *} c) => Any(c);
shared Regular lit(String c) => Literal(c);
shared Regular not(Regular r) => Lookahead(r, true);

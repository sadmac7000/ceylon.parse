"A base class for symbols in the test that defines a few handy features."
class Sym(shared variable Integer position = 0, Sym* children) {
    shared default Object? prevError = null;

    shared actual Integer hash {
        return string.hash;
    }

    shared actual Boolean equals(Object other) {
        return string.equals(other.string);
    }

    shared String sexp {
        String prefix;

        if (exists p = prevError) {
            prefix = "(``p``)";
        } else {
            prefix = "";
        }

        if (children.size == 0) {
            return prefix + shortName;
        }

        return "``prefix + shortName`` ``[for (x in children) x.sexp]``";
    }

    shared actual String string => "[``sexp``]";

    shared default String shortName {
        value start = className(this);
        assert(exists properIdx = start.lastOccurrence('.'));
        return start[(properIdx+1)...] + "@``position``";
    }
}

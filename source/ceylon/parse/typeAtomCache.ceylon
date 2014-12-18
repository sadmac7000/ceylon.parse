import ceylon.language.meta.model { Type }
import ceylon.collection { HashMap, HashSet }

"A type atom"
shared class Atom(Type t) {
    shared actual Integer hash = typeAtomCache.getAlias(t);

    shared actual Boolean equals(Object other) {
        if (is Atom other) {
            return other.hash == hash;
        }

        return false;
    }

    shared Boolean subtypeOf(Atom other) {
        return typeAtomCache.supertypeSet(hash).contains(other.hash);
    }

    shared Boolean supertypeOf(Atom other) => other.subtypeOf(this);

    shared Type type => typeAtomCache.resolve(hash);

    shared actual String string => type.string;
}

"We have to convert type objects to integers to pass them around, otherwise we
 encounter weird performance issues."
object typeAtomCache {
    value from = HashMap<Type, Integer>();
    value to = HashMap<Integer, Type>();
    value subtypes = HashMap<Integer, HashSet<Integer>>();
    value supertypes = HashMap<Integer, HashSet<Integer>>();
    variable value next = 0;

    "Get an alias for a type"
    shared Integer getAlias(Type t) {
        if (from.defines(t)) {
            value ret = from[t];
            assert(exists ret);
            return ret;
        }

        value mySubtypes = HashSet<Integer>();
        value mySupertypes = HashSet<Integer>();

        for (k->v in from) {
            if (k.subtypeOf(t)) {
                mySubtypes.add(v);
                assert(exists s = supertypes[v]);
                s.add(next);
            }

            if (k.supertypeOf(t)) {
                mySupertypes.add(v);
                assert(exists s = subtypes[v]);
                s.add(next);
            }
        }

        from.put(t, next);
        to.put(next, t);
        mySubtypes.add(next);
        mySupertypes.add(next);
        subtypes.put(next, mySubtypes);
        supertypes.put(next, mySupertypes);
        return next++;
    }

    shared Set<Integer> subtypeSet(Integer i) {
        assert(exists ret = subtypes[i]);
        return ret;
    }

    shared Set<Integer> supertypeSet(Integer i) {
        assert(exists ret = supertypes[i]);
        return ret;
    }

    "Resolve a type"
    shared Type resolve(Integer i) {
        value ret = to[i];
        assert(exists ret);
        return ret;
    }
}

"Type atom for the null type"
Atom nullAtom = Atom(`Null`);

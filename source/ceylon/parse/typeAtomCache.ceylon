import ceylon.language.meta.model { Type }
import ceylon.collection { HashMap, HashSet }

"A type atom"
shared class Atom {
    shared Integer val;

    shared new (Type<> t) {
        this.val = typeAtomCache.getAlias(t);
    }

    shared new byHash(Integer hash) {
        this.val = hash;
    }

    shared actual Integer hash => val;

    shared actual Boolean equals(Object other)
        => if (is Atom other) then other.val == val else false;

    shared Boolean subtypeOf(Atom other)
        => typeAtomCache.subtypeSet(other.val).contains(this);

    shared Set<Atom> subtypes => typeAtomCache.subtypeSet(val);

    shared Set<Atom> supertypes => typeAtomCache.supertypeSet(val);

    shared Boolean supertypeOf(Atom other) => other.subtypeOf(this);

    shared Type<> type => typeAtomCache.resolve(val);

    shared actual String string => filterTypes(type.string);
}

"Remove noise from type names"
String filterTypes(String typeName)
    => typeName.replace("ceylon.language::","")
        .replace("ceylon.parse.ceylon::", "");

"We have to convert type objects to integers to pass them around, otherwise we
 encounter weird performance issues."
object typeAtomCache {
    value from = HashMap<Type<>, Integer>();
    value to = HashMap<Integer, Type<>>();
    value subtypes = HashMap<Integer, HashSet<Atom>>();
    value supertypes = HashMap<Integer, HashSet<Atom>>();
    variable value next = 0;

    "Get an alias for a type"
    shared Integer getAlias(Type<> t) {
        if (from.defines(t)) {
            value ret = from[t];
            assert(exists ret);
            return ret;
        }

        value mySubtypes = HashSet<Atom>();
        value mySupertypes = HashSet<Atom>();

        for (k->v in from) {
            if (k.subtypeOf(t)) {
                mySubtypes.add(Atom.byHash(v));
                supertypes[v]?.add(Atom.byHash(next));
            }

            if (k.supertypeOf(t)) {
                mySupertypes.add(Atom.byHash(v));
                subtypes[v]?.add(Atom.byHash(next));
            }
        }

        from.put(t, next);
        to.put(next, t);
        mySubtypes.add(Atom.byHash(next));
        mySupertypes.add(Atom.byHash(next));
        subtypes.put(next, mySubtypes);
        supertypes.put(next, mySupertypes);
        return next++;
    }

    shared Set<Atom> subtypeSet(Integer i) {
        assert(exists ret = subtypes[i]);
        return ret;
    }

    shared Set<Atom> supertypeSet(Integer i) {
        assert(exists ret = supertypes[i]);
        return ret;
    }

    "Resolve a type"
    shared Type<> resolve(Integer i) {
        value ret = to[i];
        assert(exists ret);
        return ret;
    }
}

"Type atom for the null type"
Atom nullAtom = Atom(`Null`);

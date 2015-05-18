import ceylon.language.meta.model { Type }
import ceylon.collection { HashMap, HashSet }

"A type atom"
shared class Atom {
    shared Integer val;

    shared new (Type t) {
        this.val = typeAtomCache.getAlias(t);
    }

    new ByHash(Integer hash) {
        this.val = hash;
    }

    shared actual Integer hash => val;

    shared actual Boolean equals(Object other)
        => if (is Atom other) then other.val == val else false;

    shared Boolean subtypeOf(Atom other)
        => typeAtomCache.subtypeSet(other.val).contains(val);

    shared Set<Atom> subtypes => HashSet<Atom>{ for (x in
            typeAtomCache.subtypeSet(val)) Atom.ByHash(x) };

    shared Set<Atom> supertypes => HashSet<Atom>{ for (x in
            typeAtomCache.supertypeSet(val)) Atom.ByHash(x) };

    shared Boolean supertypeOf(Atom other) => other.subtypeOf(this);

    shared Type type => typeAtomCache.resolve(val);

    shared actual String string => filterTypes(type.string);
}

"Remove noise from type names"
String filterTypes(String typeName)
    => typeName.replace("ceylon.language::","")
        .replace("ceylon.parse.ceylon::", "");

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
                supertypes[v]?.add(next);
            }

            if (k.supertypeOf(t)) {
                mySupertypes.add(v);
                subtypes[v]?.add(next);
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

import ceylon.collection {
	HashSet,
	IdentitySet
}

alias IdentifiableSet<T> given T satisfies Object => Set<T>&Identifiable;

class CompoundedSet<T>(shared Set<T> local = HashSet<T>(), {IdentifiableSet<T>*} sources_in = {})
	satisfies Set<T> 
	given T satisfies Object {
	shared IdentitySet<IdentifiableSet<T>> sources =
		IdentitySet<IdentifiableSet<T>>{elements=sources_in;};

	shared void compound(IdentifiableSet<T> s) {
		sources.add(s);
	}

	shared Set<T> flat => getFlat(IdentitySet<CompoundedSet<T>>());

	Set<T> getFlat(IdentitySet<CompoundedSet<T>> flatstack) {
		HashSet<T> ret = HashSet<T>();

		if (flatstack.contains(this)) {
			return ret;
		}

		flatstack.add(this);
		ret.addAll(local);

		for (set in sources) {
			if (is CompoundedSet<T> set) {
				ret.addAll(set.getFlat(flatstack));
			} else {
				ret.addAll(set);
			}
		}

		return ret;
	}

	shared actual Integer hash {
		variable value count = sources.size;
		variable Integer ret = local.hash ^ (count + 1);

		for (item in sources) {
			ret += identityHash(item) ^ count;
			count--;
		}

		return ret;
	}

	shared actual CompoundedSet<T> clone() =>
		CompoundedSet<T>(local.clone(), sources.clone());

	shared actual Set<T&Other> intersection<Other>(Set<Other> other)
			given Other satisfies Object =>
			flat.intersection<Other>(other);

	shared actual Set<T|Other> union<Other>(Set<Other> other)
			given Other satisfies Object =>
			flat.union<Other>(other);

	shared actual Set<T|Other> exclusiveUnion<Other>(Set<Other> other)
			given Other satisfies Object =>
			flat.exclusiveUnion<Other>(other);

	shared actual Set<T> complement<Other>(Set<Other> other) 
			given Other satisfies Object =>
			flat.complement<Other>(other);

	shared actual Iterator<T> iterator() => flat.iterator();

	shared actual Boolean equals(Object that) => flat.equals(that);
}



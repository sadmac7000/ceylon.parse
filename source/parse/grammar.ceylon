import ceylon.language.meta { type }
import ceylon.collection {
	HashMap,
	HashSet,
	MutableSet,
	unmodifiableSet,
	unmodifiableMap
}

void flattenSet(MutableSet<Object> set) {
	value data = set.clone();
	set.clear();

	for (item in data) {
		if (is MutableSet<Object> item) {
			flattenSet(item);
			set.addAll(item);
		} else {
			set.add(item);
		}
	}
}

Map<NonterminalClass,Set<TerminalClass>>
refineNTMap(Map<NonterminalClass,Set<Object>> sets) {
	value ret = HashMap<NonterminalClass,Set<TerminalClass>>();

	for (k->v in sets) {
		value s = HashSet<TerminalClass>();

		for (item in v) {
			assert(is TerminalClass item);
			s.add(item);
		}

		ret.put(k, unmodifiableSet(s));
	}

	return unmodifiableMap(ret);
}

class Grammar(NonterminalClass start, {Nonterminal +}rules) {
	shared Set<TerminalClass> terminals {
		{TerminalClass *} ret =
			{for (r in rules) for (x in r.terminals) x};

		assert(is {TerminalClass +} ret);
		return unmodifiableSet(HashSet<TerminalClass>{elements=ret;});
	}

	Set<NonterminalClass> nonterminals {
		{NonterminalClass *} ret =
			{for (r in rules) for (x in r.nonterminals) x};

		assert(is {NonterminalClass +} ret);
		return unmodifiableSet(HashSet<NonterminalClass>{elements=ret;});
	}

	Set<NonterminalClass> produced {
		{NonterminalClass *} ret = {for (r in rules) type(r)};

		assert(is {NonterminalClass +} ret);
		return unmodifiableSet(HashSet<NonterminalClass>{elements=ret;});
	}

	Set<NonterminalClass> missing_produced =
		nonterminals.complement(produced);

	if (missing_produced.size > 0) {
		assert(missing_produced.size == 1);
		assert(missing_produced.contains(start));
	}

	shared Map<NonterminalClass,Set<TerminalClass>> firstSets {
		value sets = HashMap<NonterminalClass,HashSet<Object>>();

		for (r in rules) {
			if (! sets.defines(type(r))) {
				sets.put(type(r), HashSet<Object>());
			}

			value s = sets[type(r)];
			assert(exists s);
			s.add(r.symbols.first);
		}

		for (k->v in sets) {
			for (s in sets.items) {
				if (! s.contains(k)) {
					continue;
				}

				s.remove(k);
				s.add(v);
			}
		}

		for (k->v in sets) {
			flattenSet(v);
		}

		return refineNTMap(sets);
	}

	shared Map<NonterminalClass,Set<TerminalClass>> followSets {
		value sets = HashMap<NonterminalClass,HashSet<Object>>();

		sets.put(start, HashSet<Object>{elements={`EOS`};});

		for (n in nonterminals) {
			sets.put(n, HashSet<Object>());
		}

		for (r in rules) {
			variable NonterminalClass? prev = null;

			for (s in r.symbols) {
				if (exists last=prev) {
					value set = sets[last];
					assert(exists set);

					if (is TerminalClass s) {
						set.add(s);
					} else if (s != last) {
						value other = sets[s];
						assert(exists other);
						set.add(other);
					}
				}

				if (is NonterminalClass s) {
					prev = s;
				} else {
					prev = null;
				}
			}
		}

		for (k->v in sets) {
			flattenSet(v);
		}

		return refineNTMap(sets);
	}
}


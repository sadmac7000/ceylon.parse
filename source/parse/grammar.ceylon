import ceylon.language.meta { type }
import ceylon.collection {
	HashMap,
	HashSet,
	MutableSet,
	unmodifiableSet,
	unmodifiableMap
}

class Grammar(NonterminalClass start, {Nonterminal +}rules) {
	shared Set<TerminalClass> terminals {
		{TerminalClass *} ret =
			{for (r in rules) for (x in r.terminals) x};

		assert(ret.size > 0);
		return unmodifiableSet(HashSet<TerminalClass>{elements=ret;});
	}

	Set<NonterminalClass> nonterminals {
		{NonterminalClass *} ret =
			{for (r in rules) for (x in r.nonterminals) x};

		assert(ret.size > 0);
		return unmodifiableSet(HashSet<NonterminalClass>{elements=ret;});
	}

	Set<NonterminalClass> produced {
		{NonterminalClass *} ret = {for (r in rules) type(r)};

		assert(ret.size > 0);
		return unmodifiableSet(HashSet<NonterminalClass>{elements=ret;});
	}

	Set<NonterminalClass> missing_produced =
		nonterminals.complement(produced);

	if (missing_produced.size > 0) {
		assert(missing_produced.size == 1);
		assert(missing_produced.contains(start));
	}

	shared Map<NonterminalClass,Set<TerminalClass>> firstSets {
		value sets =
			HashMap<NonterminalClass,CompoundedSet<SymbolClass>>();

		for (r in rules) {
			if (! sets.defines(type(r))) {
				sets.put(type(r), CompoundedSet<SymbolClass>());
			}

			value s = sets[type(r)];
			assert(exists s);
			assert(is MutableSet<SymbolClass> loc=s.local);
			loc.add(r.symbols.first);
		}

		for (k->v in sets) {
			for (s in sets.items) {
				if (! s.contains(k)) {
					continue;
				}

				assert(is MutableSet<SymbolClass> loc=s.local);
				loc.remove(k);
				s.compound(v);
			}
		}

		assert(is Map<NonterminalClass,Set<TerminalClass>> sets);
		return sets;
	}

	shared Map<NonterminalClass,Set<TerminalClass>> followSets {
		value sets =
			HashMap<NonterminalClass,CompoundedSet<TerminalClass>>();

		sets.put(start,
				CompoundedSet<TerminalClass>{local=HashSet<TerminalClass>{elements={`EOS`};};});

		for (n in nonterminals) {
			sets.put(n, CompoundedSet<TerminalClass>());
		}

		for (r in rules) {
			variable NonterminalClass? prev = null;

			for (s in r.symbols) {
				if (exists last=prev) {
					value set = sets[last];
					assert(exists set);

					if (is TerminalClass s) {
						assert(is
								MutableSet<TerminalClass>
								loc =
								set.local);
						loc.add(s);
					} else {
						value other = sets[s];
						assert(exists other);

						set.compound(other);
					}
				}

				if (is NonterminalClass s) {
					prev = s;
				} else {
					prev = null;
				}
			}
		}

		for (r in rules) {
			value set = sets[type(r)];
			value adder = sets[r.symbols.last];
			assert(exists set);

			if (exists adder) {
				adder.compound(set);
			}
		}

		return sets;
	}
}


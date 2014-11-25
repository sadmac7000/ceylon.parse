import ceylon.language.meta { type }
import ceylon.language.meta.model { Method, Class }
import ceylon.collection {
    HashMap,
    HashSet,
    MutableSet,
    ArrayList,
    unmodifiableSet,
    unmodifiableMap
}

"Quick alias for a general method"
alias Rule => Method<Nothing,Anything,Nothing>;

"A do-nothing annotation class for the `rule` annotation"
shared final annotation class GrammarRule()
        satisfies OptionalAnnotation<GrammarRule, Annotated> {}

"We annotate methods of a `ParseTree` object to indicate that those methods
 correspond to production rules"
shared annotation GrammarRule rule() => GrammarRule();

"A `ParseTree` is defined by a series of BNF-style production rules. The rules
 are specifed by defining methods with the `rule` annotation. Each method must
 take values of type `Symbol` and return a value of type `Nonterminal`. The
 parser will create an appropriate production rule and call the annotated
 method in order to reduce the value."
abstract class ParseTree() {
    "Sequence of all rule methods in the class."
    shared Method<Nothing,Anything,Nothing>[] rules {
        return type(this).getMethods<Nothing>(`GrammarRule`);
    }

    "The start symbol for this grammar."
    shared formal NonterminalClass start;

    "A set of all terminal symbols used by this grammar."
    shared Set<TerminalClass> terminals {
        {TerminalClass *} ret =
            {for (r in rules) for (x in r.parameterTypes) if (is
                    TerminalClass x) x};

        assert(ret.size > 0);
        return unmodifiableSet(HashSet<TerminalClass>{elements=ret;});
    }

    class State() {
        ArrayList<Integer?> positions = ArrayList<Integer?>();

        void initializeStart() {
            assert(positions.size == 0);
        }
    }

    "A set of all nonterminal symbols used by this grammar"
    shared Set<NonterminalClass> nonterminals {
        {NonterminalClass *} ret =
            {for (r in rules) for (x in r.parameterTypes) if (is
                    NonterminalClass x) x}.chain({for (r in rules) if (is
                    NonterminalClass x = r.type) x});

        assert(ret.size > 0);
        return unmodifiableSet(HashSet<NonterminalClass>{elements=ret;});
    }

    "A set of all nonterminal symbols that are results of production rules"
    Set<NonterminalClass> produced {
        {NonterminalClass *} ret = {for (r in rules) if (is NonterminalClass
                x=r.type) x};

        assert(ret.size > 0);
        return unmodifiableSet(HashSet<NonterminalClass>{elements=ret;});
    }

    "List of all nonterminals that are not produced by any rule"
    Set<NonterminalClass> missing_produced => nonterminals.complement(produced);
    //assert(missing_produced.size == 0);

    "A map from all the nonterminals to their FIRST sets of terminals"
    shared Map<NonterminalClass,Set<TerminalClass>> firstSets {
        value sets =
            HashMap<NonterminalClass,CompoundedSet<SymbolClass>>();

        for (r in rules) {
            value key = r.type;
            assert(is NonterminalClass key);

            if (! sets.defines(key)) {
                sets.put(key, CompoundedSet<SymbolClass>());
            }

            value s = sets[r.type];
            assert(exists s);
            assert(is MutableSet<SymbolClass> loc=s.local);

            Anything? res = r.parameterTypes[0];
            assert(exists res);
            assert(is SymbolClass res);
            loc.add(res);
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

        value ret = HashMap<NonterminalClass,Set<TerminalClass>>();

        for (k->v in sets) {
            value f = HashSet<TerminalClass>();
            for (s in v) {
                assert(is TerminalClass s);
                f.add(s);
            }
            ret.put(k, unmodifiableSet(f));
        }

        return unmodifiableMap(ret);
    }

    "A map from all the nonterminals to their FOLLOW sets of terminals"
    shared Map<NonterminalClass,Set<TerminalClass>> followSets {
        value sets =
            HashMap<NonterminalClass,CompoundedSet<TerminalClass>>();

        sets.put(start, CompoundedSet<TerminalClass>{
            local=HashSet<TerminalClass>{elements={`EOS`};
        };});

        for (n in nonterminals) {
            if (! sets.defines(n)) {
                sets.put(n, CompoundedSet<TerminalClass>());
            }
        }

        for (r in rules) {
            variable NonterminalClass? prev = null;

            for (s in r.parameterTypes) {
                if (exists last=prev) {
                    value set = sets[last];
                    assert(exists set);

                    if (is TerminalClass s) {
                        assert(is MutableSet<TerminalClass> loc = set.local);
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
            value last = r.parameterTypes.last;
            assert(exists last);
            assert(is SymbolClass last);
            value set = sets[r.type];
            value adder = sets[last];
            assert(exists set);

            if (exists adder) {
                adder.compound(set);
            }
        }

        return sets;
    }
}


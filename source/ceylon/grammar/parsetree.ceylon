import ceylon.language.meta { type }
import ceylon.language.meta.model { Method, Type }
import ceylon.collection {
    HashSet,
    HashMap,
    ArrayList,
    unlinked
}

"A single token result returned by a NAT"
shared class NATResult(typeIn, sym, length) {
    shared Object sym;
    shared Integer length;
    Integer|Type typeIn;

    shared Integer type;

    if (is Integer typeIn) {
        type = typeIn;
    } else {
        assert(is Type typeIn);
        type = typeAtomCache.getAlias(typeIn);
    }
}

"Non-deterministic Abstract Token array. Presents a view of a string or other
 NAT as a non-deterministic list of tokenized values. The tokens are 'abstract'
 because they can be non-terminal; we repeatedly tokenize a stream of tokens
 into a stream of higher-order tokens."
shared interface NATArray {
    "Return a set of tokens that match starting within the given stream
     position. The index is meant to be in 'characters' regardless of what the
     NATArray consumes (so we assume that at some bottom recursion layer you
     are consuming a string)"
    shared formal Set<NATResult> at(Integer i);
}

"A result to represent the end of a stream."
shared NATResult eos = NATResult(type(eos_object), eos_object, 0);
object eos_object {}

"Recursive NAT Array. Recieves a stream of abstract tokens and produces a
 stream of more refined abstract tokens."
class RNATArray(NATArray child, ParseTree<Object> tree) satisfies NATArray {
    value cache = HashMap<Integer,Set<NATResult>>();
    shared actual Set<NATResult> at(Integer i) {
        if (cache.defines(i)) {
            value ret = cache[i];
            assert(exists ret);
            return ret;
        }

        value ret = HashSet<NATResult>{stability=unlinked; elements=child.at(i);};
        cache.put(i, ret);

        for (rule in tree.rules) {
            if (ret.contains(rule.produces)) { continue; }
            ret.addAll(rule.matchAt(child, i));
        }

        return ret;
    }
}

shared variable Integer atom_time = 0;
shared variable Integer rule_time = 0;
shared variable Integer parse_time = 0;

"We have to convert type objects to integers to pass them around, otherwise we
 encounter weird performance issues."
object typeAtomCache {
    value from = HashMap<Type, Integer>();
    value to = HashMap<Integer, Type>();
    variable value next = 0;

    "Get an alias for a type"
    shared Integer getAlias(Type t) {
        value start_time = system.nanoseconds;
        if (from.defines(t)) {
            value ret = from[t];
            assert(exists ret);
            atom_time += system.nanoseconds - start_time;
            return ret;
        }

        from.put(t, next);
        to.put(next, t);
        atom_time += system.nanoseconds - start_time;
        return next++;
    }

    "Resolve a type"
    shared Type resolve(Integer i) {
        value start_time = system.nanoseconds;
        value ret = to[i];
        assert(exists ret);
        atom_time += system.nanoseconds - start_time;
        return ret;
    }
}

"A rule. Specifies produced and consumed symbols and a method to execute them"
shared class Rule(Method<Nothing,Object> meth, ParseTree<Object> tree) {
    "Sequence of symbols consumed by this production"
    shared Integer[] consumes = [ for (x in meth.parameterTypes)
        typeAtomCache.getAlias(x) ];

    "Symbol produced by this production"
    shared Integer produces = typeAtomCache.getAlias(meth.type);

    value declaration = meth.declaration;

    "Run the production-handling code for this method."
    Object consume(Object[] syms) {
        value start_time = system.nanoseconds;
        value result = declaration.memberInvoke{container=tree;
            typeArguments=[]; arguments=syms;};
        assert(is Object result);
        rule_time += system.nanoseconds - start_time;
        return result;
    }

    "Try to match this rule in the given NATArray at the given position."
    shared {NATResult *} matchAt(NATArray tokens, Integer pos) {
        value queue = ArrayList<[Integer, NATResult*]>();
        value results = ArrayList<NATResult>();
        queue.offer([pos]);

        while (queue.size > 0) {
            value next = queue.accept();
            assert(exists next);
            value search = consumes[next.size - 1];
            assert(exists search);

            for (token in tokens.at(next[0])) {
                if (token.type != search) { continue; }

                value got = [next[0] + token.length,
                      *next[1...].withTrailing(token)];

                if ((got.size - 1) < consumes.size) {
                    queue.offer(got);
                    continue;
                }

                value args = [ for (x in got[1...]) x.sym ];

                results.add(NATResult(produces, consume(args), got[0] - pos));
            }
        }

        return results;
    }

    shared actual Integer hash = consumes.hash ^ 2 + produces.hash;

    shared actual Boolean equals(Object other) {
        if (is Rule other) {
            return other.consumes == consumes && other.produces == produces;
        } else {
            return false;
        }
    }
}

"A do-nothing annotation class for the `rule` annotation"
shared final annotation class GrammarRule()
        satisfies OptionalAnnotation<GrammarRule, Annotated> {}

"We annotate methods of a `ParseTree` object to indicate that those methods
 correspond to production rules"
shared annotation GrammarRule rule() => GrammarRule();

"A `ParseTree` is defined by a series of BNF-style production rules. The rules
 are specifed by defining methods with the `rule` annotation.  The parser will
 create an appropriate production rule and call the annotated method in order
 to reduce the value."
shared abstract class ParseTree<out RootTerminal>(NATArray tokens)
        given RootTerminal satisfies Object {
    "A list of rules for this object"
    shared variable Rule[] rules = [];
    shared Integer result = typeAtomCache.getAlias(`RootTerminal`);

    "The root node of the parse tree"
    shared RootTerminal root {
        variable NATArray top = tokens;

        if (rules.size == 0) {
            populate_rules();
        }

        value start_time = system.nanoseconds;
        while (true) {
            value k = top.at(0);
            for (sym in k) {
                if (sym.type != result) { continue; }

                if (! top.at(sym.length).contains(eos)) { break; }

                assert(is RootTerminal ret=sym.sym);
                parse_time += system.nanoseconds - start_time;
                return ret;
            }

            top = RNATArray(top, this);
        }
    }

    "Set up the list of rules"
    void populate_rules() {
        value meths = type(this).getMethods<Nothing,Object>(`GrammarRule`);

        for (r in meths) {
            rules = rules.withTrailing(Rule(r, this));
        }
    }
}

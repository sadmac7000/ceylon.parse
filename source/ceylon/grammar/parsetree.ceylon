import ceylon.language.meta { type }
import ceylon.language.meta.model { Method }
import ceylon.collection {
    HashSet,
    ArrayList,
    unlinked
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
    shared formal Set<[Object, Integer]> at(Integer i);
}

"Recursive NAT Array. Recieves a stream of abstract tokens and produces a
 stream of more refined abstract tokens."
class RNATArray(NATArray child, ParseTree<Object> tree) satisfies NATArray {
    value rules = type(tree).getMethods<Nothing,Object>(`GrammarRule`);
    shared actual Set<[Object, Integer]> at(Integer i) {
        value ret = HashSet<[Object, Integer]>{stability=unlinked; elements=child.at(i);};
        for (rule in rules) {
            value params = rule.parameterTypes;
            variable Set<[Object, Integer]> set = ret;
            variable Boolean failed = false;

            variable Integer position = i;
            variable [Object*] args = [];
            for (param in params) {
                variable Boolean matched = false;
                for (token in set) {
                    if (type(token[0]) != param) { continue; }

                    matched = true;
                    position += token[1];
                    set = child.at(position);
                    args = args.withTrailing(token[0]);
                    break;
                }

                if (! matched) {
                    failed = true;
                    break;
                }
            }

            if (! failed) {
                value decl = rule.declaration;
                value result = decl.memberInvoke{container=tree;
                    typeArguments=[]; arguments=args;};
                assert(is Object result);
                ret.add([result, position - i]); 
            }
        }

        return ret;
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

    "The root node of the parse tree"
    shared RootTerminal root {
        variable NATArray top = tokens;

        while (true) {

            value k = top.at(0);
            for (sym in k) {
                if (type(sym[0]) != `RootTerminal`) { continue; }

                if (top.at(sym[1]).size > 0) { break; }

                assert(is RootTerminal ret=sym.first);
                return ret;
            }

            top = RNATArray(top, this);
        }
    }
}

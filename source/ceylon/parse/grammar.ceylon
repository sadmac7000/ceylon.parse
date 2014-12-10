import ceylon.language.meta {
    _type = type
}
import ceylon.language.meta.model {
    Generic,
    UnionType
}
import ceylon.collection { HashMap }

"A do-nothing annotation class for the `error` annotation"
shared final annotation class GrammarErrorConstructor()
        satisfies OptionalAnnotation<GrammarErrorConstructor, Annotated> {}

"We annotate some methods of a `ParseTree` object to indicate that those
 methods can construct an error version of symbols so we can build error
 reporting into the parse tree."
shared annotation GrammarErrorConstructor errorConstructor() =>
        GrammarErrorConstructor();

"A do-nothing annotation class for the `rule` annotation"
shared final annotation class GrammarRule()
        satisfies OptionalAnnotation<GrammarRule, Annotated> {}

"We annotate methods of a `ParseTree` object to indicate that those methods
 correspond to production rules"
shared annotation GrammarRule rule() => GrammarRule();

"A do-nothing annotation class for the `tokenizer` annotation."
shared final annotation class Tokenizer()
        satisfies OptionalAnnotation<Tokenizer, Annotated> {}

"Methods annotated with `tokenizer` take a sequence and return a token."
shared annotation Tokenizer tokenizer() => Tokenizer();

"Exception thrown when we need a bad token constructor but one isn't defined"
class BadTokenConstructorException()
        extends Exception("Could not construct invalid token") {}

"A rule. Specifies produced and consumed symbols and a method to execute them"
shared class Rule(shared Object(Object*) consume, shared Integer[] consumes,
        shared Integer produces) {
    shared actual Integer hash = consumes.hash ^ 2 + produces.hash;

    shared actual Boolean equals(Object other) {
        if (is Rule other) {
            return other.consumes == consumes && other.produces == produces;
        } else {
            return false;
        }
    }
}

"A [[Grammar]] is defined by a series of BNF-style production rules. The rules
 are specifed by defining methods with the `rule` annotation.  The parser will
 create an appropriate production rule and call the annotated method in order
 to reduce the value."
shared abstract class Grammar<out Root, Data>()
        given Root satisfies Object {
    "A list of rules for this grammar"
    shared variable Rule[] rules = [];

    "The result symbol we expect from this tree"
    shared Integer result = typeAtomCache.getAlias(`Root`);

    "Error constructors"
    shared HashMap<Integer, Object(Object?, Object?)> errorConstructors =
        HashMap<Integer, Object(Object?, Object?)>();

    "Tokenizers"
    shared variable HashMap<Integer, Token?(Data, Object?)> tokenizers =
    HashMap<Integer, Token?(Data, Object?)>();

    variable Boolean populated = false;

    "Set up the list of rules"
    shared void populateRules() {
        if (populated) { return; }
        populated = true;

        value meths = _type(this).getMethods<Nothing>(`GrammarRule`);
        value errConMeths =
            _type(this).getMethods<Nothing>(`GrammarErrorConstructor`);
        value tokenizerMeths = _type(this).getMethods<Nothing>(`Tokenizer`);

        for (t in tokenizerMeths) {
            Token? tokenizer(Data s, Object? last) {
                assert(is Token? ret = t.declaration.memberInvoke(this, [],
                            s, last));
                return ret;
            }

            assert(is UnionType retType = t.type);
            value caseTypes =  retType.caseTypes;
            assert(caseTypes.size == 2);
            assert(is Generic tokenType = {for (r in caseTypes) if (
                        !r.typeOf(null)) r}.first);

            value typeArgs = tokenType.typeArguments.items;
            assert(typeArgs.size == 1);
            assert(exists type = typeArgs.first);

            tokenizers.put(typeAtomCache.getAlias(type), tokenizer);
        }

        for (c in errConMeths) {
            value type = typeAtomCache.getAlias(c.type);

            Object construct(Object? o, Object? prev) {
                assert(is Object ret = c.declaration.memberInvoke(this, [],
                            o, prev));
                return ret;
            }

            errorConstructors.put(type, construct);
        }

        for (r in meths) {
            Object consume(Object *o) {
                assert(is Object ret = r.declaration.memberInvoke(this, [], *o));
                return ret;
            }

            value consumes = [ for (p in r.parameterTypes)
                typeAtomCache.getAlias(p) ];
            value produces = typeAtomCache.getAlias(r.type);
            value rule = Rule(consume, consumes, produces);

            rules = rules.withTrailing(rule);
        }
    }

    shared default Object badTokenConstructor(Data data, Object? previous) {
        throw BadTokenConstructorException();
    }
}

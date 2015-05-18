import ceylon.language.meta {
    _type = type
}
import ceylon.language.meta.model {
    Generic,
    Type,
    UnionType,
    ClassOrInterface,
    Method,
    Function,
    TypeApplicationException
}
import ceylon.language.meta.declaration {
    FunctionDeclaration,
    ClassOrInterfaceDeclaration
}
import ceylon.collection {
    HashMap
}

"Exception thrown when we need a bad token constructor but one isn't defined"
class BadTokenConstructorException()
        extends Exception("Could not construct invalid token") {}

"Exception thrown when a grammar is ambiguous and we request an unambiguous
 parse via [[Grammar.unambiguousParse]]."
shared class AmbiguityException()
        extends Exception("Parser generated ambiguous results") {}

"Alias for any type of grammar"
shared alias AnyGrammar => Grammar<Object, Nothing>;

"A [[Grammar]] is defined by a series of BNF-style production rules. The rules
 are specifed by defining methods with the `rule` annotation.  The parser will
 create an appropriate production rule and call the annotated method in order
 to reduce the value."
shared abstract class Grammar<out Root, in Data>()
        given Data satisfies List<Object>
        given Root satisfies Object {
    "A list of rules for this grammar"
    shared variable Rule[] rules = [];

    "Omni-rule methods"
    variable FunctionDeclaration[] omniRuleMeths = [];

    "Dynamic rules cache"
    value dynamicRulesCache = HashMap<Atom,{Rule *}>();

    "Generic rule initial values structure"
    class GenericInfo(shared FunctionDeclaration declaration,
                      shared ClassOrInterfaceDeclaration target,
                      shared Integer precedence,
                      shared Associativity associativity) {
        shared actual String string = declaration.string;
        shared Rule? reify(ClassOrInterface<Object> cl, AnyGrammar g) {
            ClassOrInterface<Object> realized;
            try {
                realized = target.apply<Object>(*cl.typeArguments.items);
            } catch(TypeApplicationException e) { return null; }

            if (! realized.subtypeOf(cl)) { return null; }

            value consume =
                declaration.memberApply<Nothing,Object,Nothing>(_type(g),
                        *cl.typeArguments.items).bind(g);
            value params = zipPairs(consume.parameterTypes,
                    declaration.parameterDeclarations);
            value consumes = [ for (p in params)
                makeProductionClause(p[0], p[1], g) ];
            value produces = Atom(consume.type);
            assert(exists ruleAnnotation = declaration.annotations<GenericRule>()[0]);
            return Rule(consume, consumes, produces,
                    ruleAnnotation.precedence, ruleAnnotation.associativity, g);
        }
    }

    "Generic rule initial values"
    variable GenericInfo[] genericInfos = [];

    "The result symbol we expect from this tree"
    shared Atom result = Atom(`Root`);

    "Error constructors"
    shared Map<Atom, Object(Object?, Object?)> errorConstructors =
        HashMap<Atom, Object(Object?, Object?)>();

    "Cached result for startRules"
    variable {Rule *}? startRulesCache = null;

    "Tokenizers"
    shared Map<Atom, Token?(Data, Object?)> tokenizers =
    HashMap<Atom, Token?(Data, Object?)>();

    variable Boolean populated = false;

    "Set up the list of rules"
    shared void populateRules() {
        assert(is HashMap<Atom, Token?(Data, Object?)> tokenizers);
        assert(is HashMap<Atom, Object(Object?, Object?)> errorConstructors);

        if (populated) { return; }
        populated = true;

        value meths = _type(this).getMethods<Nothing, Object, Nothing>(`GrammarRule`);
        value errConMeths =
            _type(this).getMethods<Nothing, Object, [Object?, Object?]>(`GrammarErrorConstructor`);
        value tokenizerMeths =
            _type(this).getMethods<Nothing, Token<Object>?, [Data, Object?]>(`Tokenizer`);

        for (t in tokenizerMeths) {
            value tokenizer = t.bind(this);

            assert(is UnionType retType = t.type);
            value caseTypes =  retType.caseTypes;
            assert(caseTypes.size == 2);
            assert(is Generic tokenType = {for (r in caseTypes) if (
                        !r.typeOf(null)) r}.first);

            value typeArgs = tokenType.typeArguments.items;
            assert(typeArgs.size == 1);
            assert(exists type = typeArgs.first);

            value atom = Atom(type);
            tokenizers.put(atom, tokenizer);
        }

        for (c in errConMeths) {
            errorConstructors.put(Atom(c.type), c.bind(this));
        }

        for (r in meths) {
            addRule(r);
        }

        omniRuleMeths =
            _type(this).declaration.annotatedMemberDeclarations<FunctionDeclaration,OmniRule>();

        value genericInfosStream =
            _type(this).declaration.annotatedMemberDeclarations<FunctionDeclaration,GenericRule>().map((x) {
                        assert(exists annotation =
                                x.annotations<GenericRule>()[0]);
                        return GenericInfo(x, annotation.c,
                                annotation.precedence,
                                annotation.associativity);
                    });

        genericInfos = [*genericInfosStream];

        for (r in rules) { r.predictAll(); }
    }

    "Starting rules"
    shared {Rule *} startRules {
        if (exists k = startRulesCache) { return k; }

        value cl = ProductionClause(false, true, this, result).predicted;
        startRulesCache = cl;
        return cl;
    }

    "Get dynamic and omni rules"
    shared {Rule *} getDynamicRulesFor(Atom a)
        => dynamicRulesCache[a] else getDynamicRulesSlowpath(a);

    "Populate dynamicRulesCache and return its new value"
    {Rule *} getDynamicRulesSlowpath(Atom a) {
        value t = a.type;
        if (! is Type<Object> t) { return {}; }
        assert(is Type<Object> t);

        value ret = [*getOmniRulesFor(t).chain(getGenericRulesFor(t))];
        dynamicRulesCache.put(a,ret);
        return ret;
    }

    "Reify omni rules for a given type"
    shared {Rule *} getOmniRulesFor(Type<Object> t)
        => omniRuleMeths.map((declaration) {
            Function<Object,Nothing> consume;
            try {
                consume = declaration.memberApply<Nothing,Object,Nothing>(_type(this), t).bind(this);
            } catch(TypeApplicationException t) { return null; }
            value params = zipPairs(consume.parameterTypes,
                    declaration.parameterDeclarations);
            value consumes = [ for (p in params)
                makeProductionClause(p[0], p[1], this) ];
            value produces = Atom(t);
            assert(exists ruleAnnotation = declaration.annotations<OmniRule>()[0]);
            return Rule(consume, consumes, produces,
                    ruleAnnotation.precedence, ruleAnnotation.associativity, this);
        }).narrow<Rule>();

    "Reify generic rules for a given type"
    shared {Rule *} getGenericRulesFor(Type<Object> t)
        => genericInfos.map((info)
            => if (is ClassOrInterface<Object> t)
               then info.reify(t, this)
               else null
           ).narrow<Rule>();

    "Add a rule to the rule list"
    void addRule(Method<Nothing, Object, Nothing> r) {
        value consume = r.bind(this);

        value params = zipPairs(r.parameterTypes,
                r.declaration.parameterDeclarations);
        value consumes = [ for (p in params)
            makeProductionClause(p[0], p[1], this) ];
        value produces = Atom(r.type);

        assert(exists ruleAnnotation = r.declaration.annotations<GrammarRule>()[0]);
        value rule = Rule(consume, consumes, produces,
                ruleAnnotation.precedence, ruleAnnotation.associativity, this);

        rules = rules.withTrailing(rule);
    }

    "Returns a token to represent an unparseable region. The input data is
     exactly the contents of that region."
    shared default Object badTokenConstructor(Data data, Object? previous) {
        throw BadTokenConstructorException();
    }

    "Parse a stream"
    shared Set<Root> parse(Data data) => ParseTree(this, data).ast;

    "Parse a stream. Throw an exception if the parse is ambiguous"
    shared Root unambiguousParse(Data data) {
        value result = parse(data);

        if (result.size != 1) {
            throw AmbiguityException();
        }

        assert(exists r = result.first);
        return r;
    }

    "Default rule for sequential objects"
    genericRule(`interface Sequence`)
    shared [K+] sequence<K>(K+ ret) => ret;

    "Default rule for empty sequential objects"
    genericRule(`interface Sequential`)
    shared [K*] sequential<K>() => [];
}

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
    FunctionOrValueDeclaration,
    ClassOrInterfaceDeclaration
}
import ceylon.collection {
    HashMap,
    HashSet,
    ArrayList
}

"A do-nothing annotation class for the `error` annotation"
shared final annotation class GrammarErrorConstructor()
        satisfies OptionalAnnotation<GrammarErrorConstructor, Annotated> {}

"We annotate some methods of a `ParseTree` object to indicate that those
 methods can construct an error version of symbols so we can build error
 reporting into the parse tree."
shared annotation GrammarErrorConstructor errorConstructor() =>
        GrammarErrorConstructor();

"Associativity of a rule"
shared interface Associativity of lassoc|rassoc|nonassoc {}

"Left Associativity"
shared object lassoc satisfies Associativity {}

"Right Associativity"
shared object rassoc satisfies Associativity {}

"Non-Associativity"
shared object nonassoc satisfies Associativity {}

"A do-nothing annotation class for the `rule` annotation"
shared final annotation class GrammarRule(shared Integer precedence,
        shared Associativity associativity)
        satisfies OptionalAnnotation<GrammarRule, Annotated> {}

"A do-nothing annotation class for the `genericRule` annotation"
shared final annotation class GenericRule(shared Integer precedence,
        shared Associativity associativity, shared ClassOrInterfaceDeclaration c)
        satisfies OptionalAnnotation<GenericRule, Annotated> {}

"A do-nothing annotation class for the `omniRule` annotation"
shared final annotation class OmniRule(shared Integer precedence,
        shared Associativity associativity)
        satisfies OptionalAnnotation<OmniRule, Annotated> {}

"We annotate methods of a `ParseTree` object to indicate that those methods
 correspond to production rules"
shared annotation GrammarRule rule(Integer precedence = 0, Associativity associativity
        = nonassoc) => GrammarRule(precedence, associativity);

"Generic rules allow for one type parameter shared by both the method and its
 return value"
shared annotation GenericRule genericRule(ClassOrInterfaceDeclaration c,
        Integer precedence = 0, Associativity associativity
        = nonassoc) => GenericRule(precedence, associativity, c);

"Omni rules can return any type and are usually used to, e.g. map any symbol
 followed by whitespace to just that symbol."
shared annotation OmniRule omniRule(Integer precedence = 0,
        Associativity associativity = nonassoc)
    => OmniRule(precedence, associativity);

"A do-nothing annotation class for the `tokenizer` annotation."
shared final annotation class Tokenizer()
        satisfies OptionalAnnotation<Tokenizer, Annotated> {}

"Methods annotated with `tokenizer` take a sequence and return a token."
shared annotation Tokenizer tokenizer() => Tokenizer();

"Exception thrown when we need a bad token constructor but one isn't defined"
class BadTokenConstructorException()
        extends Exception("Could not construct invalid token") {}

shared class ProductionClause(shared Boolean variadic,
        shared Boolean once,
        shared AnyGrammar g,
        shared Atom|ProductionClause *values)
        satisfies Iterable<Atom> {

    value localAtoms = {for (x in values) if (is Atom x) x};
    value productionClauses = {for (x in values) if (is ProductionClause x) x};
    value allIterables = productionClauses.chain({localAtoms});
    value atomIterator = allIterables.reduce<{Atom *}>((x,y) => x.chain(y));
    value allAtoms = HashSet{*atomIterator};

    shared actual Boolean contains(Object type) => allAtoms.contains(type);

    shared actual Iterator<Atom> iterator() => allAtoms.iterator();

    shared actual Integer hash => variadic.hash ^ 3 + once.hash ^ 2 +
        values.hash;

    shared actual Boolean equals(Object that) {
        if (! is ProductionClause that) { return false; }
        assert(is ProductionClause that);

        if (that.variadic != variadic) { return false; }
        if (that.once != once) { return false; }
        if (that.values != values) { return false; }
        return true;
    }

    "Memoization for [[predicted]]"
    variable {Rule *}? predictedCache = null;

    "Stream of applicable scanners"
    shared {Token?(Nothing, Object?) *} scanners = [*productionClauses.map((x) => x.scanners)
            .chain(
                    localAtoms.map((a)
                        => a.subtypes.map((x) => g.tokenizers[x])
                            .narrow<Object>()
                    )
                  )
            .fold<{Token?(Nothing, Object?) *}>({})((x, y) => x.chain(y))];

    "Generate a prediction set for this clause"
    shared {Rule *} predicted {
        predict();
        assert(exists p = predictedCache);
        return p;
    }

    "Populate the cached prediction set"
    shared void predict() {
        if (exists p = predictedCache) { return; }

        value p = [*{
            for (other in g.rules)
                if ((this.select(other.produces.subtypeOf)).size > 0)
                    other
         }.chain(localAtoms.map(g.getDynamicRulesFor).fold<{Rule *}>({})
                ((x, y) => x.chain(y))
            ).chain(
                localAtoms.map((x) =>
                    x.type).narrow<Type<Tuple<Anything,Anything,Anything[]>>>()
                    .map((x) => Rule.TupleRule(x, g))
            )];

        predictedCache = p;
    }
}

"Turn a tuple type into predicates"
ProductionClause[] clausesFromTupleType(Type<[Anything*]>&Generic clauses,
        AnyGrammar g) {
    if (is Type<Tuple<Anything,Anything,Anything[]>> clauses) {
        assert(exists firstParam = `class Tuple`.typeParameterDeclarations[1]);
        assert(exists restParam = `class Tuple`.typeParameterDeclarations[2]);
        assert(exists first = clauses.typeArguments[firstParam]);
        assert(is Type<[Anything*]>&Generic rest = clauses.typeArguments[restParam]);

        value rawAtom = makeTypeAtom(first, false, false, g);
        value prodClause = if (is ProductionClause rawAtom) then rawAtom else
            ProductionClause(false, false, g, rawAtom);

        return [prodClause].append(clausesFromTupleType(rest, g));
    }

    if (is Type<[]> clauses) { return []; }

    value once = clauses is Type<[Anything+]>;
    assert(exists param = `interface Sequential`.typeParameterDeclarations[0]);
    assert(exists unitType = clauses.typeArguments[param]);
    value typeAtom = makeTypeAtom(unitType, true, once, g);
    return if (is ProductionClause typeAtom) then [typeAtom] else
        [ProductionClause(true, once, g, typeAtom)];
}

"Give the arguments list back as a sequence"
[Anything*] argsToSequence(Anything* ret) => ret;

"A rule. Specifies produced and consumed symbols and a method to execute them"
shared class Rule {
    shared Object(Object?*) consume;
    shared ProductionClause[] consumes;
    shared Atom produces;
    shared Integer precedence;
    shared Associativity associativity;
    shared actual Integer hash;
    shared AnyGrammar g;

    shared new (Function<Object,Nothing> consume,
            ProductionClause[] consumes,
            Atom produces,
            Integer precedence,
            Associativity associativity,
            AnyGrammar g) {
        this.consume = (Object?* x) => consume.apply(*x);
        this.consumes = consumes;
        this.produces = produces;
        this.precedence = precedence;
        this.associativity = associativity;
        this.hash = consumes.hash ^ 2 + produces.hash;
        this.g = g;
    }

    shared new TupleRule(Type<Tuple<Anything,Anything,Anything[]>> tuple,
            AnyGrammar g) {
        this.produces = Atom(tuple);
        this.consume = argsToSequence;
        this.precedence = 0;
        this.associativity = nonassoc;

        assert(is Type<Anything[]>&Generic tuple);
        this.consumes = clausesFromTupleType(tuple, g);
        this.hash = consumes.hash ^ 2 + produces.hash;
        this.g = g;
    }

    shared actual Boolean equals(Object other) {
        if (is Rule other) {
            return other.consumes == consumes && other.produces == produces;
        } else {
            return false;
        }
    }

    shared Boolean precedenceConflict(Rule other) {
        if (precedence >= other.precedence) { return false; }
        if (produces != other.produces) { return false; }
        if (bracketed || other.bracketed) { return false; }
        return true;
    }

    shared Boolean bracketed {
        if (exists c = consumes.first,
            c.contains(produces)) { return false; }
        if (exists c = consumes.last,
            c.contains(produces)) { return false; }
        return true;
    }

    shared Integer? forbidPosition(Rule other) {
        if (other.precedence != precedence) { return null; }
        if (other.associativity != associativity) { return null; }
        if (other.produces != produces) { return null; }

        if (associativity == rassoc) { return 0; }
        if (associativity == lassoc) { return consumes.size - 1; }
        return null;
    }

    shared void predictAll() { for (c in consumes) { c.predict(); } }
}

"Break a type down into type atoms or aggregate production clauses"
ProductionClause|Atom makeTypeAtom(Type p, Boolean f, Boolean once, AnyGrammar g) {
    if (is UnionType p) {
        return ProductionClause(f, once, g, *{for (t in p.caseTypes)
            makeTypeAtom(t, false, false, g)});
    } else {
        return Atom(p);
    }
}

"Turn a type into a production clause"
ProductionClause makeProductionClause(Type p, FunctionOrValueDeclaration f,
        AnyGrammar g) {
    ProductionClause|Atom x;
    Boolean once;

    if (f.variadic) {
        assert(is ClassOrInterface p);
        assert(exists sub = p.typeArguments.items.first);
        once = p.declaration == `interface Sequence`;
        x = makeTypeAtom(sub, true, once, g);
    } else {
        once = false;
        x = makeTypeAtom(p, false, false, g);
    }

    if (is Atom x) {
        return ProductionClause(f.variadic, once, g, x);
    } else {
        return x;
    }
}

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

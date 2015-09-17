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
    HashMap,
    HashSet
}

"Exception thrown when a parse is ambiguous and we request an unambiguous
 parse via [[Grammar.unambiguousParse]]."
shared class AmbiguityException()
        extends Exception("Parser generated ambiguous results") {}

"A [[Grammar]] is defined by a series of BNF-style production rules. The rules
 are specifed by defining methods with the `rule` annotation.  The parser will
 create an appropriate production rule and call the annotated method in order
 to reduce the value."
shared abstract class Grammar() {
    "A list of rules for this grammar"
    shared variable Rule[] rules = [];

    "Omni-rule methods"
    variable FunctionDeclaration[] omniRuleMeths = [];

    "Rules cache"
    value rulesCache = HashMap<Atom,{Rule *}>();

    "Generic rule initial values"
    variable GenericInfo[] genericInfos = [];

    variable Boolean populated = false;

    "Next identifier for rules"
    variable Integer nextRuleIdentifier = 0;

    "List of rule identifiers"
    value ruleIdentifiers = HashMap<Object,Integer>();

    "Map of rule identifiers back to rues"
    value ruleIdentifierMap = HashMap<Integer,Rule>();

    shared void registerRule(Rule r) => ruleIdentifierMap.put(r.identifier, r);
    shared Rule? getRuleByIdentifier(Integer i) => ruleIdentifierMap[i];

    "Get a unique ID for a rule"
    shared Integer getRuleIdentifier(Object key) {
        value cached = ruleIdentifiers[key];
        if (exists cached) { return cached; }
        ruleIdentifiers.put(key, nextRuleIdentifier);
        return nextRuleIdentifier++;
    }

    "Set up the list of rules"
    shared void populateRules() {
        if (populated) { return; }
        populated = true;

        value meths = _type(this).getMethods<Nothing, Object, Nothing>(`GrammarRule`);

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
    shared {Rule *} startRules<Root>()
            given Root satisfies Object
        => getRulesFor(Atom(`Root`));

    "Get rules for a particular atom"
    shared {Rule *} getRulesFor(Atom a) {
        if (exists r = rulesCache[a]) {
            return r;
        }

        value r = HashSet{*getRulesSlowpath(a)};
        rulesCache.put(a, r);

        for (item in r) { item.predictAll(); }
        return r;
    }

    "Populate rulesCache and return its new value"
    {Rule *} getRulesSlowpath(Atom a) {
        value staticRules = rules.select((x) => x.produces.subtypeOf(a));
        {Rule *} ret;

        value t = a.type;
        if (is UnionType t) {
            value caseSets = {
                for (tsub in t.caseTypes) getRulesFor(Atom(tsub))
            };

            return staticRules.chain(caseSets.fold<{Rule *}>({})((x, y) => x.chain(y)));
        } else if (is Type<Tuple<Anything,Anything,Anything[]>> t) {
            value tp = Rule.tupleRule(t, this);
            registerRule(tp);
            return staticRules.withTrailing(tp);
        } else if (is Type<Object> t){
            return
                getOmniRulesFor(t).chain(getGenericRulesFor(t)).chain(staticRules);
        } else {
            return staticRules;
        }
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
                ProductionClause(this, p[0], p[1]) ];
            value produces = Atom(t);
            assert(exists ruleAnnotation = declaration.annotations<OmniRule>()[0]);
            value r = Rule(consume, consumes, produces,
                    ruleAnnotation.precedence, ruleAnnotation.associativity, this);
            registerRule(r);
            return r;
        }).narrow<Rule>();

    "Reify generic rules for a given type"
    shared {Rule *} getGenericRulesFor(Type<Object> t)
        => genericInfos.map((info)
            => if (is ClassOrInterface<Object> t)
               then info.reify(t)
               else null
           ).narrow<Rule>();

    "Add a rule to the rule list"
    void addRule(Method<Nothing, Object, Nothing> r) {
        value consume = r.bind(this);

        value params = zipPairs(r.parameterTypes,
                r.declaration.parameterDeclarations);
        value consumes = [ for (p in params)
            ProductionClause(this, p[0], p[1]) ];
        value produces = Atom(r.type);

        assert(exists ruleAnnotation = r.declaration.annotations<GrammarRule>()[0]);
        value rule = Rule(consume, consumes, produces,
                ruleAnnotation.precedence, ruleAnnotation.associativity, this);
        registerRule(rule);

        rules = rules.withTrailing(rule);
    }

    "Parse a stream"
    shared Set<Root> parse<Root>(SOSToken start)
        given Root satisfies Object
        => StateQueue<Root>(this, start).ast;

    "Parse a stream. Throw an exception if the parse is ambiguous"
    shared Root? unambiguousParse<Root>(SOSToken start)
        given Root satisfies Object {
        value result = parse<Root>(start);

        if (result.longerThan(1)) {
            throw AmbiguityException();
        }

        return result.first;
    }

    "Default rule for sequential objects"
    genericRule(`interface Sequence`)
    shared [K+] sequence<K>(K+ ret) => ret;

    "Default rule for empty sequential objects"
    genericRule(`interface Sequential`)
    shared [K*] sequential<K>() => [];

    "Generic rule initial values structure"
    class GenericInfo(shared FunctionDeclaration declaration,
                      shared ClassOrInterfaceDeclaration target,
                      shared Integer precedence,
                      shared Associativity associativity) {
        shared actual String string = declaration.string;
        shared Rule? reify(ClassOrInterface<Object> cl) {
            ClassOrInterface<Object> realized;
            try {
                realized = target.apply<Object>(*cl.typeArguments.items);
            } catch(TypeApplicationException e) { return null; }

            if (! realized.subtypeOf(cl)) { return null; }

            value consume =
                declaration.memberApply<Nothing,Object,Nothing>(_type(outer),
                        *cl.typeArguments.items).bind(outer);
            value params = zipPairs(consume.parameterTypes,
                    declaration.parameterDeclarations);
            value consumes = [ for (p in params)
                ProductionClause(outer, p[0], p[1]) ];
            value produces = Atom(consume.type);
            assert(exists ruleAnnotation = declaration.annotations<GenericRule>()[0]);
            value ret = Rule(consume, consumes, produces,
                    ruleAnnotation.precedence, ruleAnnotation.associativity, outer);
            outer.registerRule(ret);
            return ret;
        }
    }
}

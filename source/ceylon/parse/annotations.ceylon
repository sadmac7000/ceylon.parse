import ceylon.language.meta.declaration { ClassOrInterfaceDeclaration }

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

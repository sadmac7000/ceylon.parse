import ceylon.language.meta.model {
    Generic,
    Type,
    UnionType,
    ClassOrInterface
}
import ceylon.language.meta.declaration {
    FunctionOrValueDeclaration
}
import ceylon.collection {
    HashSet,
    ArrayList
}

"Portion of a rule that matches for a single position"
shared class ProductionClause satisfies Iterable<Atom> {
    shared Boolean variadic;
    shared Boolean once;
    shared AnyGrammar g;
    shared <Atom|ProductionClause>[] values;

    shared new(Boolean variadic, Boolean once,
        AnyGrammar g, Atom|ProductionClause *values) {
        this.variadic = variadic;
        this.once = once;
        this.g = g;
        this.values = values;
    }

    value localAtoms = {for (x in values) if (is Atom x) x};
    value productionClauses = {for (x in values) if (is ProductionClause x) x};
    value allIterables = productionClauses.chain({localAtoms});
    value atomIterator = allIterables.reduce<{Atom *}>((x,y) => x.chain(y));
    value allAtoms = HashSet{*atomIterator};
    value allAtomsList = ArrayList{*allAtoms};

    shared actual Boolean contains(Object type) => allAtoms.contains(type);

    shared actual Iterator<Atom> iterator() => allAtomsList.iterator();

    shared actual Integer hash = variadic.hash ^ 3 + once.hash ^ 2 +
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

        value p = ArrayList{*{
            for (other in g.rules)
                if ((this.select(other.produces.subtypeOf)).size > 0)
                    other
         }.chain(localAtoms.map(g.getDynamicRulesFor).fold<{Rule *}>({})
                ((x, y) => x.chain(y))
            ).chain(
                localAtoms.map((x) =>
                    x.type).narrow<Type<Tuple<Anything,Anything,Anything[]>>>()
                    .map((x) => Rule.TupleRule(x, g))
            )};

        predictedCache = p;
        for (r in p) { r.predictAll(); }
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


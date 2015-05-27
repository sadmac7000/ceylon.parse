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

    shared new (AnyGrammar g, Type tIn,
            Boolean|FunctionOrValueDeclaration? f = null) {
        this.g = g;

        Type t;
        if (is Boolean f) {
            this.variadic = true;
            this.once = f;
            t = tIn;
        } else if (exists f, f.variadic) {
            assert(is ClassOrInterface tIn);
            assert(exists k = tIn.typeArguments.items.first);
            t = k;

            this.once = tIn.declaration == `interface Sequence`;
            this.variadic = true;
        } else {
            this.variadic = false;
            this.once = false;
            t = tIn;
        }


        if (is UnionType t) {
            this.values = [for (tsub in t.caseTypes)
                if (is UnionType tsub)
                    then ProductionClause(g, tsub)
                    else Atom(tsub)
                ];
        } else {
            this.values = [Atom(t)];
        }
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
                if ((this.any(other.produces.subtypeOf)))
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

        value prodClause = ProductionClause(g, first);

        return [prodClause].append(clausesFromTupleType(rest, g));
    }

    if (is Type<[]> clauses) { return []; }

    value once = clauses is Type<[Anything+]>;
    assert(exists param = `interface Sequential`.typeParameterDeclarations[0]);
    assert(exists unitType = clauses.typeArguments[param]);

    return [ProductionClause(g, unitType, once)];
}

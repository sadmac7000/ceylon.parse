import ceylon.language.meta.model {
    Generic,
    Type,
    UnionType,
    ClassOrInterface
}
import ceylon.language.meta.declaration {
    FunctionOrValueDeclaration
}

"Portion of a rule that matches for a single position"
shared class ProductionClause {
    shared Boolean variadic;
    shared Boolean once;
    shared Grammar g;
    shared Atom[] caseTypes;
    shared Atom atom;

    variable RuleBitmap? rbcache = null;

    shared new (Grammar g, Type tIn,
            Boolean|FunctionOrValueDeclaration? f = null) {
        Type t;

        this.g = g;

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

        this.atom = Atom(t);

        if (is UnionType t) {
            this.caseTypes = [for (tsub in t.caseTypes) Atom(tsub) ];
        } else {
            this.caseTypes = [this.atom];
        }
    }

    shared actual Integer hash = variadic.hash ^ 3 + once.hash ^ 2 + atom.hash;

    shared actual Boolean equals(Object that) {
        if (! is ProductionClause that) { return false; }
        assert(is ProductionClause that);

        if (that.variadic != variadic) { return false; }
        if (that.once != once) { return false; }
        if (that.atom != atom) { return false; }
        return true;
    }

    "Generate a prediction set for this clause"
    shared RuleBitmap predicted {
        predict();
        assert(exists r = rbcache);
        return r;
    }

    "Populate the cached prediction set"
    shared void predict() {
        if (rbcache exists) { return; }
        value r = RuleBitmap(g);

        for (i in g.getRulesFor(atom)) {
            r.addRule(i);
        }

        rbcache = r;
    }
}

"Turn a tuple type into predicates"
ProductionClause[] clausesFromTupleType(Type<[Anything*]>&Generic clauses,
        Grammar g) {
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

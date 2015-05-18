import ceylon.parse { ... }
import ceylon.test { test, assertEquals, assertThatException }

object precedencedAlgebraGrammar extends AlgebraGrammar() {
    rule(1)
    shared Expr mul(Expr a, Mul o, Expr b) => Expr(a.position, a, o, b);

    rule(2)
    shared Expr add(Expr a, Plus o, Expr b) => Expr(a.position, a, o, b);
}

test
void precedenceResolvedVerticalAmbiguity() {
    value root = precedencedAlgebraGrammar.unambiguousParse<Expr>("a+b*c");
    value expect = Expr (0,
        Var("a", 0),
        Plus(1),
        Expr(2,
            Var("b", 2),
            Mul(3),
            Var("c", 4)
        )
    );

    assertEquals(root, expect);
}

test
void horizontalAmbiguityDespitePrecedence() {
    assertThatException(() =>
            precedencedAlgebraGrammar.unambiguousParse<Expr>("a+b+c"))
        .hasType(`AmbiguityException`);
}

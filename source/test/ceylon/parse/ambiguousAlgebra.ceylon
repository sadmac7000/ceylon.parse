import ceylon.parse { ... }
import ceylon.test { test, assertEquals, assertThatException }

object ambiguousAlgebraGrammar extends AlgebraGrammar() {
    rule
    shared Expr add(Expr a, Plus o, Expr b) => Expr(a.position, a, o, b);

    rule
    shared Expr mul(Expr a, Mul o, Expr b) => Expr(a.position, a, o, b);
}

test
void verticalAmbiguity() {
    assertThatException(() =>
            ambiguousAlgebraGrammar.unambiguousParse<Expr>(AlgebraStartToken("a+b*c")))
        .hasType(`AmbiguityException`);
}

test
void horizontalAmbiguity() {
    assertThatException(() =>
            ambiguousAlgebraGrammar.unambiguousParse<Expr>(AlgebraStartToken("a+b+c")))
        .hasType(`AmbiguityException`);
}

test
void resolvedVerticalAmbiguity()
{
    value root = ambiguousAlgebraGrammar.unambiguousParse<Expr>(AlgebraStartToken("(a+b)*c"));
    value expect = Expr (1,
        Expr (1,
            Var("a", 1),
            Plus(2),
            Var("b", 3)
            ),
        Mul(5),
        Var("c", 6)
    );

    assertEquals(root, expect);
}

test
void resolvedHorizontalAmbiguity()
{
    value root = ambiguousAlgebraGrammar.unambiguousParse<Expr>(AlgebraStartToken("(a+b)+c"));
    value expect = Expr (1,
        Expr (1,
            Var("a", 1),
            Plus(2),
            Var("b", 3)
            ),
        Plus(5),
        Var("c", 6)
    );

    assertEquals(root, expect);
}

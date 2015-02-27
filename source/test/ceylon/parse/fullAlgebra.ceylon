import ceylon.parse { ... }
import ceylon.test { test, assertEquals }

object fullAlgebraGrammar extends AlgebraGrammar() {
    rule(1, lassoc)
    shared Expr mul(Expr a, Mul o, Expr b) => Expr(a.position, a, o, b);

    rule(1, lassoc)
    shared Expr div(Expr a, Div o, Expr b) => Expr(a.position, a, o, b);

    rule(2, lassoc)
    shared Expr add(Expr a, Plus o, Expr b) => Expr(a.position, a, o, b);

    rule(2, lassoc)
    shared Expr sub(Expr a, Minus o, Expr b) => Expr(a.position, a, o, b);
}

test
shared void associativityResolvedHorizontalAmbiguity()
{
    value root = fullAlgebraGrammar.unambiguousParse("a+b+c");
    value expect = Expr (0,
        Expr (0,
            Var("a", 0),
            Plus(1),
            Var("b", 2)
            ),
        Plus(3),
        Var("c", 4)
    );

    assertEquals(root, expect);
}

test
void multipleOperatorAssociativity()
{
    value root = fullAlgebraGrammar.unambiguousParse("a+b-c");
    value expect = Expr (0,
        Expr (0,
            Var("a", 0),
            Plus(1),
            Var("b", 2)
            ),
        Minus(3),
        Var("c", 4)
    );

    assertEquals(root, expect);
}

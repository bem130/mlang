use mylang_core::smt::*;
use mylang_core::ast::*;
use mylang_core::token::Token;
use mylang_core::span::Span;

#[test]
fn simple_unsat_due_to_eq_and_ge() {
    // x >= 1 and x == 0 should be UNSAT
    let var = IntVarId(0);
    let mut set = ConstraintSet::default();
    set.literals.push(SmtLiteral::Pos(BoolAtom::Int(IntAtom::GeConst { var, c: 1 })));
    set.literals.push(SmtLiteral::Pos(BoolAtom::Int(IntAtom::EqConst { var, c: 0 })));
    assert!(is_unsat(&set));
}

#[test]
fn simple_sat_within_interval() {
    // x >= 1 and x <= 10 is SAT
    let var = IntVarId(0);
    let mut set = ConstraintSet::default();
    set.literals.push(SmtLiteral::Pos(BoolAtom::Int(IntAtom::GeConst { var, c: 1 })));
    set.literals.push(SmtLiteral::Pos(BoolAtom::Int(IntAtom::LeConst { var, c: 10 })));
    assert!(!is_unsat(&set));
}

#[test]
fn implies_positive_implies_nonzero() {
    // phi1: x > 0  (i.e., x >= 1)
    // phi2: x != 0
    // Check phi1 => phi2
    let x_id = IntVarId(0);
    let mut phi1 = ConstraintSet::default();
    phi1.literals.push(SmtLiteral::Pos(BoolAtom::Int(IntAtom::GeConst { var: x_id, c: 1 })));

    // phi2 negated: !(x != 0) -> x == 0
    let mut phi2_neg = ConstraintSet::default();
    phi2_neg
        .literals
        .push(SmtLiteral::Pos(BoolAtom::Int(IntAtom::EqConst { var: x_id, c: 0 })));

    // phi1 && !phi2 is x >= 1 && x == 0 -> UNSAT
    assert!(implies(&phi1, &phi2_neg));
}

#[test]
fn math_ast_to_constraints_generates_expected_atom() {
    // parse math AST '$ x > 0 && x < 10 $' to ConstraintSet
    let math = MathAstNode::InfixOp {
        op: Token::AndAnd,
        left: Box::new(MathAstNode::InfixOp {
            op: Token::GreaterThan,
            left: Box::new(MathAstNode::Variable("x".to_string(), Span::default())),
            right: Box::new(MathAstNode::Literal(MathLiteral::Int(0), Span::default())),
            span: Span::default(),
        }),
        right: Box::new(MathAstNode::InfixOp {
            op: Token::LessThan,
            left: Box::new(MathAstNode::Variable("x".to_string(), Span::default())),
            right: Box::new(MathAstNode::Literal(MathLiteral::Int(10), Span::default())),
            span: Span::default(),
        }),
        span: Span::default(),
    };

    let set = constraints_from_math_ast(&math);
    assert_eq!(set.literals.len(), 2);
}

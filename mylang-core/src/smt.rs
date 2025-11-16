//! A tiny interval-based solver for simple integer constraints used by refinement checks.
//!
//! This module implements a minimal fragment sufficient to check conjunctions of
//! integer bounds (x >= c, x <= c, x == c) and simple negation mapping. The
//! solver operates by propagating interval constraints until a fixed point and
//! reports UNSAT if any interval becomes empty.

use crate::ast::MathAstNode;
use alloc::vec::Vec;
// no external String usage required here

/// Integer variable identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntVarId(pub usize);

/// Simple atoms over integer variables
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntAtom {
    GeConst { var: IntVarId, c: i64 },
    LeConst { var: IntVarId, c: i64 },
    EqConst { var: IntVarId, c: i64 },
}

/// Boolean atoms are currently only integer atoms
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoolAtom {
    Int(IntAtom),
}

/// A literal which may be positive or negated
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SmtLiteral {
    Pos(BoolAtom),
    Neg(BoolAtom),
}

/// Conjunction of literals
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ConstraintSet {
    pub literals: Vec<SmtLiteral>,
}

/// Interval representing [min, max]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interval {
    pub min: i64,
    pub max: i64,
}

impl Interval {
    pub fn new() -> Self {
        Interval {
            min: i64::MIN / 4, // use a safe "-infty" margin
            max: i64::MAX / 4,
        }
    }
}

/// Environment mapping IntVarId to intervals
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntEnv {
    pub domains: alloc::collections::BTreeMap<IntVarId, Interval>,
}

impl IntEnv {
    pub fn new() -> Self {
        IntEnv {
            domains: alloc::collections::BTreeMap::new(),
        }
    }

    fn ensure_var(&mut self, var: IntVarId) {
        if !self.domains.contains_key(&var) {
            self.domains.insert(var, Interval::new());
        }
    }

    fn update_ge(&mut self, var: IntVarId, c: i64) -> bool {
        self.ensure_var(var);
        if let Some(interval) = self.domains.get_mut(&var) {
            if c > interval.min {
                interval.min = c;
                return true;
            }
        }
        false
    }

    fn update_le(&mut self, var: IntVarId, c: i64) -> bool {
        self.ensure_var(var);
        if let Some(interval) = self.domains.get_mut(&var) {
            if c < interval.max {
                interval.max = c;
                return true;
            }
        }
        false
    }

    fn update_eq(&mut self, var: IntVarId, c: i64) -> bool {
        self.ensure_var(var);
        if let Some(interval) = self.domains.get_mut(&var) {
            let mut changed = false;
            if interval.min < c {
                interval.min = c;
                changed = true;
            }
            if interval.max > c {
                interval.max = c;
                changed = true;
            }
            return changed;
        }
        false
    }

    fn is_contradiction(&self, var: IntVarId) -> bool {
        if let Some(iv) = self.domains.get(&var) {
            iv.min > iv.max
        } else {
            false
        }
    }
}

/// Solve returning true for UNSAT, false for SAT.
/// This is a tiny propagation solver and supports only basic atoms.
pub fn is_unsat(constraints: &ConstraintSet) -> bool {
    let mut env = IntEnv::new();
    // Simple fixed-point propagation
    let mut changed = true;
    // We transform negations into equivalent positive atoms where possible.
    let mut normalized: Vec<IntAtom> = Vec::new();
    for lit in &constraints.literals {
        match lit {
            SmtLiteral::Pos(BoolAtom::Int(atom)) => normalized.push(atom.clone()),
            SmtLiteral::Neg(BoolAtom::Int(atom)) => match atom {
                IntAtom::GeConst { var, c } => {
                    normalized.push(IntAtom::LeConst { var: *var, c: *c - 1 })
                }
                IntAtom::LeConst { var, c } => {
                    normalized.push(IntAtom::GeConst { var: *var, c: *c + 1 })
                }
                IntAtom::EqConst { var, c } => {
                    // ¬(x == c) is a disjunction which we cannot represent with a single interval.
                    // To keep the solver simple, we conservatively approximate by leaving as a no-op.
                    // This makes the solver incomplete but keeps the implementation small.
                }
            },
        }
    }

    // Propagate repeatedly
    while changed {
        changed = false;
        for atom in &normalized {
            match atom {
                IntAtom::GeConst { var, c } => {
                    if env.update_ge(*var, *c) {
                        changed = true;
                    }
                    if env.is_contradiction(*var) {
                        return true;
                    }
                }
                IntAtom::LeConst { var, c } => {
                    if env.update_le(*var, *c) {
                        changed = true;
                    }
                    if env.is_contradiction(*var) {
                        return true;
                    }
                }
                IntAtom::EqConst { var, c } => {
                    if env.update_eq(*var, *c) {
                        changed = true;
                    }
                    if env.is_contradiction(*var) {
                        return true;
                    }
                }
            }
        }
    }
    false
}

/// A small helper to check implication phi1 => phi2 by asking whether phi1 && !phi2 is UNSAT.
pub fn implies(phi1: &ConstraintSet, phi2_negated: &ConstraintSet) -> bool {
    let mut combined = ConstraintSet::default();
    combined.literals.extend_from_slice(&phi1.literals);
    combined.literals.extend_from_slice(&phi2_negated.literals);
    is_unsat(&combined)
}

/// Basic translator of simple MathAstNode forms into integer atoms.
/// Supports a limited subset: `x > c`, `x < c`, `x >= c`, `x <= c`, `x == c` and conjunction (`&&`).
pub fn constraints_from_math_ast(node: &MathAstNode) -> ConstraintSet {
    use crate::ast::MathAstNode::*;
    let mut set = ConstraintSet::default();
    match node {
        InfixOp { op, left, right, .. } => {
            // handle conjunction
            if *op == crate::token::Token::AndAnd {
                // recursively flatten
                let left_set = constraints_from_math_ast(left);
                let right_set = constraints_from_math_ast(right);
                for l in left_set.literals.into_iter() {
                    set.literals.push(l);
                }
                for l in right_set.literals.into_iter() {
                    set.literals.push(l);
                }
                return set;
            }

            // only support simple comparisons where one side is variable and other is literal
            match (&**left, &**right) {
                (MathAstNode::Variable(name, _), MathAstNode::Literal(crate::ast::MathLiteral::Int(c), _)) => {
                    // map operators
                    let var = IntVarId(hash_var(name));
                    match op {
                        crate::token::Token::GreaterThan => {
                            set.literals.push(SmtLiteral::Pos(BoolAtom::Int(IntAtom::GeConst { var, c: *c + 1 })));
                        }
                        crate::token::Token::GreaterThanEquals => {
                            set.literals.push(SmtLiteral::Pos(BoolAtom::Int(IntAtom::GeConst { var, c: *c })));
                        }
                        crate::token::Token::LessThan => {
                            set.literals.push(SmtLiteral::Pos(BoolAtom::Int(IntAtom::LeConst { var, c: *c - 1 })));
                        }
                        crate::token::Token::LessThanEquals => {
                            set.literals.push(SmtLiteral::Pos(BoolAtom::Int(IntAtom::LeConst { var, c: *c })));
                        }
                        crate::token::Token::EqualsEquals => {
                            set.literals.push(SmtLiteral::Pos(BoolAtom::Int(IntAtom::EqConst { var, c: *c })));
                        }
                        crate::token::Token::BangEquals => {
                            // x != c  => we encode as Neg(EqConst)
                            set.literals
                                .push(SmtLiteral::Neg(BoolAtom::Int(IntAtom::EqConst { var, c: *c })));
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }
    set
}

/// Hash variable name to a small integer ID. Tiny helper – collisions are ok for tests/early stage.
fn hash_var(name: &str) -> usize {
    // Very small hash to map 'x' -> id 0, 'y' -> 1, etc.
    let mut h: usize = 0;
    for b in name.bytes() {
        h = h.wrapping_mul(31).wrapping_add(b as usize);
    }
    h
}

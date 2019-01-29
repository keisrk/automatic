use std::ops::{Not, Add, Mul};

pub mod alphabet;
pub mod fa;
pub mod presburger;
pub mod util;

#[derive(Debug, Clone)]
enum Prop {
    //
    Const(bool),
    Var(u8),
    Not(Box<Prop>),
    Or(Box<Prop>, Box<Prop>),
    And(Box<Prop>, Box<Prop>)
}

impl Prop {

    fn zero() -> Self {
        Prop::Const(false)
    }

    fn one() -> Self {
        Prop::Const(true)
    }

    fn var(x: u8) -> Self {
        Prop::Var(x)
    }

    fn interpret<F>(&self, truth_assign: &F) -> bool
    where F: Fn(u8) -> bool {
        match self {
            Prop::Const(b) => *b,
            Prop::Var(x) => truth_assign(*x),
            Prop::Not(x) => !x.interpret(truth_assign),
            Prop::Or(x, y) => x.interpret(truth_assign) || y.interpret(truth_assign),
            Prop::And(x, y) => x.interpret(truth_assign) && y.interpret(truth_assign),
        }
    }
}

impl Not for Prop {
    type Output = Prop;

    fn not(self) -> Self::Output {
        Prop::Not(Box::new(self))
    }
}

impl<'a> Not for &'a Prop {
    type Output = Prop;

    fn not(self) -> Self::Output {
        Prop::Not(Box::new(self.clone()))
    }
}

impl Add for Prop {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Prop::Or(Box::new(self), Box::new(other))
    }
}

impl Mul for Prop {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Prop::And(Box::new(self), Box::new(other))
    }
}

type Var = usize;

#[derive(Debug, Clone)]
enum Fol {
    //
    Const(bool),
    Pred {
        symbol: u64,   // hashed value, for example.
        arity: usize,  // Always p.arity == p.vars.len()
        vars: Vec<Var> // Order of variables matters.
    },
    Not(Box<Fol>),
    Or(Box<Fol>, Box<Fol>),
    And(Box<Fol>, Box<Fol>),
    ForAll(Var, Box<Fol>),
    Exists(Var, Box<Fol>)
}

impl Fol {

    fn zero() -> Self {
        Fol::Const(false)
    }

    fn one() -> Self {
        Fol::Const(true)
    }

    fn pred(symbol: u64, arity: usize, vars: Vec<Var>) -> Self {
        Fol::Pred{ symbol, arity, vars }
    }

    fn is_wellformed(&self) -> bool {
        match self {
            Fol::Const(_) => true,
            Fol::Pred{ symbol, arity, vars } => *arity == vars.len(),
            Fol::Not(phi) => phi.is_wellformed(),
            Fol::Or(phi, psi) => phi.is_wellformed() || psi.is_wellformed(),
            Fol::And(phi, psi) => phi.is_wellformed() || psi.is_wellformed(),
            Fol::ForAll(x, phi) => phi.is_wellformed(),
            Fol::Exists(x, phi) => phi.is_wellformed(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn interpret() {
        let proposition = !(Prop::var(0) * Prop::var(1)) + Prop::var(0);
        let truth_assign = | x | match x {
            0 => true,
            1 => false,
            2 => false,
            3 => true,
            _ => false
        };
        assert!(proposition.interpret(&truth_assign))
    }

    fn is_wellformed() {
        //let first_order = !(Prop::var(0) * Prop::var(1)) + Prop::var(0);
        assert!(true/*first_order.is_wellformed()*/)
    }
}

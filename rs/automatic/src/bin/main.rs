extern crate clap;
extern crate env_logger;
#[macro_use] extern crate log;
extern crate varisat;

use clap::{App, Arg};
use varisat::config::SolverConfig;
use varisat::dimacs;
use varisat::{Lit, ProofFormat, Solver, Trit};

fn main() {
    let mut solver = Solver::new();
    for _ in 0 .. 3 {
        solver.add_clause(&[Lit::from_dimacs(1), Lit::from_dimacs(-2)]);
        solver.add_clause(&[Lit::from_dimacs(-1), Lit::from_dimacs(3)]);
        solver.solve();
    }
    env_logger::init();
    println!("Hello, world!");
}

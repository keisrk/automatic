extern crate clap;
extern crate env_logger;
extern crate log;
extern crate varisat;

use clap::{App, Arg};
use log::{info};
use varisat::config::SolverConfig;
use varisat::{Lit, Solver};

fn main() {
    env_logger::init();

    let mut solver = Solver::new();
    for _ in 0 .. 3 {
        solver.add_clause(&[Lit::from_dimacs(1), Lit::from_dimacs(-2)]);
        solver.add_clause(&[Lit::from_dimacs(-1), Lit::from_dimacs(3)]);
        solver.solve();
        info!("{}", "solved");
    }
}

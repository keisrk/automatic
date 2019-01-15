//!
//!
use super::utils::{Var};
use std::collections::HashMap;
use std::iter::{Iterator};

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub struct LinearEquation {
    coeff: HashMap<Var, i8>,
    k: i8
}

impl LinearEquation {
    pub fn new(coeff: Vec<(Var, i8)>, k: i8) -> Self {
        LinearEquation {
            coeff: coeff.into_iter().collect(),
            k
        }
    }

    pub fn vars(&self) -> impl Iterator<Item=&Var> {
        self.coeff.keys()
    }

    pub fn mods(&self) -> Mods {
        Mods { coeff: &self.coeff, k: self.k }
    }
}
/* 
    if let Some(c) = alphabet.next() {
        let state = self.solve(c);
        if let Some(new)
        Some()
    } else {
        None
    }
*/
pub struct Mods <'a> {
    coeff: &'a HashMap<Var, i8>,
    k: i8
}

impl<'a> Iterator for Mods<'a> {
    type Item = Var;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ron::{ser, de};

    #[test]
    fn iterate() {
        let le: LinearEquation = de::from_str("(coeff: {0:-1,1:3,2:-2}, 
        k: 1)").unwrap();
        println!("{:?}", le.vars().collect::<Vec<_>>());
    }
}

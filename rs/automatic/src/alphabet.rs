use std::collections::HashMap;
use std::collections::hash_map::Keys;

pub struct Alphabet {
    // index -> x_1, ..., x_n
    vars: HashMap<usize, Var>,
}

impl Alphabet {
    pub fn new(vars: Vec<(usize, Var)>) -> Self {
        let vars = vars.into_iter().collect()
        let curr = vec![]
        Alphabet{vars, curr}
    }

    pub fn iter(&self) -> AlphabetIter {
        AlphabetIter {
        }
    }
}

impl AlphabetIter {
    curr: Vec<bool>
}

impl Iterator for AlphabetIter {
    type Item = Vec<bool>;
    fn next(&mut self)
}

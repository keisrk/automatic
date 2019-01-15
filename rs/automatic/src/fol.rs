//!
//!
use super::utils::{Var};
use super::presburger::{LinearEquation};
use std::iter::{Iterator};
use itertools::Itertools;

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Fol {
    Lit(bool, LinearEquation),
    Not(Box<Fol>),
    And(Box<Fol>, Box<Fol>),
    Or(Box<Fol>, Box<Fol>),
    ForAll(Var, Box<Fol>),
    Exists(Var, Box<Fol>)
}

impl Fol {

    fn _vars<'a>(&'a self) -> Box<Iterator<Item=&Var> + 'a> {
        match self {
            Fol::Lit(_, le) => Box::new(le.vars()),
            Fol::Not(fol) | Fol::ForAll(_, fol) | Fol::Exists(_, fol) =>
                fol._vars(),
            Fol::And(lhs, rhs) | Fol::Or(lhs, rhs) => { 
                Box::new(lhs._vars().chain(rhs._vars()))
            },
        }
    }

    pub fn vars(&self) -> impl Iterator<Item=&Var> {
        self._vars().unique()
    }

    pub fn walk(&self) -> FolWalker {
        FolWalker{next: Some(self), stack: Vec::new()}
    }

    fn to_nnf(&self, flag: bool) -> Self {
        if flag { 
            match self {
                Fol::Lit(_, _) => self.clone(),
                Fol::Not(fol) => fol.to_nnf(false),
                Fol::And(lhs, rhs) => Fol::And(Box::new(lhs.to_nnf(flag)), 
                                               Box::new(rhs.to_nnf(flag))),
                Fol::Or(lhs, rhs) => Fol::Or(Box::new(lhs.to_nnf(flag)), 
                                             Box::new(rhs.to_nnf(flag))),
                Fol::ForAll(i, fol) => Fol::ForAll(*i, 
                                             Box::new(fol.to_nnf(flag))),
                Fol::Exists(i, fol) => Fol::Exists(*i, 
                                             Box::new(fol.to_nnf(flag))),
            }
        } else {
            match self {
                Fol::Lit(flag, le) => Fol::Lit(!flag, le.clone()),
                Fol::Not(fol) => fol.to_nnf(true),
                Fol::And(lhs, rhs) => 
                    Fol::Or(Box::new(lhs.to_nnf(flag)), 
                            Box::new(rhs.to_nnf(flag))),
                Fol::Or(lhs, rhs) => 
                    Fol::And(Box::new(lhs.to_nnf(flag)), 
                             Box::new(rhs.to_nnf(flag))),
                Fol::ForAll(i, fol) => Fol::Exists(*i, 
                                             Box::new(fol.to_nnf(flag))),
                Fol::Exists(i, fol) => Fol::ForAll(*i, 
                                             Box::new(fol.to_nnf(flag))),
            }
        }
    }
}

pub struct FolWalker<'a> {
    next: Option<&'a Fol>,
    stack: Vec<&'a Fol>
}

impl<'a> Iterator for FolWalker<'a> {
    type Item = &'a Fol;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next {
            None => if let Some(next) = self.stack.pop() {
                self.next = Some(next);
                self.next()
            } else {
                None
            },
            Some(Fol::Lit(_, _)) => {self.next.take()},
            Some(Fol::Not(fol)) | Some(Fol::ForAll(_, fol)) 
            | Some(Fol::Exists(_, fol)) => 
                self.next.replace(fol),
            Some(Fol::And(lhs, rhs))|Some(Fol::Or(lhs, rhs)) => {
                self.stack.push(rhs); 
                self.next.replace(lhs)
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ron::{ser, de};

    #[test]
    fn serialize() {
        let fol = Fol::ForAll(0,
            Box::new(Fol::Exists(1,
                Box::new(Fol::Not(
                    Box::new(Fol::And(
                        Box::new(Fol::Lit(true,
                            LinearEquation::new(
                                vec![(0, -1), (1, 3), (2, -2)], 1))),
                        Box::new(Fol::Lit(true,
                            LinearEquation::new(
                                vec![(0, -1), (1, 3), (2, -2)], 1)))
                            )))))));
        println!("{:?}", ser::to_string(&fol).unwrap());
    }
    #[test]
    fn deserialize() {
        let fol = r#"
            forall(0,
                exists(1,
                    not(
                        and(
                            lit(true,
                                (coeff:{0:-1,1:3,2:-2,},k:1)),
                            lit(true,
                                (coeff:{2:-2,0:-1,1:3,},k:1))))))
        "#;
        let fol: Fol = de::from_str(fol).unwrap();
        println!("{:?}", fol);
    }
    #[test]
    fn vars() {
        let fol = r#"
            or(
               and(
                   lit(true,
                       (coeff:{0:0,1:1,2:2,},k:1)),
                   lit(true,
                       (coeff:{3:3,4:4,5:5,},k:1))),
               lit(true,
                   (coeff:{0:0,2:2,4:4,},k:1)))
        "#;
        let fol: Fol = de::from_str(fol).unwrap();
        println!("{:?}", fol.vars().cloned().collect::<Vec<_>>());
    }
    #[test]
    fn into_iter() {
        let ii: <Vec<u8> as IntoIterator>::IntoIter = vec![0,1].into_iter();
    }
    #[test]
    fn walker() {
        let fol = r#"
            or(
               and(
                   lit(true,
                       (coeff:{0:0,1:1,2:2,},k:1)),
                   lit(true,
                       (coeff:{3:3,4:4,5:5,},k:1))),
               forall(1, 
                   lit(true,
                       (coeff:{0:0,2:2,4:4,},k:1))))
        "#;
        let fol: Fol = de::from_str(fol).unwrap();
        for node in fol.walk() {
            println!("{:?}", node);
        }
    }     
    #[test]
    fn to_nnf() {
        let fol = r#"
            not(
                or(
                    and(
                        lit(true,
                            (coeff:{0:0,1:1,2:2,},k:1)),
                        lit(true,
                            (coeff:{3:3,4:4,5:5,},k:1))),
                    lit(true,
                        (coeff:{0:0,2:2,4:4,},k:1))))
        "#;
        let fol: Fol = de::from_str(fol).unwrap();
        println!("{:?}", fol.to_nnf(true));
    }
}

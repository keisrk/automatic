use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Prop {
    Lit(bool, u8),
    And(Box<Prop>, Box<Prop>),
    Or(Box<Prop>, Box<Prop>),
}

impl Prop {
    fn vars(&self) -> Vec<u8> {
        match self {
            Prop::Lit(_, var) => vec![*var],
            Prop::And(lhs, rhs) | Prop::Or(lhs, rhs) => {
                let mut buf = Vec::new();
                buf.append(&mut lhs.vars());
                buf.append(&mut rhs.vars());
                buf
            }
        }
    }
    fn to_cnf(&mut self) {
        match self {
            Prop::Lit(_, _) => {},
            Prop::And(box(Prop::Lit(_, _)), rhs) => {},
            Prop::Or(box(Prop::Lit(_, _)), rhs) => {},
            _ => {}
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub struct Bind {
    fresh: u8,
    pub bind: HashMap<u8, Prop>,
}

impl Bind {
    pub fn new(prop: &Prop) -> Self {
        let fresh = 1 + prop.vars().iter().max().unwrap();
        Bind {fresh, bind: HashMap::new()}
    }

    fn fresh(&mut self) -> u8 {
        let var = self.fresh;
        self.fresh += 1;
        var
    }

    pub fn tseitin(&mut self, prop: &Prop) -> Prop {
        match prop {
            Prop::Lit(sign, var) => {
                prop.clone()
            },
            Prop::And(lhs, rhs) => {
                let lhs = box self.tseitin(lhs);
                let rhs = box self.tseitin(rhs);
                let var = self.fresh();
                assert_eq!(self.bind.insert(var, Prop::And(lhs, rhs)),
                           None);
                Prop::Lit(true, var)
            },
            Prop::Or(lhs, rhs) => {
                let lhs = box self.tseitin(lhs);
                let rhs = box self.tseitin(rhs);
                let var = self.fresh();
                assert_eq!(self.bind.insert(var, Prop::Or(lhs, rhs)),
                           None);
                Prop::Lit(true, var)
            }
        }
    }
    pub fn dump(&self) {
        for (key, val) in self.bind.iter() {
            println!("{:?}=>{:?}", key, val);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ron::{ser, de};

    #[test]
    fn serialize() {
        let prop = Prop::And(
                       Box::new(Prop::Or(
                           Box::new(Prop::Lit(true, 0)),
                           Box::new(Prop::Lit(false, 1))
                       )),
                       Box::new(Prop::Lit(false, 2)));
        println!("{:?}", ser::to_string(&prop).unwrap());
        let mut bind = Bind::new(&prop);
        bind.tseitin(&prop);
        bind.dump();
    }
}


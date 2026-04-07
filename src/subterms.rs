use crate::terms::Term;
use crate::terms::Term::*;
use std::error::Error;
use uuid::Uuid;

#[derive(Clone, Debug)]
pub enum Subterm {
    Basis(Term),
    Application(Box<Subterm>, Box<Subterm>),
    Abstraction(Box<Subterm>, Box<Subterm>),
}

impl Subterm {
    pub fn _basis(v: Term) -> Result<Subterm, Box<dyn Error>> {
        match v {
            Variable(name) => Ok(Subterm::Basis(Variable(name))),
            _ => Err("Basis can only be variable".into()),
        }
    }
    pub fn application(v1: Box<Subterm>, v2: Box<Subterm>) -> Result<Subterm, Box<dyn Error>> {
        Ok(Subterm::Application(v1, v2))
    }

    pub fn abstraction(v1: Box<Subterm>, v2: Box<Subterm>) -> Result<Subterm, Box<dyn Error>> {
        Ok(Subterm::Abstraction(v1, v2))
    }

    fn basis_tag(uidv4: &String, name: String) -> Vec<String> {
        vec![format!("Basis_{}: \"{}\"", uidv4, name)]
    }

    fn basis_point(uidv4: &String, root: Option<String>) -> Vec<String> {
        match root {
            None => vec![],
            Some(root_value) => vec![format!("Basis_{} -> {}", uidv4, root_value)],
        }
    }
    fn abs_tag(uidv4: String, root: Option<String>) -> Vec<String> {
        let init_value = vec![
            format!("Lambda_{}", uidv4),
            format!("Lambda_{}: \"\\\\\"", uidv4),
        ];
        match root {
            None => init_value,
            Some(root_value) => [
                init_value,
                vec![format!("Lambda_{} -> {}", uidv4, root_value)],
            ]
            .concat(),
        }
    }

    fn app_tag(uidv4: String, root: Option<String>) -> Vec<String> {
        let init_value = vec![format!("App_{}", uidv4), format!("App_{}: \".\"", uidv4)];
        match root {
            None => init_value,
            Some(root_value) => {
                [init_value, vec![format!("App_{} -> {}", uidv4, root_value)]].concat()
            }
        }
    }

    fn random_id() -> String {
        Uuid::new_v4().to_string()
    }

    pub fn show_tree(sub: Subterm, root: Option<String>) -> Vec<String> {
        match sub {
            Subterm::Basis(Variable(name)) => match root {
                None => Self::basis_tag(&Self::random_id(), name),
                Some(value) => {
                    let id_ = Self::random_id();
                    [
                        Self::basis_tag(&id_, name),
                        Self::basis_point(&id_, Some(value)),
                    ]
                    .concat()
                }
            },
            Subterm::Abstraction(term1, term2) => match (term1.as_ref(), term2.as_ref()) {
                (Subterm::Basis(Variable(t1)), Subterm::Basis(Variable(t2))) => {
                    let id_ = Self::random_id();
                    let [v1, rest @ ..] = &Self::abs_tag(id_, root)[..] else {
                        unreachable!("something fishy is going on!")
                    };
                    [
                        rest,
                        &Self::show_tree(*term1, Some(v1.clone())),
                        &Self::show_tree(*term2, Some(v1.clone())),
                    ]
                    .concat()
                }
                _ => {
                    let id_ = Self::random_id();
                    let [v1, rest @ ..] = &Self::abs_tag(id_, root)[..] else {
                        unreachable!("something fishy is going on!")
                    };
                    [
                        rest,
                        &Self::show_tree(*term1, Some(v1.clone())),
                        &Self::show_tree(*term2, Some(v1.clone())),
                    ]
                    .concat()
                }
            },
            Subterm::Application(term1, term2) => match (term1.as_ref(), term2.as_ref()) {
                _ => {
                    let id_ = Self::random_id();
                    let [v1, rest @ ..] = &Self::app_tag(id_, root)[..] else {
                        unreachable!("something fishy is going on!")
                    };
                    [
                        rest,
                        &Self::show_tree(*term1, Some(v1.clone())),
                        &Self::show_tree(*term2, Some(v1.clone())),
                    ]
                    .concat()
                }
            },
            _ => unreachable!("{}", format!("A invalid subterm was used: {:?}", sub)),
        }
    }
}

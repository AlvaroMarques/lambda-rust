use std::{error::Error, str::Chars};

#[derive(Clone, Debug)]
enum Term {
    Variable(String),
    Lambda(),
    Group(Vec<Term>),
    Dot(),
}

use crate::Term::*;

#[derive(Clone, Debug)]
enum Subterm {
    Basis(Term),
    Application(Box<Subterm>, Box<Subterm>),
    Abstraction(Box<Subterm>, Box<Subterm>),
}

impl Subterm {
    fn basis(v: Term) -> Result<Subterm, Box<dyn Error>> {
        match v {
            Variable(name) => Ok(Subterm::Basis(Variable(name))),
            _ => Err("Basis can only be variable".into()),
        }
    }
    fn application(v1: Box<Subterm>, v2: Box<Subterm>) -> Result<Subterm, Box<dyn Error>> {
        Ok(Subterm::Application(v1, v2))
    }

    fn abstraction(v1: Box<Subterm>, v2: Box<Subterm>) -> Result<Subterm, Box<dyn Error>> {
        Ok(Subterm::Abstraction(v1, v2))
    }
}

impl Term {
    fn show(t: &Term) -> String {
        match t {
            Term::Dot() => String::from("."),
            Term::Lambda() => String::from("\\"),
            Term::Variable(name) => name.to_string(),
            Term::Group(terms) => {
                let terms_str_vec: Vec<String> = terms.iter().map(|tm| Term::show(tm)).collect();
                format!("({})", terms_str_vec.join(""))
            }
        }
    }

    fn parse_ptr(ptr: &mut Chars<'_>, endchr: char) -> Vec<Self> {
        match ptr.next() {
            Some(chr) if chr == endchr => vec![],
            None => vec![],
            Some('.') => [vec![Term::Dot()], Term::parse_ptr(ptr, endchr)].concat(),
            Some('\\') => [vec![Term::Lambda()], Term::parse_ptr(ptr, endchr)].concat(),
            Some('(') => [
                vec![Term::Group(Term::parse_ptr(ptr, ')'))],
                Term::parse_ptr(ptr, endchr),
            ]
            .concat(),
            Some(var) => [
                vec![Term::Variable(String::from(var))],
                Term::parse_ptr(ptr, endchr),
            ]
            .concat(),
        }
    }

    fn parse(strcons: &str) -> Term {
        let mut chars = strcons.chars();
        Term::Group(Self::parse_ptr(&mut chars, '!'))
    }

    fn group_lambdas(terms: Vec<Term>) -> Vec<Term> {
        match &terms[..] {
            [Lambda(), Group(terms), Dot(), Lambda(), l2 @ Variable(_), rest @ ..] => {
                Self::group_lambdas(
                    [
                        [Lambda(), Group([terms.clone(), vec![l2.clone()]].concat())].to_vec(),
                        rest.to_vec(),
                    ]
                    .concat(),
                )
            }
            [Lambda(), l1 @ Variable(_), l2 @ Variable(_), rest @ ..] => Self::group_lambdas(
                [
                    [Lambda(), Group(vec![l1.clone(), l2.clone()])].to_vec(),
                    rest.to_vec(),
                ]
                .concat(),
            ),
            [Lambda(), l1 @ Variable(_), Dot(), Lambda(), l2 @ Variable(_), rest @ ..] => {
                Self::group_lambdas(
                    [
                        [Lambda(), Group(vec![l1.clone(), l2.clone()])].to_vec(),
                        rest.to_vec(),
                    ]
                    .concat(),
                )
            }
            [Group(ts)] => [Group(Self::group_lambdas(ts.clone()))].to_vec(),
            [any, rest @ ..] => [vec![any.clone()], Self::group_lambdas(rest.to_vec())].concat(),
            [] => vec![],
        }
    }

    fn group_applications(terms: Vec<Self>) -> Vec<Self> {
        match &terms[..] {
            [Lambda(), g @ Variable(..), Dot(), v1 @ Variable(..), v2 @ Variable(..), rest @ ..] => {
                Self::group_applications(
                    [
                        vec![
                            Lambda(),
                            g.clone(),
                            Dot(),
                            Group(vec![v1.clone(), v2.clone()]),
                        ],
                        rest.to_vec(),
                    ]
                    .concat(),
                )
            }
            [Lambda(), g @ Group(..), Dot(), v1 @ Variable(..), v2 @ Variable(..), rest @ ..] => {
                Self::group_applications(
                    [
                        vec![
                            Lambda(),
                            g.clone(),
                            Dot(),
                            Group(vec![v1.clone(), v2.clone()]),
                        ],
                        rest.to_vec(),
                    ]
                    .concat(),
                )
            }
            [Lambda(), g @ Variable(..), Dot(), app @ Group(..), v2 @ Variable(..), rest @ ..] => {
                Self::group_applications(
                    [
                        vec![
                            Lambda(),
                            g.clone(),
                            Dot(),
                            Group(vec![app.clone(), v2.clone()]),
                        ],
                        rest.to_vec(),
                    ]
                    .concat(),
                )
            }
            [Lambda(), g @ Group(..), Dot(), app @ Group(..), v2 @ Variable(..), rest @ ..] => {
                Self::group_applications(
                    [
                        vec![
                            Lambda(),
                            g.clone(),
                            Dot(),
                            Group(vec![app.clone(), v2.clone()]),
                        ],
                        rest.to_vec(),
                    ]
                    .concat(),
                )
            }
            [Group(ts)] => [Group(Self::group_applications(ts.clone()))].to_vec(),
            [any, rest @ ..] => {
                [vec![any.clone()], Self::group_applications(rest.to_vec())].concat()
            }
            _ => terms,
        }
    }

    fn group(term: &Self) -> Self {
        let abstraction_parsing = Group(Self::group_lambdas(vec![term.clone()]));
        Group(Self::group_applications(vec![abstraction_parsing]))
    }

    fn into_subterms(term: &Self) -> Result<Subterm, Box<dyn Error>> {
        match term {
            v @ Variable(..) => Ok(Subterm::Basis(v.clone())),
            Group(terms) => match &terms[..] {
                [ts] => Self::into_subterms(&ts.clone()),
                [x @ Variable(..), y @ Variable(..)] => Subterm::application(
                    Box::new(Subterm::Basis(x.clone())),
                    Box::new(Subterm::Basis(y.clone())),
                ),
                [x @ Variable(..), y @ Variable(..), Lambda(), rest @ ..] => Subterm::application(
                    Box::new(Subterm::application(
                        Box::new(Subterm::Basis(x.clone())),
                        Box::new(Subterm::Basis(y.clone())),
                    )?),
                    Box::new(Self::into_subterms(&Group(
                        [vec![Lambda()], rest.to_vec()].concat(),
                    ))?),
                ),
                [x @ Group(..), y @ Variable(..)] => Subterm::application(
                    Box::new(Self::into_subterms(&x.clone())?),
                    Box::new(Subterm::Basis(y.clone())),
                ),
                [x @ Group(..), y @ Variable(..), Lambda(), rest @ ..] => Subterm::application(
                    Box::new(Subterm::application(
                        Box::new(Self::into_subterms(&x.clone())?),
                        Box::new(Subterm::Basis(y.clone())),
                    )?),
                    Box::new(Self::into_subterms(&Group(
                        [vec![Lambda()], rest.to_vec()].concat(),
                    ))?),
                ),
                [x @ Group(..), y @ Group(..)] => Subterm::application(
                    Box::new(Self::into_subterms(&x.clone())?),
                    Box::new(Self::into_subterms(&y.clone())?),
                ),
                [x @ Group(..), y @ Group(..), Lambda(), rest @ ..] => Subterm::application(
                    Box::new(Subterm::application(
                        Box::new(Self::into_subterms(&x.clone())?),
                        Box::new(Self::into_subterms(&y.clone())?),
                    )?),
                    Box::new(Self::into_subterms(&Group(
                        [vec![Lambda()], rest.to_vec()].concat(),
                    ))?),
                ),
                [x @ Variable(..), y @ Group(..)] => Subterm::application(
                    Box::new(Subterm::Basis(x.clone())),
                    Box::new(Self::into_subterms(&y.clone())?),
                ),
                [x @ Variable(..), y @ Group(..), Lambda(), rest @ ..] => Subterm::application(
                    Box::new(Subterm::application(
                        Box::new(Subterm::Basis(x.clone())),
                        Box::new(Self::into_subterms(&y.clone())?),
                    )?),
                    Box::new(Self::into_subterms(&Group(
                        [vec![Lambda()], rest.to_vec()].concat(),
                    ))?),
                ),
                [Lambda(), x @ Group(..), Dot(), y @ Variable(..)] => Subterm::abstraction(
                    Box::new(Self::into_subterms(&x.clone())?),
                    Box::new(Subterm::Basis(y.clone())),
                ),
                [Lambda(), x @ Group(..), Dot(), y @ Variable(..), Lambda(), rest @ ..] => {
                    Subterm::application(
                        Box::new(Subterm::abstraction(
                            Box::new(Self::into_subterms(&x.clone())?),
                            Box::new(Subterm::Basis(y.clone())),
                        )?),
                        Box::new(Self::into_subterms(&Group(
                            [vec![Lambda()], rest.to_vec()].concat(),
                        ))?),
                    )
                }
                [Lambda(), x @ Variable(..), Dot(), y @ Variable(..)] => Subterm::abstraction(
                    Box::new(Subterm::Basis(x.clone())),
                    Box::new(Subterm::Basis(y.clone())),
                ),

                [Lambda(), x @ Variable(..), Dot(), y @ Variable(..), Lambda(), rest @ ..] => {
                    Subterm::application(
                        Box::new(Subterm::abstraction(
                            Box::new(Subterm::Basis(x.clone())),
                            Box::new(Subterm::Basis(y.clone())),
                        )?),
                        Box::new(Self::into_subterms(&Group(
                            [vec![Lambda()], rest.to_vec()].concat(),
                        ))?),
                    )
                }
                [Lambda(), x @ Variable(..), Dot(), y @ Group(..)] => Subterm::abstraction(
                    Box::new(Subterm::Basis(x.clone())),
                    Box::new(Self::into_subterms(&y.clone())?),
                ),
                [Lambda(), x @ Variable(..), Dot(), y @ Group(..), Lambda(), rest @ ..] => {
                    Subterm::application(
                        Box::new(Subterm::abstraction(
                            Box::new(Subterm::Basis(x.clone())),
                            Box::new(Self::into_subterms(&y.clone())?),
                        )?),
                        Box::new(Self::into_subterms(&Group(
                            [vec![Lambda()], rest.to_vec()].concat(),
                        ))?),
                    )
                }
                [Lambda(), x @ Group(..), Dot(), y @ Group(..), Lambda(), rest @ ..] => {
                    Subterm::application(
                        Box::new(Subterm::abstraction(
                            Box::new(Self::into_subterms(&x.clone())?),
                            Box::new(Self::into_subterms(&y.clone())?),
                        )?),
                        Box::new(Self::into_subterms(&Group(
                            [vec![Lambda()], rest.to_vec()].concat(),
                        ))?),
                    )
                }
                [Lambda(), x @ Group(..), Dot(), y @ Group(..)] => Subterm::abstraction(
                    Box::new(Subterm::Basis(x.clone())),
                    Box::new(Self::into_subterms(&y.clone())?),
                ),
                _ => Err(format!("Unexpected group: {:?}", term.clone()).into()),
            },
            _ => Err(format!("Unexpected term: {:?}", term).into()),
        }
    }
}

impl Subterm {
    fn show_tree(sub: Subterm, ident: usize) -> () {
        match sub {
            Subterm::Basis(Variable(name)) => {
                println!("{}", format!("{}{}", "\t".repeat(ident), name))
            }
            Subterm::Abstraction(term1, term2) => match (term1.as_ref(), term2.as_ref()) {
                _ => {
                    println!("{}", format!("{}{}", "\t".repeat(ident), "𝜆"));
                    Self::show_tree(*term1, ident + 1);
                    Self::show_tree(*term2, ident + 1);
                }
            },
            Subterm::Application(term1, term2) => match (term1.as_ref(), term2.as_ref()) {
                _ => {
                    println!("{}", format!("{}{}", "\t".repeat(ident), "."));
                    Self::show_tree(*term1, ident + 1);
                    Self::show_tree(*term2, ident + 1);
                }
            },
            _ => unreachable!("{}", format!("A invalid subterm was used: {:?}", sub)),
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let term = Term::parse("\\xy.xz\\u.uw");
    println!("{:?}", term);
    let grouped = Term::group(&term);
    println!("{:?}", grouped);
    println!("{}", Term::show(&term));
    println!("{}", Term::show(&grouped));
    let st = Term::into_subterms(&grouped)?;
    println!("{:?}", st);
    Subterm::show_tree(st, 0);
    Ok(())
}

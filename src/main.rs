mod subterms;
mod terms;

use crate::subterms::Subterm;
use crate::terms::Term;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let term = Term::parse("\\xy.xz\\u.uw");
    println!("{:?}", term);
    let grouped = Term::group(&term);
    println!("{:?}", grouped);
    println!("{}", Term::show(&term));
    println!("{}", Term::show(&grouped));
    let st = Term::into_subterms(&grouped)?;
    println!("{:?}", st);
    println!(
        "{}",
        Subterm::show_tree(st, Some("root".to_string())).join("\n")
    );
    Ok(())
}

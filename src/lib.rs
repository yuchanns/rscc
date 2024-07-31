mod codegen;
mod parse;
mod tokenize;

use std::env;

pub use codegen::*;
pub use parse::*;
pub use tokenize::*;

use anyhow::{anyhow, Result};

pub fn run() -> Result<()> {
    let mut args: Vec<String> = env::args().collect();

    let [_, arg] = args.as_mut_slice() else {
        let name = env::args().next().unwrap_or_default();
        return Err(anyhow!("{name}: invalid number of arguments"));
    };

    let mut tokens = tokenize(arg)?;
    let nodes = parse(&mut tokens)?;
    codegen(&nodes)
}

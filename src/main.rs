use anyhow::{anyhow, Result};
use std::{env, process::ExitCode};

fn main() -> ExitCode {
    if let Err(err) = run() {
        eprintln!("{:?}", err);
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}

fn parse_number(s: &str) -> Result<(i32, &str)> {
    let end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
    let (num_str, rest) = s.split_at(end);
    let Ok(num) = num_str.parse::<i32>() else {
        return Err(anyhow!("{num_str}: invalid number of inputs"));
    };
    Ok((num, rest))
}

fn run() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let [_, arg] = args.as_slice() else {
        let name = env::args().next().unwrap_or_default();
        return Err(anyhow!("{name}: invalid number of arguments"));
    };

    let (num, mut arg) = parse_number(arg)?;

    #[cfg(not(target_os = "macos"))]
    {
        println!("  .global main");
        println!("main:");
    }
    #[cfg(target_os = "macos")]
    {
        println!("  .global _main");
        println!("_main:");
    }
    println!("  mov x0, #{}", num);

    while !arg.is_empty() {
        if arg.starts_with('+') {
            let (num, rest) = parse_number(&arg[1..])?;
            println!("  add x0, x0, #{num}");
            arg = rest;
        } else if arg.starts_with('-') {
            let (num, rest) = parse_number(&arg[1..])?;
            println!("  sub x0, x0, #{num}");
            arg = rest;
        } else {
            return Err(anyhow!("unexpected character: '{arg}'"));
        }
    }

    println!("  ret");

    Ok(())
}

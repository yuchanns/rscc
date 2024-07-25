use anyhow::{anyhow, Result};
use std::{env, process::ExitCode};

fn main() -> ExitCode {
    if let Err(err) = run() {
        eprintln!("{:?}", err);
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}

fn run() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let [_, arg] = args.as_slice() else {
        let name = env::args().next().unwrap_or_default();
        return Err(anyhow!("{name}: invalid number of arguments"));
    };

    let Ok(num) = arg.parse::<i32>() else {
        return Err(anyhow!("{arg}: invalid number of inputs"));
    };

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
    println!("  ret");

    Ok(())
}

use anyhow::Result;
use std::{fs, io::Write, process::Command};

fn run(input: &str) -> Result<Option<i32>> {
    let output = Command::new("target/debug/rscc").arg(input).output()?;

    let asm = String::from_utf8_lossy(&output.stdout);

    let tmpdots = "target/debug/tmp.s";
    let tmp = "target/debug/tmp";

    let mut tmp_file = fs::File::create(tmpdots)?;
    write!(tmp_file, "{}", asm)?;

    #[cfg(not(target_os = "macos"))]
    assert!(Command::new("gcc")
        .args(["-static", "-o", tmp, tmpdots])
        .status()?
        .success());
    #[cfg(target_os = "macos")]
    assert!(Command::new("gcc")
        .args(["-o", tmp, tmpdots])
        .status()?
        .success());

    let output = Command::new(tmp).output()?;

    Ok(output.status.code())
}

#[test]
fn test_compiler() -> Result<()> {
    assert!(Command::new("cargo").arg("build").status()?.success());

    assert_eq!(run("0")?, Some(0));
    assert_eq!(run("42")?, Some(42));
    assert_eq!(run("5+20-4")?, Some(21));
    assert_eq!(run(" 12 + 34 - 5 ")?, Some(41));
    assert_eq!(run("5+6*7")?, Some(47));
    assert_eq!(run("5*(9-6)")?, Some(15));
    assert_eq!(run("(3+5)/2")?, Some(4));

    Ok(())
}

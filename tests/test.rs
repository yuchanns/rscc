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

    assert_eq!(run("{ return 0; }")?, Some(0));
    assert_eq!(run("{ return 42; }")?, Some(42));
    assert_eq!(run("{ return 5+20-4; }")?, Some(21));
    assert_eq!(run("{ return  12 + 34 - 5 ; }")?, Some(41));
    assert_eq!(run("{ return 5+6*7; }")?, Some(47));
    assert_eq!(run("{ return 5*(9-6); }")?, Some(15));
    assert_eq!(run("{ return (3+5)/2; }")?, Some(4));
    assert_eq!(run("{ return -10+20; }")?, Some(10));
    assert_eq!(run("{ return - -10; }")?, Some(10));
    assert_eq!(run("{ return - - +10; }")?, Some(10));

    assert_eq!(run("{ return 0==1; }")?, Some(0));
    assert_eq!(run("{ return 42==42; }")?, Some(1));
    assert_eq!(run("{ return 0!=1; }")?, Some(1));
    assert_eq!(run("{ return 42!=42; }")?, Some(0));

    assert_eq!(run("{ return 0<1; }")?, Some(1));
    assert_eq!(run("{ return 1<1; }")?, Some(0));
    assert_eq!(run("{ return 2<1; }")?, Some(0));
    assert_eq!(run("{ return 0<=1; }")?, Some(1));
    assert_eq!(run("{ return 1<=1; }")?, Some(1));
    assert_eq!(run("{ return 2<=1; }")?, Some(0));

    assert_eq!(run("{ return 1>0; }")?, Some(1));
    assert_eq!(run("{ return 1>1; }")?, Some(0));
    assert_eq!(run("{ return 1>2; }")?, Some(0));
    assert_eq!(run("{ return 1>=0; }")?, Some(1));
    assert_eq!(run("{ return 1>=1; }")?, Some(1));
    assert_eq!(run("{ return 1>=2; }")?, Some(0));

    assert_eq!(run("{ a=3; return a; }")?, Some(3));
    assert_eq!(run("{ a=3; z=5; return a+z; }")?, Some(8));
    assert_eq!(run("{ a=b=3; return a+b; }")?, Some(6));
    assert_eq!(run("{ foo=3; return foo; }")?, Some(3));
    assert_eq!(run("{ foo123=3; bar=5; return foo123+bar; }")?, Some(8));

    assert_eq!(run("{ return 1; 2; 3; }")?, Some(1));
    assert_eq!(run("{ 1; return 2; 3; }")?, Some(2));
    assert_eq!(run("{ 1; 2; return 3; }")?, Some(3));

    assert_eq!(run("{ {1; {2;} return 3;} }")?, Some(3));
    assert_eq!(run("{ ;;; return 5; }")?, Some(5));

    Ok(())
}

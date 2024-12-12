use anyhow::{anyhow, Result};
use std::{fs, io::Write, process::Command};

fn run(input: &str) -> Result<Option<i32>> {
    let tmp2 = "target/debug/tmp2.o";
    let mut child = Command::new("gcc")
        .args(["-xc", "-c", "-o", tmp2, "-"])
        .stdin(std::process::Stdio::piped())
        .spawn()?;
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(
            br#"int ret3() { return 3; }
            int ret5() { return 5; }
            int add(int x, int y) { return x + y; }
            int sub(int x, int y) { return x - y; }
            int add6(int a, int b, int c, int d, int e, int f) {
                return a+b+c+d+e+f;
            }
            "#,
        )?;
    }
    let output = child.wait_with_output()?;
    if !output.status.success() {
        return Err(anyhow!(
            "failed to compile tmp2.o:\n{}",
            String::from_utf8_lossy(&output.stdout)
        ));
    }

    let child = Command::new("target/debug/rscc")
        .args([input])
        .stdout(std::process::Stdio::piped())
        .spawn()?;
    let output = child.wait_with_output()?;
    if !output.status.success() {
        return Err(anyhow!(
            "failed to compile:\n{}",
            String::from_utf8_lossy(&output.stdout)
        ));
    }

    let asm = String::from_utf8_lossy(&output.stdout);

    let tmpdots = "target/debug/tmp.s";
    let tmp = "target/debug/tmp";

    let mut tmp_file = fs::File::create(tmpdots)?;
    write!(tmp_file, "{}", asm)?;

    #[cfg(not(target_os = "macos"))]
    assert!(Command::new("gcc")
        .args(["-static", "-o", tmp, tmpdots, tmp2])
        .status()?
        .success());
    #[cfg(target_os = "macos")]
    assert!(Command::new("gcc")
        .args(["-o", tmp, tmpdots, tmp2])
        .status()?
        .success());

    let output = Command::new(tmp).output()?;

    Ok(output.status.code())
}

#[test]
fn test_compiler() -> Result<()> {
    assert!(Command::new("cargo").arg("build").status()?.success());

    assert_eq!(run("int main() { return 0; }")?, Some(0));
    assert_eq!(run("int main() { return 42; }")?, Some(42));
    assert_eq!(run("int main() { return 5+20-4; }")?, Some(21));
    assert_eq!(run("int main() { return  12 + 34 - 5 ; }")?, Some(41));
    assert_eq!(run("int main() { return 5+6*7; }")?, Some(47));
    assert_eq!(run("int main() { return 5*(9-6); }")?, Some(15));
    assert_eq!(run("int main() { return (3+5)/2; }")?, Some(4));
    assert_eq!(run("int main() { return -10+20; }")?, Some(10));
    assert_eq!(run("int main() { return - -10; }")?, Some(10));
    assert_eq!(run("int main() { return - - +10; }")?, Some(10));

    assert_eq!(run("int main() { return 0==1; }")?, Some(0));
    assert_eq!(run("int main() { return 42==42; }")?, Some(1));
    assert_eq!(run("int main() { return 0!=1; }")?, Some(1));
    assert_eq!(run("int main() { return 42!=42; }")?, Some(0));

    assert_eq!(run("int main() { return 0<1; }")?, Some(1));
    assert_eq!(run("int main() { return 1<1; }")?, Some(0));
    assert_eq!(run("int main() { return 2<1; }")?, Some(0));
    assert_eq!(run("int main() { return 0<=1; }")?, Some(1));
    assert_eq!(run("int main() { return 1<=1; }")?, Some(1));
    assert_eq!(run("int main() { return 2<=1; }")?, Some(0));

    assert_eq!(run("int main() { return 1>0; }")?, Some(1));
    assert_eq!(run("int main() { return 1>1; }")?, Some(0));
    assert_eq!(run("int main() { return 1>2; }")?, Some(0));
    assert_eq!(run("int main() { return 1>=0; }")?, Some(1));
    assert_eq!(run("int main() { return 1>=1; }")?, Some(1));
    assert_eq!(run("int main() { return 1>=2; }")?, Some(0));

    assert_eq!(run("int main() { int a; a=3; return a; }")?, Some(3));
    assert_eq!(run("int main() { int a=3; return a; }")?, Some(3));
    assert_eq!(
        run("int main() { int a=3; int z=5; return a+z; }")?,
        Some(8)
    );
    assert_eq!(run("int main() { int a=3; return a; }")?, Some(3));
    assert_eq!(
        run("int main() { int a; int b; a=b=3; return a+b; }")?,
        Some(6)
    );
    assert_eq!(run("int main() { int foo=3; return foo; }")?, Some(3));
    assert_eq!(
        run("int main() { int foo123=3; int bar=5; return foo123+bar; }")?,
        Some(8)
    );

    assert_eq!(run("int main() { return 1; 2; 3; }")?, Some(1));
    assert_eq!(run("int main() { 1; return 2; 3; }")?, Some(2));
    assert_eq!(run("int main() { 1; 2; return 3; }")?, Some(3));

    assert_eq!(run("int main() { {1; {2;} return 3;} }")?, Some(3));
    assert_eq!(run("int main() { ;;; return 5; }")?, Some(5));

    assert_eq!(run("int main() { if (0) return 2; return 3; }")?, Some(3));
    assert_eq!(run("int main() { if (1-1) return 2; return 3; }")?, Some(3));
    assert_eq!(run("int main() { if (1) return 2; return 3; }")?, Some(2));
    assert_eq!(run("int main() { if (2-1) return 2; return 3; }")?, Some(2));
    assert_eq!(
        run("int main() { if (0) { 1; 2; return 3; } else { return 4; } }")?,
        Some(4)
    );
    assert_eq!(
        run("int main() { if (1) { 1; 2; return 3; } else { return 4; } }")?,
        Some(3)
    );

    assert_eq!(
        run("int main() { int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; return j; }")?,
        Some(55)
    );
    assert_eq!(
        run("int main() { for (;;) {return 3;} return 5; }")?,
        Some(3)
    );

    assert_eq!(
        run("int main() { int i=0; while(i<10) i=i+1; return i; }")?,
        Some(10)
    );

    assert_eq!(run("int main() { int x=3; return *&x; }")?, Some(3));
    assert_eq!(
        run("int main() { int x=3; int *y=&x; int **z=&y; return **z; }")?,
        Some(3)
    );
    assert_eq!(
        run("int main() { int x=3; int y=5; return *(&x+1); }")?,
        Some(5)
    );
    assert_eq!(
        run("int main() { int x=3; int y=5; return *(&y-1); }")?,
        Some(3)
    );
    assert_eq!(
        run("int main() { int x=3; int y=5; return *(&x-(-1)); }")?,
        Some(5)
    );
    assert_eq!(
        run("int main() { int x=3; int *y=&x; *y=5; return x; }")?,
        Some(5)
    );
    assert_eq!(
        run("int main() { int x=3; int y=5; *(&x+1)=7; return y; }")?,
        Some(7)
    );
    assert_eq!(
        run("int main() { int x=3; int y=5; *(&y-2+1)=7; return x; }")?,
        Some(7)
    );
    assert_eq!(run("int main() { int x=3; return (&x+2)-&x+3; }")?, Some(5));
    assert_eq!(
        run("int main() { int x, y; x=3; y=5; return x+y; }")?,
        Some(8)
    );
    assert_eq!(run("int main() { int x=3, y=5; return x+y; }")?, Some(8));

    assert_eq!(run("int main() { return ret3(); }")?, Some(3));
    assert_eq!(run("int main() { return ret5(); }")?, Some(5));
    assert_eq!(run("int main() { return add(3,5); }")?, Some(8));
    assert_eq!(run("int main() { return sub(5,3); }")?, Some(2));
    assert_eq!(run("int main() { return add6(1,2,3,4,5,6); }")?, Some(21));
    assert_eq!(
        run("int main() { return add6(1,2,add6(3,4,5,6,7,8),9,10,11); }")?,
        Some(66)
    );
    assert_eq!(
        run("int main() { return add6(1,2,add6(3,add6(4,5,6,7,8,9),10,11,12,13),14,15,16); }")?,
        Some(136)
    );

    assert_eq!(
        run("int main() { return ret32(); } int ret32() { return 32; }")?,
        Some(32)
    );
    assert_eq!(
        run("int main() { return add2(3,4); } int add2(int x,int y) { return x+y; }")?,
        Some(7)
    );
    assert_eq!(
        run("int main() { return sub2(4,3); } int sub2(int x,int y) { return x-y; }")?,
        Some(1)
    );
    assert_eq!(
        run("int main() { return fib(9); } int fib(int x) { if (x<=1) return 1; return fib(x-1) + fib(x-2); }")?,
        Some(55)
    );

    assert_eq!(
        run("int main() { int x[2]; int *y=&x; *y=3; return *x; }")?,
        Some(3)
    );
    assert_eq!(
        run("int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *x; }")?,
        Some(3)
    );
    assert_eq!(
        run("int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *(x+1); }")?,
        Some(4)
    );
    assert_eq!(
        run("int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *(x+2); }")?,
        Some(5)
    );

    Ok(())
}

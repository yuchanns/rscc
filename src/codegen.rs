use std::{
    sync::{
        atomic::{AtomicIsize, Ordering::SeqCst},
        OnceLock,
    },
    vec::IntoIter,
};

use anyhow::{anyhow, Result};

use crate::parse::{Node, NodeKind};

static GLOBAL_DEPTH: OnceLock<AtomicIsize> = OnceLock::new();

fn current_depth() -> &'static AtomicIsize {
    GLOBAL_DEPTH.get_or_init(|| AtomicIsize::new(0))
}

fn push() {
    println!("  str x0, [sp, #-16]!");
    current_depth().fetch_add(1, SeqCst);
}

fn pop(arg: &str) {
    println!("  ldr {arg}, [sp], #16");
    current_depth().fetch_sub(1, SeqCst);
}

/// Compute the absolute address of a given node.
/// error if a given node does not reside in memory.
fn gen_addr(node: Option<&Node>) -> Result<()> {
    let Some(node) = node else {
        return Ok(());
    };

    let NodeKind::Var(name) = node.kind else {
        return Err(anyhow!("not an lvalue"));
    };
    let Some(c) = name.chars().next() else {
        return Err(anyhow!("expect a single letter"));
    };
    let offset = (c.to_ascii_lowercase() as u8 - b'a' + 1) * 8;

    println!("  sub x0, x29, #{}", offset);

    Ok(())
}

fn gen_expr(node: Option<&Node>) -> Result<()> {
    let Some(node) = node else {
        return Ok(());
    };
    if let NodeKind::Num(num) = node.kind {
        println!("  mov x0, #{num}");
        return Ok(());
    } else if let NodeKind::Neg = node.kind {
        gen_expr(node.lhs.as_deref())?;
        println!("  neg x0, x0");
        return Ok(());
    } else if let NodeKind::Var(_) = node.kind {
        gen_addr(Some(node))?;
        println!("  ldr x0, [x0]");
        return Ok(());
    } else if let NodeKind::Assign = node.kind {
        gen_addr(node.lhs.as_deref())?;
        push();
        gen_expr(node.rhs.as_deref())?;
        pop("x1");
        println!("  str x0, [x1]");
        return Ok(());
    }
    gen_expr(node.rhs.as_deref())?;
    push();
    gen_expr(node.lhs.as_deref())?;
    pop("x1");
    match &node.kind {
        NodeKind::Add => {
            println!("  add x0, x0, x1");
        }
        NodeKind::Sub => {
            println!("  sub x0, x0, x1");
        }
        NodeKind::Mul => {
            println!("  mul x0, x0, x1");
        }
        NodeKind::Div => {
            println!("  sdiv x0, x0, x1");
        }
        kind @ (NodeKind::Eq | NodeKind::Ne | NodeKind::Lt | NodeKind::Le) => {
            println!("  cmp x0, x1");
            if let NodeKind::Eq = kind {
                println!("  cset w0, eq");
            } else if let NodeKind::Ne = kind {
                println!("  cset w0, ne");
            } else if let NodeKind::Lt = kind {
                println!("  cset w0, lt");
            } else {
                println!("  cset w0, le");
            }
        }
        _ => unreachable!("gen_expr"),
    }
    Ok(())
}

fn gen_stmt(node: &Node) -> Result<()> {
    let NodeKind::ExprStmt = node.kind else {
        return Err(anyhow!("invalid statement"));
    };
    gen_expr(node.lhs.as_deref())
}

pub fn codegen(nodes: &IntoIter<Node>) -> Result<()> {
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

    // Prologue
    println!("  stp x29, x30, [sp, #-16]!");
    println!("  mov x29, sp");
    println!("  sub sp, sp, #208");

    for node in nodes.as_slice() {
        gen_stmt(node)?;
        assert_eq!(current_depth().load(SeqCst), 0);
    }

    println!("  mov sp, x29");
    println!("  ldp x29, x30, [sp], #16");
    println!("  ret");

    Ok(())
}

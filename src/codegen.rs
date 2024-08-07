use std::sync::{
    atomic::{AtomicIsize, Ordering::SeqCst},
    OnceLock,
};

use anyhow::Result;

use crate::{
    new_error_tok,
    parse::{Node, NodeKind},
    Function,
};

static GLOBAL_DEPTH: OnceLock<AtomicIsize> = OnceLock::new();

static GLOBAL_COUNT: OnceLock<AtomicIsize> = OnceLock::new();

fn current_depth() -> &'static AtomicIsize {
    GLOBAL_DEPTH.get_or_init(|| AtomicIsize::new(0))
}

fn current_count() -> &'static AtomicIsize {
    GLOBAL_COUNT.get_or_init(|| AtomicIsize::new(0))
}

fn push() {
    println!("  str x0, [sp, #-16]!");
    current_depth().fetch_add(1, SeqCst);
}

fn pop(arg: &str) {
    println!("  ldr {arg}, [sp], #16");
    current_depth().fetch_sub(1, SeqCst);
}

/// Round up `n` to the nearest multiple of `align`.
/// For instance, align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
fn align_to(n: isize, align: isize) -> isize {
    (n + align - 1) / align * align
}

/// Compute the absolute address of a given node.
/// error if a given node does not reside in memory.
fn gen_addr(node: Option<&Node>) -> Result<()> {
    let Some(node) = node else {
        return Ok(());
    };

    match &node.kind {
        NodeKind::Var(obj) => {
            println!("  sub x0, x29, #{}", obj.as_ref().borrow().offset);
        }
        NodeKind::Deref => {
            gen_expr(node.lhs.as_deref())?;
        }
        _ => return Err(new_error_tok(&node.tok, "not an lvalue")),
    }

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
    } else if let NodeKind::Deref = node.kind {
        gen_expr(node.lhs.as_deref())?;
        println!("  ldr x0, [x0]");
        return Ok(());
    } else if let NodeKind::Addr = node.kind {
        gen_addr(node.lhs.as_deref())?;
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
        _ => return Err(new_error_tok(&node.tok, "invalid expression")),
    }
    Ok(())
}

fn gen_stmt(node: &Node) -> Result<()> {
    match node.kind {
        NodeKind::If => {
            let c = current_count().fetch_add(1, SeqCst);
            gen_expr(node.cond.as_deref())?;
            println!("  cmp x0, #0");
            println!("  b.eq .L.else.{c}");
            let Some(then) = &node.then else {
                return Err(new_error_tok(&node.tok, "expected then clause"));
            };
            gen_stmt(then)?;
            println!("  b .L.end.{c}");
            println!(".L.else.{c}:");
            if let Some(els) = &node.els {
                gen_stmt(els)?;
            };
            println!(".L.end.{c}:");
            Ok(())
        }
        NodeKind::For => {
            let c = current_count().fetch_add(1, SeqCst);
            if let Some(init) = &node.init {
                gen_stmt(init)?;
            }
            println!(".L.begin.{c}:");
            gen_expr(node.cond.as_deref())?;
            println!("  cmp x0, #0");
            println!("  b.eq .L.end.{c}");
            let Some(then) = &node.then else {
                return Err(new_error_tok(&node.tok, "expected then clause"));
            };
            gen_stmt(then)?;
            gen_expr(node.inc.as_deref())?;
            println!("  b .L.begin.{c}");
            println!(".L.end.{c}:");
            Ok(())
        }
        NodeKind::Block => {
            if let Some(nodes) = &node.body {
                for node in nodes.as_slice() {
                    gen_stmt(node)?;
                }
            }
            Ok(())
        }
        NodeKind::Return => {
            gen_expr(node.lhs.as_deref())?;
            println!("  b .L.return");
            Ok(())
        }
        NodeKind::ExprStmt => gen_expr(node.lhs.as_deref()),
        _ => Err(new_error_tok(&node.tok, "invalid statement")),
    }
}

fn assign_lvar_offsets(prog: &mut Function) {
    let mut offset = 0;
    while let Some(var) = prog.locals.pop_back() {
        offset += 8;
        var.as_ref().borrow_mut().offset = offset;
    }
    prog.stack_size = align_to(offset, 16);
}

pub fn codegen(prog: &mut Function) -> Result<()> {
    assign_lvar_offsets(prog);

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
    println!("  sub sp, sp, #{}", prog.stack_size);

    for node in prog.body.as_slice() {
        gen_stmt(node)?;
        assert_eq!(current_depth().load(SeqCst), 0);
    }

    println!(".L.return:");
    println!("  mov sp, x29");
    println!("  ldp x29, x30, [sp], #16");
    println!("  ret");

    Ok(())
}

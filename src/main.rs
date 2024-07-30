use anyhow::{anyhow, Error, Result};
use std::{
    env,
    iter::Peekable,
    mem::take,
    process::ExitCode,
    sync::{
        atomic::{AtomicIsize, Ordering::SeqCst},
        OnceLock,
    },
    vec::IntoIter,
};

static GLOBAL_INPUT: OnceLock<String> = OnceLock::new();

fn current_input() -> &'static str {
    GLOBAL_INPUT.get().expect("GlobalInput not initialized")
}

fn new_error_at(loc: usize, message: &str) -> Error {
    anyhow!("{}\n{}^ {}", current_input(), " ".repeat(loc), message)
}

fn new_error_tok(tok: &Token, message: &str) -> Error {
    new_error_at(tok.pos, message)
}

// Tokenizer

#[derive(Debug, PartialEq)]
enum TokenKind {
    Punct,
    Num(isize),
    Eof,
}

// Token type
#[derive(Debug)]
struct Token {
    kind: TokenKind,
    pos: usize,
    lexeme: &'static str,
}

fn new_token(kind: TokenKind, pos: usize, lexeme: &'static str) -> Token {
    Token { kind, pos, lexeme }
}

fn read_punct(input: &str) -> Option<usize> {
    let mut chars = input.chars();
    match (chars.next(), chars.next()) {
        (Some(a), Some(b)) if "=!<>".contains(a) && b == '=' => Some(2),
        (Some(a), _) if a.is_ascii_punctuation() => Some(1),
        _ => None,
    }
}

// Tokenize `current_input` and nreturns new tokens
fn tokenize() -> Result<Peekable<IntoIter<Token>>> {
    let mut input = current_input();
    let mut tokens = Vec::new();
    let mut pos = 0;

    while !input.is_empty() {
        // Skip whitespace characters
        if input.starts_with(|c: char| c.is_ascii_whitespace()) {
            pos += 1;
            input = &input[1..];
            continue;
        }

        // Numeric literal
        if input.starts_with(|c: char| c.is_ascii_digit()) {
            let end = input
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(input.len());
            let (lexeme, rest) = input.split_at(end);
            let Ok(num) = lexeme.parse::<isize>() else {
                return Err(new_error_at(
                    pos,
                    &format!("{lexeme}: invalid number of inputs"),
                ));
            };
            tokens.push(new_token(TokenKind::Num(num), pos, lexeme));
            pos += end;
            input = rest;
            continue;
        }

        // Punctuator
        if let Some(punct_len) = read_punct(input) {
            tokens.push(new_token(TokenKind::Punct, pos, &input[..punct_len]));
            pos += punct_len;
            input = &input[punct_len..];
            continue;
        }

        return Err(new_error_at(pos, "invalid token"));
    }

    tokens.push(new_token(TokenKind::Eof, pos, ""));

    Ok(tokens.into_iter().peekable())
}

// Parser

#[derive(Debug, PartialEq)]
enum NodeKind {
    Add,        // +
    Sub,        // -
    Mul,        // *
    Div,        // /
    Neg,        // unary -
    Eq,         // ==
    Ne,         // !=
    Lt,         // <
    Le,         // <=
    Num(isize), // Integer
}

// AST node type
#[derive(Debug)]
struct Node {
    kind: NodeKind,
    lhs: Option<Box<Node>>,
    rhs: Option<Box<Node>>,
}

fn new_binary(kind: NodeKind, lhs: Option<Node>, rhs: Option<Node>) -> Node {
    Node {
        kind,
        lhs: lhs.map(Box::new),
        rhs: rhs.map(Box::new),
    }
}

fn new_unary(kind: NodeKind, expr: Option<Node>) -> Node {
    Node {
        kind,
        lhs: expr.map(Box::new),
        rhs: None,
    }
}

fn new_num(val: isize) -> Node {
    Node {
        kind: NodeKind::Num(val),
        lhs: None,
        rhs: None,
    }
}

// expr = equality
fn expr(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
    equality(tokens)
}

// equality = relational ("==" relational | "!=" relational)*
fn equality(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
    let mut node = relational(tokens)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "==") {
            tokens.next();
            node = Some(new_binary(NodeKind::Eq, node, relational(tokens)?));
            continue;
        }
        if equal(tok, "!=") {
            tokens.next();
            node = Some(new_binary(NodeKind::Ne, node, relational(tokens)?));
            continue;
        }
        break;
    }

    Ok(node)
}

// relational = add ("<" add | "<=" add | ">=" add)*
fn relational(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
    let mut node = add(tokens)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "<") {
            tokens.next();
            node = Some(new_binary(NodeKind::Lt, node, add(tokens)?));
            continue;
        }
        if equal(tok, "<=") {
            tokens.next();
            node = Some(new_binary(NodeKind::Le, node, add(tokens)?));
            continue;
        }
        if equal(tok, ">") {
            tokens.next();
            node = Some(new_binary(NodeKind::Lt, add(tokens)?, node));
            continue;
        }
        if equal(tok, ">=") {
            tokens.next();
            node = Some(new_binary(NodeKind::Le, add(tokens)?, node));
            continue;
        }
        break;
    }

    Ok(node)
}

// add = mul ("+" mul | "-" mul)*
fn add(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
    let mut node = mul(tokens)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "+") {
            tokens.next();
            node = Some(new_binary(NodeKind::Add, node, mul(tokens)?));
            continue;
        }
        if equal(tok, "-") {
            tokens.next();
            node = Some(new_binary(NodeKind::Sub, node, mul(tokens)?));
            continue;
        }
        break;
    }

    Ok(node)
}

// mul = unary ("*" unary | "/" unary)*
fn mul(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
    let mut node = unary(tokens)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "*") {
            tokens.next();
            node = Some(new_binary(NodeKind::Mul, node, primary(tokens)?));
            continue;
        }
        if equal(tok, "/") {
            tokens.next();
            node = Some(new_binary(NodeKind::Div, node, primary(tokens)?));
            continue;
        }
        break;
    }

    Ok(node)
}

// unary = ("+" | "-") unary
//       | primary
fn unary(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
    let Some(tok) = tokens.peek() else {
        return Ok(None);
    };
    if equal(tok, "+") {
        tokens.next();
        return unary(tokens);
    }
    if equal(tok, "-") {
        tokens.next();
        return Ok(Some(new_unary(NodeKind::Neg, unary(tokens)?)));
    }
    primary(tokens)
}

// primary = "(" expr ")" | num
fn primary(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
    let Some(tok) = tokens.peek() else {
        return Ok(None);
    };
    if equal(tok, "(") {
        tokens.next();
        let node = expr(tokens)?;
        skip(tokens, ")")?;
        return Ok(node);
    } else if let TokenKind::Num(num) = tok.kind {
        let node = new_num(num);
        tokens.next();
        return Ok(Some(node));
    }

    unreachable!("primary")
}

// Code generator

static GLOBAL_DEPTH: OnceLock<AtomicIsize> = OnceLock::new();

fn current_depth() -> &'static AtomicIsize {
    GLOBAL_DEPTH.get_or_init(|| AtomicIsize::new(0))
}

fn push() {
    println!("  str x0, [sp, #-16]!");
    current_depth().fetch_add(1, SeqCst);
}

fn pop(arg: &str) {
    println!(" ldr {arg}, [sp], #16");
    current_depth().fetch_sub(1, SeqCst);
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

// Consumes the current token if it matches `op`
fn equal(token: &Token, op: &str) -> bool {
    token.lexeme == op
}

// Ensure that the current token is `op`
fn skip(tokens: &mut Peekable<IntoIter<Token>>, op: &str) -> Result<()> {
    let Some(tok) = tokens.peek() else {
        return Err(anyhow!("expected token"));
    };
    if !equal(tok, op) {
        return Err(new_error_tok(tok, &format!("expected '{op}'")));
    }
    tokens.next();
    Ok(())
}

fn main() -> ExitCode {
    if let Err(err) = run() {
        eprintln!("{:?}", err);
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}

fn run() -> Result<()> {
    let mut args: Vec<String> = env::args().collect();

    let [_, arg] = args.as_mut_slice() else {
        let name = env::args().next().unwrap_or_default();
        return Err(anyhow!("{name}: invalid number of arguments"));
    };

    // Tokenize and parse
    GLOBAL_INPUT
        .set(take(arg))
        .expect("failed to initialize GlobalInput");
    let mut tokens = tokenize()?;
    let node = expr(&mut tokens)?;

    if let Some(tok) = tokens.next() {
        if tok.kind != TokenKind::Eof {
            return Err(new_error_tok(&tok, "extra token"));
        }
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

    // Traverse the AST to emit assembly
    gen_expr(node.as_ref())?;

    println!("  ret");

    Ok(())
}

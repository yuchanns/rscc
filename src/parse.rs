use std::{iter::Peekable, vec::IntoIter};

use crate::{
    new_error_tok,
    tokenize::{equal, skip, Token, TokenKind},
};
use anyhow::Result;

#[derive(Debug, PartialEq)]
pub enum NodeKind {
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
    /// unary -
    Neg,
    /// ==
    Eq,
    /// !=
    Ne,
    /// <
    Lt,
    /// <=
    Le,
    /// =
    Assign,
    /// Expression statement
    ExprStmt,
    /// Veriable
    Var(&'static str),
    /// Integer
    Num(isize),
}

/// AST node type
#[derive(Debug)]
pub struct Node {
    /// Node kind
    pub kind: NodeKind,
    /// Left-hand side
    pub lhs: Option<Box<Node>>,
    /// Right-hand side
    pub rhs: Option<Box<Node>>,
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

fn new_var_node(name: &'static str) -> Node {
    Node {
        kind: NodeKind::Var(name),
        lhs: None,
        rhs: None,
    }
}

/// stmt = expr-stmt
fn stmt(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Node> {
    expr_stmt(tokens)
}

/// expr-stmt = expr ";"
fn expr_stmt(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Node> {
    let node = new_unary(NodeKind::ExprStmt, expr(tokens)?);
    skip(tokens, ";")?;
    Ok(node)
}

/// expr = assign
pub fn expr(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
    assign(tokens)
}

/// assign = equality ("=" assign)?
pub fn assign(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
    let mut node = equality(tokens)?;
    if let Some(tok) = tokens.peek() {
        if equal(tok, "=") {
            tokens.next();
            node = Some(new_binary(NodeKind::Assign, node, assign(tokens)?));
        }
    }
    Ok(node)
}

/// equality = relational ("==" relational | "!=" relational)*
pub fn equality(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
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

/// relational = add ("<" add | "<=" add | ">=" add)*
pub fn relational(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
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

/// add = mul ("+" mul | "-" mul)*
pub fn add(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
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

/// mul = unary ("*" unary | "/" unary)*
pub fn mul(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
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

/// unary = ("+" | "-") unary
///       | primary
pub fn unary(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
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

/// primary = "(" expr ")" | ident | num
pub fn primary(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Option<Node>> {
    let Some(tok) = tokens.peek() else {
        return Ok(None);
    };
    if equal(tok, "(") {
        tokens.next();
        let node = expr(tokens)?;
        skip(tokens, ")")?;
        return Ok(node);
    } else if let TokenKind::Ident = tok.kind {
        let node = new_var_node(tok.lexeme);
        tokens.next();
        return Ok(Some(node));
    } else if let TokenKind::Num(num) = tok.kind {
        let node = new_num(num);
        tokens.next();
        return Ok(Some(node));
    }

    Err(new_error_tok(tok, "expected an expression"))
}

/// program = stmt*
pub fn parse(tokens: &mut Peekable<IntoIter<Token>>) -> Result<IntoIter<Node>> {
    let mut nodes = Vec::new();
    while let Some(tok) = tokens.peek() {
        if tok.kind == TokenKind::Eof {
            break;
        }
        nodes.push(stmt(tokens)?);
    }
    Ok(nodes.into_iter())
}

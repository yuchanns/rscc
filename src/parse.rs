use std::{cell::RefCell, collections::VecDeque, iter::Peekable, rc::Rc, vec::IntoIter};

use crate::{
    new_error_tok,
    tokenize::{equal, skip, Token, TokenKind},
};
use anyhow::Result;

#[derive(Debug, PartialEq, Clone)]
pub struct Obj {
    /// Variable name
    pub name: &'static str,
    /// Offset from RBP
    pub offset: isize,
}

#[derive(Debug)]
pub struct Function {
    pub body: IntoIter<Node>,
    pub locals: VecDeque<Rc<RefCell<Obj>>>,
    pub stack_size: isize,
}

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
    /// "return"
    Return,
    /// { ... }
    Block,
    /// Expression statement
    ExprStmt,
    /// Veriable
    Var(Rc<RefCell<Obj>>),
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
    pub body: Option<IntoIter<Node>>,
}

fn find_var(tok: &Token, locals: &VecDeque<Rc<RefCell<Obj>>>) -> Option<Rc<RefCell<Obj>>> {
    let name = tok.lexeme;
    locals
        .iter()
        .find(|obj| obj.as_ref().borrow().name == name)
        .map(Rc::clone)
}

fn new_binary(kind: NodeKind, lhs: Option<Node>, rhs: Option<Node>) -> Node {
    Node {
        kind,
        lhs: lhs.map(Box::new),
        rhs: rhs.map(Box::new),
        body: None,
    }
}

fn new_unary(kind: NodeKind, expr: Option<Node>) -> Node {
    Node {
        kind,
        lhs: expr.map(Box::new),
        rhs: None,
        body: None,
    }
}

fn new_num(val: isize) -> Node {
    Node {
        kind: NodeKind::Num(val),
        lhs: None,
        rhs: None,
        body: None,
    }
}

fn new_var_node(var: Rc<RefCell<Obj>>) -> Node {
    Node {
        kind: NodeKind::Var(var),
        lhs: None,
        rhs: None,
        body: None,
    }
}

fn new_lvar(name: &'static str, locals: &mut VecDeque<Rc<RefCell<Obj>>>) -> Rc<RefCell<Obj>> {
    let obj = Rc::new(RefCell::new(Obj { name, offset: 0 }));
    locals.push_back(Rc::clone(&obj));
    obj
}

/// stmt = "return" expr ";"
///      | "{" compound-stmt
///      | expr-stmt
fn stmt(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Node> {
    if let Some(tok) = tokens.peek() {
        if equal(tok, "return") {
            tokens.next();
            let node = new_unary(NodeKind::Return, expr(tokens, locals)?);
            skip(tokens, ";")?;
            return Ok(node);
        }

        if equal(tok, "{") {
            tokens.next();
            return compound_stmt(tokens, locals);
        }
    }
    expr_stmt(tokens, locals)
}

// compound-stmt = stmt * "}"
fn compound_stmt(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Node> {
    let mut nodes = Vec::new();
    while let Some(tok) = tokens.peek() {
        if equal(tok, "}") {
            break;
        }
        nodes.push(stmt(tokens, locals)?);
    }
    tokens.next();
    Ok(Node {
        kind: NodeKind::Block,
        lhs: None,
        rhs: None,
        body: Some(nodes.into_iter()),
    })
}

/// expr-stmt = expr ";"
fn expr_stmt(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Node> {
    let node = new_unary(NodeKind::ExprStmt, expr(tokens, locals)?);
    skip(tokens, ";")?;
    Ok(node)
}

/// expr = assign
pub fn expr(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Option<Node>> {
    assign(tokens, locals)
}

/// assign = equality ("=" assign)?
pub fn assign(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Option<Node>> {
    let mut node = equality(tokens, locals)?;
    if let Some(tok) = tokens.peek() {
        if equal(tok, "=") {
            tokens.next();
            node = Some(new_binary(NodeKind::Assign, node, assign(tokens, locals)?));
        }
    }
    Ok(node)
}

/// equality = relational ("==" relational | "!=" relational)*
pub fn equality(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Option<Node>> {
    let mut node = relational(tokens, locals)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "==") {
            tokens.next();
            node = Some(new_binary(NodeKind::Eq, node, relational(tokens, locals)?));
            continue;
        }
        if equal(tok, "!=") {
            tokens.next();
            node = Some(new_binary(NodeKind::Ne, node, relational(tokens, locals)?));
            continue;
        }
        break;
    }

    Ok(node)
}

/// relational = add ("<" add | "<=" add | ">=" add)*
pub fn relational(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Option<Node>> {
    let mut node = add(tokens, locals)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "<") {
            tokens.next();
            node = Some(new_binary(NodeKind::Lt, node, add(tokens, locals)?));
            continue;
        }
        if equal(tok, "<=") {
            tokens.next();
            node = Some(new_binary(NodeKind::Le, node, add(tokens, locals)?));
            continue;
        }
        if equal(tok, ">") {
            tokens.next();
            node = Some(new_binary(NodeKind::Lt, add(tokens, locals)?, node));
            continue;
        }
        if equal(tok, ">=") {
            tokens.next();
            node = Some(new_binary(NodeKind::Le, add(tokens, locals)?, node));
            continue;
        }
        break;
    }

    Ok(node)
}

/// add = mul ("+" mul | "-" mul)*
pub fn add(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Option<Node>> {
    let mut node = mul(tokens, locals)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "+") {
            tokens.next();
            node = Some(new_binary(NodeKind::Add, node, mul(tokens, locals)?));
            continue;
        }
        if equal(tok, "-") {
            tokens.next();
            node = Some(new_binary(NodeKind::Sub, node, mul(tokens, locals)?));
            continue;
        }
        break;
    }

    Ok(node)
}

/// mul = unary ("*" unary | "/" unary)*
pub fn mul(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Option<Node>> {
    let mut node = unary(tokens, locals)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "*") {
            tokens.next();
            node = Some(new_binary(NodeKind::Mul, node, primary(tokens, locals)?));
            continue;
        }
        if equal(tok, "/") {
            tokens.next();
            node = Some(new_binary(NodeKind::Div, node, primary(tokens, locals)?));
            continue;
        }
        break;
    }

    Ok(node)
}

/// unary = ("+" | "-") unary
///       | primary
pub fn unary(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Option<Node>> {
    let Some(tok) = tokens.peek() else {
        return Ok(None);
    };
    if equal(tok, "+") {
        tokens.next();
        return unary(tokens, locals);
    }
    if equal(tok, "-") {
        tokens.next();
        return Ok(Some(new_unary(NodeKind::Neg, unary(tokens, locals)?)));
    }
    primary(tokens, locals)
}

/// primary = "(" expr ")" | ident | num
pub fn primary(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Option<Node>> {
    let Some(tok) = tokens.peek() else {
        return Ok(None);
    };
    if equal(tok, "(") {
        tokens.next();
        let node = expr(tokens, locals)?;
        skip(tokens, ")")?;
        return Ok(node);
    } else if let TokenKind::Ident = tok.kind {
        let var = if let Some(var) = find_var(tok, locals) {
            var
        } else {
            new_lvar(tok.lexeme, locals)
        };
        let node = new_var_node(var);
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
pub fn parse(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Function> {
    skip(tokens, "{")?;
    let mut nodes = Vec::new();
    let mut locals = VecDeque::new();
    nodes.push(compound_stmt(tokens, &mut locals)?);

    Ok(Function {
        body: nodes.into_iter(),
        locals,
        stack_size: 0,
    })
}

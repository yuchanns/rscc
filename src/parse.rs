/// This module contains a recursive descent parser for the C language.
///
/// Most functions in this module are named after the symbols they are supposed to read from an
/// input token list. For example, stmt() is responsible for reading a statement from a token
/// list. The function then construct an AST node representing a statement.
///
/// Each function conceptually returns two values, an AST node and remaning part of the input
/// tokens. The remaining tokens are returned to the caller via a mutable reference of the
/// peekable iterator.
///
/// Input tokens are represented by a vector of Token instances. Unlike many recursive descent
/// parses, we don't have the notion of the "input token stream".
/// Most parsing functions don't change the global state of the parser.
/// So it is very easy to lookahead arbitrary number of tokens in this parser.
use std::{cell::RefCell, collections::VecDeque, iter::Peekable, rc::Rc, vec::IntoIter};

use crate::{
    current_input, new_error_at, new_error_tok,
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
    /// unary &
    Addr,
    /// unary *
    Deref,
    /// "return"
    Return,
    /// "if"
    If,
    /// "for" or "while"
    For,
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
    pub tok: Token,
    /// Left-hand side
    pub lhs: Option<Box<Node>>,
    /// Right-hand side
    pub rhs: Option<Box<Node>>,
    pub body: Option<IntoIter<Node>>,
    /// "if" or "for" statement
    pub cond: Option<Box<Node>>,
    pub then: Option<Box<Node>>,
    pub els: Option<Box<Node>>,
    pub init: Option<Box<Node>>,
    pub inc: Option<Box<Node>>,
}

fn find_var(tok: &Token, locals: &VecDeque<Rc<RefCell<Obj>>>) -> Option<Rc<RefCell<Obj>>> {
    let name = tok.lexeme;
    locals
        .iter()
        .find(|obj| obj.as_ref().borrow().name == name)
        .map(Rc::clone)
}

fn new_binary(kind: NodeKind, lhs: Option<Node>, rhs: Option<Node>, tok: Token) -> Node {
    Node {
        kind,
        lhs: lhs.map(Box::new),
        rhs: rhs.map(Box::new),
        body: None,
        cond: None,
        then: None,
        els: None,
        init: None,
        inc: None,
        tok,
    }
}

fn new_unary(kind: NodeKind, expr: Option<Node>, tok: Token) -> Node {
    Node {
        kind,
        lhs: expr.map(Box::new),
        rhs: None,
        body: None,
        cond: None,
        then: None,
        els: None,
        init: None,
        inc: None,
        tok,
    }
}

fn new_num(val: isize, tok: Token) -> Node {
    Node {
        kind: NodeKind::Num(val),
        lhs: None,
        rhs: None,
        body: None,
        cond: None,
        then: None,
        els: None,
        init: None,
        inc: None,
        tok,
    }
}

fn new_var_node(var: Rc<RefCell<Obj>>, tok: Token) -> Node {
    Node {
        kind: NodeKind::Var(var),
        lhs: None,
        rhs: None,
        body: None,
        cond: None,
        then: None,
        els: None,
        init: None,
        inc: None,
        tok,
    }
}

fn new_lvar(name: &'static str, locals: &mut VecDeque<Rc<RefCell<Obj>>>) -> Rc<RefCell<Obj>> {
    let obj = Rc::new(RefCell::new(Obj { name, offset: 0 }));
    locals.push_back(Rc::clone(&obj));
    obj
}

/// stmt = "return" expr ";"
///      | "if" "(" expr ")" stmt ("else" stmt)?
///      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
///      | "while" "(" expr ")" stmt
///      | "{" compound-stmt
///      | expr-stmt
fn stmt(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Node> {
    if let Some(tok) = tokens.peek() {
        if equal(tok, "return") {
            let tok = tokens.next().unwrap();
            let node = new_unary(NodeKind::Return, expr(tokens, locals)?, tok);
            skip(tokens, ";")?;
            return Ok(node);
        }

        if equal(tok, "if") {
            let tok = tokens.next().unwrap();
            skip(tokens, "(")?;
            let cond = expr(tokens, locals)?;
            skip(tokens, ")")?;
            let then = Some(stmt(tokens, locals)?);
            let els = if let Some(tok) = tokens.peek() {
                if equal(tok, "else") {
                    tokens.next();
                    Some(stmt(tokens, locals)?)
                } else {
                    None
                }
            } else {
                None
            };
            return Ok(Node {
                kind: NodeKind::If,
                lhs: None,
                rhs: None,
                body: None,
                cond: cond.map(Box::new),
                then: then.map(Box::new),
                els: els.map(Box::new),
                init: None,
                inc: None,
                tok,
            });
        }

        if equal(tok, "for") {
            let tok = tokens.next().unwrap();
            skip(tokens, "(")?;
            let init = expr_stmt(tokens, locals)?;
            let Some(maby_cond) = tokens.peek() else {
                return Err(new_error_at(current_input().len(), "expected token"));
            };
            let cond = if equal(maby_cond, ";") {
                None
            } else {
                expr(tokens, locals)?
            };
            skip(tokens, ";")?;
            let Some(maby_inc) = tokens.peek() else {
                return Err(new_error_at(current_input().len(), "expected token"));
            };
            let inc = if equal(maby_inc, ")") {
                None
            } else {
                expr(tokens, locals)?
            };
            skip(tokens, ")")?;
            let then = stmt(tokens, locals)?;

            return Ok(Node {
                kind: NodeKind::For,
                lhs: None,
                rhs: None,
                body: None,
                cond: cond.map(Box::new),
                then: Some(Box::new(then)),
                els: None,
                init: Some(Box::new(init)),
                inc: inc.map(Box::new),
                tok,
            });
        }

        if equal(tok, "while") {
            let tok = tokens.next().unwrap();
            skip(tokens, "(")?;
            let cond = expr(tokens, locals)?;
            skip(tokens, ")")?;
            let then = stmt(tokens, locals)?;

            return Ok(Node {
                kind: NodeKind::For,
                lhs: None,
                rhs: None,
                body: None,
                cond: cond.map(Box::new),
                then: Some(Box::new(then)),
                els: None,
                init: None,
                inc: None,
                tok,
            });
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
    let tok = tokens.next().unwrap();
    Ok(Node {
        kind: NodeKind::Block,
        lhs: None,
        rhs: None,
        body: Some(nodes.into_iter()),
        cond: None,
        then: None,
        els: None,
        init: None,
        inc: None,
        tok,
    })
}

/// expr-stmt = expr? ";"
fn expr_stmt(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Node> {
    if let Some(tok) = tokens.peek() {
        if equal(tok, ";") {
            let tok = tokens.next().unwrap();
            return Ok(Node {
                kind: NodeKind::Block,
                lhs: None,
                rhs: None,
                body: None,
                cond: None,
                then: None,
                els: None,
                init: None,
                inc: None,
                tok,
            });
        }
    }
    let tok = tokens.peek().unwrap().clone();
    let node = new_unary(NodeKind::ExprStmt, expr(tokens, locals)?, tok);
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
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Assign,
                node,
                assign(tokens, locals)?,
                tok,
            ));
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
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Eq,
                node,
                relational(tokens, locals)?,
                tok,
            ));
            continue;
        }
        if equal(tok, "!=") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Ne,
                node,
                relational(tokens, locals)?,
                tok,
            ));
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
            let tok = tokens.next().unwrap();
            node = Some(new_binary(NodeKind::Lt, node, add(tokens, locals)?, tok));
            continue;
        }
        if equal(tok, "<=") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(NodeKind::Le, node, add(tokens, locals)?, tok));
            continue;
        }
        if equal(tok, ">") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(NodeKind::Lt, add(tokens, locals)?, node, tok));
            continue;
        }
        if equal(tok, ">=") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(NodeKind::Le, add(tokens, locals)?, node, tok));
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
            let tok = tokens.next().unwrap();
            node = Some(new_binary(NodeKind::Add, node, mul(tokens, locals)?, tok));
            continue;
        }
        if equal(tok, "-") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(NodeKind::Sub, node, mul(tokens, locals)?, tok));
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
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Mul,
                node,
                primary(tokens, locals)?,
                tok,
            ));
            continue;
        }
        if equal(tok, "/") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Div,
                node,
                primary(tokens, locals)?,
                tok,
            ));
            continue;
        }
        break;
    }

    Ok(node)
}

/// unary = ("+" | "-" | "*" | "&") unary
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
        let tok = tokens.next().unwrap();
        return Ok(Some(new_unary(NodeKind::Neg, unary(tokens, locals)?, tok)));
    }
    if equal(tok, "&") {
        let tok = tokens.next().unwrap();
        return Ok(Some(new_unary(NodeKind::Addr, unary(tokens, locals)?, tok)));
    }
    if equal(tok, "*") {
        let tok = tokens.next().unwrap();
        return Ok(Some(new_unary(
            NodeKind::Deref,
            unary(tokens, locals)?,
            tok,
        )));
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
        let tok = tokens.next().unwrap();
        let node = new_var_node(var, tok);
        return Ok(Some(node));
    } else if let TokenKind::Num(num) = tok.kind {
        let tok = tokens.next().unwrap();
        let node = new_num(num, tok);
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

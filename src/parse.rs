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
use std::{cell::RefCell, collections::VecDeque, iter::Peekable, rc::Rc, sync::Arc, vec::IntoIter};

use crate::{
    add_type, consume, is_integer, new_error_et, new_error_tok, pointer_to,
    tokenize::{equal, skip, Token, TokenKind},
    Type, TY_INT,
};
use anyhow::Result;

#[derive(Debug, PartialEq, Clone)]
pub struct Obj {
    /// Variable name
    pub name: &'static str,
    // Type
    pub ty: Option<Arc<Type>>,
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
    pub ty: Option<Arc<Type>>,
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

fn new_node(kind: NodeKind, tok: Token) -> Node {
    Node {
        kind,
        lhs: None,
        rhs: None,
        body: None,
        cond: None,
        then: None,
        els: None,
        init: None,
        inc: None,
        tok,
        ty: None,
    }
}

fn find_var(tok: &Token, locals: &VecDeque<Rc<RefCell<Obj>>>) -> Option<Rc<RefCell<Obj>>> {
    let name = tok.lexeme;
    locals
        .iter()
        .find(|obj| obj.as_ref().borrow().name == name)
        .map(Rc::clone)
}

fn new_binary(kind: NodeKind, lhs: Option<Node>, rhs: Option<Node>, tok: Token) -> Node {
    let mut node = new_node(kind, tok);
    node.lhs = lhs.map(Box::new);
    node.rhs = rhs.map(Box::new);
    node
}

fn new_unary(kind: NodeKind, expr: Option<Node>, tok: Token) -> Node {
    let mut node = new_node(kind, tok);
    node.lhs = expr.map(Box::new);
    node
}

fn new_num(val: isize, tok: Token) -> Node {
    new_node(NodeKind::Num(val), tok)
}

fn new_var_node(var: Rc<RefCell<Obj>>, tok: Token) -> Node {
    new_node(NodeKind::Var(var), tok)
}

fn new_lvar(
    name: &'static str,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
    ty: Arc<Type>,
) -> Rc<RefCell<Obj>> {
    let obj = Rc::new(RefCell::new(Obj {
        name,
        offset: 0,
        ty: Some(ty.clone()),
    }));
    locals.push_back(Rc::clone(&obj));
    obj
}

fn get_ident(tok: &Token) -> Result<&'static str> {
    let TokenKind::Ident = tok.kind else {
        return Err(new_error_tok(tok, "expected an identifier"));
    };
    Ok(tok.lexeme)
}

/// declspec = "int"
fn declspec(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Arc<Type>> {
    skip(tokens, "int")?;
    Ok(TY_INT.clone())
}

/// declarator = "*"* ident
fn declarator(tokens: &mut Peekable<IntoIter<Token>>, ty: &Arc<Type>) -> Result<Arc<Type>> {
    let mut ty = (**ty).clone();
    while consume(tokens, "*") {
        ty = pointer_to(Some(&Arc::new(ty)));
    }

    let Some(tok) = tokens.next() else {
        return Err(new_error_et());
    };
    if tok.kind != TokenKind::Ident {
        return Err(new_error_tok(&tok, "expected a variable name"));
    };

    ty.name = Some(Box::new(tok));
    Ok(Arc::new(ty))
}

/// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)* ";"
pub fn declaration(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Node> {
    let basety = declspec(tokens)?;

    let mut nodes = Vec::new();

    let mut i = 0;

    while let Some(tok) = tokens.peek() {
        if equal(tok, ";") {
            break;
        }
        if i > 0 {
            skip(tokens, ",")?;
        }
        i += 1;

        let ty = declarator(tokens, &basety)?;
        let Some(ntok) = &ty.clone().name else {
            return Err(new_error_et());
        };
        let var = new_lvar(get_ident(ntok)?, locals, ty);

        let Some(tok) = tokens.peek() else {
            return Err(new_error_et());
        };

        if !equal(tok, "=") {
            continue;
        }

        let lhs = Some(new_var_node(var, (**ntok).clone()));
        tokens.next();
        let rhs = assign(tokens, locals)?;
        let Some(tok) = tokens.peek() else {
            return Err(new_error_et());
        };
        let node = new_binary(NodeKind::Assign, lhs, rhs, tok.clone());
        nodes.push(new_unary(NodeKind::ExprStmt, Some(node), tok.clone()));
    }

    let Some(tok) = tokens.next() else {
        return Err(new_error_et());
    };
    let mut node = new_node(NodeKind::Block, tok);
    node.body = Some(nodes.into_iter());
    Ok(node)
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
            let mut node = new_node(NodeKind::If, tok);
            node.cond = cond.map(Box::new);
            node.then = then.map(Box::new);
            node.els = els.map(Box::new);
            return Ok(node);
        }

        if equal(tok, "for") {
            let tok = tokens.next().unwrap();
            skip(tokens, "(")?;
            let init = expr_stmt(tokens, locals)?;
            let Some(maby_cond) = tokens.peek() else {
                return Err(new_error_et());
            };
            let cond = if equal(maby_cond, ";") {
                None
            } else {
                expr(tokens, locals)?
            };
            skip(tokens, ";")?;
            let Some(maby_inc) = tokens.peek() else {
                return Err(new_error_et());
            };
            let inc = if equal(maby_inc, ")") {
                None
            } else {
                expr(tokens, locals)?
            };
            skip(tokens, ")")?;
            let then = stmt(tokens, locals)?;

            let mut node = new_node(NodeKind::For, tok);
            node.cond = cond.map(Box::new);
            node.then = Some(Box::new(then));
            node.init = Some(Box::new(init));
            node.inc = inc.map(Box::new);
            return Ok(node);
        }

        if equal(tok, "while") {
            let tok = tokens.next().unwrap();
            skip(tokens, "(")?;
            let cond = expr(tokens, locals)?;
            skip(tokens, ")")?;
            let then = stmt(tokens, locals)?;

            let mut node = new_node(NodeKind::For, tok);
            node.cond = cond.map(Box::new);
            node.then = Some(Box::new(then));
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
        if equal(tok, "int") {
            nodes.push(declaration(tokens, locals)?);
        } else {
            let mut node = stmt(tokens, locals)?;
            add_type(&mut Some(&mut node))?;
            nodes.push(node);
        }
    }
    let tok = tokens.next().unwrap();

    let mut node = new_node(NodeKind::Block, tok);
    node.body = Some(nodes.into_iter());
    Ok(node)
}

/// expr-stmt = expr? ";"
fn expr_stmt(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut VecDeque<Rc<RefCell<Obj>>>,
) -> Result<Node> {
    if let Some(tok) = tokens.peek() {
        if equal(tok, ";") {
            let tok = tokens.next().unwrap();

            return Ok(new_node(NodeKind::Block, tok));
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

fn is_integer_type(node: &Option<Node>) -> bool {
    node.as_ref()
        .is_some_and(|node| is_integer(node.ty.as_ref()))
}

fn has_base_type(node: &Option<Node>) -> bool {
    node.as_ref()
        .is_some_and(|node| node.ty.as_ref().is_some_and(|ty| ty.base.is_some()))
}

/// In C, `+` operator is overloaded to perform the pointer arithmetic.
/// If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
/// so that p+n points to the location n elements (not bytes) ahead of p.
/// In other words, we need to scale an integer value before adding to a pointer value.
/// This function takes care of the scaling.
pub fn new_add(mut lhs: Option<Node>, mut rhs: Option<Node>, tok: Token) -> Result<Node> {
    add_type(&mut lhs.as_mut())?;
    add_type(&mut rhs.as_mut())?;

    if is_integer_type(&lhs) && is_integer_type(&rhs) {
        return Ok(new_binary(NodeKind::Add, lhs, rhs, tok));
    }

    if has_base_type(&lhs) && has_base_type(&rhs) {
        return Err(new_error_tok(&tok, "invalid operands"));
    }

    // Canonicalize `num + ptr` to `ptr + num`
    if !has_base_type(&lhs) && has_base_type(&rhs) {
        std::mem::swap(&mut lhs, &mut rhs);
    }

    // ptr + num
    rhs = Some(new_binary(
        NodeKind::Mul,
        rhs,
        Some(new_num(8, tok.clone())),
        tok.clone(),
    ));
    Ok(new_binary(NodeKind::Add, lhs, rhs, tok))
}

/// Like `+`, `-` is overloaded for the pointer type.
pub fn new_sub(mut lhs: Option<Node>, mut rhs: Option<Node>, tok: Token) -> Result<Node> {
    add_type(&mut lhs.as_mut())?;
    add_type(&mut rhs.as_mut())?;

    // num - num
    if is_integer_type(&lhs) && is_integer_type(&rhs) {
        return Ok(new_binary(NodeKind::Sub, lhs, rhs, tok));
    }

    // ptr - num
    if has_base_type(&lhs) && is_integer_type(&rhs) {
        let mut node = new_binary(
            NodeKind::Mul,
            rhs,
            Some(new_num(8, tok.clone())),
            tok.clone(),
        );
        add_type(&mut Some(&mut node))?;
        rhs = Some(node);
        let ty = match &lhs {
            Some(lhs_value) => lhs_value.ty.clone(),
            None => None,
        };
        let mut node = new_binary(NodeKind::Sub, lhs, rhs, tok);
        node.ty = ty;
        return Ok(node);
    }

    // ptr - ptr, which returns how many elements are between the two.
    if has_base_type(&lhs) && has_base_type(&rhs) {
        let mut node = new_binary(NodeKind::Sub, lhs, rhs, tok.clone());
        node.ty = Some(TY_INT.clone());
        return Ok(new_binary(
            NodeKind::Div,
            Some(node),
            Some(new_num(8, tok.clone())),
            tok,
        ));
    }
    Err(new_error_tok(&tok, "invalid operands"))
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
            node = Some(new_add(node, mul(tokens, locals)?, tok)?);
            continue;
        }
        if equal(tok, "-") {
            let tok = tokens.next().unwrap();
            node = Some(new_sub(node, mul(tokens, locals)?, tok)?);
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
            return Err(new_error_tok(tok, "undefined variable"));
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

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
use std::{cell::RefCell, iter::Peekable, rc::Rc, sync::Arc, vec::IntoIter};

use crate::{
    add_type, array_of, consume, func_type, is_integer, new_error_et, new_error_tok, pointer_to,
    tokenize::{equal, skip, Token, TokenKind},
    Type, TypeKind, TY_INT,
};
use anyhow::Result;

/// Veriable or function
#[derive(Debug, Default)]
pub struct Obj {
    /// Variable name
    pub name: &'static str,
    // Type
    pub ty: Option<Arc<Type>>,
    // local or global/function
    pub is_local: bool,
    /// Local variable
    pub offset: isize,
    /// Global variable or function
    pub is_function: bool,

    /// Function
    pub params: Option<Vec<Rc<RefCell<Obj>>>>,
    pub body: Option<Node>,
    pub locals: Vec<Rc<RefCell<Obj>>>,
    pub stack_size: isize,
}

#[derive(Debug, Default)]
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
    #[default]
    Return,
    /// "if"
    If,
    /// "for" or "while"
    For,
    /// { ... }
    Block,
    /// Function call
    FunCall(&'static str),
    /// Expression statement
    ExprStmt,
    /// Veriable
    Var(Rc<RefCell<Obj>>),
    /// Integer
    Num(isize),
}

/// AST node type
#[derive(Debug, Default)]
pub struct Node {
    /// Node kind
    pub kind: NodeKind,
    pub tok: Token,
    pub ty: Option<Arc<Type>>,
    /// Left-hand side
    pub lhs: Option<Box<Node>>,
    /// Right-hand side
    pub rhs: Option<Box<Node>>,
    /// "if" or "for" statement
    pub cond: Option<Box<Node>>,
    pub then: Option<Box<Node>>,
    pub els: Option<Box<Node>>,
    pub init: Option<Box<Node>>,
    pub inc: Option<Box<Node>>,

    /// Block
    pub body: Option<IntoIter<Node>>,

    pub args: Option<IntoIter<Node>>,
}

fn new_node(kind: NodeKind, tok: Token) -> Node {
    Node {
        kind,
        tok,
        ..Default::default()
    }
}

fn find_var(
    tok: &Token,
    locals: &[Rc<RefCell<Obj>>],
    globals: &[Rc<RefCell<Obj>>],
) -> Option<Rc<RefCell<Obj>>> {
    let name = tok.lexeme;
    locals
        .iter()
        .chain(globals.iter())
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

fn new_var(name: &'static str, ty: Arc<Type>) -> Rc<RefCell<Obj>> {
    Rc::new(RefCell::new(Obj {
        name,
        ty: Some(ty),
        ..Default::default()
    }))
}

fn new_lvar(
    name: &'static str,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    ty: Arc<Type>,
) -> Rc<RefCell<Obj>> {
    let obj = new_var(name, ty);
    {
        let mut var = obj.borrow_mut();
        var.is_local = true;
        locals.push(Rc::clone(&obj));
    }
    obj
}

fn new_gvar(
    name: &'static str,
    globals: &mut Vec<Rc<RefCell<Obj>>>,
    ty: Arc<Type>,
) -> Rc<RefCell<Obj>> {
    let obj = new_var(name, ty);
    globals.push(Rc::clone(&obj));
    obj
}

fn get_ident(tok: &Token) -> Result<&'static str> {
    let TokenKind::Ident = tok.kind else {
        return Err(new_error_tok(tok, "expected an identifier"));
    };
    Ok(tok.lexeme)
}

fn get_number(tok: &Token) -> Result<isize> {
    let TokenKind::Num(num) = tok.kind else {
        return Err(new_error_tok(tok, "expected a number"));
    };
    Ok(num)
}

/// declspec = "int"
fn declspec(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Arc<Type>> {
    skip(tokens, "int")?;
    Ok(TY_INT.clone())
}

fn func_params(tokens: &mut Peekable<IntoIter<Token>>, ty: Arc<Type>) -> Result<Type> {
    let mut params = Vec::new();
    while let Some(tok) = tokens.peek() {
        if equal(tok, ")") {
            skip(tokens, ")")?;
            break;
        }
        if !params.is_empty() {
            skip(tokens, ",")?;
        }
        let basety = declspec(tokens)?;
        let ty = declarator(tokens, &basety)?;
        params.push(ty);
    }
    let mut ty = func_type(ty);
    ty.params = Some(params);
    Ok(ty)
}

// type-suffix = "(" func-params
// | "[" num "]" type-suffix
// | ε
fn type_suffix(tokens: &mut Peekable<IntoIter<Token>>, ty: Type) -> Result<Type> {
    if let Some(tok) = tokens.peek() {
        if equal(tok, "(") {
            tokens.next();
            return func_params(tokens, Arc::new(ty));
        }
        if equal(tok, "[") {
            tokens.next();
            let Some(tok) = tokens.next() else {
                return Err(new_error_et());
            };
            let num = get_number(&tok)?;
            skip(tokens, "]")?;
            let ty = type_suffix(tokens, ty)?;
            return Ok(array_of(Arc::new(ty), num as usize));
        }
    }
    Ok(ty)
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

    let mut ty = type_suffix(tokens, ty)?;

    ty.name = Some(Box::new(tok));
    Ok(Arc::new(ty))
}

/// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)* ";"
pub fn declaration(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
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
        let rhs = assign(tokens, locals, globals)?;
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
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Node> {
    if let Some(tok) = tokens.peek() {
        if equal(tok, "return") {
            let tok = tokens.next().unwrap();
            let node = new_unary(NodeKind::Return, expr(tokens, locals, globals)?, tok);
            skip(tokens, ";")?;
            return Ok(node);
        }

        if equal(tok, "if") {
            let tok = tokens.next().unwrap();
            skip(tokens, "(")?;
            let cond = expr(tokens, locals, globals)?;
            skip(tokens, ")")?;
            let then = Some(stmt(tokens, locals, globals)?);
            let els = if let Some(tok) = tokens.peek() {
                if equal(tok, "else") {
                    tokens.next();
                    Some(stmt(tokens, locals, globals)?)
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
            let init = expr_stmt(tokens, locals, globals)?;
            let Some(maby_cond) = tokens.peek() else {
                return Err(new_error_et());
            };
            let cond = if equal(maby_cond, ";") {
                None
            } else {
                expr(tokens, locals, globals)?
            };
            skip(tokens, ";")?;
            let Some(maby_inc) = tokens.peek() else {
                return Err(new_error_et());
            };
            let inc = if equal(maby_inc, ")") {
                None
            } else {
                expr(tokens, locals, globals)?
            };
            skip(tokens, ")")?;
            let then = stmt(tokens, locals, globals)?;

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
            let cond = expr(tokens, locals, globals)?;
            skip(tokens, ")")?;
            let then = stmt(tokens, locals, globals)?;

            let mut node = new_node(NodeKind::For, tok);
            node.cond = cond.map(Box::new);
            node.then = Some(Box::new(then));
            return Ok(node);
        }

        if equal(tok, "{") {
            tokens.next();
            return compound_stmt(tokens, locals, globals);
        }
    }
    expr_stmt(tokens, locals, globals)
}

// compound-stmt = stmt * "}"
fn compound_stmt(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Node> {
    let mut nodes = Vec::new();
    while let Some(tok) = tokens.peek() {
        if equal(tok, "}") {
            break;
        }
        if equal(tok, "int") {
            nodes.push(declaration(tokens, locals, globals)?);
        } else {
            let mut node = stmt(tokens, locals, globals)?;
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
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Node> {
    if let Some(tok) = tokens.peek() {
        if equal(tok, ";") {
            let tok = tokens.next().unwrap();

            return Ok(new_node(NodeKind::Block, tok));
        }
    }
    let tok = tokens.peek().unwrap().clone();
    let node = new_unary(NodeKind::ExprStmt, expr(tokens, locals, globals)?, tok);
    skip(tokens, ";")?;
    Ok(node)
}

/// expr = assign
pub fn expr(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Option<Node>> {
    assign(tokens, locals, globals)
}

/// assign = equality ("=" assign)?
pub fn assign(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Option<Node>> {
    let mut node = equality(tokens, locals, globals)?;
    if let Some(tok) = tokens.peek() {
        if equal(tok, "=") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Assign,
                node,
                assign(tokens, locals, globals)?,
                tok,
            ));
        }
    }
    Ok(node)
}

/// equality = relational ("==" relational | "!=" relational)*
pub fn equality(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Option<Node>> {
    let mut node = relational(tokens, locals, globals)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "==") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Eq,
                node,
                relational(tokens, locals, globals)?,
                tok,
            ));
            continue;
        }
        if equal(tok, "!=") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Ne,
                node,
                relational(tokens, locals, globals)?,
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
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Option<Node>> {
    let mut node = add(tokens, locals, globals)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "<") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Lt,
                node,
                add(tokens, locals, globals)?,
                tok,
            ));
            continue;
        }
        if equal(tok, "<=") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Le,
                node,
                add(tokens, locals, globals)?,
                tok,
            ));
            continue;
        }
        if equal(tok, ">") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Lt,
                add(tokens, locals, globals)?,
                node,
                tok,
            ));
            continue;
        }
        if equal(tok, ">=") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Le,
                add(tokens, locals, globals)?,
                node,
                tok,
            ));
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

fn get_base_size(node: &Option<Node>) -> usize {
    node.as_ref()
        .and_then(|n| n.ty.as_ref())
        .and_then(|t| t.base.as_ref())
        .map(|b| b.size)
        .unwrap_or(0)
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
        Some(new_num(get_base_size(&lhs) as isize, tok.clone())),
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
            Some(new_num(get_base_size(&lhs) as isize, tok.clone())),
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
        let size = get_base_size(&lhs) as isize;
        let mut node = new_binary(NodeKind::Sub, lhs, rhs, tok.clone());
        node.ty = Some(TY_INT.clone());
        return Ok(new_binary(
            NodeKind::Div,
            Some(node),
            Some(new_num(size, tok.clone())),
            tok,
        ));
    }
    Err(new_error_tok(&tok, "invalid operands"))
}

/// add = mul ("+" mul | "-" mul)*
pub fn add(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Option<Node>> {
    let mut node = mul(tokens, locals, globals)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "+") {
            let tok = tokens.next().unwrap();
            node = Some(new_add(node, mul(tokens, locals, globals)?, tok)?);
            continue;
        }
        if equal(tok, "-") {
            let tok = tokens.next().unwrap();
            node = Some(new_sub(node, mul(tokens, locals, globals)?, tok)?);
            continue;
        }
        break;
    }

    Ok(node)
}

/// mul = unary ("*" unary | "/" unary)*
pub fn mul(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Option<Node>> {
    let mut node = unary(tokens, locals, globals)?;
    while let Some(tok) = tokens.peek() {
        if equal(tok, "*") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Mul,
                node,
                primary(tokens, locals, globals)?,
                tok,
            ));
            continue;
        }
        if equal(tok, "/") {
            let tok = tokens.next().unwrap();
            node = Some(new_binary(
                NodeKind::Div,
                node,
                primary(tokens, locals, globals)?,
                tok,
            ));
            continue;
        }
        break;
    }

    Ok(node)
}

/// unary = ("+" | "-" | "*" | "&") unary
///       | postfix
pub fn unary(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Option<Node>> {
    let Some(tok) = tokens.peek() else {
        return Ok(None);
    };
    if equal(tok, "+") {
        tokens.next();
        return unary(tokens, locals, globals);
    }
    if equal(tok, "-") {
        let tok = tokens.next().unwrap();
        return Ok(Some(new_unary(
            NodeKind::Neg,
            unary(tokens, locals, globals)?,
            tok,
        )));
    }
    if equal(tok, "&") {
        let tok = tokens.next().unwrap();
        return Ok(Some(new_unary(
            NodeKind::Addr,
            unary(tokens, locals, globals)?,
            tok,
        )));
    }
    if equal(tok, "*") {
        let tok = tokens.next().unwrap();
        return Ok(Some(new_unary(
            NodeKind::Deref,
            unary(tokens, locals, globals)?,
            tok,
        )));
    }
    postfix(tokens, locals, globals)
}

/// postfix = primary ("[" expr "]")*
pub fn postfix(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Option<Node>> {
    let mut node = primary(tokens, locals, globals)?;
    while let Some(tok) = tokens.peek() {
        if !equal(tok, "[") {
            break;
        }
        let start = tokens.next().unwrap();
        let idx = expr(tokens, locals, globals)?;
        skip(tokens, "]")?;
        node = Some(new_unary(
            NodeKind::Deref,
            Some(new_add(node, idx, start.clone())?),
            start,
        ));
    }
    Ok(node)
}

/// funcall = ident "(" (assign ("," assign)*)? ")"
pub fn funcall(
    start: Token,
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Node> {
    tokens.next();
    let mut args = Vec::new();

    while let Some(tok) = tokens.peek() {
        if equal(tok, ")") {
            skip(tokens, ")")?;
            break;
        }
        if !args.is_empty() {
            skip(tokens, ",")?;
        }
        if let Some(arg) = assign(tokens, locals, globals)? {
            args.push(arg);
        }
    }
    let mut node = new_node(NodeKind::FunCall(start.lexeme), start);
    node.args = Some(args.into_iter());
    Ok(node)
}

/// primary = "(" expr ")" | "sizeof" unary | ident func-args? | num
pub fn primary(
    tokens: &mut Peekable<IntoIter<Token>>,
    locals: &mut Vec<Rc<RefCell<Obj>>>,
    globals: &[Rc<RefCell<Obj>>],
) -> Result<Option<Node>> {
    let Some(tok) = tokens.peek() else {
        return Ok(None);
    };
    if equal(tok, "(") {
        tokens.next();
        let node = expr(tokens, locals, globals)?;
        skip(tokens, ")")?;
        return Ok(node);
    } else if equal(tok, "sizeof") {
        let tok = tokens.next().unwrap();
        let mut node = unary(tokens, locals, globals)?;
        add_type(&mut node.as_mut())?;
        return Ok(Some(new_num(node.unwrap().ty.unwrap().size as isize, tok)));
    } else if let TokenKind::Ident = tok.kind {
        let tok = tokens.next().unwrap();
        // Function call
        if let Some(next) = tokens.peek() {
            if equal(next, "(") {
                return Ok(Some(funcall(tok, tokens, locals, globals)?));
            }
        }
        // Variable
        let Some(var) = find_var(&tok, locals, globals) else {
            return Err(new_error_tok(&tok, "undefined variable"));
        };
        let node = new_var_node(var, tok);
        return Ok(Some(node));
    } else if let TokenKind::Num(num) = tok.kind {
        let tok = tokens.next().unwrap();
        let node = new_num(num, tok);
        return Ok(Some(node));
    }

    Err(new_error_tok(tok, "expected an expression"))
}

fn create_param_lvars(params: &[Arc<Type>], locals: &mut Vec<Rc<RefCell<Obj>>>) -> Result<()> {
    for param in params.iter() {
        let Some(ntok) = &param.name else {
            continue;
        };

        new_lvar(get_ident(ntok)?, locals, param.clone());
    }
    Ok(())
}

pub fn function(
    tokens: &mut Peekable<IntoIter<Token>>,
    globals: &mut Vec<Rc<RefCell<Obj>>>,
    basety: &Arc<Type>,
) -> Result<()> {
    let ty = declarator(tokens, basety)?;
    skip(tokens, "{")?;
    let Some(name) = &ty.name else {
        return Err(new_error_et());
    };
    let mut locals = Vec::new();
    if let Some(params) = &ty.params {
        create_param_lvars(params, &mut locals)?;
    }
    let params_len = locals.len();
    let body = compound_stmt(tokens, &mut locals, globals)?;
    let f = new_gvar(get_ident(name)?, globals, ty);
    let mut f = f.borrow_mut();
    f.is_function = true;
    f.params = Some(locals[0..params_len].to_vec());
    f.body = Some(body);
    f.locals = locals;

    Ok(())
}

pub fn global_variable(
    tokens: &mut Peekable<IntoIter<Token>>,
    globals: &mut Vec<Rc<RefCell<Obj>>>,
    basety: &Arc<Type>,
) -> Result<()> {
    let mut first = true;
    while !consume(tokens, ";") {
        if !first {
            skip(tokens, ",")?;
        }
        first = false;
        let ty = declarator(tokens, basety)?;
        let Some(ntok) = &ty.name else {
            return Err(new_error_et());
        };
        let name = get_ident(ntok)?;
        new_gvar(name, globals, ty);
    }
    Ok(())
}

/// Lookahead tokens and returns true if a given token is a start
/// of a function definition or declaration.
pub fn is_function(tokens: &mut Peekable<IntoIter<Token>>) -> Result<bool> {
    let Some(tok) = tokens.peek() else {
        return Ok(false);
    };
    if equal(tok, ";") {
        return Ok(false);
    }
    let dummy = Arc::new(Type::default());
    let ty = declarator(tokens, &dummy)?;
    Ok(ty.kind == TypeKind::Func)
}

/// program = (function-definition | global-variable)*
pub fn parse(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Vec<Rc<RefCell<Obj>>>> {
    let mut globals = Vec::new();
    while let Some(tok) = tokens.peek() {
        if tok.kind == TokenKind::Eof {
            break;
        }
        let basety = declspec(tokens)?;

        // Function
        if is_function(&mut tokens.clone())? {
            function(tokens, &mut globals, &basety)?;
            continue;
        }

        // Global variable
        global_variable(tokens, &mut globals, &basety)?;
    }
    Ok(globals)
}

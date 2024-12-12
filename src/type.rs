use std::sync::{Arc, LazyLock};

use crate::{new_error_tok, Node, NodeKind, Token};
use anyhow::Result;

#[derive(Debug, PartialEq, Clone, Default)]
pub enum TypeKind {
    #[default]
    Int,
    Ptr,
    Func,
    Array,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Type {
    pub kind: TypeKind,
    /// sizeof() value
    pub size: usize,
    /// Pointer-to or array-of type. We intentionally use the same member
    /// to represent pointer/array duality in C.
    ///
    /// In many contexts in which a pointer is expected, we examine this
    /// member instead of "kind" member to determine whether a type is a
    /// pointer or not. That means in many contexts "array of T" is
    /// naturally handled as if it were "pointer to T", as required by
    /// the C spec.
    pub base: Option<Arc<Type>>,
    /// Declaration
    pub name: Option<Box<Token>>,
    /// Array
    pub array_len: Option<usize>,
    /// Function type
    pub return_ty: Option<Arc<Type>>,
    pub params: Option<Vec<Arc<Type>>>,
    pub next: Option<Arc<Type>>,
}

pub static TY_INT: LazyLock<Arc<Type>> = LazyLock::new(|| {
    Arc::new(Type {
        kind: TypeKind::Int,
        size: 8,
        ..Default::default()
    })
});

pub fn is_integer(ty: Option<&Arc<Type>>) -> bool {
    if let Some(ty) = ty {
        if let TypeKind::Int = ty.kind {
            return true;
        }
    }
    false
}

pub fn pointer_to(base: Option<&Arc<Type>>) -> Type {
    Type {
        kind: TypeKind::Ptr,
        size: 8,
        base: base.cloned(),
        ..Default::default()
    }
}

pub fn func_type(return_ty: Arc<Type>) -> Type {
    Type {
        kind: TypeKind::Func,
        return_ty: Some(return_ty),
        ..Default::default()
    }
}

pub fn array_of(base: Arc<Type>, len: usize) -> Type {
    Type {
        kind: TypeKind::Array,
        size: base.size * len,
        base: Some(base),
        array_len: Some(len),
        ..Default::default()
    }
}

pub fn add_type(node: &mut Option<&mut Node>) -> Result<()> {
    let Some(node) = node else {
        return Ok(());
    };
    if node.ty.is_some() {
        return Ok(());
    }

    add_type(&mut node.lhs.as_deref_mut())?;
    add_type(&mut node.rhs.as_deref_mut())?;
    add_type(&mut node.cond.as_deref_mut())?;
    add_type(&mut node.then.as_deref_mut())?;
    add_type(&mut node.els.as_deref_mut())?;
    add_type(&mut node.init.as_deref_mut())?;
    add_type(&mut node.inc.as_deref_mut())?;

    if let Some(body) = node.body.take() {
        let mut nodes = body.collect::<Vec<_>>();
        for node in &mut nodes {
            add_type(&mut Some(node))?;
        }
        if let Some(args) = node.args.as_mut() {
            for arg in args.as_mut_slice() {
                add_type(&mut Some(arg))?;
            }
        }
        node.body = Some(nodes.into_iter());
    }

    match &node.kind {
        NodeKind::Add | NodeKind::Sub | NodeKind::Mul | NodeKind::Div | NodeKind::Neg => {
            let Some(lhs) = &node.lhs else {
                return Err(new_error_tok(&node.tok, "expected left-hand side"));
            };
            node.ty = lhs.ty.clone();
            Ok(())
        }
        NodeKind::Assign => {
            let Some(lhs) = &node.lhs else {
                return Err(new_error_tok(&node.tok, "expected left-hand side"));
            };
            if let Some(ty) = &lhs.ty {
                if let TypeKind::Array = ty.kind {
                    return Err(new_error_tok(&node.tok, "not an lvalue"));
                }
            }
            node.ty = lhs.ty.clone();
            Ok(())
        }
        NodeKind::Eq
        | NodeKind::Ne
        | NodeKind::Lt
        | NodeKind::Le
        | NodeKind::Num(_)
        | NodeKind::FunCall(_) => {
            node.ty = Some(TY_INT.clone());
            Ok(())
        }
        NodeKind::Var(var) => {
            node.ty = var.as_ref().borrow().ty.clone();
            Ok(())
        }
        NodeKind::Addr => {
            let Some(lhs) = &node.lhs else {
                return Err(new_error_tok(&node.tok, "expected left-hand side"));
            };
            if let Some(ty) = &lhs.ty {
                if let TypeKind::Array = ty.kind {
                    node.ty = Some(pointer_to(ty.base.as_ref()).into());
                    return Ok(());
                }
            }
            node.ty = Some(pointer_to(lhs.ty.as_ref()).into());
            Ok(())
        }
        NodeKind::Deref => {
            if let Some(lhs) = &node.lhs {
                if let Some(ty) = &lhs.ty {
                    let Some(ty) = &ty.base else {
                        return Err(new_error_tok(&node.tok, "invalid pointer dereference"));
                    };
                    node.ty = Some(ty.clone());
                    return Ok(());
                }
            }
            node.ty = Some(TY_INT.clone());
            Ok(())
        }
        _ => Ok(()),
    }
}

use std::sync::{Arc, LazyLock};

use crate::{new_error_tok, Node, NodeKind};
use anyhow::Result;

#[derive(Debug, PartialEq, Clone)]
pub enum TypeKind {
    Int,
    Ptr,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub base: Option<Arc<Type>>,
}

pub static TY_INT: LazyLock<Arc<Type>> = LazyLock::new(|| {
    Arc::new(Type {
        kind: TypeKind::Int,
        base: None,
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

fn pointer_to(base: Option<&Arc<Type>>) -> Type {
    Type {
        kind: TypeKind::Ptr,
        base: base.cloned(),
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
        node.body = Some(nodes.into_iter());
    }

    match node.kind {
        NodeKind::Add
        | NodeKind::Sub
        | NodeKind::Mul
        | NodeKind::Div
        | NodeKind::Neg
        | NodeKind::Assign => {
            let Some(lhs) = &node.lhs else {
                return Err(new_error_tok(&node.tok, "expected left-hand side"));
            };
            node.ty = lhs.ty.clone();
            Ok(())
        }
        NodeKind::Eq
        | NodeKind::Ne
        | NodeKind::Lt
        | NodeKind::Le
        | NodeKind::Var(_)
        | NodeKind::Num(_) => {
            node.ty = Some(TY_INT.clone());
            Ok(())
        }
        NodeKind::Addr => {
            let Some(lhs) = &node.lhs else {
                return Err(new_error_tok(&node.tok, "expected left-hand side"));
            };
            node.ty = Some(pointer_to(lhs.ty.as_ref()).into());
            Ok(())
        }
        NodeKind::Deref => {
            if let Some(lhs) = &node.lhs {
                if let Some(ty) = lhs.ty.as_ref() {
                    if let TypeKind::Ptr = ty.kind {
                        node.ty = ty.base.clone();
                        return Ok(());
                    }
                }
            }
            node.ty = Some(TY_INT.clone());
            Ok(())
        }
        _ => Ok(()),
    }
}

use std::path::PathBuf;

use serde::Serialize;

#[derive(Debug, Serialize)]
pub struct SourceFile {
    pub path: PathBuf,
    pub items: Vec<Item>,
}
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize)]
pub struct NodeId(pub usize);

#[derive(Debug, Serialize)]
pub struct Item {
    pub id: NodeId,
    pub kind: ItemKind,
}

#[derive(Debug, Serialize)]
pub struct Fn {
    pub id: NodeId,
    pub name: String,
    pub body: Expr,
    pub return_ty: Option<Ty>,
}
#[derive(Debug, Serialize)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
}
#[derive(Debug, Serialize)]
pub enum ExprKind {
    Block(Block),
    Unit,
}

#[derive(Debug, Serialize)]
pub struct Block {
    pub id: NodeId,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Serialize)]
pub enum ItemKind {
    Fn(Box<Fn>),
}

#[derive(Debug, Serialize)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
}

#[derive(Debug, Serialize)]
pub enum StmtKind {
    Item(Item),
    Expr(Box<Expr>),
}

#[derive(Debug, Serialize)]
pub struct Ty {
    pub id: NodeId,
    pub kind: TyKind,
}
#[derive(Debug, Serialize)]
pub enum TyKind {
    Unit,
}

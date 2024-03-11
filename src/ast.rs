use std::path::PathBuf;

#[derive(Debug)]
pub struct SourceFile {
    pub path: PathBuf,
    pub items: Vec<Item>,
}
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct NodeId(pub usize);

#[derive(Debug)]
pub struct Item {
    pub id: NodeId,
    pub kind: ItemKind,
}

#[derive(Debug)]
pub struct Fn {
    pub id: NodeId,
    pub name: String,
    pub body: Expr,
    pub return_ty: Option<Ty>,
}
#[derive(Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
}
#[derive(Debug)]
pub enum ExprKind {
    Block(Block),
    Unit,
}

#[derive(Debug)]
pub struct Block {
    pub id: NodeId,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum ItemKind {
    Fn(Box<Fn>),
}

#[derive(Debug)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Item(Item),
    Expr(Box<Expr>),
}

#[derive(Debug)]
pub struct Ty {
    pub id: NodeId,
    pub kind: TyKind,
}
#[derive(Debug)]
pub enum TyKind {
    Unit,
}

use std::path::PathBuf;

pub struct SourceFile {
    path: PathBuf,
    items: Vec<Item>,
}
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct NodeId(usize);

#[derive(Debug)]
pub struct Item {
    id: NodeId,
    kind: ItemKind,
}

#[derive(Debug)]
pub struct Fn {
    id: NodeId,
    name: String,
    body: Expr,
}
#[derive(Debug)]
pub struct Expr {
    id: NodeId,
    kind: ExprKind,
}
#[derive(Debug)]
enum ExprKind {
    Block(Block),
}

#[derive(Debug)]
struct Block {
    id: NodeId,
    stmts: Vec<Stmt>,
}

#[derive(Debug)]
enum ItemKind {
    Fn(Box<Fn>),
}

#[derive(Debug)]
struct Stmt {
    id: NodeId,
    kind: StmtKind,
}

#[derive(Debug)]
enum StmtKind {
    Item(Item),
}

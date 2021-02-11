use crate::core::{HasLocation, SourceLocation};

pub struct Identifier {
    pub location: SourceLocation,
    pub name: String,
}
impl HasLocation for Identifier {
    fn location(&self) -> SourceLocation {
        self.location
    }
}

pub struct Declaration {
    pub location: SourceLocation,
    pub kind: DeclarationKind,
}
impl HasLocation for Declaration {
    fn location(&self) -> SourceLocation {
        self.location
    }
}

pub struct Statement {
    pub location: SourceLocation,
    pub kind: StatementKind,
}
impl HasLocation for Statement {
    fn location(&self) -> SourceLocation {
        self.location
    }
}

pub enum StatementKind {
    Expression(Box<Expression>),
    Return(Option<Expression>),
}

pub struct FunctionCall {
    pub callee: Box<Expression>,
    pub args: Vec<Arg>,
}

pub struct Arg {
    pub label: Option<Identifier>,
    pub value: Box<Expression>,
}
impl HasLocation for Arg {
    fn location(&self) -> SourceLocation {
        match &self.label {
            Some(label) => SourceLocation::between(label, self.value.as_ref()),
            None => self.value.location(),
        }
    }
}
pub struct Expression {
    pub location: SourceLocation,
    pub kind: ExpressionKind,
}
pub enum ExpressionKind {
    Var(Identifier),
    FunctionCall(FunctionCall),
    BinaryStringLiteral(String),
}

impl HasLocation for Expression {
    fn location(&self) -> SourceLocation {
        self.location
    }
}

pub struct Block {
    pub location: SourceLocation,
    pub statements: Vec<Statement>,
}

impl HasLocation for Block {
    fn location(&self) -> SourceLocation {
        self.location
    }
}

pub enum DeclarationKind {
    ImportAs(ImportAs),
    FunctionDef(FunctionDef),
    ExternFunctionDef(ExternFunctionDef),
}

pub struct QualifiedPath(pub Vec<Identifier>);

pub struct ImportAs {
    pub path: QualifiedPath,
    pub as_name: Identifier,
}

pub struct FunctionDef {
    pub signature: FunctionSignature,
    pub body: Block,
}

pub struct ExternFunctionDef {
    pub name: Identifier,
    pub params: Vec<TypeAnnotation>,
    pub return_type: TypeAnnotation,
    pub extern_name: Identifier,
}

pub struct FunctionSignature {
    pub location: SourceLocation,
    pub name: Identifier,
    pub params: Vec<Param>,
    pub return_type: Option<TypeAnnotation>,
}
impl HasLocation for FunctionSignature {
    fn location(&self) -> SourceLocation {
        self.location
    }
}

pub struct TypeAnnotation {
    pub location: SourceLocation,
    pub kind: TypeAnnotationKind,
}
impl HasLocation for TypeAnnotation {
    fn location(&self) -> SourceLocation {
        self.location
    }
}

pub enum TypeAnnotationKind {
    Var(Identifier),
    Apply(Box<TypeAnnotation>, Vec<Box<TypeAnnotation>>),
    Pointer(Box<TypeAnnotation>),
    MutPointer(Box<TypeAnnotation>),
}

pub struct Param {
    pub name: Identifier,
    pub annotation: Option<TypeAnnotation>,
}
impl HasLocation for Param {
    fn location(&self) -> SourceLocation {
        match &self.annotation {
            Some(annotation) => SourceLocation::between(&self.name, annotation),
            None => self.name.location(),
        }
    }
}

pub struct SourceFile {
    pub location: SourceLocation,
    pub declarations: Vec<Declaration>,
}
impl HasLocation for SourceFile {
    fn location(&self) -> SourceLocation {
        self.location
    }
}

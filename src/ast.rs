use crate::{location::Location, precedence::Precedence};
use variant_count::VariantCount;


#[derive(Debug, Clone)]
pub struct AstIfStatement {
    pub loc: Location,
    pub condition: AstExpr,
    pub then_block: Box<AstBlock>,
    pub else_block: Option<Box<AstBlock>>,
}

#[derive(Debug, Clone)]
pub struct AstWhileStatement {
    pub loc: Location,
    pub condition: AstExpr,
    pub block: Box<AstBlock>,
}

#[derive(Debug, Clone, VariantCount)]
pub enum AstStatement {
    Expr(AstExpr),
    If(AstIfStatement),
    VarAssign(AstVarAssign),
    While(AstWhileStatement),
}

#[derive(Debug, Clone)]
pub struct AstFunCall {
    pub loc: Location,
    pub name: String,
    pub args: Vec<AstExpr>,
}

#[derive(Debug, Clone)]
pub struct AstVarRead {
    pub loc: Location,
    pub name: String,
}

#[derive(Debug, Clone, Copy, VariantCount)]
pub enum AstBinaryOpKind {
    Plus,
    Minus,
    Mult,
    AndAnd,
    LessThen,
    GreaterEqual,
    NotEqual,
    EqualsEquals,
    OrOr,
}

#[derive(Debug, Clone)]
pub struct AstBinaryOp {
    pub loc: Location,
    pub kind: AstBinaryOpKind,
    pub lhs: Box<AstExpr>,
    pub rhs: Box<AstExpr>,
}

#[derive(Debug, Clone, VariantCount)]
pub enum AstExprKind {
    FuncCall(AstFunCall),
    LitFloat(f64),
    LitInt(i64),
    LitChar(char),
    LitString(String),
    LitBool(bool),
    VarRead(AstVarRead),
    BinarayOp(AstBinaryOp),
}

#[derive(Debug, Clone)]
pub struct AstExpr {
    pub loc: Location,
    pub kind: AstExprKind,
}

#[derive(Debug, Clone)]
pub struct AstBlock {
    pub statements: Vec<AstStatement>,
}

#[derive(Debug, Clone)]
pub struct AstProcDef {
    pub loc: Location,
    pub name: String,
    pub body: AstBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, VariantCount)]
pub enum AstTypes {
    VOID,
    I64,
    U8,
    BOOL,
    PTR,
}

#[derive(Debug, Clone)]
pub struct AstVarDef {
    pub loc: Location,
    pub name: String,
    pub var_type: AstTypes,
}

#[derive(Debug, Clone)]
pub struct AstVarAssign {
    pub loc: Location,
    pub name: String,
    pub expr: AstExpr,
}

#[derive(Debug, VariantCount)]
pub enum AstTop {
    ProcDef(AstProcDef),
    VarDef(AstVarDef),
}

#[derive(Debug)]
pub struct AstModule {
    pub tops: Vec<AstTop>,
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryOpDef {
    pub kind     : AstBinaryOpKind,
    pub prec     : Precedence,
}

pub fn expr_kind_to_name(expr: &AstExpr) -> &'static str {
    match expr.kind {
        AstExprKind::FuncCall(_) => "function call",
        AstExprKind::LitFloat(_) => "float literal",
        AstExprKind::LitInt(_) => "integral literal",
        AstExprKind::LitChar(_) => "char literal",
        AstExprKind::LitString(_) => "string literal",
        AstExprKind::LitBool(_) => "bool literal",
        AstExprKind::VarRead(_) => "read variable",
        AstExprKind::BinarayOp(_) => "binary operator",
    }
}

pub fn name_to_type(type_name: &str) -> Option<AstTypes> {
    match type_name {
        "void" => Some(AstTypes::VOID),
        "i64" => Some(AstTypes::I64),
        "u8" => Some(AstTypes::U8),
        "bool" => Some(AstTypes::BOOL),
        "ptr" => Some(AstTypes::PTR),
        _ => None,
    }
}

// TODO CHECK IF WE CAN USE From/INTO pattern
// also update name_to_type
pub fn type_to_str(type_kind: AstTypes) -> &'static str {
    match type_kind {
        AstTypes::VOID =>"void",
        AstTypes::I64 => "i64",
        AstTypes::U8 =>  "u8",
        AstTypes::BOOL =>"bool",
        AstTypes::PTR => "ptr",
    }
}

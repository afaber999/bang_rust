use crate::{location::Location, precedence::Precedence};
use variant_count::VariantCount;


#[derive(Debug, Clone)]
pub struct AstIfStatement<'a> {
    pub loc: Location<'a>,
    pub condition: AstExpr<'a>,
    pub then_block: Box<AstBlock<'a>>,
    pub else_block: Option<Box<AstBlock<'a>>>,
}

#[derive(Debug, Clone)]
pub struct AstWhileStatement<'a> {
    pub loc: Location<'a>,
    pub condition: AstExpr<'a>,
    pub block: Box<AstBlock<'a>>,
}

#[derive(Debug, Clone, VariantCount)]
pub enum AstStatement<'a> {
    Expr(AstExpr<'a>),
    If(AstIfStatement<'a>),
    VarAssign(AstVarAssign<'a>),
    While(AstWhileStatement<'a>),
    VarDef(AstVarDef<'a>),
}

#[derive(Debug, Clone)]
pub struct AstFunCall<'a> {
    pub loc: Location<'a>,
    pub name: &'a str,
    pub args: Vec<AstExpr<'a>>,
}

#[derive(Debug, Clone)]
pub struct AstVarRead<'a> {
    pub loc: Location<'a>,
    pub name: &'a str,
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
pub struct AstBinaryOp<'a> {
    pub loc: Location<'a>,
    pub kind: AstBinaryOpKind,
    pub lhs: Box<AstExpr<'a>>,
    pub rhs: Box<AstExpr<'a>>,
}

#[derive(Debug, Clone, VariantCount)]
pub enum AstExprKind<'a> {
    FuncCall(AstFunCall<'a>),
    LitFloat(f64),
    LitInt(i64),
    LitChar(char),
    LitString(String),
    LitBool(bool),
    VarRead(AstVarRead<'a>),
    BinarayOp(AstBinaryOp<'a>),
}

#[derive(Debug, Clone)]
pub struct AstExpr<'a> {
    pub loc: Location<'a>,
    pub kind: AstExprKind<'a>,
}

#[derive(Debug, Clone)]
pub struct AstBlock<'a> {
    pub statements: Vec<AstStatement<'a>>,
}

#[derive(Debug, Clone)]
pub struct AstProcParam<'a> {
    pub loc: Location<'a>,
    pub param_name: &'a str,
    pub param_type: AstTypes,
}

#[derive(Debug, Clone)]
pub struct AstProcDef<'a> {
    pub loc: Location<'a>,
    pub name: &'a str,
    pub body: AstBlock<'a>,
    pub params: Vec<AstProcParam<'a>>,
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
pub struct AstVarDef<'a> {
    pub loc: Location<'a>,
    pub name: &'a str,
    pub init_expr : Option<AstExpr<'a>>,
    pub var_type: AstTypes,
}

#[derive(Debug, Clone)]
pub struct AstVarAssign<'a> {
    pub loc: Location<'a>,
    pub name: &'a str,
    pub expr: AstExpr<'a>,
}

#[derive(Debug, VariantCount)]
pub enum AstTop<'a> {
    ProcDef(AstProcDef<'a>),
    VarDef(AstVarDef<'a>),
}

#[derive(Debug)]
pub struct AstModule<'a> {
    pub tops: Vec<AstTop<'a>>,
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

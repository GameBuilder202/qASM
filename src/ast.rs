use std::{collections::HashMap, fmt::Display, ops::Range};

use lalrpop_util::{lalrpop_mod, ParseError};
lalrpop_mod!(grammar);

use codespan_reporting::diagnostic::{Diagnostic, Label};

type CGateInfo = (usize, Vec<(String, IdentType)>, Vec<ResolvedInst>);

#[derive(Debug, Clone)]
pub struct Program {
    // (qbits, cbits, qregs, cregs, mem_size)
    pub headers: (usize, usize, usize, usize, usize),
    pub custom_gates: HashMap<String, CGateInfo>,
    pub instructions: Vec<ResolvedInst>,
}

#[derive(Debug, Clone)]
pub struct SourceSpan {
    pub file: usize,
    pub span: Range<usize>,
}
impl SourceSpan {
    pub fn new(file: usize, span: Range<usize>) -> Self {
        Self { file, span }
    }
}

type ParseResult<'a> = Result<Program, ResolveError>;

pub fn parse(code: &str, file: usize) -> (ParseResult, Vec<Diagnostic<usize>>) {
    let mut errs = Vec::new();
    let ast = grammar::ProgramParser::new().parse(file, &mut errs, code);
    let mut diags = Vec::new();

    for err in errs {
        let diag = Diagnostic::error();
        match err {
            ParseError::InvalidToken { location } => diags.push(
                diag.with_message("Invalid token")
                    .with_labels(vec![Label::primary(file, location..location)]),
            ),

            ParseError::UnrecognizedEof { location, .. } => diags.push(
                diag.with_message("Unexpected EOF")
                    .with_labels(vec![Label::primary(file, location..location)]),
            ),

            ParseError::UnrecognizedToken { token, expected } => diags.push(
                diag.with_message("Unrecognized token")
                    .with_labels(vec![Label::primary(file, token.0..token.2)])
                    .with_notes(expected),
            ),

            ParseError::ExtraToken { token } => diags.push(
                diag.with_message("Extra token found")
                    .with_labels(vec![Label::primary(file, token.0..token.2)]),
            ),

            ParseError::User { .. } => {}
        }
    }

    if let Ok(ast) = ast {
        (ast, diags)
    } else {
        let mut diag = Diagnostic::error();
        let err = ast.unwrap_err();
        match err {
            ParseError::InvalidToken { location } => {
                diag = diag
                    .with_message("Invalid token")
                    .with_labels(vec![Label::primary(file, location..location)])
            }

            ParseError::UnrecognizedEof { location, .. } => {
                diag = diag
                    .with_message("Unexpected EOF")
                    .with_labels(vec![Label::primary(file, location..location)])
            }

            ParseError::UnrecognizedToken { token, .. } => {
                diag = diag
                    .with_message("Unrecognized token")
                    .with_labels(vec![Label::primary(file, token.0..token.2)])
            }

            ParseError::ExtraToken { token } => {
                diag = diag
                    .with_message("Extra token found")
                    .with_labels(vec![Label::primary(file, token.0..token.2)])
            }

            ParseError::User { .. } => todo!(),
        }

        (Err(ResolveError::ParseError(diag)), diags)
    }
}

pub fn resolve_ast(
    headers: (usize, usize, usize, usize, usize),
    ast: Vec<Inst>,
) -> Result<Program, ResolveError> {
    let mut label_map = HashMap::new();
    let mut custom_gates = HashMap::new();
    let mut resolved_insts: Vec<ResolvedInst> = Vec::new();

    // Get all labels from the AST
    let mut i = 0usize;
    for inst in &ast {
        if let Inst::Label(name) = inst {
            label_map.insert(name, i);
        } else {
            i += 1
        }
    }

    // Update all instructions with the label map
    for inst in &ast {
        match inst {
            Inst::Label(_) => {}

            Inst::Jmp(name, s) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push(ResolvedInst::Jmp(*offset, s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            Inst::Jeq(name, s) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push(ResolvedInst::Jeq(*offset, s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            Inst::Jne(name, s) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push(ResolvedInst::Jne(*offset, s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            Inst::Jg(name, s) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push(ResolvedInst::Jg(*offset, s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            Inst::Jge(name, s) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push(ResolvedInst::Jge(*offset, s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            Inst::Jl(name, s) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push(ResolvedInst::Jle(*offset, s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            // Custom instruction stuff
            Inst::CustomGateDef(name, qbits, args, body) => {
                let body = resolve_ast(headers, body.clone())?.instructions;
                custom_gates.insert(name.clone(), (*qbits, args.clone(), body));
            }
            Inst::Custom(name, qbits, args, s) => {
                if let Some(gate) = custom_gates.get(name) {
                    let gate_qbits = gate.0;
                    let gate_args = &gate.1;
                    let diag = Diagnostic::error();

                    if qbits.len() != gate_qbits {
                        return Err(ResolveError::CustomGateError(
                            diag.with_message("Given qubits to gate don't match gate definition")
                                .with_labels(vec![Label::primary(s.file, s.span.clone())])
                                .with_notes(vec![format!(
                                    "Note: Gate expects {} qubits, but {} were given",
                                    gate_qbits,
                                    qbits.len()
                                )]),
                        ));
                    }
                    if args.iter().map(|x| x.get_type()).collect::<Vec<_>>()
                        != gate_args.iter().map(|x| x.1).collect::<Vec<_>>()
                    {
                        return Err(ResolveError::CustomGateError(
                            diag.with_message("Given args to gate don't match gate definition")
                                .with_labels(vec![Label::primary(s.file, s.span.clone())])
                                .with_notes(vec![format!(
                                    "Note: Gate expects arg types {}, but given arg types were {}",
                                    format_vec(gate_args.iter().map(|x| x.1).collect()),
                                    format_vec(args.iter().map(|x| x.get_type()).collect()),
                                )]),
                        ));
                    }

                    resolved_insts.push(inst.into())
                } else {
                    return Err(ResolveError::UndefinedGate(name.clone(), s.clone()));
                }
            }

            Inst::Err => {}

            _ => resolved_insts.push(inst.into()),
        }
    }

    Ok(Program {
        headers,
        custom_gates,
        instructions: resolved_insts,
    })
}

fn format_vec<T: Display>(vec: Vec<T>) -> String {
    let mut s = String::from("[");
    for e in vec {
        s += format!("{}, ", e).as_str()
    }
    s.pop();
    s.pop();
    s += "]";

    s
}

#[derive(Debug, Clone)]
pub enum Inst {
    // Quantum Instructions
    Qsel(usize, SourceSpan),
    Id(usize, SourceSpan),
    Hadamard(usize, SourceSpan),
    Cnot(usize, usize, SourceSpan),
    Ccnot(usize, usize, usize, SourceSpan),
    X(usize, SourceSpan),
    Y(usize, SourceSpan),
    Z(usize, SourceSpan),
    Rx(usize, Rotation, SourceSpan),
    Ry(usize, Rotation, SourceSpan),
    Rz(usize, Rotation, SourceSpan),
    U(usize, Rotation, Rotation, Rotation, SourceSpan),
    S(usize, SourceSpan),
    T(usize, SourceSpan),
    Sdg(usize, SourceSpan),
    Tdg(usize, SourceSpan),
    Phase(usize, Rotation, SourceSpan),
    Ch(usize, usize, SourceSpan),
    Cy(usize, usize, SourceSpan),
    Cz(usize, usize, SourceSpan),
    CPhase(usize, usize, Rotation, SourceSpan),
    Swap(usize, usize, SourceSpan),
    SqrtX(usize, SourceSpan),
    SqrtSwap(usize, usize, SourceSpan),
    CSwap(usize, usize, usize, SourceSpan),
    Measure(usize, usize, usize, SourceSpan),

    // Custom gate stuff
    //            (name,  qbits, args,                     body)
    CustomGateDef(String, usize, Vec<(String, IdentType)>, Vec<Inst>),
    Custom(String, Vec<usize>, Vec<IdentVal>, SourceSpan),

    // Classical Instructions
    Mov(Operand, Operand, SourceSpan),
    Add(Operand, Operand, Operand, SourceSpan),
    Sub(Operand, Operand, Operand, SourceSpan),
    Mul(Operand, Operand, Operand, SourceSpan),
    UMul(Operand, Operand, Operand, SourceSpan),
    Div(Operand, Operand, Operand, SourceSpan),
    SMul(Operand, Operand, Operand, SourceSpan),
    SUMul(Operand, Operand, Operand, SourceSpan),
    SDiv(Operand, Operand, Operand, SourceSpan),
    Not(Operand, Operand, SourceSpan),
    And(Operand, Operand, Operand, SourceSpan),
    Or(Operand, Operand, Operand, SourceSpan),
    Xor(Operand, Operand, Operand, SourceSpan),
    Nand(Operand, Operand, Operand, SourceSpan),
    Nor(Operand, Operand, Operand, SourceSpan),
    Xnor(Operand, Operand, Operand, SourceSpan),

    // Misc
    Cmp(Operand, Operand, SourceSpan),
    Jmp(String, SourceSpan),
    Jeq(String, SourceSpan),
    Jne(String, SourceSpan),
    Jg(String, SourceSpan),
    Jge(String, SourceSpan),
    Jl(String, SourceSpan),
    Jle(String, SourceSpan),
    Hlt,

    Label(String),

    Err,
}

#[derive(Debug, Clone)]
pub enum ResolvedInst {
    // Quantum Instructions
    Qsel(usize, SourceSpan),
    Id(usize, SourceSpan),
    Hadamard(usize, SourceSpan),
    Cnot(usize, usize, SourceSpan),
    Ccnot(usize, usize, usize, SourceSpan),
    X(usize, SourceSpan),
    Y(usize, SourceSpan),
    Z(usize, SourceSpan),
    Rx(usize, Rotation, SourceSpan),
    Ry(usize, Rotation, SourceSpan),
    Rz(usize, Rotation, SourceSpan),
    U(usize, Rotation, Rotation, Rotation, SourceSpan),
    S(usize, SourceSpan),
    T(usize, SourceSpan),
    Sdg(usize, SourceSpan),
    Tdg(usize, SourceSpan),
    Phase(usize, Rotation, SourceSpan),
    Ch(usize, usize, SourceSpan),
    Cy(usize, usize, SourceSpan),
    Cz(usize, usize, SourceSpan),
    CPhase(usize, usize, Rotation, SourceSpan),
    Swap(usize, usize, SourceSpan),
    SqrtX(usize, SourceSpan),
    SqrtSwap(usize, usize, SourceSpan),
    CSwap(usize, usize, usize, SourceSpan),
    Measure(usize, usize, usize, SourceSpan),

    // Custom gate stuff
    Custom(String, Vec<usize>, Vec<IdentVal>, SourceSpan),

    // Classical Instructions
    Mov(Operand, Operand, SourceSpan),
    Add(Operand, Operand, Operand, SourceSpan),
    Sub(Operand, Operand, Operand, SourceSpan),
    Mul(Operand, Operand, Operand, SourceSpan),
    UMul(Operand, Operand, Operand, SourceSpan),
    Div(Operand, Operand, Operand, SourceSpan),
    SMul(Operand, Operand, Operand, SourceSpan),
    SUMul(Operand, Operand, Operand, SourceSpan),
    SDiv(Operand, Operand, Operand, SourceSpan),
    Not(Operand, Operand, SourceSpan),
    And(Operand, Operand, Operand, SourceSpan),
    Or(Operand, Operand, Operand, SourceSpan),
    Xor(Operand, Operand, Operand, SourceSpan),
    Nand(Operand, Operand, Operand, SourceSpan),
    Nor(Operand, Operand, Operand, SourceSpan),
    Xnor(Operand, Operand, Operand, SourceSpan),

    // Misc
    Cmp(Operand, Operand, SourceSpan),
    Jmp(usize, SourceSpan),
    Jeq(usize, SourceSpan),
    Jne(usize, SourceSpan),
    Jg(usize, SourceSpan),
    Jge(usize, SourceSpan),
    Jl(usize, SourceSpan),
    Jle(usize, SourceSpan),

    Hlt,
}

impl From<&Inst> for ResolvedInst {
    // The most painful damn function ever to write
    fn from(value: &Inst) -> Self {
        match value {
            // Quantum instructions
            Inst::Qsel(q, s) => Self::Qsel(*q, s.clone()),
            Inst::Id(q, s) => Self::Id(*q, s.clone()),
            Inst::Hadamard(q, s) => Self::Hadamard(*q, s.clone()),
            Inst::Cnot(q1, q2, s) => Self::Cnot(*q1, *q2, s.clone()),
            Inst::Ccnot(q1, q2, q3, s) => Self::Ccnot(*q1, *q2, *q3, s.clone()),
            Inst::X(q, s) => Self::X(*q, s.clone()),
            Inst::Y(q, s) => Self::Y(*q, s.clone()),
            Inst::Z(q, s) => Self::Z(*q, s.clone()),
            Inst::Rx(q, r, s) => Self::Rx(*q, r.clone(), s.clone()),
            Inst::Ry(q, r, s) => Self::Ry(*q, r.clone(), s.clone()),
            Inst::Rz(q, r, s) => Self::Rz(*q, r.clone(), s.clone()),
            Inst::U(q, r1, r2, r3, s) => Self::U(*q, r1.clone(), r2.clone(), r3.clone(), s.clone()),
            Inst::S(q, s) => Self::S(*q, s.clone()),
            Inst::T(q, s) => Self::T(*q, s.clone()),
            Inst::Sdg(q, s) => Self::Sdg(*q, s.clone()),
            Inst::Tdg(q, s) => Self::Tdg(*q, s.clone()),
            Inst::Phase(q, r, s) => Self::Phase(*q, r.clone(), s.clone()),
            Inst::Ch(q1, q2, s) => Self::Ch(*q1, *q2, s.clone()),
            Inst::Cy(q1, q2, s) => Self::Cy(*q1, *q2, s.clone()),
            Inst::Cz(q1, q2, s) => Self::Cz(*q1, *q2, s.clone()),
            Inst::CPhase(q1, q2, r, s) => Self::CPhase(*q1, *q2, r.clone(), s.clone()),
            Inst::Swap(q1, q2, s) => Self::Swap(*q1, *q2, s.clone()),
            Inst::SqrtX(q, s) => Self::SqrtX(*q, s.clone()),
            Inst::SqrtSwap(q1, q2, s) => Self::SqrtSwap(*q1, *q2, s.clone()),
            Inst::CSwap(q1, q2, q3, s) => Self::CSwap(*q1, *q2, *q3, s.clone()),
            Inst::Measure(q, cr, cb, s) => Self::Measure(*q, *cr, *cb, s.clone()),

            // Custom gate stuff
            Inst::Custom(name, qbits, args, s) => {
                Self::Custom(name.clone(), qbits.clone(), args.clone(), s.clone())
            }

            // Classical Instructions
            Inst::Mov(cr1, val, s) => Self::Mov(cr1.clone(), val.clone(), s.clone()),
            Inst::Add(cr1, cr2, cr3, s) => {
                Self::Add(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::Sub(cr1, cr2, cr3, s) => {
                Self::Sub(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::Mul(cr1, cr2, cr3, s) => {
                Self::Mul(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::UMul(cr1, cr2, cr3, s) => {
                Self::UMul(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::Div(cr1, cr2, cr3, s) => {
                Self::Div(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::SMul(cr1, cr2, cr3, s) => {
                Self::SMul(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::SUMul(cr1, cr2, cr3, s) => {
                Self::SUMul(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::SDiv(cr1, cr2, cr3, s) => {
                Self::SDiv(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::Not(cr1, cr2, s) => Self::Not(cr1.clone(), cr2.clone(), s.clone()),
            Inst::And(cr1, cr2, cr3, s) => {
                Self::And(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::Or(cr1, cr2, cr3, s) => {
                Self::Or(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::Xor(cr1, cr2, cr3, s) => {
                Self::Xor(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::Nand(cr1, cr2, cr3, s) => {
                Self::Nand(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::Nor(cr1, cr2, cr3, s) => {
                Self::Nor(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }
            Inst::Xnor(cr1, cr2, cr3, s) => {
                Self::Xnor(cr1.clone(), cr2.clone(), cr3.clone(), s.clone())
            }

            // Misc
            Inst::Cmp(cr1, val, s) => Self::Cmp(cr1.clone(), val.clone(), s.clone()),
            Inst::Hlt => Self::Hlt,

            _ => unreachable!(), /* Atleast hopefully unreachable if I didn't mess up stuff */
        }
    }
}

impl ResolvedInst {
    pub fn get_span(&self) -> &SourceSpan {
        match self {
            Self::Qsel(_, s) => s,
            Self::Id(_, s) => s,
            Self::Hadamard(_, s) => s,
            Self::Cnot(_, _, s) => s,
            Self::Ccnot(_, _, _, s) => s,
            Self::X(_, s) => s,
            Self::Y(_, s) => s,
            Self::Z(_, s) => s,
            Self::Rx(_, _, s) => s,
            Self::Ry(_, _, s) => s,
            Self::Rz(_, _, s) => s,
            Self::U(_, _, _, _, s) => s,
            Self::S(_, s) => s,
            Self::T(_, s) => s,
            Self::Sdg(_, s) => s,
            Self::Tdg(_, s) => s,
            Self::Phase(_, _, s) => s,
            Self::Ch(_, _, s) => s,
            Self::Cy(_, _, s) => s,
            Self::Cz(_, _, s) => s,
            Self::CPhase(_, _, _, s) => s,
            Self::Swap(_, _, s) => s,
            Self::SqrtX(_, s) => s,
            Self::SqrtSwap(_, _, s) => s,
            Self::CSwap(_, _, _, s) => s,
            Self::Measure(_, _, _, s) => s,

            // Custom gate stuff
            Self::Custom(_, _, _, s) => s,

            // Classical Instructions
            Self::Mov(_, _, s) => s,
            Self::Add(_, _, _, s) => s,
            Self::Sub(_, _, _, s) => s,
            Self::Mul(_, _, _, s) => s,
            Self::UMul(_, _, _, s) => s,
            Self::Div(_, _, _, s) => s,
            Self::SMul(_, _, _, s) => s,
            Self::SUMul(_, _, _, s) => s,
            Self::SDiv(_, _, _, s) => s,
            Self::Not(_, _, s) => s,
            Self::And(_, _, _, s) => s,
            Self::Or(_, _, _, s) => s,
            Self::Xor(_, _, _, s) => s,
            Self::Nand(_, _, _, s) => s,
            Self::Nor(_, _, _, s) => s,
            Self::Xnor(_, _, _, s) => s,

            // Misc
            Self::Cmp(_, _, s) => s,
            Self::Jmp(_, s) => s,
            Self::Jeq(_, s) => s,
            Self::Jne(_, s) => s,
            Self::Jg(_, s) => s,
            Self::Jge(_, s) => s,
            Self::Jl(_, s) => s,
            Self::Jle(_, s) => s,

            Self::Hlt => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    Imm(i64),
    Reg(usize),
    Addr(MemAddr),
    Ident(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IdentVal {
    Rot(Rot),

    Imm(i64),
    Reg(usize),
    Addr(MemAddr),
}
impl IdentVal {
    pub fn get_type(&self) -> IdentType {
        match self {
            Self::Rot(_) => IdentType::Rot,
            Self::Imm(_) => IdentType::Imm,
            Self::Reg(_) => IdentType::Reg,
            Self::Addr(_) => IdentType::MemAddr,
        }
    }

    pub fn get_rot(&self) -> Option<Rot> {
        match self {
            Self::Rot(rot) => Some(*rot),
            _ => None,
        }
    }
}
impl From<&IdentVal> for Operand {
    fn from(value: &IdentVal) -> Self {
        match value {
            IdentVal::Imm(imm) => Self::Imm(*imm),
            IdentVal::Reg(reg) => Self::Reg(*reg),
            IdentVal::Addr(addr) => Self::Addr(*addr),

            IdentVal::Rot(_) => unreachable!(), // hopefully...
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdentType {
    Rot,

    Imm,
    Reg,
    MemAddr,
}
impl Display for IdentType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rot => write!(f, "Rot"),
            Self::Imm => write!(f, "Imm"),
            Self::Reg => write!(f, "Reg"),
            Self::MemAddr => write!(f, "Addr"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemAddr {
    Address(usize),
    // (reg, align, offset) = reg * align + offset
    Indirect(usize, usize, usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rot(pub i64, pub u64);
impl Rot {
    pub fn get_angle(&self) -> f64 {
        (self.0 as f64) * std::f64::consts::PI / (self.1 as f64)
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rotation {
    Rot(Rot),
    Ident(String),
}
impl Rotation {
    pub fn get_rot(&self, args: &HashMap<String, IdentVal>) -> Option<Rot> {
        match self {
            Self::Rot(rot) => Some(*rot),
            Self::Ident(ident) => {
                if let Some(rot) = args.get(ident) {
                    rot.get_rot()
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ResolveError {
    UndefinedLabel(String, SourceSpan),
    ParseError(Diagnostic<usize>),
    UndefinedGate(String, SourceSpan),
    CustomGateError(Diagnostic<usize>),
}

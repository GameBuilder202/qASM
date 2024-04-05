use std::{collections::HashMap, fmt::Display, ops::Range};

use lalrpop_util::{lalrpop_mod, ParseError};
lalrpop_mod!(grammar);

use codespan_reporting::diagnostic::{Diagnostic, Label};

type CGateInfo = (
    usize,
    Vec<(String, IdentType)>,
    Vec<(ResolvedInst, SourceSpan)>,
);

#[derive(Debug, Clone)]
pub struct Program {
    // (qbits, cbits, qregs, cregs, mem_size)
    pub headers: (usize, usize, usize, usize, usize),
    pub custom_gates: HashMap<String, CGateInfo>,
    pub instructions: Vec<(ResolvedInst, SourceSpan)>,
}

#[derive(Debug, Clone, Default)]
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

            ParseError::UnrecognizedToken { token, expected } => {
                diag = diag
                    .with_message("Unrecognized token")
                    .with_labels(vec![Label::primary(file, token.0..token.2)])
                    .with_notes(expected)
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
    ast: Vec<(Inst, SourceSpan)>,
) -> Result<Program, ResolveError> {
    let mut label_map = HashMap::new();
    let mut custom_gates = HashMap::new();
    let mut resolved_insts = Vec::new();

    // Get all labels from the AST
    let mut i = 0usize;
    for (inst, _) in &ast {
        if let Inst::Label(name) = inst {
            label_map.insert(name, i);
        } else {
            i += 1
        }
    }

    // Update all instructions with the label map
    for (inst, s) in &ast {
        match inst {
            Inst::Label(_) => {}

            Inst::Jmp(name) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push((ResolvedInst::Jmp(*offset), s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            Inst::Jeq(name) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push((ResolvedInst::Jeq(*offset), s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            Inst::Jne(name) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push((ResolvedInst::Jne(*offset), s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            Inst::Jg(name) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push((ResolvedInst::Jg(*offset), s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            Inst::Jge(name) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push((ResolvedInst::Jge(*offset), s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            Inst::Jl(name) => {
                if let Some(offset) = label_map.get(name) {
                    resolved_insts.push((ResolvedInst::Jle(*offset), s.clone()))
                } else {
                    return Err(ResolveError::UndefinedLabel(name.clone(), s.clone()));
                }
            }

            // Custom instruction stuff
            Inst::CustomGateDef(name, qbits, args, body) => {
                let body = resolve_ast(headers, body.to_vec())?.instructions;
                custom_gates.insert(name.clone(), (*qbits, args.clone(), body));
            }
            Inst::Custom(name, qbits, args) => {
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

                    resolved_insts.push((inst.into(), s.clone()))
                } else {
                    return Err(ResolveError::UndefinedGate(name.clone(), s.clone()));
                }
            }

            Inst::Err => {}

            _ => resolved_insts.push((inst.into(), s.clone())),
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
    Qsel(usize),
    Id(usize),
    Hadamard(usize),
    Cnot(usize, usize),
    Ccnot(usize, usize, usize),
    X(usize),
    Y(usize),
    Z(usize),
    Rx(usize, Rotation),
    Ry(usize, Rotation),
    Rz(usize, Rotation),
    U(usize, Rotation, Rotation, Rotation),
    S(usize),
    T(usize),
    Sdg(usize),
    Tdg(usize),
    Phase(usize, Rotation),
    Ch(usize, usize),
    Cy(usize, usize),
    Cz(usize, usize),
    CPhase(usize, usize, Rotation),
    Swap(usize, usize),
    SqrtX(usize),
    SqrtSwap(usize, usize),
    CSwap(usize, usize, usize),
    Measure(usize, usize, usize),

    // Custom gate stuff
    //            (name,  qbits, args,                     body)
    CustomGateDef(
        String,
        usize,
        Vec<(String, IdentType)>,
        Vec<(Inst, SourceSpan)>,
    ),
    Custom(String, Vec<usize>, Vec<IdentVal>),

    // Classical Instructions
    Mov(Operand, Operand),
    Add(Operand, Operand, Operand),
    Sub(Operand, Operand, Operand),
    Mul(Operand, Operand, Operand),
    UMul(Operand, Operand, Operand),
    Div(Operand, Operand, Operand),
    SMul(Operand, Operand, Operand),
    SUMul(Operand, Operand, Operand),
    SDiv(Operand, Operand, Operand),
    Not(Operand, Operand),
    And(Operand, Operand, Operand),
    Or(Operand, Operand, Operand),
    Xor(Operand, Operand, Operand),
    Nand(Operand, Operand, Operand),
    Nor(Operand, Operand, Operand),
    Xnor(Operand, Operand, Operand),

    // Misc
    Cmp(Operand, Operand),
    Jmp(String),
    Jeq(String),
    Jne(String),
    Jg(String),
    Jge(String),
    Jl(String),
    Jle(String),
    Hlt,

    Label(String),

    Err,
}

#[derive(Debug, Clone)]
pub enum ResolvedInst {
    // Quantum Instructions
    Qsel(usize),
    Id(usize),
    Hadamard(usize),
    Cnot(usize, usize),
    Ccnot(usize, usize, usize),
    X(usize),
    Y(usize),
    Z(usize),
    Rx(usize, Rotation),
    Ry(usize, Rotation),
    Rz(usize, Rotation),
    U(usize, Rotation, Rotation, Rotation),
    S(usize),
    T(usize),
    Sdg(usize),
    Tdg(usize),
    Phase(usize, Rotation),
    Ch(usize, usize),
    Cy(usize, usize),
    Cz(usize, usize),
    CPhase(usize, usize, Rotation),
    Swap(usize, usize),
    SqrtX(usize),
    SqrtSwap(usize, usize),
    CSwap(usize, usize, usize),
    Measure(usize, usize, usize),

    // Custom gate stuff
    Custom(String, Vec<usize>, Vec<IdentVal>),

    // Classical Instructions
    Mov(Operand, Operand),
    Add(Operand, Operand, Operand),
    Sub(Operand, Operand, Operand),
    Mul(Operand, Operand, Operand),
    UMul(Operand, Operand, Operand),
    Div(Operand, Operand, Operand),
    SMul(Operand, Operand, Operand),
    SUMul(Operand, Operand, Operand),
    SDiv(Operand, Operand, Operand),
    Not(Operand, Operand),
    And(Operand, Operand, Operand),
    Or(Operand, Operand, Operand),
    Xor(Operand, Operand, Operand),
    Nand(Operand, Operand, Operand),
    Nor(Operand, Operand, Operand),
    Xnor(Operand, Operand, Operand),

    // Misc
    Cmp(Operand, Operand),
    Jmp(usize),
    Jeq(usize),
    Jne(usize),
    Jg(usize),
    Jge(usize),
    Jl(usize),
    Jle(usize),

    Hlt,
}

impl From<&Inst> for ResolvedInst {
    // The most painful damn function ever to write
    fn from(value: &Inst) -> Self {
        match value {
            // Quantum instructions
            Inst::Qsel(q) => Self::Qsel(*q),
            Inst::Id(q) => Self::Id(*q),
            Inst::Hadamard(q) => Self::Hadamard(*q),
            Inst::Cnot(q1, q2) => Self::Cnot(*q1, *q2),
            Inst::Ccnot(q1, q2, q3) => Self::Ccnot(*q1, *q2, *q3),
            Inst::X(q) => Self::X(*q),
            Inst::Y(q) => Self::Y(*q),
            Inst::Z(q) => Self::Z(*q),
            Inst::Rx(q, r) => Self::Rx(*q, r.clone()),
            Inst::Ry(q, r) => Self::Ry(*q, r.clone()),
            Inst::Rz(q, r) => Self::Rz(*q, r.clone()),
            Inst::U(q, r1, r2, r3) => Self::U(*q, r1.clone(), r2.clone(), r3.clone()),
            Inst::S(q) => Self::S(*q),
            Inst::T(q) => Self::T(*q),
            Inst::Sdg(q) => Self::Sdg(*q),
            Inst::Tdg(q) => Self::Tdg(*q),
            Inst::Phase(q, r) => Self::Phase(*q, r.clone()),
            Inst::Ch(q1, q2) => Self::Ch(*q1, *q2),
            Inst::Cy(q1, q2) => Self::Cy(*q1, *q2),
            Inst::Cz(q1, q2) => Self::Cz(*q1, *q2),
            Inst::CPhase(q1, q2, r) => Self::CPhase(*q1, *q2, r.clone()),
            Inst::Swap(q1, q2) => Self::Swap(*q1, *q2),
            Inst::SqrtX(q) => Self::SqrtX(*q),
            Inst::SqrtSwap(q1, q2) => Self::SqrtSwap(*q1, *q2),
            Inst::CSwap(q1, q2, q3) => Self::CSwap(*q1, *q2, *q3),
            Inst::Measure(q, cr, cb) => Self::Measure(*q, *cr, *cb),

            // Custom gate stuff
            Inst::Custom(name, qbits, args) => {
                Self::Custom(name.clone(), qbits.clone(), args.clone())
            }

            // Classical Instructions
            Inst::Mov(cr1, val) => Self::Mov(cr1.clone(), val.clone()),
            Inst::Add(cr1, cr2, cr3) => Self::Add(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::Sub(cr1, cr2, cr3) => Self::Sub(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::Mul(cr1, cr2, cr3) => Self::Mul(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::UMul(cr1, cr2, cr3) => Self::UMul(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::Div(cr1, cr2, cr3) => Self::Div(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::SMul(cr1, cr2, cr3) => Self::SMul(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::SUMul(cr1, cr2, cr3) => Self::SUMul(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::SDiv(cr1, cr2, cr3) => Self::SDiv(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::Not(cr1, cr2) => Self::Not(cr1.clone(), cr2.clone()),
            Inst::And(cr1, cr2, cr3) => Self::And(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::Or(cr1, cr2, cr3) => Self::Or(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::Xor(cr1, cr2, cr3) => Self::Xor(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::Nand(cr1, cr2, cr3) => Self::Nand(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::Nor(cr1, cr2, cr3) => Self::Nor(cr1.clone(), cr2.clone(), cr3.clone()),
            Inst::Xnor(cr1, cr2, cr3) => Self::Xnor(cr1.clone(), cr2.clone(), cr3.clone()),

            // Misc
            Inst::Cmp(cr1, val) => Self::Cmp(cr1.clone(), val.clone()),
            Inst::Hlt => Self::Hlt,

            _ => unreachable!(), /* Atleast hopefully unreachable if I didn't mess up stuff */
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

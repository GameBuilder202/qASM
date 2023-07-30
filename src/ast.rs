use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Program {
    // (qbits, cbits, qregs, cregs)
    pub headers: (u16, u16, usize, usize),
    pub instructions: Vec<ResolvedInst>,
}

pub fn resolve_ast(headers: (u16, u16, usize, usize), ast: Vec<Inst>) -> Program {
    let mut label_map = HashMap::new();
    let mut resolved_insts: Vec<ResolvedInst> = Vec::new();

    // Get all labels from the AST
    for (i, inst) in ast.iter().enumerate() {
        if let Inst::Label(name) = inst {
            label_map.insert(name, i);
        }
    }

    // Update all instructions with the label map
    for inst in ast {
        match inst {
            Inst::Label(_) => {}

            default => resolved_insts.push(default.into()),
        }
    }

    Program {
        headers,
        instructions: resolved_insts,
    }
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

    // Classical Instructions
    Add(usize, Operand, Operand),
    Sub(usize, Operand, Operand),
    Mul(usize, Operand, Operand),
    UMul(usize, Operand, Operand),
    Div(usize, Operand, Operand),
    SMul(usize, Operand, Operand),
    SUMul(usize, Operand, Operand),
    SDiv(usize, Operand, Operand),
    Not(usize, Operand),
    And(usize, Operand, Operand),
    Or(usize, Operand, Operand),
    Xor(usize, Operand, Operand),
    Nand(usize, Operand, Operand),
    Nor(usize, Operand, Operand),
    Xnor(usize, Operand, Operand),

    // Misc
    Label(String),
    Hlt,
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

    // Classical Instructions
    Add(usize, Operand, Operand),
    Sub(usize, Operand, Operand),
    Mul(usize, Operand, Operand),
    UMul(usize, Operand, Operand),
    Div(usize, Operand, Operand),
    SMul(usize, Operand, Operand),
    SUMul(usize, Operand, Operand),
    SDiv(usize, Operand, Operand),
    Not(usize, Operand),
    And(usize, Operand, Operand),
    Or(usize, Operand, Operand),
    Xor(usize, Operand, Operand),
    Nand(usize, Operand, Operand),
    Nor(usize, Operand, Operand),
    Xnor(usize, Operand, Operand),

    // Misc
    Hlt,
}

impl From<Inst> for ResolvedInst {
    // The most painful damn function ever to write
    fn from(value: Inst) -> Self {
        match value {
            Inst::Qsel(q) => Self::Qsel(q),
            Inst::Id(q) => Self::Id(q),
            Inst::Hadamard(q) => Self::Hadamard(q),
            Inst::Cnot(q1, q2) => Self::Cnot(q1, q2),
            Inst::Ccnot(q1, q2, q3) => Self::Ccnot(q1, q2, q3),
            Inst::X(q) => Self::X(q),
            Inst::Y(q) => Self::Y(q),
            Inst::Z(q) => Self::Z(q),
            Inst::Rx(q, r) => Self::Rx(q, r),
            Inst::Ry(q, r) => Self::Ry(q, r),
            Inst::Rz(q, r) => Self::Rz(q, r),
            Inst::U(q, r1, r2, r3) => Self::U(q, r1, r2, r3),
            Inst::S(q) => Self::S(q),
            Inst::T(q) => Self::T(q),
            Inst::Sdg(q) => Self::Sdg(q),
            Inst::Tdg(q) => Self::Tdg(q),
            Inst::Phase(q, r) => Self::Phase(q, r),
            Inst::Ch(q1, q2) => Self::Ch(q1, q2),
            Inst::Cy(q1, q2) => Self::Cy(q1, q2),
            Inst::Cz(q1, q2) => Self::Cz(q1, q2),
            Inst::CPhase(q1, q2, r) => Self::CPhase(q1, q2, r),
            Inst::Swap(q1, q2) => Self::Swap(q1, q2),
            Inst::SqrtX(q) => Self::SqrtX(q),
            Inst::SqrtSwap(q1, q2) => Self::SqrtSwap(q1, q2),
            Inst::CSwap(q1, q2, q3) => Self::CSwap(q1, q2, q3),
            Inst::Measure(q, cr, cb) => Self::Measure(q, cr, cb),
            Inst::Add(cr1, cr2, cr3) => Self::Add(cr1, cr2, cr3),
            Inst::Sub(cr1, cr2, cr3) => Self::Sub(cr1, cr2, cr3),
            Inst::Mul(cr1, cr2, cr3) => Self::Mul(cr1, cr2, cr3),
            Inst::UMul(cr1, cr2, cr3) => Self::UMul(cr1, cr2, cr3),
            Inst::Div(cr1, cr2, cr3) => Self::Div(cr1, cr2, cr3),
            Inst::SMul(cr1, cr2, cr3) => Self::SMul(cr1, cr2, cr3),
            Inst::SUMul(cr1, cr2, cr3) => Self::SUMul(cr1, cr2, cr3),
            Inst::SDiv(cr1, cr2, cr3) => Self::SDiv(cr1, cr2, cr3),
            Inst::Not(cr1, cr2) => Self::Not(cr1, cr2),
            Inst::And(cr1, cr2, cr3) => Self::And(cr1, cr2, cr3),
            Inst::Or(cr1, cr2, cr3) => Self::Or(cr1, cr2, cr3),
            Inst::Xor(cr1, cr2, cr3) => Self::Xor(cr1, cr2, cr3),
            Inst::Nand(cr1, cr2, cr3) => Self::Nand(cr1, cr2, cr3),
            Inst::Nor(cr1, cr2, cr3) => Self::Nor(cr1, cr2, cr3),
            Inst::Xnor(cr1, cr2, cr3) => Self::Xnor(cr1, cr2, cr3),
            Inst::Hlt => Self::Hlt,

            _ => unreachable!(), /* Atleast hopefully unreachable if I didn't mess up stuff */
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    Imm(i64),
    Reg(usize),
}

#[derive(Debug, Clone, Copy)]
pub struct Rotation(pub i64, pub u64);
impl Rotation {
    pub fn get_angle(&self) -> f64 {
        (self.0 as f64) * std::f64::consts::PI / (self.1 as f64)
    }
}

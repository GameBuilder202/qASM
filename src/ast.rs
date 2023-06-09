#[derive(Debug, Clone)]
pub struct Program {
    // (qbits, cbits, qregs, cregs)
    pub headers: (u16, u16, usize, usize),
    pub instructions: Vec<Inst>,
}

#[derive(Debug, Clone, Copy)]
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

    Hlt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    Imm(i64),
    Reg(usize),
}

#[derive(Debug, Clone, Copy)]
pub struct Rotation(pub u64, pub u64);
impl Rotation {
    pub fn get_angle(&self) -> f64 {
        (self.0 as f64) * std::f64::consts::PI / (self.1 as f64)
    }
}

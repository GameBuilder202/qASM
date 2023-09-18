use std::cmp::Ordering;
use std::f64::consts::{
    FRAC_1_SQRT_2 as ONE_BY_ROOT_2, FRAC_PI_2 as PI_BY_2, FRAC_PI_4 as PI_BY_4,
};
use std::fmt::Display;
use std::num::Wrapping;

use lazy_static::lazy_static;

use num_traits::{One, Zero};

use nalgebra::{dmatrix, DMatrix};
use nalgebra::{Complex, ComplexField};
use nalgebra::{DVector, Vector2, Vector4};

use bitvec::prelude::{BitArr, BitArray, Lsb0};

use rand::Rng;
use rand::{rngs::ThreadRng, thread_rng};

use crate::ast::*;

type Complexf64 = Complex<f64>;
type DCf64Mat = DMatrix<Complexf64>;

lazy_static! {
    static ref MAT_2_IDENTITY: DCf64Mat = dmatrix![
        Complex::one(), Complex::zero();
        Complex::zero(), Complex::one();
    ];
    static ref HADAMARD_MAT: DCf64Mat = dmatrix![
        Complex::from(ONE_BY_ROOT_2), Complex::from(ONE_BY_ROOT_2);
        Complex::from(ONE_BY_ROOT_2), Complex::from(-ONE_BY_ROOT_2);
    ];
    static ref PAULI_X_MAT: DCf64Mat = dmatrix![
        Complex::zero(), Complex::one();
        Complex::one(), Complex::zero();
    ];
    static ref PAULI_Y_MAT: DCf64Mat = dmatrix![
        Complex::zero(), -Complex::i();
        Complex::i(), Complex::zero();
    ];
    static ref PAULI_Z_MAT: DCf64Mat = dmatrix![
        Complex::one(), Complex::zero();
        Complex::zero(), -Complex::one();
    ];
    static ref S_MAT: DCf64Mat = r1(PI_BY_2);
    static ref T_MAT: DCf64Mat = r1(PI_BY_4);
    static ref S_DG_MAT: DCf64Mat = S_MAT.adjoint();
    static ref T_DG_MAT: DCf64Mat = T_MAT.adjoint();
    static ref SQRT_X_MAT: DCf64Mat = dmatrix![
        Complex::new(0.5, 0.5), Complex::new(0.5, -0.5);
        Complex::new(0.5, -0.5), Complex::new(0.5, 0.5);
    ];
    static ref SQRT_SWAP_MAT: DCf64Mat = dmatrix![
        Complex::one(), Complex::zero(), Complex::zero(), Complex::zero();
        Complex::zero(), Complex::new(0.5, 0.5), Complex::new(0.5, -0.5), Complex::zero();
        Complex::zero(), Complex::new(0.5, -0.5), Complex::new(0.5, 0.5), Complex::zero();
        Complex::zero(), Complex::zero(), Complex::zero(), Complex::one();
    ];
}

#[inline]
fn u(theta: f64, phi: f64, lambda: f64) -> DCf64Mat {
    let half_theta = theta / 2f64;
    dmatrix![
        Complex::from(half_theta.cos()), -half_theta.sin() * Complex::new(lambda.cos(), lambda.sin());
        half_theta.sin() * Complex::new(phi.cos(), phi.sin()), half_theta.cos() * Complex::new((lambda + phi).cos(), (lambda + phi).sin());
    ]
}
#[inline]
fn rx(theta: f64) -> DCf64Mat {
    u(theta, -PI_BY_2, PI_BY_2)
}
#[inline]
fn ry(theta: f64) -> DCf64Mat {
    u(theta, 0f64, 0f64)
}
#[inline]
fn rz(theta: f64) -> DCf64Mat {
    let half_theta = theta / 2f64;
    // If only i could do this
    // Complex::new(half_theta.cos(), -half_theta.sin()) * r1(theta)
    dmatrix![
        Complex::new(half_theta.cos(), -half_theta.sin()), Complex::zero();
        Complex::zero(), Complex::new(half_theta.cos(), half_theta.sin());
    ]
}
#[inline]
fn r1(theta: f64) -> DCf64Mat {
    u(0f64, theta, 0f64)
}

pub struct Emulator<'a> {
    prog: &'a Program,

    qbits: u16,
    cbits: u16,

    qregs: Vec<DVector<Complexf64>>,
    cregs: Vec<Word>,

    pc: usize,
    qreg_sel: usize,
    flags: BitArr!(for 3, in u8, Lsb0),

    rng: ThreadRng,
}

impl<'a> Emulator<'a> {
    pub fn new(prog: &'a Program) -> Self {
        let qbits = prog.headers.0;
        let cbits = prog.headers.1;
        let qregs = prog.headers.2;
        let cregs = prog.headers.3;

        Self {
            prog,
            qbits,
            cbits,

            qregs: vec![
                {
                    let mut dvec = DVector::zeros(1 << qbits);
                    dvec[0] = Complex::one();
                    dvec
                };
                qregs
            ],
            cregs: vec![
                Word {
                    bits: cbits,
                    data: Wrapping(0),
                };
                cregs
            ],

            pc: 0,
            qreg_sel: 0,
            flags: BitArray::ZERO,

            rng: thread_rng(),
        }
    }

    pub fn reset(&mut self) {
        *self = Self {
            prog: self.prog,
            qbits: self.qbits,
            cbits: self.cbits,

            qregs: vec![
                {
                    let mut dvec = DVector::zeros(1 << self.qbits);
                    dvec[0] = Complex::one();
                    dvec
                };
                self.qregs.len()
            ],
            cregs: vec![
                Word {
                    bits: self.cbits,
                    data: Wrapping(0)
                };
                self.cregs.len()
            ],

            pc: 0,
            qreg_sel: 0,
            flags: BitArray::ZERO,

            rng: thread_rng(),
        }
    }

    pub fn run(&mut self) -> Result<(), EmulatorError> {
        loop {
            let res = self.step()?;
            if res == StepResult::Halted {
                break;
            }

            if res == StepResult::Continue {
                self.pc += 1
            }
        }
        Ok(())
    }

    fn step(&mut self) -> Result<StepResult, EmulatorError> {
        let Some(inst) = self.prog.instructions.get(self.pc) else {
            return Err(EmulatorError::PCOutOfBounds)
        };

        match *inst {
            // Quantum Instructions
            ResolvedInst::Qsel(qreg) => {
                if qreg > self.qregs.len() {
                    return Err(EmulatorError::QregIndexOutOfBounds);
                }
                self.qreg_sel = qreg
            }
            ResolvedInst::Id(qbit) => self.apply_mat_1(&MAT_2_IDENTITY, qbit),
            ResolvedInst::Hadamard(qbit) => self.apply_mat_1(&HADAMARD_MAT, qbit),
            ResolvedInst::Cnot(qbit1, qbit2) => {
                let qreg = &mut self.qregs[self.qreg_sel];

                let qbit1_mask = 1 << qbit1;
                let qbit2_mask = 1 << qbit2;

                for state in 0..(1 << self.qbits) {
                    if state & qbit1_mask == 0 || state & qbit2_mask != 0 {
                        continue;
                    }
                    qreg.swap_rows(state, state | qbit2_mask)
                }
            }
            ResolvedInst::Ccnot(qbit1, qbit2, qbit3) => {
                let qreg = &mut self.qregs[self.qreg_sel];

                let qbit1_mask = 1 << qbit1;
                let qbit2_mask = 1 << qbit2;
                let qbit3_mask = 1 << qbit3;

                for state in 0..(1 << self.qbits) {
                    if state & qbit1_mask == 0 || state & qbit2_mask == 0 || state & qbit3_mask != 0
                    {
                        continue;
                    }
                    qreg.swap_rows(state, state | qbit3_mask)
                }
            }
            ResolvedInst::X(qbit) => self.apply_mat_1(&PAULI_X_MAT, qbit),
            ResolvedInst::Y(qbit) => self.apply_mat_1(&PAULI_Y_MAT, qbit),
            ResolvedInst::Z(qbit) => self.apply_mat_1(&PAULI_Z_MAT, qbit),
            ResolvedInst::Rx(qbit, rot) => self.apply_mat_1(&rx(rot.get_angle()), qbit),
            ResolvedInst::Ry(qbit, rot) => self.apply_mat_1(&ry(rot.get_angle()), qbit),
            ResolvedInst::Rz(qbit, rot) => self.apply_mat_1(&rz(rot.get_angle()), qbit),
            ResolvedInst::U(qbit, theta, phi, lambda) => self.apply_mat_1(
                &u(theta.get_angle(), phi.get_angle(), lambda.get_angle()),
                qbit,
            ),
            ResolvedInst::S(qbit) => self.apply_mat_1(&S_MAT, qbit),
            ResolvedInst::T(qbit) => self.apply_mat_1(&T_MAT, qbit),
            ResolvedInst::Sdg(qbit) => self.apply_mat_1(&S_DG_MAT, qbit),
            ResolvedInst::Tdg(qbit) => self.apply_mat_1(&T_DG_MAT, qbit),
            ResolvedInst::Phase(qbit, rot) => self.apply_mat_1(&r1(rot.get_angle()), qbit),
            ResolvedInst::Ch(qbit1, qbit2) => {
                self.apply_controlled_mat(&HADAMARD_MAT, vec![qbit1], qbit2)
            }
            ResolvedInst::Cy(qbit1, qbit2) => {
                self.apply_controlled_mat(&PAULI_Y_MAT, vec![qbit1], qbit2)
            }
            ResolvedInst::Cz(qbit1, qbit2) => {
                self.apply_controlled_mat(&PAULI_Z_MAT, vec![qbit1], qbit2)
            }
            ResolvedInst::CPhase(qbit1, qbit2, rot) => {
                self.apply_controlled_mat(&r1(rot.get_angle()), vec![qbit1], qbit2)
            }
            ResolvedInst::Swap(qbit1, qbit2) => {
                let qreg = &mut self.qregs[self.qreg_sel];

                let qbit1_mask = 1 << qbit1;
                let qbit2_mask = 1 << qbit2;

                for state in 0..(1 << self.qbits) {
                    if state & qbit1_mask != 0 || state & qbit2_mask != 0 {
                        continue;
                    }
                    qreg.swap_rows(state | qbit1_mask, state | qbit2_mask)
                }
            }
            ResolvedInst::SqrtX(qbit) => self.apply_mat_1(&SQRT_X_MAT, qbit),
            ResolvedInst::SqrtSwap(qbit1, qbit2) => self.apply_mat_2(&SQRT_SWAP_MAT, qbit1, qbit2),
            ResolvedInst::CSwap(qbit1, qbit2, qbit3) => {
                let qreg = &mut self.qregs[self.qreg_sel];

                let qbit1_mask = 1 << qbit1;
                let qbit2_mask = 1 << qbit2;
                let qbit3_mask = 1 << qbit3;

                for state in 0..(1 << self.qbits) {
                    if state & qbit1_mask == 0 || state & qbit2_mask != 0 || state & qbit3_mask != 0
                    {
                        continue;
                    }
                    qreg.swap_rows(state | qbit2_mask, state | qbit3_mask)
                }
            }

            ResolvedInst::Measure(qbit, creg, cbit) => {
                let qreg = &mut self.qregs[self.qreg_sel];
                let qbit_mask = 1 << qbit;

                let mut prob = Complexf64::zero();

                for state in 0..(1 << self.qbits) {
                    if state & qbit_mask != 0 {
                        continue;
                    }

                    let amplitude = qreg[state];
                    prob += amplitude * amplitude
                }

                if prob.im != 0f64 {
                    panic!("Invalid probability for qubit {}", qbit)
                }
                let prob = prob.re;
                if prob == 0f64 || prob == 1f64 {
                    self.cregs[creg].set_bit(cbit, prob == 0f64);
                    return Ok(StepResult::Continue);
                }
                let rand = self.rng.gen::<f64>();

                if rand < prob {
                    for state in 0..(1 << self.qbits) {
                        if state & qbit_mask == 0 {
                            continue;
                        }
                        qreg[state] = Complex::zero()
                    }
                    self.cregs[creg].set_bit(cbit, false)
                } else {
                    for state in 0..(1 << self.qbits) {
                        if state & qbit_mask != 0 {
                            continue;
                        }
                        qreg[state] = Complex::zero()
                    }
                    self.cregs[creg].set_bit(cbit, true)
                }

                let sqrtsumsq = {
                    let sumsq = qreg.iter().map(|x| x * x).sum::<Complexf64>();
                    sumsq.sqrt()
                };
                for x in qreg.iter_mut() {
                    *x /= sqrtsumsq
                }
            }

            // Classical Instructions
            ResolvedInst::Mov(r1, val) => {
                let val = self.get_val(val);
                self.cregs[r1].set_to(val)
            }
            ResolvedInst::Add(r1, r2, r3) => {
                let val1 = self.get_val(r2);
                let val2 = self.get_val(r3);
                self.cregs[r1].set_to(val1 + val2)
            }
            ResolvedInst::Sub(r1, r2, r3) => {
                let val1 = self.get_val(r2);
                let val2 = self.get_val(r3);
                self.cregs[r1].set_to(val1 - val2)
            }
            ResolvedInst::Mul(r1, r2, r3) => {
                let val1 = self.get_val(r2).0 as u64;
                let val2 = self.get_val(r3).0 as u64;
                let val = val1.wrapping_mul(val2) as i64;
                self.cregs[r1].set_to(Wrapping(val))
            }
            ResolvedInst::UMul(r1, r2, r3) => {
                let val1 = self.get_val(r2).0 as u64;
                let val2 = self.get_val(r3).0 as u64;
                let val = val1.wrapping_mul(val2) as i64;
                self.cregs[r1].set_to(Wrapping(val >> 32))
            }
            ResolvedInst::Div(r1, r2, r3) => {
                let val1 = self.get_val(r2).0 as u64;
                let val2 = self.get_val(r3).0 as u64;
                let val = (val1 / val2) as i64;
                self.cregs[r1].set_to(Wrapping(val))
            }
            ResolvedInst::SMul(r1, r2, r3) => {
                let val1 = self.get_val(r2);
                let val2 = self.get_val(r3);
                self.cregs[r1].set_to(val1 * val2)
            }
            ResolvedInst::SUMul(r1, r2, r3) => {
                let val1 = self.get_val(r2);
                let val2 = self.get_val(r3);
                self.cregs[r1].set_to((val1 * val2) >> 32)
            }
            ResolvedInst::SDiv(r1, r2, r3) => {
                let val1 = self.get_val(r2);
                let val2 = self.get_val(r3);
                self.cregs[r1].set_to(val1 / val2)
            }
            ResolvedInst::Not(r1, r2) => {
                let val1 = self.get_val(r2);
                self.cregs[r1].set_to(!val1)
            }
            ResolvedInst::And(r1, r2, r3) => {
                let val1 = self.get_val(r2);
                let val2 = self.get_val(r3);
                self.cregs[r1].set_to(val1 & val2)
            }
            ResolvedInst::Or(r1, r2, r3) => {
                let val1 = self.get_val(r2);
                let val2 = self.get_val(r3);
                self.cregs[r1].set_to(val1 | val2)
            }
            ResolvedInst::Xor(r1, r2, r3) => {
                let val1 = self.get_val(r2);
                let val2 = self.get_val(r3);
                self.cregs[r1].set_to(val1 ^ val2)
            }
            ResolvedInst::Nand(r1, r2, r3) => {
                let val1 = self.get_val(r2);
                let val2 = self.get_val(r3);
                self.cregs[r1].set_to(!(val1 & val2))
            }
            ResolvedInst::Nor(r1, r2, r3) => {
                let val1 = self.get_val(r2);
                let val2 = self.get_val(r3);
                self.cregs[r1].set_to(!(val1 | val2))
            }
            ResolvedInst::Xnor(r1, r2, r3) => {
                let val1 = self.get_val(r2);
                let val2 = self.get_val(r3);
                self.cregs[r1].set_to(!(val1 ^ val2))
            }

            // Misc
            ResolvedInst::Cmp(r1, val) => {
                let cmp = self.cregs[r1].get_val().cmp(&self.get_val(val));
                match cmp {
                    Ordering::Less => {
                        self.flags.set(0, true);
                        self.flags.set(1, false);
                        self.flags.set(2, false);
                    }
                    Ordering::Equal => {
                        self.flags.set(0, false);
                        self.flags.set(1, true);
                        self.flags.set(2, false);
                    }
                    Ordering::Greater => {
                        self.flags.set(0, false);
                        self.flags.set(1, false);
                        self.flags.set(2, true);
                    }
                }
            }
            ResolvedInst::Jmp(offset) => {
                self.pc = offset;
                return Ok(StepResult::Branched);
            }
            ResolvedInst::Jeq(offset) => {
                if self.flags[1] {
                    self.pc = offset;
                    return Ok(StepResult::Branched);
                }
            }
            ResolvedInst::Jne(offset) => {
                if !self.flags[1] {
                    self.pc = offset;
                    return Ok(StepResult::Branched);
                }
            }
            ResolvedInst::Jg(offset) => {
                if self.flags[2] {
                    self.pc = offset;
                    return Ok(StepResult::Branched);
                }
            }
            ResolvedInst::Jge(offset) => {
                if self.flags[1] || self.flags[2] {
                    self.pc = offset;
                    return Ok(StepResult::Branched);
                }
            }
            ResolvedInst::Jl(offset) => {
                if self.flags[0] {
                    self.pc = offset;
                    return Ok(StepResult::Branched);
                }
            }
            ResolvedInst::Jle(offset) => {
                if self.flags[0] || self.flags[1] {
                    self.pc = offset;
                    return Ok(StepResult::Branched);
                }
            }

            ResolvedInst::Hlt => return Ok(StepResult::Halted),
        }

        Ok(StepResult::Continue)
    }

    pub fn get_cregs_state(&self) -> &Vec<Word> {
        &self.cregs
    }

    fn get_val(&self, thing: Operand) -> Wrapping<i64> {
        match thing {
            Operand::Imm(imm) => Wrapping(imm),
            Operand::Reg(reg) => self.cregs[reg].get_val(),
        }
    }

    fn apply_mat_1(&mut self, mat: &DCf64Mat, qbit: usize) {
        let qreg = &mut self.qregs[self.qreg_sel];

        let mut in_gate_state = Vector2::zeros();
        let mut out_gate_state = Vector2::zeros();

        let qbit_mask = 1 << qbit;

        for state in 0..(1 << self.qbits) {
            if state & qbit_mask != 0 {
                continue;
            }

            in_gate_state[0] = qreg[state];
            in_gate_state[1] = qreg[state | qbit_mask];

            mat.mul_to(&in_gate_state, &mut out_gate_state);

            qreg[state] = out_gate_state[0];
            qreg[state | qbit_mask] = out_gate_state[1];
        }
    }

    fn apply_mat_2(&mut self, mat: &DCf64Mat, qbit1: usize, qbit2: usize) {
        let qreg = &mut self.qregs[self.qreg_sel];

        let mut in_gate_state = Vector4::zeros();
        let mut out_gate_state = Vector4::zeros();

        let qbit1_mask = 1 << qbit1;
        let qbit2_mask = 1 << qbit2;

        for state in 0..(1 << self.qbits) {
            if state & qbit1_mask != 0 || state & qbit2_mask != 0 {
                continue;
            }

            in_gate_state[0] = qreg[state];
            in_gate_state[1] = qreg[state | qbit1_mask];
            in_gate_state[2] = qreg[state | qbit2_mask];
            in_gate_state[3] = qreg[state | qbit1_mask | qbit2_mask];

            mat.mul_to(&in_gate_state, &mut out_gate_state);

            qreg[state] = out_gate_state[0];
            qreg[state | qbit1_mask] = out_gate_state[1];
            qreg[state | qbit2_mask] = out_gate_state[2];
            qreg[state | qbit1_mask | qbit2_mask] = out_gate_state[3];
        }
    }

    fn apply_controlled_mat(&mut self, mat: &DCf64Mat, controls: Vec<usize>, target: usize) {
        let qreg = &mut self.qregs[self.qreg_sel];

        let controls_mask = controls
            .iter()
            .map(|control| 1 << control)
            .collect::<Vec<_>>();
        let target_mask = 1 << target;

        let mut in_gate_state = Vector2::zeros();
        let mut out_gate_state = Vector2::zeros();

        for state in 0..(1 << self.qbits) {
            if controls_mask
                .iter()
                .map(|mask| state & mask == 0)
                .any(|b| b)
                || state & target_mask != 0
            {
                continue;
            }

            in_gate_state[0] = qreg[state];
            in_gate_state[1] = qreg[state | target_mask];

            mat.mul_to(&in_gate_state, &mut out_gate_state);

            qreg[state] = out_gate_state[0];
            qreg[state | target_mask] = out_gate_state[1];
        }
    }
}

impl<'a> Display for Emulator<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Emulator state: (qbits={}, cbits={})",
            self.qbits, self.cbits
        )?;
        writeln!(f, "qregs=[")?;
        for qreg in &self.qregs {
            write!(f, "{}", qreg)?
        }
        write!(f, "]")?;

        Ok(())
    }
}

#[derive(Clone, Copy)]
pub struct Word {
    pub bits: u16,
    pub data: Wrapping<i64>,
}

impl Word {
    pub fn get_val(&self) -> Wrapping<i64> {
        self.data
    }

    pub fn set_to(&mut self, val: Wrapping<i64>) {
        self.data = val & Wrapping(((1 << self.bits) as i64).wrapping_sub(1))
    }

    pub fn set_bit(&mut self, bit: usize, val: bool) {
        self.set_to(self.data & Wrapping(!(1 << bit)) | Wrapping((val as i64) << bit))
    }
}

impl std::fmt::Debug for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)?;
        Ok(())
    }
}

impl Display for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_val())?;
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StepResult {
    Halted,
    Continue,
    Branched,
}

#[derive(Debug, Clone, Copy)]
pub enum EmulatorError {
    PCOutOfBounds,
    QregIndexOutOfBounds,
}

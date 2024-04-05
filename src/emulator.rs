use std::cmp::Ordering;
use std::collections::HashMap;
use std::f64::consts::{
    FRAC_1_SQRT_2 as ONE_BY_ROOT_2, FRAC_PI_2 as PI_BY_2, FRAC_PI_4 as PI_BY_4,
};
use std::fmt::Display;
use std::num::Wrapping;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use lazy_static::lazy_static;

use num_traits::{One, Zero};

use nalgebra::{matrix, SMatrix};
use nalgebra::{Complex, ComplexField};
use nalgebra::{DVector, Vector2, Vector4};

use bitvec::prelude::{BitArr, BitArray, Lsb0};

use rand::Rng;
use rand::{rngs::ThreadRng, thread_rng};

use crate::ast::*;

type Complexf64 = Complex<f64>;
type DCf64Mat2 = SMatrix<Complexf64, 2, 2>;
type DCf64Mat4 = SMatrix<Complexf64, 4, 4>;

lazy_static! {
    static ref MAT_2_IDENTITY: DCf64Mat2 = matrix![
        Complex::one(), Complex::zero();
        Complex::zero(), Complex::one();
    ];
    static ref HADAMARD_MAT: DCf64Mat2 = matrix![
        Complex::from(ONE_BY_ROOT_2), Complex::from(ONE_BY_ROOT_2);
        Complex::from(ONE_BY_ROOT_2), Complex::from(-ONE_BY_ROOT_2);
    ];
    static ref PAULI_X_MAT: DCf64Mat2 = matrix![
        Complex::zero(), Complex::one();
        Complex::one(), Complex::zero();
    ];
    static ref PAULI_Y_MAT: DCf64Mat2 = matrix![
        Complex::zero(), -Complex::i();
        Complex::i(), Complex::zero();
    ];
    static ref PAULI_Z_MAT: DCf64Mat2 = matrix![
        Complex::one(), Complex::zero();
        Complex::zero(), -Complex::one();
    ];
    static ref S_MAT: DCf64Mat2 = r1(PI_BY_2);
    static ref T_MAT: DCf64Mat2 = r1(PI_BY_4);
    static ref S_DG_MAT: DCf64Mat2 = S_MAT.adjoint();
    static ref T_DG_MAT: DCf64Mat2 = T_MAT.adjoint();
    static ref SQRT_X_MAT: DCf64Mat2 = matrix![
        Complex::new(0.5, 0.5), Complex::new(0.5, -0.5);
        Complex::new(0.5, -0.5), Complex::new(0.5, 0.5);
    ];
    static ref SQRT_SWAP_MAT: DCf64Mat4 = matrix![
        Complex::one(), Complex::zero(), Complex::zero(), Complex::zero();
        Complex::zero(), Complex::new(0.5, 0.5), Complex::new(0.5, -0.5), Complex::zero();
        Complex::zero(), Complex::new(0.5, -0.5), Complex::new(0.5, 0.5), Complex::zero();
        Complex::zero(), Complex::zero(), Complex::zero(), Complex::one();
    ];
}

#[inline]
fn u(theta: f64, phi: f64, lambda: f64) -> DCf64Mat2 {
    let half_theta = theta / 2f64;
    matrix![
        Complex::from(half_theta.cos()), -half_theta.sin() * Complex::new(lambda.cos(), lambda.sin());
        half_theta.sin() * Complex::new(phi.cos(), phi.sin()), half_theta.cos() * Complex::new((lambda + phi).cos(), (lambda + phi).sin());
    ]
}
#[inline]
fn rx(theta: f64) -> DCf64Mat2 {
    u(theta, -PI_BY_2, PI_BY_2)
}
#[inline]
fn ry(theta: f64) -> DCf64Mat2 {
    u(theta, 0f64, 0f64)
}
#[inline]
fn rz(theta: f64) -> DCf64Mat2 {
    let half_theta = theta / 2f64;
    // If only i could do this
    // Complex::new(half_theta.cos(), -half_theta.sin()) * r1(theta)
    matrix![
        Complex::new(half_theta.cos(), -half_theta.sin()), Complex::zero();
        Complex::zero(), Complex::new(half_theta.cos(), half_theta.sin());
    ]
}
#[inline]
fn r1(theta: f64) -> DCf64Mat2 {
    u(0f64, theta, 0f64)
}

pub struct Emulator<'a> {
    prog: &'a Program,

    qbits: usize,
    cbits: usize,

    qregs: Vec<DVector<Complexf64>>,
    cregs: Vec<Word>,

    pc: usize,
    prev_pc: usize,
    qreg_sel: usize,
    flags: BitArr!(for 3, in u8, Lsb0),
    mem: Vec<Word>,

    rng: ThreadRng,
}

impl<'a> Emulator<'a> {
    pub fn new(prog: &'a Program) -> Self {
        let qbits = prog.headers.0;
        let cbits = prog.headers.1;
        let qregs = prog.headers.2;
        let cregs = prog.headers.3;
        let mem_size = prog.headers.4;

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
            prev_pc: 0,
            qreg_sel: 0,
            flags: BitArray::ZERO,
            mem: vec![
                Word {
                    bits: cbits,
                    data: Wrapping(0)
                };
                mem_size
            ],

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
            prev_pc: 0,
            qreg_sel: 0,
            flags: BitArray::ZERO,
            mem: vec![
                Word {
                    bits: self.cbits,
                    data: Wrapping(0)
                };
                self.mem.len()
            ],

            rng: thread_rng(),
        }
    }

    pub fn run(&mut self) -> Result<(), Diagnostic<usize>> {
        loop {
            let res = self.step()?;

            self.prev_pc = self.pc;
            if res == StepResult::Halted {
                break;
            }
            if res == StepResult::Continue {
                self.pc += 1;
            } else if let StepResult::Branch(new_pc) = res {
                self.pc = new_pc
            }
        }
        Ok(())
    }

    fn step(&mut self) -> Result<StepResult, Diagnostic<usize>> {
        let Some((inst, span)) = self.prog.instructions.get(self.pc) else {
            let (_, span) = &self.prog.instructions[self.prev_pc];
            return Err(Diagnostic::error()
                .with_message("PC went out of bounds")
                .with_labels(vec![Label::primary(span.file, span.span.clone())])
                .with_notes(vec![
                    format!(
                        "Note: Current PC value is {} (changed from value {})",
                        self.pc, self.prev_pc
                    ),
                    String::from("Maybe you are missing a 'hlt' instruction?"),
                ]));
        };

        self.run_instr(
            inst,
            span,
            &(0..self.qbits).collect::<Vec<_>>(),
            &HashMap::new(),
        )
    }

    fn run_instr(
        &mut self,
        inst: &ResolvedInst,
        s: &SourceSpan,
        mapping: &[usize],
        args: &HashMap<String, IdentVal>,
    ) -> Result<StepResult, Diagnostic<usize>> {
        match inst {
            // Quantum Instructions
            ResolvedInst::Qsel(qreg) => {
                if qreg > &self.qregs.len() {
                    return Err(Diagnostic::error()
                        .with_message("qreg index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qregs is {} as declared in headers",
                            self.qregs.len()
                        )]));
                }
                self.qreg_sel = *qreg
            }
            ResolvedInst::Id(qbit) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_mat_2(&MAT_2_IDENTITY, *qbit)
            }
            ResolvedInst::Hadamard(qbit) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_mat_2(&HADAMARD_MAT, *qbit)
            }
            ResolvedInst::Cnot(qbit1, qbit2) => {
                let Some(qbit1) = mapping.get(*qbit1) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit1))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                let Some(qbit2) = mapping.get(*qbit2) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit2))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit1 > &self.qbits || qbit2 > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }

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
                let Some(qbit1) = mapping.get(*qbit1) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit1))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                let Some(qbit2) = mapping.get(*qbit2) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit2))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                let Some(qbit3) = mapping.get(*qbit3) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit3))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit1 > &self.qbits || qbit2 > &self.qbits || qbit3 > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }

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
            ResolvedInst::X(qbit) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_mat_2(&PAULI_X_MAT, *qbit)
            }
            ResolvedInst::Y(qbit) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_mat_2(&PAULI_Y_MAT, *qbit)
            }
            ResolvedInst::Z(qbit) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_mat_2(&PAULI_Z_MAT, *qbit)
            }
            ResolvedInst::Rx(qbit, rot) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                let Some(rot) = rot.get_rot(args) else {
                    return Err(Diagnostic::error()
                        .with_message("Argument passed is not a rotation")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())]));
                };
                self.apply_mat_2(&rx(rot.get_angle()), *qbit)
            }
            ResolvedInst::Ry(qbit, rot) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                let Some(rot) = rot.get_rot(args) else {
                    return Err(Diagnostic::error()
                        .with_message("Argument passed is not a rotation")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())]));
                };
                self.apply_mat_2(&ry(rot.get_angle()), *qbit)
            }
            ResolvedInst::Rz(qbit, rot) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                let Some(rot) = rot.get_rot(args) else {
                    return Err(Diagnostic::error()
                        .with_message("Argument passed is not a rotation")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())]));
                };
                self.apply_mat_2(&rz(rot.get_angle()), *qbit)
            }
            ResolvedInst::U(qbit, theta, phi, lambda) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                let Some(theta) = theta.get_rot(args) else {
                    return Err(Diagnostic::error()
                        .with_message("Argument passed is not a rotation")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())]));
                };
                let Some(phi) = phi.get_rot(args) else {
                    return Err(Diagnostic::error()
                        .with_message("Argument passed is not a rotation")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())]));
                };
                let Some(lambda) = lambda.get_rot(args) else {
                    return Err(Diagnostic::error()
                        .with_message("Argument passed is not a rotation")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())]));
                };
                self.apply_mat_2(
                    &u(theta.get_angle(), phi.get_angle(), lambda.get_angle()),
                    *qbit,
                )
            }
            ResolvedInst::S(qbit) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_mat_2(&S_MAT, *qbit)
            }
            ResolvedInst::T(qbit) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_mat_2(&T_MAT, *qbit)
            }
            ResolvedInst::Sdg(qbit) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_mat_2(&S_DG_MAT, *qbit)
            }
            ResolvedInst::Tdg(qbit) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_mat_2(&T_DG_MAT, *qbit)
            }
            ResolvedInst::Phase(qbit, rot) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                let Some(rot) = rot.get_rot(args) else {
                    return Err(Diagnostic::error()
                        .with_message("Argument passed is not a rotation")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())]));
                };
                self.apply_mat_2(&r1(rot.get_angle()), *qbit)
            }
            ResolvedInst::Ch(qbit1, qbit2) => {
                let Some(qbit1) = mapping.get(*qbit1) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit1))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                let Some(qbit2) = mapping.get(*qbit2) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit2))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit1 > &self.qbits || qbit2 > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_controlled_mat(&HADAMARD_MAT, vec![*qbit1], *qbit2)
            }
            ResolvedInst::Cy(qbit1, qbit2) => {
                let Some(qbit1) = mapping.get(*qbit1) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit1))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                let Some(qbit2) = mapping.get(*qbit2) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit2))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit1 > &self.qbits || qbit2 > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_controlled_mat(&PAULI_Y_MAT, vec![*qbit1], *qbit2)
            }
            ResolvedInst::Cz(qbit1, qbit2) => {
                let Some(qbit1) = mapping.get(*qbit1) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit1))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                let Some(qbit2) = mapping.get(*qbit2) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit2))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit1 > &self.qbits || qbit2 > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_controlled_mat(&PAULI_Z_MAT, vec![*qbit1], *qbit2)
            }
            ResolvedInst::CPhase(qbit1, qbit2, rot) => {
                let Some(qbit1) = mapping.get(*qbit1) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit1))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                let Some(qbit2) = mapping.get(*qbit2) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit2))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit1 > &self.qbits || qbit2 > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                let Some(rot) = rot.get_rot(args) else {
                    return Err(Diagnostic::error()
                        .with_message("Argument passed is not a rotation")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())]));
                };
                self.apply_controlled_mat(&r1(rot.get_angle()), vec![*qbit1], *qbit2)
            }
            ResolvedInst::Swap(qbit1, qbit2) => {
                let Some(qbit1) = mapping.get(*qbit1) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit1))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                let Some(qbit2) = mapping.get(*qbit2) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit2))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit1 > &self.qbits || qbit2 > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }

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
            ResolvedInst::SqrtX(qbit) => {
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_mat_2(&SQRT_X_MAT, *qbit)
            }
            ResolvedInst::SqrtSwap(qbit1, qbit2) => {
                let Some(qbit1) = mapping.get(*qbit1) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit1))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                let Some(qbit2) = mapping.get(*qbit2) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit2))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit1 > &self.qbits || qbit2 > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                self.apply_mat_4(&SQRT_SWAP_MAT, *qbit1, *qbit2)
            }
            ResolvedInst::CSwap(qbit1, qbit2, qbit3) => {
                let Some(qbit1) = mapping.get(*qbit1) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit1))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                let Some(qbit2) = mapping.get(*qbit2) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit2))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                let Some(qbit3) = mapping.get(*qbit3) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit3))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit1 > &self.qbits || qbit2 > &self.qbits || qbit3 > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }

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
                let Some(qbit) = mapping.get(*qbit) else {
                    return Err(Diagnostic::error()
                        .with_message(format!("qbit {} is not mapped in local scope", qbit))
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Local scope has qbits 0 to {} mapped",
                            mapping.len()
                        )]));
                };
                if qbit > &self.qbits {
                    return Err(Diagnostic::error()
                        .with_message("qbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of qbits is {} as declared in headers",
                            self.qbits
                        )]));
                }
                if creg > &self.cregs.len() {
                    return Err(Diagnostic::error()
                        .with_message("creg index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of cregs is {} as declared in headers",
                            self.cregs.len()
                        )]));
                }
                if cbit > &self.cbits {
                    return Err(Diagnostic::error()
                        .with_message("cbit index out of bounds")
                        .with_labels(vec![Label::primary(s.file, s.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of cbits is {} as declared in headers",
                            self.cbits
                        )]));
                }

                let qreg = &mut self.qregs[self.qreg_sel];
                let qbit_mask = 1 << qbit;

                let mut prob = 0f64;

                for state in 0..(1 << self.qbits) {
                    if state & qbit_mask != 0 {
                        continue;
                    }

                    let amplitude = qreg[state];
                    prob += amplitude.abs().powi(2)
                }

                if prob > 1f64 {
                    return Err(Diagnostic::error().with_message(format!(
                        "Error: qubit {} has invalid probability (prob: {})",
                        qbit, prob
                    )));
                }
                if prob == 0f64 || prob == 1f64 {
                    self.cregs[*creg].set_bit(*cbit, prob == 0f64);
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
                    self.cregs[*creg].set_bit(*cbit, false)
                } else {
                    for state in 0..(1 << self.qbits) {
                        if state & qbit_mask != 0 {
                            continue;
                        }
                        qreg[state] = Complex::zero()
                    }
                    self.cregs[*creg].set_bit(*cbit, true)
                }

                let sqrtsumsq = {
                    let sumsq = qreg.iter().map(|x| x * x).sum::<Complexf64>();
                    sumsq.sqrt()
                };
                for x in qreg.iter_mut() {
                    *x /= sqrtsumsq
                }
            }

            // Custom gate stuff
            ResolvedInst::Custom(name, qbits, args) => {
                let gate = &self.prog.custom_gates[name];
                let args_ = gate
                    .1
                    .iter()
                    .zip(args)
                    .map(|x| (x.0 .0.clone(), x.1))
                    .collect::<Vec<_>>();
                let mut args = HashMap::new();
                for arg in args_ {
                    args.insert(arg.0, arg.1.clone());
                }

                for (inst, s) in &gate.2 {
                    let res = self.run_instr(inst, s, qbits, &args);
                    if let Err(diag) = res {
                        return Err(diag.with_labels(vec![Label::secondary(
                            s.file,
                            s.span.clone(),
                        )
                        .with_message("Originating from this custom gate call")]));
                    }
                }
            }

            // Classical Instructions
            ResolvedInst::Mov(r1, val) => {
                let val = self.get_val(val, args, s)?;
                self.update(r1, val, args, s)?
            }
            ResolvedInst::Add(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?;
                let val2 = self.get_val(r3, args, s)?;
                self.update(r1, val1 + val2, args, s)?
            }
            ResolvedInst::Sub(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?;
                let val2 = self.get_val(r3, args, s)?;
                self.update(r1, val1 - val2, args, s)?
            }
            ResolvedInst::Mul(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?.0 as u64;
                let val2 = self.get_val(r3, args, s)?.0 as u64;
                self.update(r1, Wrapping(val1.wrapping_mul(val2) as i64), args, s)?
            }
            ResolvedInst::UMul(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?.0 as u64;
                let val2 = self.get_val(r3, args, s)?.0 as u64;
                self.update(r1, Wrapping(val1.wrapping_mul(val2) as i64 >> 32), args, s)?
            }
            ResolvedInst::Div(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?.0 as u64;
                let val2 = self.get_val(r3, args, s)?.0 as u64;
                self.update(r1, Wrapping((val1 / val2) as i64), args, s)?
            }
            ResolvedInst::SMul(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?;
                let val2 = self.get_val(r3, args, s)?;
                self.update(r1, val1 * val2, args, s)?
            }
            ResolvedInst::SUMul(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?;
                let val2 = self.get_val(r3, args, s)?;
                self.update(r1, (val1 * val2) >> 32, args, s)?
            }
            ResolvedInst::SDiv(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?;
                let val2 = self.get_val(r3, args, s)?;
                self.update(r1, val1 / val2, args, s)?
            }
            ResolvedInst::Not(r1, r2) => {
                let val1 = self.get_val(r2, args, s)?;
                self.update(r1, !val1, args, s)?
            }
            ResolvedInst::And(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?;
                let val2 = self.get_val(r3, args, s)?;
                self.update(r1, val1 & val2, args, s)?
            }
            ResolvedInst::Or(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?;
                let val2 = self.get_val(r3, args, s)?;
                self.update(r1, val1 | val2, args, s)?
            }
            ResolvedInst::Xor(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?;
                let val2 = self.get_val(r3, args, s)?;
                self.update(r1, val1 ^ val2, args, s)?
            }
            ResolvedInst::Nand(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?;
                let val2 = self.get_val(r3, args, s)?;
                self.update(r1, !(val1 & val2), args, s)?
            }
            ResolvedInst::Nor(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?;
                let val2 = self.get_val(r3, args, s)?;
                self.update(r1, !(val1 | val2), args, s)?
            }
            ResolvedInst::Xnor(r1, r2, r3) => {
                let val1 = self.get_val(r2, args, s)?;
                let val2 = self.get_val(r3, args, s)?;
                self.update(r1, !(val1 ^ val2), args, s)?
            }

            // Misc
            ResolvedInst::Cmp(r1, val) => {
                let cmp = self.get_val(r1, args, s)?.cmp(&self.get_val(val, args, s)?);
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
                return Ok(StepResult::Branch(*offset));
            }
            ResolvedInst::Jeq(offset) => {
                if self.flags[1] {
                    return Ok(StepResult::Branch(*offset));
                }
            }
            ResolvedInst::Jne(offset) => {
                if !self.flags[1] {
                    return Ok(StepResult::Branch(*offset));
                }
            }
            ResolvedInst::Jg(offset) => {
                if self.flags[2] {
                    return Ok(StepResult::Branch(*offset));
                }
            }
            ResolvedInst::Jge(offset) => {
                if self.flags[1] || self.flags[2] {
                    return Ok(StepResult::Branch(*offset));
                }
            }
            ResolvedInst::Jl(offset) => {
                if self.flags[0] {
                    return Ok(StepResult::Branch(*offset));
                }
            }
            ResolvedInst::Jle(offset) => {
                if self.flags[0] || self.flags[1] {
                    return Ok(StepResult::Branch(*offset));
                }
            }

            ResolvedInst::Hlt => return Ok(StepResult::Halted),
        }

        Ok(StepResult::Continue)
    }

    pub fn get_cregs_state(&self) -> &Vec<Word> {
        &self.cregs
    }

    pub fn get_mem_state(&self) -> &Vec<Word> {
        &self.mem
    }

    fn get_arg(
        &self,
        name: &String,
        args: &HashMap<String, IdentVal>,
        span: &SourceSpan,
    ) -> Result<Operand, Diagnostic<usize>> {
        if let Some(val) = args.get(name) {
            Ok(val.into())
        } else {
            Err(Diagnostic::error()
                .with_message(format!("'{}' does not exist in current scope", name))
                .with_labels(vec![Label::primary(span.file, span.span.clone())]))
        }
    }

    fn get_val(
        &self,
        thing: &Operand,
        args: &HashMap<String, IdentVal>,
        span: &SourceSpan,
    ) -> Result<Wrapping<i64>, Diagnostic<usize>> {
        match thing {
            Operand::Imm(imm) => Ok(Wrapping(*imm)),
            Operand::Reg(reg) => {
                if reg > &self.cregs.len() {
                    Err(Diagnostic::error()
                        .with_message("creg index out of bounds")
                        .with_labels(vec![Label::primary(span.file, span.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of cregs is {} as declared in headers",
                            self.cregs.len()
                        )]))
                } else {
                    Ok(self.cregs[*reg].get_val())
                }
            }
            Operand::Addr(addr) => {
                let addr_val = self.mem_addr_val(*addr, span)?;
                if addr_val > self.mem.len() {
                    Err(Diagnostic::error()
                        .with_message("Memory index out of bounds")
                        .with_labels(vec![Label::primary(span.file, span.span.clone())
                            .with_message(format!(
                                "Attempted to access memory index {}",
                                addr_val
                            ))])
                        .with_notes(vec![format!(
                            "Note: Number of words in memory is {} as declared in headers",
                            self.mem.len()
                        )]))
                } else {
                    Ok(self.mem[addr_val].get_val())
                }
            }
            Operand::Ident(name) => self.get_val(&self.get_arg(name, args, span)?, args, span),
        }
    }

    fn mem_addr_val(&self, addr: MemAddr, span: &SourceSpan) -> Result<usize, Diagnostic<usize>> {
        match addr {
            MemAddr::Address(addr) => Ok(addr),
            MemAddr::Indirect(reg, align, offset) => {
                if reg > self.cregs.len() {
                    Err(Diagnostic::error()
                        .with_message("creg index out of bounds")
                        .with_labels(vec![Label::primary(span.file, span.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of cregs is {} as declared in headers",
                            self.cregs.len()
                        )]))
                } else {
                    Ok((self.cregs[reg].get_val().0 as usize) * align + offset)
                }
            }
        }
    }

    fn update(
        &mut self,
        src: &Operand,
        dst: Wrapping<i64>,
        args: &HashMap<String, IdentVal>,
        span: &SourceSpan,
    ) -> Result<(), Diagnostic<usize>> {
        match src {
            Operand::Reg(reg) => {
                if reg > &self.cregs.len() {
                    Err(Diagnostic::error()
                        .with_message("creg index out of bounds")
                        .with_labels(vec![Label::primary(span.file, span.span.clone())])
                        .with_notes(vec![format!(
                            "Note: Number of cregs is {} as declared in headers",
                            self.cregs.len()
                        )]))
                } else {
                    self.cregs[*reg].set_to(dst);
                    Ok(())
                }
            }
            Operand::Addr(addr) => {
                let addr_val = self.mem_addr_val(*addr, span)?;
                if addr_val > self.mem.len() {
                    Err(Diagnostic::error()
                        .with_message("Memory index out of bounds")
                        .with_labels(vec![Label::primary(span.file, span.span.clone())
                            .with_message(format!(
                                "Attempted to access memory index {}",
                                addr_val
                            ))])
                        .with_notes(vec![format!(
                            "Note: Number of words in memory is {} as declared in headers",
                            self.mem.len()
                        )]))
                } else {
                    self.mem[addr_val].set_to(dst);
                    Ok(())
                }
            }
            Operand::Ident(name) => self.update(&self.get_arg(name, args, span)?, dst, args, span),

            Operand::Imm(_) => unreachable!("Parser allowed bork code"),
        }
    }

    fn apply_mat_2(&mut self, mat: &DCf64Mat2, qbit: usize) {
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

    fn apply_mat_4(&mut self, mat: &DCf64Mat4, qbit1: usize, qbit2: usize) {
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

    fn apply_controlled_mat(&mut self, mat: &DCf64Mat2, controls: Vec<usize>, target: usize) {
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
    bits: usize,
    data: Wrapping<i64>,
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
    Branch(usize),
}

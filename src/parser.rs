use chumsky::{combinator::Repeated, prelude::*};

use crate::ast::*;

pub fn parser() -> impl Parser<char, Result<Program, ResolveError>, Error = Simple<char>> {
    let num = text::int(10).map(|s: String| s.parse::<usize>().unwrap());
    let signed_num = just("-").or_not().then(num.or_not()).map(|(c, num)| {
        let num = if let Some(num) = num { num } else { 1 };
        if c.is_none() {
            num as isize
        } else {
            -(num as isize)
        }
    });

    let qbits_header = just("qbits")
        .then(repeated(just(' ')))
        .ignore_then(num)
        .map(|num| num as u16);

    let cbits_header = just("cbits")
        .then(repeated(just(' ')))
        .ignore_then(num)
        .map(|num| num as u16);
    let qregs_header = just("qregs").then(repeated(just(' '))).ignore_then(num);
    let cregs_header = just("cregs").then(repeated(just(' '))).ignore_then(num);

    let qreg = just("qr").ignore_then(num);
    let creg = just("cr").ignore_then(num).map(Operand::Reg);
    let qbit = just('q').ignore_then(num);
    let cbit = just('c').ignore_then(num);
    let imm = num.map(|num| Operand::Imm(num as i64));

    let rot = signed_num
        .then_ignore(just("pi"))
        .then(just('/').ignore_then(num).or_not())
        .map(|rot| {
            let mlt = rot.0 as i64;
            let div = match rot.1 {
                Some(div) => div as u64,
                None => 1,
            };
            Rotation(mlt, div)
        });

    macro_rules! extract_pat {
        ($var:expr => $($id:ident)::+($($names:ident),*) else $expr:expr) => {
            match $var {
                $($id)::+($($names),*) => ($($names),*),
                _ => $expr
            }
        };
    }

    qbits_header
        .then_ignore(repeated(text::newline()))
        .then(cbits_header)
        .then_ignore(repeated(text::newline()))
        .then(qregs_header)
        .then_ignore(repeated(text::newline()))
        .then(cregs_header)
        .then_ignore(repeated(text::newline()))
        .map(FlattenTuple2::into_flatten)
        .then(
            choice((
                // Quantum Instructions
                choice((
                    just("qsel")
                        .then(repeated(just(' ')))
                        .ignore_then(qreg)
                        .map(Inst::Qsel),
                    just("id")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .map(Inst::Id),
                    just("h")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .map(Inst::Hadamard),
                    just("cnot")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(qbit)
                        .map(|qbits| Inst::Cnot(qbits.0, qbits.1)),
                    just("ccnot")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(qbit)
                        .map(FlattenTuple1::into_flatten)
                        .map(|(qbit1, qbit2, qbit3)| Inst::Ccnot(qbit1, qbit2, qbit3)),
                    just('x')
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .map(Inst::X),
                    just('y')
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .map(Inst::Y),
                    just('z')
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .map(Inst::Z),
                    just("rx")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(just(' '))
                        .then(rot)
                        .map(|(qbit, rot)| Inst::Rx(qbit, rot)),
                    just("ry")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(just(' '))
                        .then(rot)
                        .map(|(qbit, rot)| Inst::Ry(qbit, rot)),
                    just("rz")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(just(' '))
                        .then(rot)
                        .map(|(qbit, rot)| Inst::Rz(qbit, rot)),
                    just('u')
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(rot)
                        .then_ignore(repeated(just(' ')))
                        .then(rot)
                        .then_ignore(repeated(just(' ')))
                        .then(rot)
                        .map(FlattenTuple2::into_flatten)
                        .map(|(qbit, rot1, rot2, rot3)| Inst::U(qbit, rot1, rot2, rot3)),
                    just("s")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .map(Inst::S),
                    just("t")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .map(Inst::T),
                    just("sdg")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .map(Inst::Sdg),
                    just("tdg")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .map(Inst::Tdg),
                    just("p")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(rot)
                        .map(|(qbit, rot)| Inst::Phase(qbit, rot)),
                    just("ch")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(qbit)
                        .map(|(qbit1, qbit2)| Inst::Ch(qbit1, qbit2)),
                    just("cy")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(qbit)
                        .map(|(qbit1, qbit2)| Inst::Cy(qbit1, qbit2)),
                    just("cz")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(qbit)
                        .map(|(qbit1, qbit2)| Inst::Cz(qbit1, qbit2)),
                    just("cp")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(rot)
                        .map(FlattenTuple1::into_flatten)
                        .map(|(control, qbit, rot)| Inst::CPhase(control, qbit, rot)),
                    just("swap")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(qbit)
                        .map(|(qbit1, qbit2)| Inst::Swap(qbit1, qbit2)),
                    just("sqrtx")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .map(Inst::SqrtX),
                    just("sqrtswp")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(qbit)
                        .map(|(qbit1, qbit2)| Inst::SqrtSwap(qbit1, qbit2)),
                    just("cswap")
                        .then(repeated(just(' ')))
                        .ignore_then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(qbit)
                        .then_ignore(repeated(just(' ')))
                        .then(qbit)
                        .map(FlattenTuple1::into_flatten)
                        .map(|(control, qbit1, qbit2)| Inst::CSwap(control, qbit1, qbit2)),
                )),
                just("m")
                    .then(repeated(just(' ')))
                    .ignore_then(qbit)
                    .then_ignore(repeated(just(' ')))
                    .then(creg)
                    .then_ignore(repeated(just(' ')))
                    .then(cbit)
                    .map(FlattenTuple1::into_flatten)
                    .map(|(qbit, creg, cbit)| {
                        Inst::Measure(
                            qbit,
                            extract_pat!(creg => Operand::Reg(v) else unreachable!()),
                            cbit,
                        )
                    }),
                // Classical Instructions
                choice((
                    just("mov")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|(dst, src)| {
                            Inst::Mov(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src,
                            )
                        }),
                    just("add")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::Add(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("sub")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::Sub(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("mul")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::Mul(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("umul")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::UMul(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("div")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::Div(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("smul")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::SMul(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("sumul")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::SUMul(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("sdiv")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::SDiv(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("not")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|(dst, src)| {
                            Inst::Not(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src,
                            )
                        }),
                    just("and")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::And(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("or")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::Or(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("xor")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::Xor(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("nand")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::Nand(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("nor")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::Nor(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                    just("xnor")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|ops| ops.into_flatten())
                        .map(|(dst, src1, src2)| {
                            Inst::Xnor(
                                extract_pat!(dst => Operand::Reg(v) else unreachable!()),
                                src1,
                                src2,
                            )
                        }),
                )),
                // Misc
                choice((
                    just("cmp")
                        .then(repeated(just(' ')))
                        .ignore_then(creg)
                        .then_ignore(repeated(just(' ')))
                        .then(creg.or(imm))
                        .map(|(r1, r2)| {
                            Inst::Cmp(extract_pat!(r1 => Operand::Reg(v) else unreachable!()), r2)
                        }),
                    just("jmp")
                        .then(repeated(just(' ')))
                        .ignore_then(text::ident())
                        .map(Inst::Jmp),
                    just("jeq")
                        .then(repeated(just(' ')))
                        .ignore_then(text::ident())
                        .map(Inst::Jeq),
                    just("jne")
                        .then(repeated(just(' ')))
                        .ignore_then(text::ident())
                        .map(Inst::Jne),
                    just("jg")
                        .then(repeated(just(' ')))
                        .ignore_then(text::ident())
                        .map(Inst::Jg),
                    just("jge")
                        .then(repeated(just(' ')))
                        .ignore_then(text::ident())
                        .map(Inst::Jge),
                    just("jl")
                        .then(repeated(just(' ')))
                        .ignore_then(text::ident())
                        .map(Inst::Jl),
                    just("jle")
                        .then(repeated(just(' ')))
                        .ignore_then(text::ident())
                        .map(Inst::Jle),
                    text::ident().then_ignore(just(":")).map(Inst::Label),
                    just("hlt").to(Inst::Hlt),
                )),
            ))
            .then_ignore(repeated(text::newline()))
            .repeated(),
        )
        .then_ignore(end())
        .map(|p| resolve_ast(p.0, p.1))
}

fn repeated<I, O>(
    p: impl Parser<I, O, Error = Simple<I>>,
) -> Repeated<impl Parser<I, O, Error = Simple<I>>>
where
    I: Clone + std::hash::Hash + Eq,
    O: Clone,
{
    p.repeated().at_least(1)
}

trait FlattenTuple1 {
    type Output;
    fn into_flatten(self) -> Self::Output;
}

trait FlattenTuple2 {
    type Output;
    fn into_flatten(self) -> Self::Output;
}
impl<A, B, C> FlattenTuple1 for ((A, B), C) {
    type Output = (A, B, C);
    fn into_flatten(self) -> Self::Output {
        ((self.0).0, (self.0).1, self.1)
    }
}
impl<A, B, C, D> FlattenTuple2 for (((A, B), C), D) {
    type Output = (A, B, C, D);
    fn into_flatten(self) -> Self::Output {
        (((self.0).0).0, ((self.0).0).1, (self.0).1, self.1)
    }
}

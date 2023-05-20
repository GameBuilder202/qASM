use chumsky::{combinator::Repeated, prelude::*};

use crate::ast::*;

pub fn parser() -> impl Parser<char, Program, Error = Simple<char>> {
    let num = text::int(10).map(|s: String| s.parse::<usize>().unwrap());

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
    let creg = just("cr").ignore_then(num).map(|creg| Operand::Reg(creg));
    let qbit = just('q').ignore_then(num);
    // let cbit = just('c').ignore_then(num);
    let imm = num.map(|num| Operand::Imm(num as i64));

    let rot = num
        .or_not()
        .then_ignore(just("pi"))
        .then(just('/').ignore_then(num).or_not())
        .map(|rot| {
            let mlt = match rot.0 {
                Some(mlt) => mlt as u64,
                None => 1,
            };
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
        .map(|tup| FlattenTuple2::into_flatten(tup))
        .then(
            choice((
                // Quantum Instructions
                just("qsel")
                    .then(repeated(just(' ')))
                    .ignore_then(qreg)
                    .map(|qreg| Inst::Qsel(qreg)),
                just("h")
                    .then(repeated(just(' ')))
                    .ignore_then(qbit)
                    .map(|qbit| Inst::Hadamard(qbit)),
                just("cnot")
                    .then(repeated(just(' ')))
                    .ignore_then(qbit)
                    .then_ignore(repeated(just(' ')))
                    .then(qbit)
                    .map(|qbits| Inst::Cnot(qbits.0, qbits.1)),
                just('x')
                    .then(repeated(just(' ')))
                    .ignore_then(qbit)
                    .map(|qbit| Inst::X(qbit)),
                just('y')
                    .then(repeated(just(' ')))
                    .ignore_then(qbit)
                    .map(|qbit| Inst::Y(qbit)),
                just('z')
                    .then(repeated(just(' ')))
                    .ignore_then(qbit)
                    .map(|qbit| Inst::Z(qbit)),
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
                    .map(|tup| FlattenTuple2::into_flatten(tup))
                    .map(|ops| Inst::U(ops.0, ops.1, ops.2, ops.3)),
                just("s")
                    .then(repeated(just(' ')))
                    .ignore_then(qbit)
                    .map(|qbit| Inst::S(qbit)),
                just("t")
                    .then(repeated(just(' ')))
                    .ignore_then(qbit)
                    .map(|qbit| Inst::T(qbit)),
                just("sdg")
                    .then(repeated(just(' ')))
                    .ignore_then(qbit)
                    .map(|qbit| Inst::Sdg(qbit)),
                just("tdg")
                    .then(repeated(just(' ')))
                    .ignore_then(qbit)
                    .map(|qbit| Inst::Tdg(qbit)),
                // Classical Instructions
                just("add")
                    .then(repeated(just(' ')))
                    .ignore_then(creg)
                    .then_ignore(repeated(just(' ')))
                    .then(creg.or(imm))
                    .then_ignore(repeated(just(' ')))
                    .then(creg.or(imm))
                    .map(|ops| ops.into_flatten())
                    .map(|ops| {
                        Inst::Add(
                            extract_pat!(ops.0 => Operand::Reg(v) else unreachable!()),
                            ops.1,
                            ops.2,
                        )
                    }),
                just("hlt").map(|_| Inst::Hlt),
            ))
            .then_ignore(repeated(text::newline()))
            .repeated(),
        )
        .then_ignore(end())
        .map(|p| Program {
            headers: p.0,
            instructions: p.1,
        })
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

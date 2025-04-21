# Documentation for using qASM
This documentation assumes some level of familiarity with normal assembly languages.
If you don't have that experience, consider reconsidering why you are trying out a quantum **assembly language**.

The documentation also assumes familiarity with quantum computing terms and jargon. If you don't have that experience, consider reconsidering why you are trying out a **quantum** assembly language.

## Usage

Due to a common condition called "lazy", there are no prebuilt release binaries for the emulator.
Go get [rustup](https://rustup.rs/) to install cargo and use the emulator.\
<sub><sup>Unless you also suffer from the "lazy", in which case your best bet is hope someone else can compile the program for you.</sup></sub>

Currently, there are 2 major divisions of qASM code[^1], the main one being executed qASM code, and the other being imported/library qASM code.
These don't have official names (currently) other than "the file being passed into the command line input file" and "the file being imported"[^2].
For the sake of having a temporary name, they will be called "main code" and "library code" respectively.

### Main code

The start of the file consists of optional import statement(s)

```
import "/path/to/file.qASM"
```

followed by program headers

```
qbits n
cbits n
qregs n
cregs n
mem   n
```

The order of the headers *is important*.
The `qbits` header specifies the number of
qubits in each quantum register.
The `cbits` header specifies the number of classical bits in each classical
register.
The `qregs` header specifies the number of quantum registers. The `cregs` header specifies the number of classical registers. The `n` after each header is any integer number. 
Operands/arguments for all instructions are delimited by spaces and not commas.

Code execution should always end with a `hlt` instruction.
If the emulator reaches the end of code but does not encounter a `hlt` instruction, it will end with a "PC out of bounds" error.

#### Quantum Instructions
The general format for quantum instructions are:
```
op qn <other arguments>
```
Where `op` is the name/opcode, `qn` specifies a specific qubit `n` of the currently selected quantum register.
`<other arguments>` can include more qubits as arguments, or in the case of some instructions, a rotation argument.
Quantum registers can be selected via the `qsel` instruction, which has the general format `qsel qrn`.

Rotation arguments are either rational multiples of $\pi$, `api/b` where `a` and `b` are optional integers (`b` is positive), or a hardcoded angle in radians `rad(...)`, or a hardcoded angle in degrees `deg(...)`.
Internally, all angles are stored in radians, so be aware of floating-point imprecision when using `deg(...)`.

List of currently implemented quantum instructions:

##### Identity
```
id qn
```
Applies the identity (do-nothing) operation to qubit `n`.

##### Hadamard
```
h qn
```
Applies a hadamard operation to qubit `n`.

##### CNOT
```
cnot qa qb
```
Applies a CNOT operation with qubit `a` as control and qubit `b` as target.

##### CCNOT/Toffoli
```
ccnot qa qb qc
```
Aplies a CCNOT operation with qubits `a` and `b` being control and qubit `c` being the target.

##### Pauli gates
```
x qn
y qn
z qn
```
Applies the corresponding pauli gate to qubit `n`.

##### Axis rotation gates
```
rx qn api/b
ry qn api/b
rz qn api/b
```
Rotates the statevector of qubit `n` by $a\pi/b$ radians along the corresponding axis on the bloch sphere.

##### U gate
```
u qn api/b cpi/d epi/f
```
Rotates the statevector of qubit `n` by the 3 Euler angles $a\pi/b$, $c\pi/d$, and $e\pi/f$.

##### Phase Gates
```
s qn
t qn
sdg qn
tdg qn
p qn api/b
```
$S$ gate, $T$ gate, $S^{\dagger}$ gate, $T^{\dagger}$ gate, Phase gate respectively.

The Phase gate applies a relative phase of $a\pi/b$ radians to qubit `n`, and the $S$ and $T$ gates are special cases where the phase is $\frac{\pi}{2}$ and $\frac{\pi}{4}$ respectively.
The $S^{\dagger}$ and $T^{\dagger}$ gates are inverses of the $S$ and $T$ gates respectively.

##### Controlled gates
```
ch qa qb
cy qa qb
cz qa qb
cp qa qb api/b
```
Controlled hadamard, Y Gate, Z Gate, Phase gate respectively, where qubit `a` is control and qubit `b` is target.

##### Swap Gate
```
swap qa qb
```
Swaps the states of qubits `a` and `b`

##### Sqrt Gates
```
sqrtx qn
sqrtswp qa qb
```
Halfway applies the Pauli X and Swap gate operation respectively.

##### Controlled Swap
```
cswap qa qb qc
```
Controlled swap with qubit `a` as control and qubits `b` and `c` as the qubits to swap the states of.

##### Measure
```
m qa crb cd
```
Measure the state of qubit `a` into bit `d` of classical register `b`

*Note: Remove any measurement operations before running the emulator with `--print-state` (or `-p`) as the emulator does not ignore them currently when run with that flag set*

#### Classical Instructions
General format for classical instructions are:
```
op <operands>
```
Where `op` is the name/opcode, operands may include `crn`, which specifies a specific classical register `n`, an immediate literal value, a memory address, or an identifier.
Also, (almost) all operands have the source and destination oparands separate (destination operand first).
Identifier operands are a bit more than just names, they are explained in more detail in the [library code section below](#library-code).

List of currently implemented classical instructions:

*Note: Destination operand can only be a register, memory address, or an identifier, but the other arguments can be an immediate, register, memory address, or identifier.*

| `op`   | Operation/value on/of destination register |
| ------ | ----------- |
| mov    | Move a value into register. |
| movstr | Move a string into memory address. |
| add    | Add. |
| sub    | Subtract. |
| mult   | Unsigned multiply. |
| umult  | Upper word of unsigned multiply. |
| div    | Divide. |
| smult  | Signed multiply. |
| sumult | Upper word of signed multiply. |
| sdiv   | Signed division. |
| not    | Logical NOT. |
| and    | Logical AND. |
| or     | Logical OR.  |
| xor    | Logical XOR. |
| nand   | Logical NAND. |
| nor    | Logical NOR. |
| xnor   | Logical XNOR. |

Memory addresses come in 3 forms
```
[addr]
[addr + offset]
[addr * align + offset]
```
where `addr` is allowed to be a register or number, and the others can only be numbers.
`movstr` is the one special instruction that can only take memory addresses as its destination.

#### Misc.
These instructions are here because.

| Instruction name | Description |
| ---------------- | ----------- |
| qsel             | Selects a quantum register for proceeding quantum instructions. |
| cmp              | Updates flags based on comparing values in op1 and op2. |
| jmp              | Unconditionally jump to a label. |
| jeq              | Jump to label if comparison resulted in EQ flag set. |
| jne              | Jump to label if comparsion resulted in EQ flag NOT set. |
| jg               | Jump to label if comparison resulted in GREATER flag set. |
| jge              | Jump to label if comparison resulted in GREATER or EQ flag set. |
| jl               | Jump to label if comparison resulted in LESSER flag set. |
| jle              | Jump to label if comparison resulted in LESSER or EQ flag set. |
| hlt              | Halt the program. |

The `EQ`, `GREATER`, and `LESSER` flags are internal to the emulator, and qASM code has no way of accessing their values other than the conditional jump instructions.
For the interested, the flags correspond to the variants of the `std::cmp::Ordering` enum in rust.

Syntax for labels is:
```
name:
```
And the code follows on the next line.

#### Custom Gates
qASM gives you the ability to define your own gates. Note that these do not behave like calling functions/subroutines, instead think of them as macros with the inputs to the gate being substituted at runtime. The syntax is:
```
gate name (qbits = n) {
    <code>
}
```
Where the `<code>` block only has access to `n` qubits in its scope. Additionally, the gate can take operands of other types as well, as shown with all possible operand types in one gate:
```
gate name (qubits = 2) (rot : Rot) (imm : Imm) (reg : Reg) (addr : Addr) {
    <code>
}
```
In the scope of `<code>`, the identifiers `rot`, `imm`, `reg`, and `addr` are available for use in instructions.

*Note: Labels in custom gates may be bork.*

To call custom gates, you use them almost like normal instructions; qubit arguments are passed normally, but the other arguments are separated from the qubit arguments with a comma
```
name q1 q3, pi/2 1 cr1 [0]
```
This is because some stupid parsing stuff with lalrpop.

### Library code

These files are much more simple and stripped down that the previous one.
The only allowed constructs here are imports and custom gate definitions, and these can be public or private, with the same syntax as before.
Public items additionally have a `pub` modifier before them (items are private by default).

The exact syntax is subject to change, but the overall layout of these files is
```
namespace test

import "foo.qASM"
import "bar.qASM"

gate super_secret_private_gate (qubits = 1) {
    h q0
}
pub gate public_gate (qubits = 2) (r : Rot) {
    super_secret_private_gate q0
    cnot q0 q1
    rx q0 r
}
```
As hinted by the first line, each imported file declares a namespace under which items are accessed under.
Technically, the file declares the absolute namesapce under which it resides, so if this namespace `test` is supposed to be under namespace `foo`, the start of the file would be
```
namespace foo::test
```

Public items under a namespace are accessed also using `::` as in `the::namespace::item`, so in the above example, if `foo.qASM` exported a custom gate `qft` under namespace `foo`, it would be accessed via `foo::qft`.
The namespaces under which imported files reside in (such as for `foo.qASM` and `bar.qASM` in the above examples) are *not* exposed to the file importing it. It has to manually import those files to access their namespaces and its items.
This is because since imports are file-based, as opposed to some module-based system, privateness can only be guaranteed within a file and not within a multi-file structure.


[^1]: Kind of, its more like there *will* be 2 major divisions of qASM code,
    its under construction in a major refactor at the moment.
    This documentation describes what the end result is planned to look like.
    If this footnote still exists after the refactor, then scream at me.

[^2]: Name recommendations anyone pls?

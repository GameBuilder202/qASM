# Documentation for using qASM
What the title says.

## Usage

Currently there are no prebuilt binaries so you will have to use cargo manually to build and run the emulator.
The general layout of qASM code is:

```
qbits n
cbits n
qregs n
cregs n
mem   n

<code>
```

The order of the headers is important and should not be changed. The `qbits` header specifies the number of
qubits in each quantum register. The `cbits` header specifies the number of classical bits in each classical
register. The `qregs` header specifies the number of quantum registers. The `cregs` header specifies the number
of classical registers. The `n` after each header is any integer number. Operands/arguments for all
instructions are delimited by spaces and not commas.

Code execution should always end at a `hlt` instruction. If the emulator reaches the end of code but does not
encounter a `hlt` instruction, it will end with a "PC out of bounds" error.

## Quantum Instructions
The general format for quantum instructions are:
```
op qn <other arguments>
```
Where `op` is the name/opcode, `qn` specifies a specific qubit `n` of the currently selected quantum register.
`<other arguments>` can include more qubits as arguments, or in the case of some instructions, a rotation expressed
as a rational multiple of pi, in the format `[n]pi[/n]`, where `n` can be any integer number, and items
in `[]` are optional. Quantum registers can be selected via the `qsel` instruction, which has the general format
`qsel qrn` where `n` is any non-negative number.

List of currently implemented quantum instructions:

### Hadamard
```
h qn
```
Applies a hadamard operation to qubit `n`.

### CNOT
```
cnot qa qb
```
Applies a CNOT operation with qubit `a` as control and qubit `b` as target.

### CCNOT/Toffoli
```
ccnot qa qb qc
```
Aplies a CCNOT operation with qubits `a` and `b` being control and qubit `c` being the target.

### Pauli gates
```
x qn
y qn
z qn
```
Applies the corresponding pauli gate to qubit `n`.

### Axis rotation gates
```
rx qn api/b
ry qn api/b
rz qn api/b
```
Rotates the statevector of qubit `n` by $a\pi/b$ radians along the corresponding axis on the bloch sphere.

### U gate
```
u qn api/b cpi/d epi/f
```
Rotates the statevector of qubit `n` by the 3 Euler angles $a\pi/b$, $c\pi/d$, and $e\pi/f$.

### Phase Gates
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

### Controlled gates
```
ch qa qb
cy qa qb
cz qa qb
cp qa qb api/b
```
Controlled hadamard, Y Gate, Z Gate, Phase gate respectively, where qubit `a` is control and qubit `b` is target.

### Swap Gate
```
swap qa qb
```
Swaps the states of qubits `a` and `b`

### Sqrt Gates
```
sqrtx qn
sqrtswp qa qb
```
Halfway applies the Pauli X and Swap gate operation respectively.

### Controlled Swap
```
cswap qa qb qc
```
Controlled swap with qubit `a` as control and qubits `b` and `c` as the qubits to swap the states of.

### Measure
```
m qa crb cd
```
Measure the state of qubit `a` into bit `d` of classical register `b`

*Note: Remove any measurement operations before running the emulator with `--print-state` (or `-p`) as the emulator does not ignore them currently when run with that flag set*

## Classical Instructions
General format for classical instructions are:
```
op <operands>
```
Where `op` is the name/opcode, operands may include `crn`, which specifies a specific classical register `n`, or an immediate literal value, or a memory address with optional align and offset.
These behave basically as you would expect if you have normal assembly experience. Also, (almost) all operands have the source and destination oparands separate (destination operand first).

List of currently implemented classical instructions:

*Note: Destination operand can only be a register, memory address, or an identifier, but the other arguments can be an immediate, register, memory address, or identifier.*

| `op`   | Operation/value on/of destination register |
| ------ | ----------- |
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

## Misc.
These instructions are here because.

| Instruction name | Description |
| ---------------- | ----------- |
| qsel             | Selects a quantum register so that proceeding quantum instructions act on that qreg. |
| cmp              | Updates flags based on comparing values in op1 and op2. |
| jmp              | Unconditionally jump to a label. |
| jeq              | Jump to label if comparison resulted in EQ flag set. |
| jne              | Jump to label if comparsion did not result in EQ flag set. |
| jg               | Jump to label if comparison resulted in GREATER flag set. |
| jge              | Jump to label if comparison resulted in GREATER or EQ flag set. |
| jl               | Jump to label if comparison resulted in LESSER flag set. |
| jle              | Jump to label if comparison resulted in LESSER or EQ flag set. |
| hlt              | Halt the program. |

Syntax for labels is:
```
name:
```
And the code follows on the next line.

## Custom Gates
qASM gives you the ability to define your own gates. Note that these do not behave like calling functions/subroutines, instead think of them as macros with the inputs to the gate being substituted at runtime. The syntax is:
```
gate name (qbits = n) {
    <code>
}
```
Where the `<code>` block only has access to `n` qubits in its scope. Additionally, the gate can take operands of other types as well, as shown with all possible operand types in one gate:
```
gate name (qubits = 0) (rot : Rot) (imm : Imm) (reg : Reg) (addr : Addr) {
    <code>
}
```
In the scope of `<code>`, the identifiers `rot`, `imm`, `reg`, and `addr` are available for use in instructions.

*Note: Labels in custom gates may be bork.*

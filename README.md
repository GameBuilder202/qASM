# qASM

Not to be confused with OpenQASM. An assembly language that contains both quantum and classical instructions.
This repository contains the emulator for qASM.

# Usage

Currently there are no prebuilt binaries so you will have to use cargo manually to build and run the emulator.
The general layout of qASM code is:

```
qbits <n>
cbits <n>
qregs <n>
cregs <n>

<code>

hlt
```

The order of the headers is important and should not be changed. The `qbits` header specifies the number of
qubits in each quantum register. The `cbits` header specifies the number of classical bits in each classical
register. The `qregs` header specifies the number of quantum registers. The `cregs` header specifies the number
of classical registers. Each header should be followed by a non-negative number. Operands/arguments for each
instructions are delimited by spaces and not commas.

The general format for quantum instructions are:
```
<op> q<n> <other arguments>
```
Where `<op` is the name/opcode, `q<n>` specifies a specific qubit `n` of the currently selected quantum register.
`<other arguments>` can include more qubits as arguments, or in the case of some instructions, a rotation expressed
as a rational multiple of pi, in the format `[<n>]pi[/<n>]`, where `<n>` can be any non-negative number, and items
in `[]` are optional. Quantum registers can be selected via the `qsel` instruction, which has the general format
`qsel qr<n>` where `<n>` is any non-negative number.

List of currently implemented quantum instructions: (format is "Name - `<op>`")
- Hadamard - h
- Cnot - cnot
- Pauli X - x
- Pauli Y - y
- Pauli Z - z
- Rx - rx
- Ry - ry
- Rz - rz
- U gate - u
- S gate - s
- T gate - t
- S-dagger - sdg
- T-dagger - tdg

General format for classical instructions are:
```
<op> <operands>
```
Where `<op>` is the name/opcode, operands may include `cr<n>`, which specifies a specific classical register `<n>`, or
an immediate literal value (for now non-negative due to not implemented in parser yet) Other than these differences,
they behave basically the same as any other assembly language instructions.

List of currently implemented classical instructions: (format is "`<op>` - Description")
- add - Takes 3 arguments, first being a register and the other 2 being either a register or immediate. Puts the sum of the
immediates/values in the registers into the first register.

# Examples
This program simulates the |Phi+> bell state:
```
qbits 2
cbits 8
qregs 1
cregs 1

qsel qr0
h q0
cnot q0 q1

hlt
```

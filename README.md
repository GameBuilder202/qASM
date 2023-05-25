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
Where `<op>` is the name/opcode, `q<n>` specifies a specific qubit `n` of the currently selected quantum register.
`<other arguments>` can include more qubits as arguments, or in the case of some instructions, a rotation expressed
as a rational multiple of pi, in the format `[<n>]pi[/<n>]`, where `<n>` can be any non-negative number, and items
in `[]` are optional. Quantum registers can be selected via the `qsel` instruction, which has the general format
`qsel qr<n>` where `<n>` is any non-negative number.

List of currently implemented quantum instructions:

| Quantum Gate        | Instruction name | Syntax example      | Explanation |
| ------------------- | ---------------- | ------------------- | ----------- |
| Hadamard            | h                | `h q0`              | Applies a Hadamard to qubit 0 |
| CNOT                | cnot             | `cnot q0 q1`        | Applies a CNOT to qubit 1 with qubit 0 being the control |
| CCNOT/Toffoli       | ccnot            | `ccnot q0 q1 q2`    | Applies a Toffoli to qubit 2 with qubit 0 and qubit 1 being the controls |
| Pauli X             | x                | `x q0`              | Applies a Pauli X to qubit 0 |
| Pauli Y             | y                | `y q0`              | Applies a Pauli Y to qubit 0 |
| Pauli Z             | z                | `z q0`              | Applies a Pauli Z to qubit 0 |
| Rx                  | rx               | `rx q0 pi/3`        | Rotates the statevector of qubit 0 by pi/3 radians along X axis on bloch sphere |
| Ry                  | ry               | `ry q0 pi`          | Rotates the statevector of qubit 0 by pi radians along Y axis on bloch sphere |
| Rz                  | rz               | `rz q0 pi/4`        | Rotates the statevector of qubit 0 by pi/4 radians along Z axis on bloch sphere |
| U gate              | u                | `u q0 pi pi/3 pi/6` | Rotates the statevector of qubit 0 by the 3 Euler angles pi, pi/3, pi/6 |
| S gate              | s                | `s q0`              | Applies an S gate to qubit 0 |
| T gate              | t                | `t q0`              | Applies a T gate to qubit 0 |
| S-dagger            | sdg              | `sdg q0`            | Applies a S-dagger or the inverse of S gate to qubit 0 |
| T-dagger            | tdg              | `tdg q0`            | Applies a T-dagger or the inverse of T gate to qubit 0 |
| Phase gate          | p                | `p q0 pi/3`         | Applies a rotation to the $\ket{1}$ state by pi/3 radians |
| Controlled Hadamard | ch               | `ch q0 q1`          | Applies a controlled Hadamard to qubit 1 with qubit 0 being the control |
| Controlled Pauli Y  | cy               | `cy q0 q1`          | Applies a controlled Pauli Y to qubit 1 with qubit 0 being the control |
| Controlled Pauli Z  | cz               | `cz q0 q1`          | Applies a controlled Pauli Z to qubit 1 with qubit 0 being the control |
| Swap                | swap             | `swap q0 q1`        | Swaps the states of qubits 0 and 1 |
| Square Root NOT     | sqrtx            | `sqrtx q0 `         | Applies a sqrt(NOT)/sqrt(Pauli X) to qubit 0 |
| Square Root Swap    | sqrtswp          | `sqrtswp q0 q1`     | Applies a sqrt(Swap) to qubits 0 and 1, halfway swapping their state |

General format for classical instructions are:
```
<op> <operands>
```
Where `<op>` is the name/opcode, operands may include `cr<n>`, which specifies a specific classical register `<n>`, or
an immediate literal value (for now non-negative due to not implemented in parser yet) Other than these differences,
they behave basically the same as any other assembly language instructions.

List of currently implemented classical instructions:

| Instruction name | Description |
| ---------------- | ----------- |
| add              | Takes 3 arguments, first being a register and the other 2 being either a register or immediate. Puts the sum of the immediates/values in the registers into the first register |

# Examples
This program simulates the $\ket{\Phi^+}$ bell state:
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

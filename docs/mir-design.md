# MIR Design

MIR (Mid-level IR) is the executable semantic layer in Lyra's compilation pipeline.

## Pipeline Position

```
AST (slang) -> HIR -> MIR -> LLVM IR
```

MIR sits between high-level representation (HIR) and low-level code generation (LLVM IR). It answers the question: **"How does this meaning execute?"**

## Core Design: Rust-Style MIR

MIR follows Rust's MIR design, not traditional three-address code.

### Why Not Three-Address Code?

Three-address code flattens expressions into individual operations:

```
t0 = a + b
t1 = t0 * c
t2 = d
result = t1 + t2
```

Problems with this approach:

- Explosion of temporaries obscures intent
- Lost structure makes optimization analysis harder
- SSA conversion required before meaningful analysis
- Reconstruction needed for readable output

### Place/Operand/Rvalue Model

MIR uses a structured assignment model:

```
Assign(Place, Rvalue)
```

**Place**: A writable/readable storage location. Not an address, not a value - a location that can be read from or written to.

**Operand**: A value source for computation:

- `Constant(value)` - literal value
- `Copy(place)` - read from a place

**Rvalue**: A computation that produces a value:

- `Use(operand)` - simple value use
- `BinaryOp(op, operand, operand)` - binary operation
- `UnaryOp(op, operand)` - unary operation
- `Aggregate(...)` - struct/array construction

### Examples

Simple assignment:

```
a = 1
```

```
Assign(Place(a), Use(Constant(1)))
```

Expression assignment:

```
a = b + c
```

```
Assign(Place(a), BinaryOp(Add, Copy(b), Copy(c)))
```

Nested expression:

```
a = (b + c) * d
```

```
Assign(Place(a), BinaryOp(Mul,
    BinaryOp(Add, Copy(b), Copy(c)),
    Copy(d)))
```

Key insight: expressions remain structured. No temporary explosion.

## Control Flow Structure

### Function and BasicBlock

```
Function
  -> BasicBlock*
       -> Statement*
       -> Terminator
```

A Function contains BasicBlocks. Each BasicBlock contains:

- Zero or more Statements (sequential execution)
- Exactly one Terminator (control flow transfer)

### Terminators

Terminators define how control leaves a basic block:

| Terminator                             | Description        |
| -------------------------------------- | ------------------ |
| `Goto(block)`                          | Unconditional jump |
| `Branch(cond, then_block, else_block)` | Conditional branch |
| `Return(operand?)`                     | Function return    |
| `Switch(operand, cases, default)`      | Multi-way branch   |

### Why Basic Blocks?

Basic blocks provide:

- Clear control flow graph for analysis
- Natural boundary for optimization passes
- Direct mapping to LLVM IR blocks
- Explicit representation of all control transfers

## Effect Classification

SystemVerilog has 100+ system tasks with various runtime effects. MIR does not encode each task individually. Instead, it classifies **effect types**.

### Effect Categories

| Category           | Description                      | Examples               |
| ------------------ | -------------------------------- | ---------------------- |
| Pure computation   | No side effects                  | Math, expressions      |
| Immediate effects  | Execute now, visible immediately | `$display`, `$write`   |
| Scheduled effects  | Queue for later execution        | NBA (`<=`), `$monitor` |
| Simulation control | Control simulation flow          | `$finish`, `$stop`     |

### MIR Statement Types

Based on effect classification, MIR has a small set of statement types:

| Statement         | Effect Category                     |
| ----------------- | ----------------------------------- |
| `Assign`          | Pure (blocking)                     |
| `ScheduleNBA`     | Scheduled (non-blocking assignment) |
| `RegisterMonitor` | Scheduled                           |
| `Display`         | Immediate                           |
| `Finish`          | Simulation control                  |

This approach:

- Keeps MIR statement set minimal
- Groups semantically similar tasks
- Makes effect analysis straightforward
- Allows backend-specific handling of each category

## What MIR Contains

- Place/Operand/Rvalue expressions
- Basic blocks with terminators
- Effect-classified statements
- Type information
- Module and function structure

## What MIR Does NOT Contain

**No SSA**: MIR places can be assigned multiple times. SSA conversion happens during LLVM IR generation when beneficial.

**No three-address temporaries**: Expressions stay structured. No explosion of `t0`, `t1`, `t2`.

**No function pointers**: SystemVerilog doesn't have first-class functions. Method dispatch is resolved statically.

**No elaborated instances**: MIR represents module templates, not the elaborated instance tree. Hierarchy is handled separately.

**No low-level details**: Register allocation, instruction selection, and machine-specific concerns belong in LLVM IR.

## Design Rationale

### Why Rust-Style?

Rust's MIR has proven effective for:

- Borrow checking and ownership analysis
- Optimization passes (const propagation, dead code)
- Multiple backend targets
- Clear semantics for debugging

These benefits apply equally to SystemVerilog compilation.

### Why Effect Classification?

SystemVerilog's system tasks are numerous but fall into few semantic categories. Classifying by effect:

- Reduces MIR complexity
- Groups tasks that need similar handling
- Makes scheduling semantics explicit
- Enables backend-specific optimization per category

### Why No Early SSA?

SSA is valuable for certain optimizations but:

- Adds complexity for simple transformations
- Not needed for C++ codegen (expressions map directly)
- Can be introduced in LLVM IR when beneficial
- Preserves readability of intermediate representation

MIR's job is semantic clarity, not optimization-ready form.

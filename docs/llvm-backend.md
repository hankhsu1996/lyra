# LLVM Backend

This document describes design decisions for MIR -> LLVM IR lowering.

## Pipeline Position

```
HIR -> MIR -> LLVM IR -> executable
              |
              +-> Runtime library (lyra_rt_*)
```

MIR fixes all execution semantics. The LLVM backend only translates—it does not interpret SystemVerilog rules.

## Core Principles

| Principle                           | Rationale                                                                    |
| ----------------------------------- | ---------------------------------------------------------------------------- |
| Layout from TypeId                  | Types determine bit-width, words, two/four-state; never infer from constants |
| Native ops for small widths         | LLVM handles i1/i32/i64/i128 efficiently                                     |
| Runtime APIs for large widths       | LLVM shouldn't be forced to handle 500-bit SV semantics                      |
| No LLVM intrinsics for SV semantics | Use `lyra_rt_*` functions, not `llvm.*` intrinsics                           |

## Width Threshold

| Width       | Strategy                                   |
| ----------- | ------------------------------------------ |
| <= 128 bits | Native LLVM scalar integers and operations |
| > 128 bits  | Global constant blobs + runtime API calls  |

The exact threshold (128 vs 64) may be tuned, but the principle is fixed: LLVM handles what it's good at; runtime handles SV-specific semantics.

## Constant Representation

### Small Constants

Materialize directly as LLVM constants:

```
store i64 42, ptr %A
```

### Wide Constants (Two-State)

Emit as LLVM global constant arrays:

```
; 500-bit constant (8 words)
@C = constant [8 x i64] [i64 ..., i64 ..., ...]
```

Assignment via runtime call:

```
call void @lyra_rt_assign_bits2(ptr %dst, ptr @C, i32 8, i32 500)
```

### Wide Constants (Four-State)

Three separate globals or a struct:

```
@C_value = constant [8 x i64] [...]
@C_xmask = constant [8 x i64] [...]
@C_zmask = constant [8 x i64] [...]
```

## Layout Calculation

Layout always derived from TypeId:

| Field         | Calculation            |
| ------------- | ---------------------- |
| bitwidth      | From type              |
| words         | `(bitwidth + 63) / 64` |
| is_four_state | From type              |

Padding bits (in the last word) are not tracked separately. The `bitwidth` defines semantic validity; writes may clear padding for determinism.

## Runtime APIs vs LLVM Intrinsics

**LLVM intrinsics** (`llvm.*`): Only for operations LLVM understands natively—`llvm.memcpy`, `llvm.ctpop`, etc.

**Runtime APIs** (`lyra_rt_*`): For SystemVerilog semantics that LLVM cannot express:

- Wide integer arithmetic
- 4-state propagation
- SV-specific operations

LLVM does not treat runtime APIs specially unless attributes are added.

## Four-State in LLVM

Four-state values are represented as three arrays:

| Array  | Purpose             |
| ------ | ------------------- |
| value  | The 0/1 bit pattern |
| x_mask | Unknown bits        |
| z_mask | High-impedance bits |

Invariant `x_mask & z_mask == 0` is checked at runtime in debug builds.

### Branchless Propagation

Hot paths avoid branches by propagating masks through bitwise operations:

```
unknown = x_mask | z_mask
; Bitwise ops combine value and masks so unknown bits contaminate results
```

### case/casez/casex

These must inspect masks directly. Correctness over performance—not hot paths.

## What LLVM Backend Does NOT Do

| Excluded               | Why                    |
| ---------------------- | ---------------------- |
| Interpret SV semantics | MIR already fixed them |
| Create new types       | Uses TypeId from HIR   |
| Handle scheduling      | Runtime responsibility |
| Elaborate hierarchy    | Runtime responsibility |

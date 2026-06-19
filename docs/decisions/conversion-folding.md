# Conversion folding

Date: 2026-06-19 Status: accepted

## Context

slang inserts an implicit conversion wherever an expression's type differs from its context's type.
A common case is a 2-state literal feeding a 4-state target -- `logic [7:0] a = 8'd5;` -- where
slang types `8'd5` as `bit[7:0]` and wraps it in a conversion to `logic[7:0]`. At the value level
the bits do not change; only the state representation widens (a zero unknown plane is added).

The C++ backend carried a render-time peephole that detected "a `ConversionExpr` whose operand is an
integer literal and whose destination only restates the same bits (same width, same signedness, no
X/Z dropped by a 2-state target)" and emitted the literal directly in the destination shape,
skipping the runtime conversion.

The question raised: should that fold move earlier -- into HIR-to-MIR -- so MIR never carries the
"redundant-looking" `ConversionExpr` wrapping a literal at all?

## Decision

No. MIR keeps the conversion faithfully: a `ConversionExpr` wrapping a literal stays. Constant
folding is the downstream optimizer's job, not HIR-to-MIR's. The backend's render-time peephole is
removed; the conversion render is a pure `(source kind, destination kind)` map that never inspects
its operand.

The deciding argument is symmetry. A 2-state -> 4-state conversion of a variable and of a constant
are the same operation:

```
a = y;       // y : bit[7:0]    -> ConversionExpr(y)
a = 8'd5;    // 8'd5 : bit[7:0] -> ConversionExpr(8'd5)
```

Folding only the constant case special-cases the node on whether its operand happens to be a literal
-- which is an optimizer's concern ("is this expression constant?"), not a structural fact MIR
should encode. MIR is the program expressed in primitives; `mir.md` states the primitive set is
shaped by the downstream optimizer, which each backend consumes as primitives "that its optimizer
can fold, propagate, and pattern-match across." The LLVM backend -- the load-bearing target -- folds
the constant conversion for free. The C++ backend is a medium for reading MIR and may emit a runtime
conversion for the constant case; that is acceptable and faithfully mirrors the MIR.

What was genuinely wrong was the peephole itself: a renderer inspecting a node's operand to decide
the output form is a backend re-deciding a fact (`mir.md` invariant 10) -- an optimization decision
living in the render layer. Removing it, not relocating it, is the fix.

## Rejected

- **Fold at HIR-to-MIR through a single conversion constructor.** A factory through which every
  conversion is built, folding the no-op restate of a literal into a re-typed literal so the node
  never exists. Rejected: it makes MIR do constant folding (the optimizer's job), and it breaks
  symmetry -- the same conversion gets two MIR shapes depending on whether the operand is a literal.
  The "redundant node" framing that motivated it was a miscategorization: the node is a faithful
  primitive the optimizer folds, not a field that restates what the structure already fixes.
- **Keep the backend peephole.** Leaves a renderer making a fold decision by inspecting operands --
  an optimization decision in the render layer (against the render-makes-no-decisions direction, and
  the eventual mechanical-renderer rewrite). The C++ codegen nicety it buys is irrelevant: C++ is a
  reading medium and the real target folds anyway.

## Consequences

- MIR carries a `ConversionExpr` over any operand, literal or not; the dumper shows the conversion
  faithfully.
- The conversion render is a pure `(source kind, destination kind)` map; the C++ output mirrors the
  MIR (a runtime conversion for the constant case), and the C++ compiler / LLVM fold it downstream.
- Bit-changing constant conversions (`8'd5` -> `16'd5`) were never folded at MIR and still are not
  -- the same rule, applied uniformly: MIR faithful, optimizer folds.

## Cross-references

- `architecture/mir.md` invariant 10 (a backend never re-derives a stated fact) and Notes (MIR is
  primitives shaped for the downstream optimizer to fold).
- `progress/refactor.md` R7.

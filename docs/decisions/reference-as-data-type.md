# Reference Is a Direction at HIR, a Data Type at MIR

## Date

2026-06-23

## Status

Accepted

## Why this decision matters

A SystemVerilog `ref` formal argument (LRM 13.5.2) and a `ref` port (LRM 23.3.3.2) both name storage
owned somewhere else: reading the alias reads that storage, writing it writes that storage, and the
alias holds no storage of its own. This record fixes which layer a reference lives in, why it is a
data type and not a borrowed pointer, and that the two SystemVerilog constructs share one
representation. It binds the `ref`-port work and the existing pass-by-reference formal lowering.

## Findings that shaped the design

### F1. SystemVerilog has no reference type; `ref` is a direction

A reference appears in SystemVerilog only as a direction: a formal-argument direction (LRM 13.5.2)
or a port direction (LRM 23.3.3.2). There is no reference data type -- `ref int x;` is not a legal
variable declaration. HIR is SystemVerilog-faithful (`hir.md`), so at HIR a reference is a property
of a formal or a port, not a type. Modeling it as a type at HIR would be unfaithful to the language.

### F2. A generic programming language carries aliasing in the type, so MIR makes it a type

MIR is a generic programming language whose peers are Rust, C++, and Python (`mir.md`). Such a
language has no "argument direction" axis in its type system; it expresses "this binding aliases
storage elsewhere" by giving the binding a reference type (Rust `&T` / `&mut T`, a C++ pointer or
`reference_wrapper`). MIR invariants 7 and 10 make this binding: the type is the sole carrier of a
member's classification, and no member may carry a classifying flag beside its type. So aliasing
cannot be a flag next to an `int` type; it must be a reference type. Every IR converges here -- even
C++ source references, which are not first-class, lower to pointers in LLVM IR. At an IR layer a
reference is a value with a type.

### F3. The reference type preserves the observable-cell protocol, so it is not a borrowed pointer

MIR already has a borrowed `PointerType` (`T*`). A reference is not that. When a SystemVerilog `ref`
writes its target, the target is often an observable signal, and the write must fire that signal's
update event (LRM 9.4.2) so processes sensitive to it wake. A raw pointer write reaches memory but
does not wake subscribers. The reference type therefore routes reads through the cell's read and
writes through the cell's `Set` (its update-event path), viewing either an observable cell or a
plain cell. This observable-aware aliasing is a real semantic distinct from a raw address, and it is
what makes the reference its own type rather than a `PointerType`.

### F4. A `ref` formal and a `ref` port are the same type, differing only in where the value comes from

Both alias a (possibly observable) cell and route through the observable protocol. They differ only
in when the alias is established and where the binding lives: a formal is filled at the call and
lives as a callable body local for the call's duration; a port is filled at construction and lives
as an object member for the instance's lifetime. The type is identical. This is the same shape as
the `self` binding (`mir.md` inv 11, `callable-receiver.md`): one binding, supplied differently per
form.

## The decision

1. **A reference is a direction at HIR and a data type at MIR.** The HIR-to-MIR boundary translates
   a `ref` / `const ref` direction into a binding whose type is the reference type, the same way it
   translates a signal into an observable wrapper and an `always_comb` into a callable. HIR keeps
   the SystemVerilog direction; MIR keeps the generic-language type.

2. **One reference type serves both `ref` formals and `ref` ports.** A `ref` formal is the reference
   type appearing as a callable body local; a `ref` port is the same reference type appearing as a
   unit member. There is no port-specific reference type. `const ref` rides a const marker on the
   type.

3. **The reference type is distinct from a borrowed pointer because it preserves the observable
   protocol** (F3): a read goes through the cell's read, a write through the cell's `Set` so the
   update event fires. A borrowed pointer, which does not, is the wrong type for a reference.

4. **A `ref` port's member is filled at construction, in the resolve phase, as a one-time alias
   binding.** The parent resolves the connected variable and binds the child's reference member to
   that variable's final cell. Per `elaboration_lifecycle.md` this is a resolve-phase binding, not a
   persistent cross-unit slot the parent holds and reads during simulation: a `ref` needs no
   simulation-time reach, so it is realized as an alias bind, not through the stored-slot machinery
   that input/output reactive edges use. It is not an upward self-climb and not a continuous
   assignment, and it must seal to a direct final cell (no reference-to-a-reference).

5. **A `ref` port has one storage; an input or output port has two.** An input or output port keeps
   the child's own cell and a continuous assignment between the two objects' cells. A `ref` port has
   no child-side cell: the member is the alias. The two must not be conflated -- modeling a `ref`
   port as a continuous assignment would introduce a second cell and a one-delta propagation delay
   that LRM 23.3.3.2 says do not exist.

## Consequences

- A `ref`-port child member owns no storage; reads and writes reach the connected cell directly with
  zero delta, and a write fires the connected signal's update event so the other side's sensitive
  processes wake.
- The implementation reuses the existing reference type and its runtime wrapper. The `ref` port
  reaches the child's reference member through the same route mechanism as any hierarchical
  reference, then binds it through the one canonical reference-store that fills any reference-typed
  lvalue. That store shares the reference-value construction with a `ref` formal's fill (F4: one
  type, supplied differently), while the placement differs -- a formal fills a call argument, a port
  stores into a member. The `ref` port carries no port-only route recipe and no port-only alias
  path, and holds no persistent slot: reaching the member is a one-time resolve-phase navigation,
  since a `ref` needs no simulation-time reach.
- A `const ref` port rides the const marker already on the type; a pass-through `ref` port rides the
  reference value being copyable (the child forwards its own reference into a deeper child).
- `inout` ports stay out of scope: they are bidirectional net connections in the deferred
  net-resolution domain (LRM 23.3.3.2), not references.

## Alternatives considered

**Model `ref` as a binding-kind flag beside the value type at MIR.** Rejected. MIR invariants 7 and
10 forbid a classifier outside the type system; aliasing is a fact about the binding that the type
must carry, not a side flag on an otherwise plain value type.

**Model the reference as a borrowed `PointerType` (`T*`).** Rejected. A raw pointer write does not
fire the target signal's update event, so a process sensitive to the aliased signal would not wake
(LRM 9.4.2). The reference must route through the observable protocol, which is a different type.

**Model a `ref` port like an output port (own storage plus a continuous assignment).** Rejected. It
introduces a second cell and a one-delta propagation delay; LRM 23.3.3.2 requires the `ref` port to
be the connected variable -- one cell, zero delta.

**A `ref`-port-specific reference type, separate from the formal's reference type.** Rejected. The
two constructs have identical semantics -- alias a cell, route through the observable protocol --
and differ only in where the value is supplied (F4). A second type would duplicate one concept.

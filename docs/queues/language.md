# Language

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. Every unchecked item must have a short tag/ID for tracking (e.g. L1). Each item should be independently addressable -- it can be investigated and landed as a standalone change. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Working queue for general SystemVerilog language feature gaps. Items that grow into multi-step streams get promoted to dedicated queues (see assertions.md, dpi.md).

Features tracked elsewhere:

- DPI import/export -> `dpi.md`
- Assertions, sampled value functions, assertion control -> `assertions.md`
- VCD dump tasks -> `observability.md`
- Specialization architecture -> `specialization.md`

## Progress

- [ ] L1 -- Parameter override at instantiation
- [ ] L2 -- Type parameters
- [ ] L3 -- `ref` arguments
- [ ] L4 -- Tasks (general, non-DPI)
- [ ] L5 -- Default argument values and named argument binding
- [ ] L6 -- `final` blocks
- [ ] L7 -- Bit vector system functions (`$isunknown`, `$countones`, `$onehot`, `$onehot0`)
- [x] L8a -- Named event core (`event e`, `-> e`, `@e`)
- [ ] L8b -- `.triggered` semantics (`@(e.triggered)`, `wait(e.triggered)`)
- [ ] L8c -- Nonblocking event trigger (`->> e`)
- [ ] L9 -- `inout` ports and tri-state nets
- [ ] L10 -- Struct/union return types
- [ ] L11 -- `real`/`shortreal` return types for user-defined functions
- [ ] L12 -- Out-of-bounds X semantics
- [ ] L13 -- Real number delays
- [ ] L14 -- `static` lifetime functions
- [ ] L15 -- Wildcard inside with non-literal constants
- [ ] L16 -- `$sscanf`
- [ ] L17 -- File positioning functions (`$fseek`, `$ftell`, `$feof`, `$ferror`)
- [ ] L18 -- `$cast`
- [ ] L19 -- `fork`/`join` parallel blocks
- [ ] L20 -- Class types (will get own queue when work begins)

## L1: Parameter override at instantiation

LRM 23.10. `mod #(16) u1()` and `mod #(.WIDTH(16)) u1()`. Currently only top-level `-G` override is supported. This blocks any design that instantiates parameterized submodules with non-default values through the standard syntax. The specialization-based compilation model already handles parameter specialization; this gap is in the instantiation-site syntax and elaboration discovery.

## L2: Type parameters

LRM 6.20.3. `parameter type T = int`. Requires type-level parameterization in the specialization key. Separate from value parameters (L1) because the type system and specialization machinery need different handling.

## L3: `ref` arguments

LRM 13.5.2. Pass by reference for subroutine arguments. Requires alias semantics in the MIR place model -- the callee's local place must alias the caller's actual argument storage.

## L4: Tasks (general, non-DPI)

LRM 13.3. Task definitions with timing controls (`#delay`, `@event`, `wait`). Requires process suspension within a subroutine call frame. DPI task import/export is tracked separately in `dpi.md` (D7a-D7c).

## L5: Default argument values and named argument binding

LRM 13.5.3 (defaults), 13.5.4 (named). `function f(int x = 0)` and `f(.x(42))`. Straightforward lowering -- fill missing args with defaults, reorder named args to positional.

## L6: `final` blocks

LRM 9.2.3. Procedural block that executes once at end of simulation. Used by ibex (`ibex_tracer.sv` for file cleanup, assertion macros for `ASSERT_FINAL`). Requires a new process scheduling category in the engine (after all time-based activity completes).

## L7: Bit vector system functions

LRM 20.9. `$countbits`, `$countones`, `$onehot`, `$onehot0`, `$isunknown`. Expression-level functions, no scheduling complexity. `$isunknown` is the highest priority -- used by ibex `ASSERT_KNOWN` macros.

## L8b: `.triggered` semantics

LRM 15.5.3. `@(e.triggered)` and `wait(e.triggered)`. Unlike `@e` (edge-sensitive, can miss a prior trigger), `.triggered` is a time-step-scoped latch: set to 1 by `->` or `->>`, cleared on time advance, readable as a level-sensitive condition. This means a `wait(e.triggered)` that executes after `-> e` in the same time step passes through without suspending. Requires per-event state that tracks whether a trigger occurred this time step, and a time-advance hook to clear it. Critically, `.triggered` is time-step scoped, not delta-scoped -- it remains set across all delta cycles within the same time step and only clears on time advance. This is the fundamental semantic difference from `@e`. Separate from L8a because the latch semantics interact with the scheduler's time-step boundary, which is a different concern from the core trigger/wait plumbing.

## L8c: Nonblocking event trigger

LRM 15.5.2. `->> e` schedules the trigger in the NBA region instead of firing immediately in the Active region. This is a region-scheduling problem, not an event-declaration problem. The runtime already has Active, Inactive, and NBA queues; `->>` needs to defer the wakeup to the NBA region, analogous to how nonblocking assignments defer their commits. Separate from L8a because mixing immediate and deferred trigger semantics in one change risks getting the region ordering wrong.

## L9: `inout` ports and tri-state nets

LRM 23.3.3 (inout ports), 6.7 (nets). Requires resolution functions for multi-driver scenarios. Large feature -- wired nets (`wand`, `wor`), strength modeling, and tri-state buffering. Low priority unless a target design requires bidirectional buses.

## L10: Struct/union return types

LRM 13.4. Functions returning struct or union types. Requires aggregate return value lowering in the call ABI.

## L11: `real`/`shortreal` return types for user-defined functions

Currently supported for DPI imports but not for user-defined SV functions. Requires return-value lowering for floating-point types in the internal call ABI.

## L12: Out-of-bounds X semantics

LRM 11.5.1. Out-of-bounds array/vector reads should return X; out-of-bounds writes should be ignored. Currently produces undefined behavior. Requires bounds-check guards in the LLVM lowering for every indexed access.

## L13: Real number delays

LRM 9.4.1. `#1.5` with fractional time values. Requires real-to-tick conversion using the timescale precision.

## L14: `static` lifetime functions

LRM 13.4.2. Functions with `static` lifetime preserve local variable state across calls. Requires persistent storage allocation for function-local variables.

## L15: Wildcard inside with non-literal constants

Known limitation: `val inside {PAT}` where `PAT` is a `localparam` with X/Z bits uses `==` instead of `==?`. Only literal X/Z patterns are detected as wildcards.

## L16: `$sscanf`

LRM 21.3.4. String scanning with format specifiers. Mirror of `$fscanf` but from string input.

## L17: File positioning functions

LRM 21.3. `$fseek`, `$ftell`, `$rewind`, `$feof`, `$ferror`. Standard file I/O positioning. Straightforward -- wraps C library calls.

## L18: `$cast`

LRM 8.16. Dynamic type casting. Primarily needed for class hierarchies, but also applies to enum and other type conversions.

## L19: `fork`/`join` parallel blocks

LRM 9.3.2. `fork`/`join`, `fork`/`join_any`, `fork`/`join_none`. Parallel process spawning within a procedural block. Requires dynamic process creation and join synchronization in the scheduler. Large feature with implications for the process model.

## L20: Class types

LRM 8. Full object-oriented type system: class definitions, inheritance, polymorphism, virtual methods, constructors, garbage collection. Too large for this queue -- will get a dedicated `classes.md` queue when work begins. Tracked here as a placeholder only.

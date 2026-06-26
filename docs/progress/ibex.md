# Ibex bring-up

Tracks the work to simulate the Ibex RISC-V CPU (lowRISC's open-source RISC-V core; top
`ibex_simple_system_tb`) end-to-end on Lyra. Ibex is the standing real-world integration target: a
multi-file, vendor-grade design that exercises a broad slice of synthesizable SystemVerilog plus a
pure-SV testbench. This file is the running inventory of the language features Ibex needs that Lyra
does not yet support.

Done when `ibex_simple_system_tb` simulates end-to-end with **no source modifications and no
simulator-specific defines**, using only accepted Lyra compilation options.
`disable_assertions = true` is an accepted option (a real, intended flag), so skipping concurrent
assertions does not count as a trick. What does count as a trick, and is never an accepted end
state: a discovery-only define such as `SYNTHESIS` or `VERILATOR`, stubbing a module, or editing the
design. Those are only ever used to reveal the next gap during discovery.

## Method

Iterative gap discovery: point the compiler at the design, find the first unsupported construct,
record it here, support it, then repeat -- each fix reveals the next gap. Because the pipeline stops
at the first error, the list below is the **observed frontier**, not a closed set: it grows as items
are closed. To widen each pass, individual modules are compiled as their own top so one module's
first blocker does not hide every other module's.

The whole Ibex RTL already parses, type-checks, and elaborates through the frontend with no errors
-- every gap below is in feature lowering or the missing execution path, not in the frontend.

## Two walls

1. **Feature-lowering gaps** -- the unsupported SystemVerilog forms below.
2. **No execution backend** -- even with every feature supported, there is no way to run the result:
   only `dump hir` / `dump mir` / `emit cpp` are wired, and there is no LIR / LLVM / JIT / AOT and
   no project mode. This wall is owned by `architecture-reset.md`; Ibex cannot run until it falls,
   independently of the feature work here.

## Feature gaps

Ordered by leverage (how much of the design each unblocks). Checkboxes flip as each construct gains
full support.

### Highest leverage

- [x] **Generate-`for` loops** (`for (genvar ...) begin ... end`). The dominant blocker -- the first
      stop in roughly a dozen modules and present throughout the design. A generate loop unrolls
      into elaborated structure at compile time; this is structural elaboration, distinct from a
      procedural `for`.
- [x] **`parameter` / `localparam` referenced inside a continuous-assign expression** (e.g. a
      part-select bound `x[Aw-1:0]` where `Aw` is a `localparam`). Parameters appear in expressions
      pervasively, so this reaches far past the leaf modules where it is the first blocker. Scalar
      parameters fold in any expression context; an unpacked-array `localparam` referenced and
      element-selected (the ibex_alu shuffle-mask form) now materializes too.

### Common forms

- [x] **Expose `disable_assertions` on the current entry path.** Untouched, the design's first
      blocker in many modules is a concurrent assertion (the `assert`/`assume`/`cover property`
      family and the macros wrapping them). The accepted handling is the `disable_assertions`
      compilation option, which skips assertion constructs during lowering. Implementing SVA proper
      (sampled-value functions `$rose`/`$fell`/`$stable`/`$past`, the `Observed` region) is a
      separate, optional feature off the critical path to running Ibex.
- [ ] **Packed array whose element is a struct or enum** (a packed array of a packed aggregate, not
      just of a scalar bit/logic).
- [ ] **Net-typed port connections** -- connecting a net (`wire`) across a module port, as the
      testbench does when wiring the DUT. The first wall the full top-level testbench hits.
- [x] **`$signed` / `$unsigned`** system functions.

### Localized / long tail

- [ ] **Hierarchical / cross-unit reference to a parameter** (reaching a sub-instance's parameter
      through a dotted path, e.g. the `mhpmcounter` accessor in the DPI block).
- [x] **Constant of an unpacked array type** -- an elaboration-time `localparam` array referenced
      (and element-selected) in an expression. An unpacked struct or union constant is still
      blocked, but on unpacked-struct / union _type_ support rather than on constant
      materialization.
- [ ] Remaining structural-expression forms surfaced as later passes get deeper (recorded here as
      discovery continues).

## Robustness (crashes, not missing features)

Some unsupported inputs abort with an internal error instead of a located, graceful diagnostic.
Independent of whether the feature is supported, each of these should fail cleanly:

- [ ] An out-of-range arena/index access aborts on a few modules instead of reporting the
      unsupported construct.
- [x] `$signed` / `$unsigned` aborts as an "unresolved system subroutine" rather than a clean
      unsupported diagnostic (subsumed once the feature above lands).
- [x] A non-literal where an integer constant is expected aborts instead of diagnosing.

## Cross-references

- Execution-path wall: `architecture-reset.md`.
- Net-typed ports and hierarchical references also surface in the hierarchy workstream; close them
  there if that file's items cover the same construct.
- The Ibex-side how-to-reproduce and status snapshot live in the Ibex checkout's `LYRA.md`.

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
2. **Execution backend** -- the C++ path is wired and runs end to end: `lyra run` emits, builds, and
   executes a supported design. The blanket "no way to run anything" wall is down, so a
   fully-lowered Ibex can in principle run. What remains here is not the ability to run at all but
   the reach to run Ibex specifically: project mode (`lyra.toml` lookup) is still not wired, so
   sources, includes, and defines must be passed explicitly on the command line. The LLVM / JIT path
   is a separate backend tracked in `architecture-reset.md`; it is not required for the C++ run.

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
- [x] **Enclosing-`genvar` references in nested generate** -- an inner generate whose loop bound or
      body reads an outer generate's `genvar`, and `genvar`-dependent part-select bounds (the
      ibex_alu butterfly form, e.g. `mask[stg][N*(2*seg+1)-1 : N*2*seg]`). A generate scope's
      `genvar` is a runtime induction value reached across scopes, so a `genvar`-dependent bound
      stays runtime rather than folding to a single elaboration constant.
- [x] **Nonblocking assignment to an enclosing-scope variable from inside a generate block** -- a
      `<=` write, in an `always_ff` within a conditional or loop generate block, whose target is a
      variable declared in the surrounding scope (the canonical `always_ff @(posedge clk) q <= d;`
      flop where `q` lives in the enclosing module). A deferred write now captures a reference to
      the target cell, so the navigation to an enclosing cell is evaluated once at submit time
      rather than reconstructed in the deferred body. Pervasive in banked, width-parameterized RTL:
      with this, the counter, prefetch-buffer, fetch-FIFO, decoder, and compressed-decoder families
      lower and emit C++ end-to-end.
- [ ] **Variable-width part-select inside a conditional generate** -- a part-select whose _width_
      depends on a `genvar` (e.g. `x[i-1:0]`, which widens with `i`) appearing in a
      `generate     if`/`else` branch. Distinct from the genvar-dependent select _bounds_ of
      constant width above (the ibex_alu butterfly form), which already lower; here the selected
      width itself is non-constant, and only the combination with a conditional generate scope
      fails.

### Common forms

- [x] **Expose `disable_assertions` on the current entry path.** Untouched, the design's first
      blocker in many modules is a concurrent assertion (the `assert`/`assume`/`cover property`
      family and the macros wrapping them). The accepted handling is the `disable_assertions`
      compilation option, which skips assertion constructs during lowering. Implementing SVA proper
      (sampled-value functions `$rose`/`$fell`/`$stable`/`$past`, the `Observed` region) is a
      separate, optional feature off the critical path to running Ibex.
- [x] **Packed array whose element is a struct or enum** (a packed array of a packed aggregate, not
      just of a scalar bit/logic).
- [x] **Net-typed port connections** -- connecting a net (`wire`) across a module port, as the
      testbench does when wiring the DUT: the parent's clock / reset signal drives the DUT's input
      net, a single-driver connection. Both directions lower (a parent driving a child input net, a
      child output net driving a parent net or variable), so the full top-level testbench now passes
      this wall to the cross-unit parameter reference below. Tracked under `nets.md` (N2).
- [x] **`$signed` / `$unsigned`** system functions.
- [x] **`$clog2`** system function (LRM 20.8.1). A type-agnostic value query: ceil(log2) of the
      argument read as unsigned, with `$clog2(0)` defined as 0. It lowers to a runtime value query,
      so a constant argument folds downstream and a `genvar`-dependent argument (the ibex_alu
      butterfly bit-count bound) stays runtime. With this, `ibex_alu` lowers and emits C++
      end-to-end.

### Localized / long tail

- [ ] **Hierarchical / cross-unit reference to a parameter** (reaching a sub-instance's parameter
      through a dotted path, e.g. the `mhpmcounter` accessor in the DPI block).
- [ ] **Cross-unit reference resolved through a generate-instantiated child scope** -- a reference
      that descends into or out of a module instance created inside a generate block (ibex_ex_block
      instantiates its multiplier and divider this way). Today it aborts during lowering.
- [ ] **`$value$plusargs`** -- runtime plusarg query (ibex_tracer reads the trace-enable plusarg).
- [ ] **A procedural statement form in ibex_cs_registers** -- one unsupported statement shape,
      recorded for follow-up once isolated.
- [x] **Constant of an unpacked array type** -- an elaboration-time `localparam` array referenced
      (and element-selected) in an expression. An unpacked struct or union constant is still
      blocked, but on unpacked-struct / union _type_ support rather than on constant
      materialization.
- [ ] **Reduction operator over a `$bits`-derived part-select in a continuous assign** -- a
      structural (continuous-assign) right-hand side that applies a reduction operator to a
      part-select whose width comes from `$bits` (the `ibex_top` parity check,
      `assign unused = ^busy_q[$bits(mubi_t)-1:1]`). This is `ibex_top`'s first stop once its
      children lower.
- [ ] Further structural-expression forms surfaced as later passes get deeper (recorded here as
      discovery continues).

## Robustness (crashes, not missing features)

Some unsupported inputs abort with an internal error instead of a located, graceful diagnostic.
Independent of whether the feature is supported, each of these should fail cleanly:

- [x] `$signed` / `$unsigned` aborts as an "unresolved system subroutine" rather than a clean
      unsupported diagnostic (subsumed once the feature above lands).
- [x] A non-literal where an integer constant is expected aborts instead of diagnosing.

## Cross-references

- Execution-path wall: `architecture-reset.md`.
- Net-typed ports and hierarchical references also surface in the hierarchy workstream; close them
  there if that file's items cover the same construct.
- The Ibex-side how-to-reproduce and status snapshot live in the Ibex checkout's `LYRA.md`.

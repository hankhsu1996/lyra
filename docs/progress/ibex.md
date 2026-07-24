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

- [x] **Hierarchical / cross-unit reference to a parameter or enum constant** -- reaching a
      sub-instance's `localparam` or enum value through a dotted path (the `MHPMCounterNum` accessor
      in the DPI block). A hierarchically reached compile-time constant resolves to its value
      independent of the path, matching a same-scope reference to the same constant.
- [x] **DPI-C export** -- `export "DPI-C"` exposes an SV subroutine to C (LRM 35.5).
      `ibex_simple_system` exports `mhpmcounter_num` / `mhpmcounter_get`, and `ibex_if_stage`
      exports the icache scramble-key helper `simutil_get_scramble_key`, both to the Verilator C++
      testbench -- unneeded by the pure-SV `ibex_simple_system_tb` run, but the unmodified source
      still declares them. Now lowers through the C++ backend under the single top-level instance
      case (`dpi.md` D4, D4b, D6, D6b, all landed). The multi-instance dispatch (D4a) is not needed
      for either `ibex_top` or `ibex_simple_system_tb`, which each carry a single top-level
      exporting instance.
- [ ] **`$readmemh`** -- memory backdoor load from a hex file (LRM 21.4.1). The `ibex_simple_system`
      SRAM model calls `$readmemh(MemInitFile, mem)` in an `initial` block through the vendor helper
      `prim_util_memload.svh` (included from `prim_ram_1p` / `prim_ram_2p`), which is how the
      testbench boots a program image into RAM under the `SRAMInitFile` parameter. Not yet
      supported. Current top-level frontier for `ibex_simple_system_tb`.
- [ ] **Hierarchical reference reaching a module instance from a nested generate scope** -- a dotted
      reference, written inside a conditional or loop generate block, that descends into a module
      instance owned by an enclosing scope (the RVFI trap logic in `ibex_core`,
      `id_stage_i.controller_i.exc_req_d` inside `gen_rvfi_no_wb_stage`; `ibex_ex_block` reaches its
      generate-instantiated multiplier and divider the same way). Status un-reverified since it was
      last observed; hidden behind `$readmemh` on the full top and out of the path for `ibex_top`
      standalone. Recheck once `$readmemh` lowers.
- [x] **`$value$plusargs` / `$test$plusargs`** -- runtime plusarg query (LRM 21.6). The full surface
      is live: `$test$plusargs` probes for a prefix, `$value$plusargs` parses the matched plusarg's
      remainder under `%d` / `%o` / `%h` / `%x` / `%b` / `%s`, and the host command line populates
      the plusargs source (`+`-prefixed argv entries flow through `lyra run` to the built program).
      `ibex_tracer` can now be enabled by a real trace-enable plusarg. Real (`%e` / `%f` / `%g`)
      conversions remain out of scope.
- [x] **A procedural statement form in ibex_cs_registers** -- the module lowers and emits C++
      end-to-end as its own top.
- [x] **Constant of an unpacked array type** -- an elaboration-time `localparam` array referenced
      (and element-selected) in an expression. An unpacked struct or union constant is still
      blocked, but on unpacked-struct / union _type_ support rather than on constant
      materialization.
- [x] **Reduction operator over a `$bits`-derived part-select in a continuous assign** -- a
      structural (continuous-assign) right-hand side that applies a reduction operator to a
      part-select whose width comes from `$bits` (the `ibex_top` parity check,
      `assign unused = ^busy_q[$bits(mubi_t)-1:1]`). `$bits` lowers to an elaboration constant and
      the mixed-domain part-select bound reads correctly (`operators.md` W14).
- [ ] Further structural-expression forms surfaced as later passes get deeper (recorded here as
      discovery continues).

## Robustness (crashes, not missing features)

Some unsupported inputs abort with an internal error instead of a located, graceful diagnostic.
Independent of whether the feature is supported, each of these should fail cleanly:

- [x] `$signed` / `$unsigned` aborts as an "unresolved system subroutine" rather than a clean
      unsupported diagnostic (subsumed once the feature above lands).
- [x] A non-literal where an integer constant is expected aborts instead of diagnosing.
- [x] A package function calling a peer package function (LRM 26.3) tripped the writeback pre-check
      on a body with no enclosing structural scope. Fixed: the check now inspects the callee kind
      first, so a body without an enclosing structural scope only reaches the lookup when a callee
      that actually needs it appears. The `prim_cipher_pkg` intra-package call chain that first
      surfaced this crash now lowers.
- [x] A `unique` / `unique0` / `priority` case inside a package function (LRM 12.5.4 in a LRM 26.3
      body) tripped the deferred-check machinery, which reaches the runtime through the enclosing
      body's receiver. A package body carries no receiver. The lowering now recognises this and runs
      the case as an ordinary cascade, dropping only the runtime warning (which the LRM allows the
      tool to skip). The `prim_secded_pkg::is_width_valid` nested `unique case` that first surfaced
      this crash now lowers. Reinstating the runtime warning for package callables is a separate
      follow-up on services-threading, not a Ibex-bring-up gap.

## Cross-references

- Execution-path wall: `architecture-reset.md`.
- Net-typed ports and hierarchical references also surface in the hierarchy workstream; close them
  there if that file's items cover the same construct.
- The Ibex-side how-to-reproduce and status snapshot live in the Ibex checkout's `LYRA.md`.

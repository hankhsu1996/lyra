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

## Status

`ibex_simple_system_tb` simulates end-to-end. The whole design lowers with no diagnostics, emits C++
a host compiler accepts, links, loads `hello_test` through `$readmemh`, executes it, and terminates
on the testbench's own software request -- with the program's expected output and a full instruction
trace. Sources, includes, and defines are still passed on the command line rather than read from
`lyra.toml`, which is the one accepted-option gap left between this and the closing condition above.

**Nothing runs this design automatically**, so that claim is only ever as fresh as the last time
someone ran it by hand. It has already been false once while standing here unchanged: a lowering
identity minted for a construct the assertion policy elides aborted seventeen of the design's
twenty-four modules for three days, and no test in the repository covers the option that reaches it.
Re-run before trusting this section, and treat a status sentence here as a measurement rather than a
property.

The run reports `unique` and `priority` violations from most modules, and they are Lyra's error
rather than the design's. Every one of those statements carries a `default` item or a final `else`,
and a catch-all suppresses the no-match report: LRM 12.4.2 issues it "if no condition matches unless
there is an explicit `else`", and LRM 12.5.3 issues it if no `case_item` matches, where `default` is
one of the `case_item` forms. Lyra counts only the compared arms, so a design that spells its
catch-all out is warned at on every unmatched pass. The design still executes correctly, so this is
noise rather than a wrong answer -- but it is noise on nearly every module, and it is what stands at
the frontier now that the forms below are cleared.

## Two walls

1. **Feature-lowering gaps** -- the unsupported SystemVerilog forms below. This wall is down. What
   the list below now tracks is any further form a deeper pass turns up, not a standing blocker.
2. **Execution backend** -- also down for the C++ path, which carries the run above. What remains is
   convenience rather than reach: project mode (`lyra.toml` lookup) is still not wired, so sources,
   includes, and defines must be passed explicitly on the command line. The LLVM / JIT path is a
   separate backend tracked in `execution-backend.md`; it is not required for the C++ run.

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
- [x] **Variable-width part-select inside a conditional generate** -- a part-select whose _width_
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
- [x] **`$readmemh` / `$readmemb`** -- memory load from a hex / binary text file (LRM 21.4). The
      `ibex_simple_system` SRAM model calls `$readmemh(MemInitFile, mem)` in an `initial` block
      through the vendor helper `prim_util_memload.svh` (included from `prim_ram_1p` /
      `prim_ram_2p`), which is how the testbench boots a program image into RAM under the
      `SRAMInitFile` parameter. Supported: `@address` directives, `//` and `/* */` comments,
      per-digit x / z, an explicit start / finish range (descending when start > finish), and
      unaddressed words left unchanged. Ibex needs only the one-dimensional form.
- [x] **Hierarchical reference reaching a module instance from a nested generate scope** -- a dotted
      reference, written inside a conditional or loop generate block, that descends into a module
      instance owned by an enclosing scope (the RVFI trap logic in `ibex_core`, and `ibex_ex_block`
      reaching its generate-instantiated multiplier and divider the same way). No longer a lowering
      blocker: with `$readmemh` cleared, the whole `ibex_simple_system_tb` lowers to MIR with no
      diagnostics and emits C++, so this form carries through the full design.
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
- [x] **Enum value as an operand of a packed concatenation** -- the compressed decoder builds a
      32-bit instruction by concatenating an enum member (the opcode). Cleared by representing an
      enum value as its base packed integral (the enum value model is now base-integral; see
      `datatypes.md` Enum). The same change removed the anonymous-enum cross-module name collision
      that otherwise blocked the full-system C++ compile.
- [x] **A constant read where sensitivity is inferred** -- a `parameter`, `localparam`, or enum
      member read inside `always_comb`, `always @*`, a `wait`, or a continuous assign (an
      `exc_cause_e` member, a CSR bit-index parameter, the instruction-encoding `casez` in
      `ibex_tracer`). LRM 9.2.2.2.1 infers the implicit sensitivity list from net and variable
      identifiers, and a constant is neither, so such a read contributes nothing to it -- neither
      folding it into the subscription nor materializing a cell for it is right, since a value that
      cannot change is not something a process can wait on. Hits `ibex_controller`, `ibex_if_stage`,
      `ibex_cs_registers`, `ibex_tracer`.
- [x] **DPI-C export from inside a generate scope** -- an `export "DPI-C"` declared within a
      generate block (the icache scramble-key helpers in `ibex_if_stage`). The entry point is a
      program-global C symbol and so sits outside every scope (LRM 35.7), which is where the
      generate scope's own name has to reach it. A class of a compilation unit is now named by one
      identifier valid anywhere the unit is, rather than by a position a reference has to
      reconstruct.
- [x] **A net whose value is an unpacked array** -- an unpacked-array port declared with no data
      type is an implicit net array, and LRM 6.7.1 admits it: a net's data type may be a fixed-size
      unpacked array whose elements are themselves valid for a net, making it one net that resolves
      per bit. Resolution is now stated over any value a net may hold rather than over one value
      type. The system bus (`shared/rtl/bus.sv`) declares its per-host request, address, and
      write-enable ports this way.
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
      body) crashed, because the deferred check reached the runtime through the enclosing body's
      receiver and a package body carries no receiver. Runtime access no longer needs a receiver, so
      such a body lowers and its violation report fires like any other. The
      `prim_secded_pkg::is_width_valid` nested `unique case` that first surfaced this crash now
      lowers and reports.
- [x] A labelled concurrent assertion aborted lowering whenever the assertion policy elided it,
      which took out every module including the vendor assertion macro header -- most of the design.
      A statement label creates a named block around the statement it labels (LRM 16.3), and the
      front end lists that block beside the process it belongs to rather than inside it, so the
      block kept an identity of its own after the process was removed and nothing went on to fill
      it. Whether the design has such a block is now the owning process's answer.

## Cross-references

- Execution-path wall: `execution-backend.md`.
- Net-typed ports and hierarchical references also surface in the hierarchy workstream; close them
  there if that file's items cover the same construct.
- The Ibex-side how-to-reproduce and status snapshot live in the Ibex checkout's `LYRA.md`.

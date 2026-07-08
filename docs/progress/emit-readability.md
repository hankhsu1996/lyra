# Emitted Code Readability

The emitted C++ exists to compile, but it is also the **human-readable rendering of MIR** -- the way
a developer validates that MIR's semantic model captures the SystemVerilog source correctly. Until
LIR and LLVM IR exist, it is the primary surface for that validation alongside the MIR dumper: a
developer reads the SystemVerilog source, reads the emitted C++, and confirms MIR's semantic model
produced the expected shape. Compile and run is the machine half of the verification; reading the
emit is the human half.

This file tracks the gap between "it compiles" and "it reads like code a person would write." The
work is done when a developer can open an emitted unit and follow it top-down without boilerplate or
incidental structure getting in the way.

**Compile-time trades.** Emit readability outranks emit compile time. A change that makes the
emitted form less readable to shave per-case compile time is a wrong trade -- compile-time wins must
come from the runtime library, build infrastructure (precompiled headers, parallel compilation), or
backend-internal organization that does not change the emitted form. `compiler_overview.md` carries
this as a Forbidden Shape.

This is the artifact-legibility companion to `dev-ergonomics.md`: that file owns the run / observe /
locate-divergence feedback loop; this one owns the readability of what the loop produces.

## Sub-Steps

- [x] The emitted C++ can be reformatted to a consistent layout (indentation, wrapping of long
      lines) on request. This is opt-in and best-effort: it never gates emission and is skipped when
      no formatter is available, so it is a convenience for reading, not a build dependency. It
      addresses layout only; the structural items below stand on their own.
- [x] A generated scope carries only the behavior it actually has -- a scope with no processes, or
      no children, emits no empty placeholder for them.
- [x] A scope's children are linked implicitly at construction; the traversal the scheduler walks is
      not spelled out again in every emitted class.
- [x] A generated class reads top-down: nested scopes, then construction, then behavior, then state.
- [x] An owned sub-object member is declared without a redundant explicit default-initializer.
- [x] A per-instance scope name is a compile-time label, not a string assembled by runtime
      concatenation.
- [x] A block that coincides with a scope its enclosing construct already opened (a process, loop,
      or branch body) does not emit a second, redundant brace scope.
- [x] A loop counter is declared with its type left to the initializer, the way a hand-written loop
      reads, rather than respelling the full type.
- [x] A literal that a value-preserving conversion would otherwise wrap is emitted directly in the
      target representation, with no conversion around it.
- [x] A common-width integer literal uses its named shorthand rather than a fully parameterized
      constructor.
- [x] A formatted-output value reads as one concise constructor rather than a stack of descriptor
      and value-view wrappers, and the descriptor spells out only the fields that differ from their
      defaults.
- [ ] A formatted-print statement reads as a format string and its arguments, mirroring the
      SystemVerilog source one line for one line, rather than an expanded item list. Direction: lean
      on the standard library's format facility, teaching it the runtime value types and reusing the
      existing formatting engine; the parsed-item list stays in the IR, only the emitted form
      changes. This is a self-contained print-representation migration sized for its own change set,
      with a wide regression surface across the print tests; `%m` is already unsupported and stays
      out of scope. The intermediate concise-constructor form above is superseded when this lands.
- [ ] An expression is parenthesized only where operator precedence requires it; an outermost
      expression carries no enclosing parentheses.
- [ ] The design's top-level entry -- constructing each top-level unit and binding the assembled
      hierarchy so the simulation can run -- is a mechanical rendering of ordinary generated
      behavior, shared by every backend, not a hand-fabricated harness. Today the ahead-of-time path
      composes it as bespoke target text that names the runtime driver surface directly, and the
      in-process path re-implements the same construct-and-bind sequence on its own -- the one place
      a backend invents structure instead of translating it (a render-contract gap; see
      `../architecture/backend_contract.md`). Direction: express the construct-and-bind sequence as
      generated behavior lowered like any other, so both backends render it through the same path;
      only the thin outer entry-point shell (a compiled program's entry point, the in-process
      driver) stays target-specific and merely invokes it. A self-contained follow-up, not part of
      the current change set.

## Out of Scope

- Behavioral or language-feature coverage. Those live in the per-feature progress files.
- The run / observe / locate-divergence loop (`dev-ergonomics.md`).
- Shortening fully-qualified type names. Generated code cannot assume the namespace context it will
  be read in, so full qualification is kept deliberately; it is not a gap to close.
- In-place declaration of a static-lifetime body local. A SystemVerilog static local is
  per-instance, and C++ has no per-instance variable declared inside a method body, so it is emitted
  as a member on the enclosing class rather than at its source position, and its name carries a
  uniqueness suffix. The hoist is inherent to per-instance static storage; it is not a gap to close.

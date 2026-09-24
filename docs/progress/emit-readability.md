# Emitted Code Readability

The emitted C++ exists to compile, but it is also the **human-readable rendering of MIR** -- the way
a developer validates that MIR's semantic model captures the SystemVerilog source correctly. Until
LIR and LLVM IR exist, it is the primary surface for that validation alongside the MIR dumper: a
developer reads the SystemVerilog source, reads the emitted C++, and confirms MIR's semantic model
produced the expected shape. Compile and run is the machine half of the verification; reading the
emit is the human half.

This file tracks the gap between "it compiles" and "it reads like code a person would write." The
work is done when a developer can open either of the files an emitted unit becomes -- what it
declares, or what realizes it -- and follow that file top-down without boilerplate or incidental
structure getting in the way.

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
- [x] An aggregate whose elements all take one value is described by that value and a count, not by
      writing the value out once per element. Giving a 32768-element array a uniform value used to
      reach the target language as a single four-megabyte expression, which no host compiler
      accepts, so a design doing what a memory reset does could not be built at all. The general
      form is worth holding on to as more aggregate kinds arrive: a uniform aggregate must not cost
      its own length to describe.
- [x] The same, for an aggregate that is uniform apart from a few named positions. Describing it
      takes a construction followed by writes to those positions rather than one expression, which
      is what the steps-yielding-a-value form is for. The execution backend refuses that shape today
      for a reason of its own -- an interior write names storage, and a local there holds a value --
      so the case is recorded against that path and returns when its storage model does.
- [x] The same again where the aggregate is packed. A packed value's own repeat form describes a
      uniform one in constant size, and a run of elements taking the default between two named ones
      is one such repeat however long it is, so the aggregate costs its named positions to describe
      rather than its length. Both execution paths take this shape: a packed value is a single
      value, so it is built as one expression with nothing written into afterwards, and the storage
      question that holds the unpacked case back does not arise.
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
- [ ] A string literal reads as the text it is. In a string-typed context it is emitted today as the
      decimal integer its characters spell, wrapped in a conversion to the string type, so the text
      the source wrote appears nowhere in the emitted unit and a constant known at compile time is
      rebuilt on every execution. A literal reaching a formatted print already reads as its own
      text, which is the form the rest should take. The surface is every string literal on both
      backends, so it is sized for its own change set.
- [ ] An expression is parenthesized only where operator precedence requires it; an outermost
      expression carries no enclosing parentheses.
- [ ] A cell reads as the identifier the source wrote rather than as its position with the
      identifier after it. The position has to lead today because the target holds everything a
      class declares in one name space, and a class holds its storage beside the behaviors it takes
      over -- whose identifiers are the identifiers of the cells they answer with, since that is
      what a referrer spells. Nothing shorter is available while both live in one name space, so
      this closes only if the two stop sharing one, which is a question about how a unit offers what
      it published rather than about how a field is spelled.
- [ ] The design's top-level entry -- constructing each top-level unit and attaching the assembled
      hierarchy so the simulation can run -- is a mechanical rendering of ordinary generated
      behavior, shared by every backend, not a hand-fabricated harness. Today the C++ backend
      composes it as bespoke target text that names the runtime driver surface directly -- the one
      place a backend invents structure instead of translating it (a render-contract gap; see
      `../architecture/backend_contract.md`). The LLVM backend's entry already hands the root's
      definition to the runtime and lets it construct and bind. The pinned model is
      `../decisions/root-unit-elaboration.md`: elaboration is the synthetic `$root` unit's construct
      entry, lowered like any other and rendered by both backends through the same path; only the
      thin runner shell, a compiled program's entry point, stays target-specific, creating the
      engine and calling the root construct, then bind / run. A self-contained follow-up, not part
      of the current change set.

      **Where the shell has to stay, and it is a language rule rather than a shortage of work.** A
      compiled program's entry cannot itself be an ordinary body of the root unit: the target
      reserves that name at global scope and forbids it the language linkage a unit's bodies are
      reached by, and a body of a unit is reached either through that unit's own name space or
      under a linkage name. So the shell survives any amount of work, and what it may shrink to is
      one line handing its arguments to a body that is ordinary.

      **What stands between here and that line.** The body would take what a host hands over and
      answer with what a host expects, and its one statement would call the runtime's design entry
      with the label the root carries and the entry that makes the root's object. Every piece of
      that is sayable except one: **the entry is handed over as a value, and a reference to one of
      a unit's own bodies that answers to no name has no value form.** A call reaches such a body by
      naming its unit and which body it is; nothing names one where a value is wanted. That is the
      piece to design first, and it is the same shape one layer over -- an identity that is not a
      name -- so the design question is where a value form of it belongs rather than whether one
      can exist.

      **The measure of the gap, so a later reader can tell whether it moved.** What the entry writes
      out is target text that names runtime library identifiers directly, which only the dispatch
      owning type spellings may do. It named four of them and now names one; the remaining one is
      the runtime's design entry itself, and it goes when the body above becomes ordinary.

## Out of Scope

- Behavioral or language-feature coverage. Those live in the per-feature progress files.
- The run / observe / locate-divergence loop (`dev-ergonomics.md`).
- Shortening fully-qualified type names. Generated code cannot assume the namespace context it will
  be read in, so full qualification is kept deliberately; it is not a gap to close.
- In-place declaration of a static-lifetime body local. A SystemVerilog static local is
  per-instance, and C++ has no per-instance variable declared inside a method body, so it is emitted
  as a member on the enclosing class rather than at its source position, spelled from the position
  it took there rather than from the name the source wrote. The hoist is inherent to per-instance
  static storage, and the spelling is what keeps a name the compiler composes out of the space a
  design declares in (`../decisions/a-name-is-a-relation-not-an-identity.md`); neither is a gap to
  close. Which declaration a position is, is a question the IR dump answers.

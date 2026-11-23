# Decisions

A record here is a choice of direction, one that reaches across layers, is costly to reverse, and
that a later change could reverse without knowing it had been chosen. It opens with the requirement
the choice serves and writes each alternative it turned down as the requirement that alternative
fails and the premise the failure rests on, so a change whose requirements differ can see that the
record does not bind it. `../style.md`, under Decisions, is the rule.

A record is history. How Lyra works now is stated once, in [`../architecture/`](../architecture/),
and a record points there rather than restating it. An accepted record is not rewritten; a later
choice that reverses or narrows one is a new record naming the one it replaces, and the old one
gains a line naming its successor.

A file is named in `kebab-case.md` for the choice, never for when it was made; the date is in its
Status line.

## Index

### The language and the front end

- [the-standard-decides-what-text-is-legal](the-standard-decides-what-text-is-legal.md) -- the
  default reading is the front end's own, legality is judged once there and corrected there, and its
  severity is shipped as given.
- [front-end-semantic-boundary](front-end-semantic-boundary.md) -- the front end owns what a name
  denotes and what a procedure is sensitive to; Lyra translates what it resolved and never
  recomputes it.
- [a-place-in-the-source-is-the-front-ends](a-place-in-the-source-is-the-front-ends.md) -- a place
  is held as the front end gave it, the front end shows every message, and a message opens with
  `error:`, `warning:` or `note:`.
- [the-front-end-fork-keeps-a-branch-per-release](the-front-end-fork-keeps-a-branch-per-release.md)
  -- the slang fork keeps one branch per upstream release, which only grows, so every pinned commit
  stays fetchable.

### Compiling a design

- [compiling-a-design-and-running-it-are-separate-acts](compiling-a-design-and-running-it-are-separate-acts.md)
  -- compiling produces a program complete without the compiler, and running a design is building
  and executing it.
- [every-body-the-front-end-does-not-share-is-lowered](every-body-the-front-end-does-not-share-is-lowered.md)
  -- which instances elaborate alike is the front end's word, and equal code is found by comparing
  what was lowered, never by a key worked out before.
- [an-elaboration-time-value-is-an-input](an-elaboration-time-value-is-an-input.md) -- a body is
  lowered as the expressions it states, and a value belongs to the class only where a different
  value would be a different class.
- [a-name-is-a-relation-not-an-identity](a-name-is-a-relation-not-an-identity.md) -- a declaration
  carries no name, and a program-wide symbol is self-delimiting parts under a category.
- [nothing-lyra-compiles-is-distributed-as-a-binary](nothing-lyra-compiles-is-distributed-as-a-binary.md)
  -- every unit is compiled by Lyra from source, so a unit may know another's published storage
  whole.
- [project-file](project-file.md) -- `lyra.toml` declares a library and the design run of it, never
  an invocation, and the command line selects within it.

### Values and the runtime library

- [a-value-is-its-machine-data](a-value-is-its-machine-data.md) -- a value is held as the machine
  holds data of its size, judged by what each cost scales with, beginning with an operation costing
  the bits it touches.
- [the-library-is-parametric-in-the-types-a-design-composes](the-library-is-parametric-in-the-types-a-design-composes.md)
  -- the library never chooses by a type's kind nor walks its composition; it reaches a composed
  value through a service, a trait item, or a description read as data.
- [the-classes-of-std-are-written-in-systemverilog](the-classes-of-std-are-written-in-systemverilog.md)
  -- `semaphore`, `mailbox` and `weak_reference` are SystemVerilog Lyra carries, compiled as the
  unit `std`.
- [lyra-links-no-cpp-front-end](lyra-links-no-cpp-front-end.md) -- Lyra links no C++ front end,
  generated code matches no C++ compiler's ABI, and the foreign boundary is C.

### Objects and lifetime

- [the-class-model-is-javas](the-class-model-is-javas.md) -- every class question takes Java's
  answer, never the one C++ or a backend's host language brings.
- [object-model](object-model.md) -- a class handle is a managed reference, reclaimed by precise
  tracing, and nothing that survives a safepoint hides in a backend's opaque state.

### Execution

- [an-evaluation-runs-its-operands-first-to-last](an-evaluation-runs-its-operands-first-to-last.md)
  -- the order of one evaluation's operands is decided above every target, so a design answers the
  same on every backend and host compiler.
- [run-time-failure-is-not-an-outcome](run-time-failure-is-not-an-outcome.md) -- an activation
  settles a value or a departure; a failure is told apart by who acts on it, and every ending walks
  one tail.
- [combinational-updates-are-coalesced-where-a-build-asks](combinational-updates-are-coalesced-where-a-build-asks.md)
  -- every update is an event by default; a library may be built in a dialect where combinational
  procedures see each other's net change.
- [coroutine-splitting-is-llvms](coroutine-splitting-is-llvms.md) -- the execution backend states
  where a body suspends, and LLVM's coroutine passes lay out its frame.

### Building Lyra and its programs

- [a-build-compiles-for-the-oldest-platform-it-supports](a-build-compiles-for-the-oldest-platform-it-supports.md)
  -- the build compiles for glibc 2.34 and libstdc++ 13.2 with a clang that needs nothing of the
  machine.
- [the-pinned-clang-builds-the-lyra-that-is-run](the-pinned-clang-builds-the-lyra-that-is-run.md) --
  what Lyra writes is the same on every run of one build, and the build that is run is the pinned
  clang's.
- [the-design-is-optimized-with-the-runtime-at-the-link](the-design-is-optimized-with-the-runtime-at-the-link.md)
  -- under `--lto` the design and the runtime are optimized as one program by the compiler that
  compiles the design.

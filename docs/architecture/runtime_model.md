# Runtime Model

A compilation unit is a class. Compile-time produces the class definition: its parameters, its
constructor body, its callable bodies. Runtime has two contexts. The **constructor** runs once per
object at time zero and builds the object graph. The **simulation** runs at time >= 0 and drives
behavior under scheduler control. The two contexts do different jobs; HIR splits its lowering paths
because of this split.

## A module is a class

Read every SystemVerilog construct in a module as a piece of a C++ class:

- `module Foo #(parameter N = 4);` is class `Foo` with a constructor parameter `N`.
- `for (genvar i = 0; i < N; i = i + 1) begin : g child u(); end` is a `for` loop in the class
  constructor. It instantiates `N` children.
- `if (N > 0) begin ... end` is an `if` in the class constructor. It picks which sub-tree to
  instantiate.
- `initial begin ... end` is a class method that the scheduler calls once at simulation time zero.
- `always_ff @(posedge clk) begin ... end` is a class method registered with an event source and
  called by the scheduler on each matching event.

Generate constructs are constructor code. `initial` and `always*` are runtime methods. They run in
different contexts and they do different jobs.

## Constructor runs at t = 0, simulation runs at t >= 0

The constructor allocates per-object state, binds the parameter values it was called with, walks
generate-if / generate-for / generate-case to decide which children to instantiate, instantiates
them, and returns. It does not advance simulation time. It does not yield to the scheduler. It does
not run any `always` body. When it returns, the object graph is fixed.

The simulation runs after the constructor returns. Processes get pulled off scheduler queues and
resumed. They read and write per-object state. They do not create or destroy objects. The graph
shape does not change once the simulation starts.

## Generate is not compile-time expansion

A generate-for with 1024 iterations does not produce 1024 things at compile time. Compile-time
produces one class definition with a `for` loop in its constructor. At runtime, the constructor runs
that loop 1024 times to build 1024 children. Compile-time work scales with the number of distinct
module specializations, not with the instance count or the generate iteration count.

This is the central principle of the compiler. If you find compile-time work that scales with
instance count, the design has been violated.

## Structural code has no general assignment

Constructor-side code is not a general procedural language. There is no `=` that means "store into
this variable" inside generate. The only writes during construction are:

- **Parameter binding** -- the constructor records the parameter values it was called with. This is
  owned by the constructor, not by a free-standing assignment.
- **Generate-for iteration step** -- the loop's own iteration advances the loop variable. Slang
  hands us an `Assignment` node for `i = i + 1` because that is how SystemVerilog spells the step,
  but HIR keeps only the **next-value expression** (`i + 1`). The write is part of the loop's
  semantic, owned by the loop, not by an `AssignExpr`.

So HIR has no structural `AssignExpr` and no structural Lvalue lowering. The constructor body is a
tree of generate constructs over pure value expressions.

## Simulation code does have general assignment

Inside an `initial` or `always*` body, `=` and `<=` are first-class assignment forms. Compound
assignments (`+=`, etc.) flow through slang's `LValueReference` machinery. Lvalue is a first-class
HIR concept on the process side, and only on the process side.

## Why this matters for lowering

HIR carries the split explicitly. A **structural scope** holds generate constructs, parameter
expressions, and child scopes. No procedural variables. No assignments. A **process scope** holds an
`initial` or `always*` body. Procedural variables. Assignments. It may read structural variables but
does not own them.

HIR-to-MIR lowers the two scopes through different functions because they target different runtime
contexts. A process body cannot run constructor code; it cannot instantiate a child. A constructor
body cannot run process code; it cannot wait for an event. A single lowering path that erased the
distinction would force one side to carry information the other side does not need, or hide a
constraint that the type system should be enforcing.

## What this rules out

- A process that instantiates a child, evaluates a generate construct, or otherwise changes the
  object graph.
- A structural expression that reads or writes a procedural variable.
- A structural expression that depends on simulation-time state (the current time, an event, a
  process activation).
- A free-standing `AssignExpr` in a structural scope.
- A pipeline where the frontend-elaborated instance graph drives compile-time work.
- A constructor body that yields to the scheduler or advances simulation time.

## Adjacent docs

- `compiler_overview.md` covers the compile-time / runtime split at the level above.
- `compilation_unit_model.md` defines what is class-level (compile-time artifact) and what is
  per-instance (constructor input).
- `hierarchy_and_generate.md` defines the object graph that the constructor builds.
- `scheduling.md` defines how the simulation context dispatches processes.

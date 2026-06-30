# Variable initialization (LRM 10.5) as constructor block statement

Date: 2026-06-16 Status: accepted

## Context

A SystemVerilog variable declaration may carry an initializer at its declaration site:

```sv
module Top;
  int a = 1;
  int b = a + 1;
endmodule
```

LRM 10.5 names this construct a _variable declaration assignment_, parenthetically _variable
initialization_, and defines it as **"a special case of procedural assignment"** that "shall occur
before any initial or always procedures are started". The semantic content is an assignment whose
timing is pinned to elaboration.

Before this decision, MIR carried the variable initializer two ways:

1. `mir::MemberDecl.initializer: ExprId` -- the init expression as a field on the declaration
   itself.
2. `constructor.body.root_stmts` -- the constructor-time statement sequence (`AttachChild`,
   `RegisterSignal`, `CreateProcesses`, generate construction, ...).

The C++ backend plucked path (1) into an inline class-body NSDMI (`Var<int> a{1};`) and rendered
path (2) inside a static `init(Self* self)` helper. A `RenderContext::in_class_member_init_` flag
switched receiver-access rendering between NSDMI position and function-body position, because
`MemberAccessExpr(self, X)` and `ParamRef(p)` had to spell as the bare field name in NSDMI scope but
`self->X` in the body.

The two-channel shape forced every backend to read state from two places. It also locked the render
to a C++-specific syntactic mode dependency that a LIR / LLVM-IR backend would have no analogue for.

## Decision

**MIR has exactly one shape for construction-time work: statements in `constructor.body.root_stmts`.
For every value-assignable member, HIR-to-MIR inserts an
`AssignExpr(MemberAccess(self, var), value)` statement at the position the variable is declared in
source order, before any `RegisterSignal` / `AttachChild` / `CreateProcesses` for the same scope.
The value is the user-supplied expression when present, otherwise the LRM Table 6-7 type default;
the statement shape is uniform either way. The `mir::MemberDecl.initializer` field is removed; a
member declaration carries name and type only.**

Vars whose type has no value-assignment semantics -- owned children (pointer, vector), object
companions, upward references (`ExternalRefType`), and named events -- do not receive an init
statement. Their declaration shape itself fixes the field at construction:

- Pointer / vector / object / external-unit-object fields default to their C++ default (null
  pointer, empty container).
- An `ExternalRefType` field is constructed at the declaration site with the symbol payload
  (ancestor, by-name tail, leaf signal) and relocates at Bind.
- A `NamedEvent` field is default-constructed; the event identity is fixed for the lifetime of the
  scope and never reassigned.

### C++ render

A class member is declared with a value-init brace, never an `=`:

```cpp
lyra::runtime::Var<lyra::value::PackedArray> a{};
lyra::runtime::Var<lyra::value::PackedArray> b{};
```

The C++ constructor body is a thin wrapper that delegates to a static helper:

```cpp
Test(Scope* parent, std::string name, RuntimeServices& services)
    : Instance(parent, std::move(name), services) {
  init(this);
}
static void init(Test* self) {
  self->a.Set(self->Services(), PackedArray::Int(1));
  self->b.Set(self->Services(), self->a.Get() + PackedArray::Int(1));
  self->RegisterSignal("a", &self->a);
  self->RegisterSignal("b", &self->b);
}
```

Every other MIR callable body (process, method, closure) also renders as a static function whose
first parameter is `Self* self`; the constructor's delegation to `init(this)` is the one place C++'s
implicit `this` is mentioned. The body code emits `self->X` mechanically with no context-dependent
receiver lookup. `RenderContext::in_class_member_init_` and `WithClassMemberInit` are removed in
this decision.

### Runtime changes that make the shape possible

For an assignment statement in the constructor body to work, the C++ runtime must supply two things
that were previously deferred:

1. **`Var<T>::Set` needs a valid `RuntimeServices` reference.** Previously `Scope::services_` was
   null until `Engine::BindDesign` ran, so a constructor-body `Set` threw
   `Scope::Services: scope not bound`. The constructor now takes `RuntimeServices& services` as a
   third parameter and wires `services_` at construction. `Scope::Bind` no longer takes a services
   argument; it only relocates `ExternUp` members, creates processes, and recurses. The generated
   host `main.cpp` reflects this: `Engine` is constructed first, then each top instance receives
   `engine.Services()` at construction.
2. **`Var<T>` must be default-constructible.** A `Var<PackedArray> a{};` value-initializes the inner
   `T` through `T value_{}`. `PackedArray`, `UnpackedArray<T>`, and `DynamicArray<T>` gain default
   constructors that produce a 0-bit unestablished shape; the cell installs its declared type at
   construction, before any store, and a store then requires the right-hand side to already be at
   that representation (LRM 10.6.1). The `WordCountForBits` / `PackedWords` 0-bit guards are relaxed
   correspondingly. The default state is unobservable: MIR guarantees the install runs before any
   read.

### Why every value var gets an init statement, including the default case

The "user wrote `= value`" and "no user value, take LRM Table 6-7 default" cases differ in MIR by
exactly one expression node -- the lowered HIR initializer versus the type default literal -- but
the statement shape is identical. A unified statement list lets a backend treat construction-time
state as one mechanism: walk the list, render each statement. A backend that peephole-folds "first
stmt is a literal-Set on a freshly declared var" into NSDMI is free to do so, but the primary
contract is the statement list.

LLVM-IR makes this contract literal: every var's initial value lowers to a `store` at the start of
the constructor function, with no NSDMI analogue.

### Why filter non-assignable types out of the init statement

Owned children, upward references, and named events have no value-assignment operation:

- An owned child is constructed by `ConstructExternalUnitStmt` or generate-construction stmts later
  in `root_stmts`, with its own arguments.
- An `ExternalRefType` is constructed at the declaration site with symbol arguments that have no
  value-assignment counterpart.
- A `NamedEvent` is non-copyable and non-movable by design (LRM 15.5; the runtime takes pointers
  into the event object); assigning over it would require a copy/move operator the type does not
  provide.

The HIR-side declaration for these types is the equivalent of "this field exists with its
construction-time identity"; there is no later "set its value" semantic to encode. Emitting an
`AssignExpr` for them would either fail to compile (events) or invent a no-op write (containers,
pointers). The filter is structural: any var whose MIR type is in the set
`{Pointer, Vector, ExternalRef, Object, ExternalUnitObject, Event}` does not get an init statement.

## Rejected alternatives

- **Keep `MemberDecl.initializer` and continue using inline class-body NSDMI.** Matches C++ idiom
  most closely and is the previous shape. Costs: MIR carries two ways to express the same
  construction-time work; the `RenderContext::in_class_member_init_` flag switches receiver
  rendering by syntactic position; every future backend either learns the same dual-mode dispatch or
  re-lowers `.initializer` into a statement before generating code. The bookkeeping does not pay for
  itself outside of C++ specifically.

- **Inline the C++ constructor body with a `Self* self = this;` preamble.** Invents a body statement
  that no MIR node justifies. The render must add content the MIR did not produce, which is
  precisely the smell the mechanical-translation rule was written to prevent.

- **A `RenderContext` flag that maps `self`-reads to `this` inside the constructor body.** Adds
  walker state at the moment R18 plans to remove `RenderContext` entirely. Render becomes
  context-dependent on the very rule the mechanical-translation principle wants pure. Wrong
  direction.

- **Use C++ member-initializer list
  (`Top(...) : Instance(...), a(1), b(this->a.Get() + Int(1)) { ... }`).** Native C++ alternative to
  NSDMI and a closer match to "born with value" timing. Same conceptual position as NSDMI -- a
  separate syntactic position from the constructor body, with `this` available but `self` not. The
  render would still need a flag to switch receiver rendering. No advantage.

- **Skip the init statement when there is no user-supplied initializer.** Forces every backend to
  re-derive the type default at the field declaration position, locking each backend to a per-type
  default lookup that mirrors `SynthesizeDefaultValueExpr` semantically. The unified always-emit
  shape lets `SynthesizeDefaultValueExpr` produce the literal once at HIR-to-MIR and every backend
  reads the same statement.

## Consequences

- `mir::MemberDecl.initializer` is removed; a declaration carries name and type only.
- HIR-to-MIR's class lowerer appends an `AssignExpr` statement to `constructor.body.root_stmts` for
  every value-assignable member, using the user-supplied expression or the LRM Table 6-7 default.
- The C++ backend's `RenderField` emits a pure value-init declaration (`Var<T> name{};` or
  `T name{};`), with no inline initializer. Upward references retain their symbol-arg construction.
- `RenderContext::in_class_member_init_`, `WithClassMemberInit`, and the `RenderField`
  initializer-extraction path are removed.
- `Scope` takes `RuntimeServices&` at construction; `Scope::Bind()` no longer takes a services
  argument; the generated host `main.cpp` constructs `Engine` before the top instance and passes
  `engine.Services()` to the top's constructor.
- `PackedArray`, `UnpackedArray<T>`, `DynamicArray<T>` gain default constructors that install their
  declared type at construction; `WordCountForBits` and `PackedWords::PackedWords` accept 0-bit
  input.
- LIR / LLVM-IR lowering reads the construction-time state straight from
  `constructor.body.root_stmts`. No backend re-derives type defaults or syntactic position from a
  side field.
- Forbidden Shape: two MIR mechanisms for "code that runs at construction time", or a render that
  adds body statements not present in MIR.

## Cross-references

- LRM 10.5 -- Variable declaration assignment (variable initialization)
- LRM 6.8 -- Variable declarations
- LRM 6.21 -- Scope and lifetime
- LRM 10.6.1 -- Variable declaration assignment shape preservation
- `docs/architecture/mir.md` -- invariant 11 (`self` binding); Forbidden Shapes
- `docs/decisions/callable-receiver.md` -- explicit `self` first binding

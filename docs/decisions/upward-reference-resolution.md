# Upward Hierarchical Reference Resolution by Self-Climb at Construction

## Date

2026-06-03

## Status

Accepted

## Why this decision matters

An upward hierarchical reference is a child naming a signal in one of its ancestors
(`always_comb x = Top.g;` inside a module instantiated under `Top`). Unlike a downward reference --
where the referrer owns the child it navigates -- the referrer does not own, instantiate, or even
know the type of the ancestor it must reach. What had to be settled is how the referrer, alone,
locates the ancestor object at run time and binds a direct reference to the signal, without ever
naming the ancestor's unit.

Several plausible mechanisms were considered and rejected for concrete architectural reasons before
the design converged. This record captures those reasons so the convergence is not relitigated, and
because the rejected paths each look reasonable until a specific invariant rules them out.

## Findings that shaped the design

### F1. The referrer must resolve and store the reference alone

A cross-unit reference is a stored direct pointer, resolved once and read on the hot path with no
per-access lookup. A downward reference reaches its target by navigating from an owned child member
(`&c->x`); the child is a declared dependency, so the referrer has somewhere to start. An upward
reference has no owned member to start from -- the ancestor is not a dependency -- so the open
question is how the referrer, alone, locates the ancestor at run time and binds the pointer.

### F2. slang gives the ancestor as `path[0]`, and `upwardCount` is anchored at the lexical scope

For an upward reference, slang's `HierarchicalReference` carries `path[0]` = the ancestor (the
ancestor found by upward name search) and `path[1..]` = a downward navigation from that ancestor.
`upwardCount` is the number of scopes climbed during the _lexical_ name lookup. Measured directly (a
standalone slang probe), `upwardCount` counts scopes that have no runtime object:

- A reference inside an `always_comb begin ... end` with a local declaration reports `upwardCount`
  one higher than the runtime parent hop count, because the statement block is a lexical scope with
  no runtime object.
- A reference inside a `generate for` reports `upwardCount` one higher again, because slang models
  the loop as a `GenerateBlockArray` container scope plus per-iteration `GenerateBlock` scopes,
  while the runtime materializes only the per-iteration block (the array is a vector member, not a
  scope -- `hierarchy_and_generate.md` invariant 9).

**Consequence:** `upwardCount` cannot be used as a runtime hop count. Matching it by materializing
the `GenerateBlockArray` as a runtime scope is rejected: it would contradict the array-as-vector-
member model, which is correct.

### F3. The depth is not computable at the child's compile time

A compilation unit compiles knowing only its own contents plus the _interfaces_ (name and signature:
ports and parameters) of the units it directly references by name; it never sees another unit's body
(`compilation_unit_model.md` invariants 3 and 8). A query depends only on its declared inputs, with
no global identity coupling (`incremental_build.md` invariants 1 and 6). Walking the fully
elaborated instance tree to count hops at compile time is a design-level lookup that these
invariants forbid.

The child therefore cannot see its parent at compile time, so the climb depth is unknowable when the
child is compiled.

**Consequence:** computing a constant climb count `K` at the child's compile time is rejected,
whether by counting materialized scopes or by reading `upwardCount`. Resolution must be deferred to
construction, the first phase at which the ancestor chain exists.

### F4. The same artifact can sit at different depths

One compiled artifact is instantiated many times (`hierarchy_and_generate.md` invariant 5). The same
module can be instantiated directly under `Top` (depth 1) and again under an intermediate module
(depth 2), both naming `Top.g`. No single constant baked into the artifact is correct for both
sites.

**Consequence:** making the climb depth a specialization key (one artifact per depth) is rejected.
It couples the feature to specialization that does not exist yet, and F3 already rules out computing
the depth at compile time regardless.

### F5. The reference cannot be threaded down from the ancestor

A tempting alternative is to resolve from the ancestor: have `Top` hand a pointer to `g` down
through the hierarchy (an implicit port or constructor argument forwarded at each level). This is
rejected because it requires `Top` to know that a descendant needs `g`. A parent sees only its
child's _signature_ (ports and parameters), never its body, and an upward reference lives in the
child's body, not its declared interface. Threading would force a unit's interface to grow with its
descendants' internal references, which violates the rule that a unit depends only on the interfaces
it references by name.

This is the precise difference between an upward reference and an explicit `ref` port. A `ref` port
connection is written at the parent (`.p(sig)`), so the parent knows the target and can supply it;
an upward reference is implicit in the child, so the parent cannot. They are both cross-unit
references, but they are not the same mechanism.

### F6. The ancestor is always on the direct ancestor line; siblings are reached downward

Measured directly: `Top.other.y` from a child of `Top` reports `path = [Top, other, y]` with
`upwardCount = 1`. `path[0]` (the ancestor) is the common ancestor, always on the referrer's direct
`parent` chain; `path[1..]` (`other`, `y`) is a downward navigation from the ancestor into a sibling
subtree.

**Consequence:** the climb only ever walks up the direct ancestor line to find the ancestor. It
never steps sideways into a sibling. A sibling or cousin target is reached by the existing downward
tail navigation, applied from the ancestor.

### F7. The ancestor is matched by name and the signal is fetched by name -- the referrer never names the ancestor's type

A climb that tries `dynamic_cast<Top*>` at each step and advances on failure is trial-and-error and
pulls in RTTI. Worse, any cast to the ancestor's type makes the referrer depend on a unit it does
not instantiate: an ancestor is not a declared dependency, so the referrer cannot name its class
without baking in another unit's layout, which `emission_model.md` forbids (invariants 2 and 5). The
conformant form matches the ancestor by name and then asks the ancestor for the signal by name,
through the generic `Scope` SDK -- no ancestor type appears in the referrer at all.

The match key is the ancestor's **module definition name**, read from the resolved ancestor symbol
(`path[0].symbol`'s definition name) -- a semantic value, never from source syntax (a wrapped
sub-expression like `Top.g[3]` may carry no syntax at all). It is class-level: every instance of the
referring module shares the same module name, so one artifact serves all depths. At runtime the
climb compares it against each ancestor's instance name (`Scope::Name()`) **or** module definition
name (`Scope::DefName()`); a module-name reference (`Top.g`) matches the ancestor whose `DefName()`
is `"Top"`, including the aliased `module Tb; Top dut();` where the instance name is `dut`. The one
form this does not yet honor is LRM 23.9 instance-name precedence: a reference written as an
**instance** name (`u_mid.s`) keys on the module name too, so with several instances of that module
nesting on the chain it can bind the nearer one. That is a known gap (`docs/progress/hierarchy.md`
D2d) -- the resolved AST gives the ancestor's name and definition name, but not which form the
source wrote, and recovering that needs the source token the elaborated AST does not retain.

Once the ancestor is found, the reference may still descend into the ancestor's subtree before
reaching the leaf (`Top.sib.y` climbs to `Top`, then steps down into the owned child `sib`). That
tail is by-name too, for the same reason the climb is: the referrer owns neither the ancestor nor
its children, so it cannot navigate by typed pointer. Each step asks the current scope for an owned
child through `Scope::GetChild` -- the twin of `GetSignal` for the object tree, which the owner
answers by indexing its own storage (an array hop is the owner's own `vector` subscript, never a
re-derived offset). The final leaf is fetched with `Scope::GetSignal("y")`, which the ancestor's own
emitted code answers by returning a pointer to its own member. The referrer casts the returned
`void*` to its own `Var<T>` cell type -- a type it already knows from its declaration -- never to
the ancestor's type. When the leaf sits directly on the ancestor the tail is empty, the zero-case of
the same walk. The by-name SDK realization (`DefName`, `GetSignal`, `GetChild`, the
construction-time climb) is owned by `emission_model.md`; this record fixes only the resolution
strategy, not its emitted shape.

## Decision

An upward hierarchical reference resolves by the referrer climbing its own ancestor chain at
construction. Concretely:

1. **Resolution is at construction, by the referrer, not the ancestor.** The child walks up its own
   `parent` chain to the ancestor, binds a direct pointer inside its own extern member, and reads
   through that member on the hot path. The ancestor is never involved; its construction of the
   child is unchanged and knows nothing about the child's upward references.

2. **The ancestor (`path[0]`) is matched by its module definition name, and the signal is fetched by
   name.** The key is the resolved ancestor's module name (a semantic value, never from syntax). The
   climb compares it against each ancestor's `Scope::Name()` or `Scope::DefName()` and stops at the
   nearest match, then calls `Scope::GetSignal(<signal>)` to obtain the leaf and casts the returned
   `void*` to the member's own declared cell type. No `dynamic_cast` retry loop, and no cast to the
   ancestor's type. (Instance-name precedence, LRM 23.9, is a known gap -- see F7.)

3. **The climb walks only the direct ancestor line.** It never steps into a sibling. A sibling or
   cousin target is the downward tail (`path[1..]`) applied from the ancestor, using the existing
   downward navigation.

4. **Upward is an extern member; downward is owned navigation.** They are not the same shape -- a
   downward reference navigates a child the referrer owns, an upward reference is an extern member
   naming a signal the referrer does not own:

   ```cpp
   struct Leaf {                                  // always_comb x = Top.g;  y = Top.sib.s;
     Var<int> x, y;
     ExternUp<int> g{this, "Top", {}, "g"};           // empty tail: leaf on the ancestor
     ExternUp<int> s{this, "Top", {{"sib", {}}}, "s"};// tail steps down into owned child `sib`
     // always_comb:  x = g.Get();   sensitivity subscribes to g.AsObservable()
   };

   struct Top {
     Var<int> g;
     unique_ptr<Sib> sib;
     unique_ptr<Leaf> l;
     Top(...) { ... }                                   // knows nothing about Leaf's g / s
     string_view DefName() const override { return "Top"; }
     void* GetSignal(string_view n) override {          // answers for its own signals
       if (n == "g") return &g;
       return nullptr;
     }
     Scope* GetChild(ChildRef ref) override {           // answers for its own children
       if (ref.name == "sib") return sib.get();         // (an array hop: bank.at(ref.indices[0]))
       return nullptr;
     }
   };
   ```

   The walk -- climb the parent chain by `Name()`/`DefName()` to the ancestor, step down any tail
   through `Scope::GetChild`, then `Scope::GetSignal` the leaf -- lives once in the runtime SDK
   (`Scope::ResolveUpwardScope` plus the by-name `GetChild`/`GetSignal` surface), driven by
   `ExternUp<T>::Relocate` at Bind. The referrer carries the reference as an ordinary member whose
   type is `ExternalRef<T>` (the symbol rides on the type); reads, writes, and sensitivity are the
   normal structural-var paths, dispatched by that type. MIR keeps no cross-unit slot or resolve
   statement for an upward reference -- the typed member is the whole story, which is why
   `type is the classification` holds here (see `docs/architecture/mir.md` and `emission_model.md`).

5. **No compile-time depth, no global tree walk, no interface threading, no RTTI retry.** The climb
   is the construction-time resolution that `reference_resolution.md` prescribes (resolve once into
   a stored direct reference). It is a local navigation of the object tree, which the
   forbidden-shape list permits; the list forbids flattened symbol-name lookup and design-global
   path tables, neither of which a parent-chain walk is.

## Consequences

- **Multiple depths work with one artifact and no specialization.** The same module climbs to the
  correct ancestor whether the ancestor is one or several levels up; depth is never represented as a
  number anywhere.
- **The match keys are the runtime instance name and module definition name (`Scope::Name()` /
  `Scope::DefName()`).** The `ExternalRef` type carries only the match key, the by-name tail down
  through the ancestor's owned children, and the leaf signal name (the `GetSignal` argument); it
  carries no ancestor or child type, so the referrer's artifact stays self-contained.
- **The climb is a construction-time loop, run once per extern member.** It is never on the
  simulation hot path, which reads the stored pointer.
- **Upward references are not unified with `ref` ports.** A `ref` port is supplied by the parent
  because the connection is explicit there; an upward reference is resolved by the child because the
  parent cannot know about it. Both are cross-unit references resolved at construction, but their
  head resolution differs by necessity.
- **Out of scope, guarded explicitly:** an upward reference written inside a generate block; a
  `$root`-anchored absolute path; an upward reference through an interface port; an ancestor that is
  a named generate or procedural block rather than a module instance (such a scope does not yet
  expose itself as a climb-match target with `GetSignal`); and the corner where a nearer ancestor
  carries the same instance name as the named one.

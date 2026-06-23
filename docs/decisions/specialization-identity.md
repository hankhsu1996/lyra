# Specialization Identity by Content-Addressed Binding Hash

## Date

2026-06-23

## Status

Accepted

## Why this decision matters

A parameterized module instantiated with different bindings must compile to distinct artifacts that
each behave according to their own parameters. Today a compiled unit's identity is the bare module
name, so two specializations of one module (e.g. `Reg #(.INIT(3))` and `Reg #(.INIT(7))`) carry the
same name; emitting more than one collapses them and the last one wins. This record fixes how a
specialization is identified, named, and reached across the unit boundary. It binds every
parameterized instantiation and constrains the deferred specialization-dedup optimization
(`docs/progress/performance.md`).

## Findings that shaped the design

### F1. The identity must be a function of the bindings, not of the body

A cross-unit reference is resolved by the referrer, which cannot see the target's body: a unit
compiles against another unit's interface (name and signature), never its body or internal layout
(`compilation_unit_model.md` inv 8, `reference_resolution.md`). So when a parent constructs a child,
it must be able to name which specialization it wants from what it has -- the module name plus the
child's parameter bindings -- without inspecting the child's compiled code.

Consequence: the identity is `f(def name, selected bindings)`, never `f(body)`. Fingerprinting the
lowered body is ruled out, because the consumer that must compute the same identity has no access to
it.

### F2. The identity is a name, computed independently by both sides

Cross-unit access is by name against the interface; a design-global ordinal, coordinate, or table
that two units share to refer to each other is forbidden (`compilation_unit_model.md`,
`emission_model.md`, `identity_and_ownership.md`). The producer (the unit naming itself) and the
consumer (the parent naming the child it constructs) therefore compute the same name from the same
bindings by the same deterministic function; they agree with no global view. This rules out an
opaque key carried in a design-global map (the shape the previous iteration used).

### F3. A binding is one of two recursive structures, encoded structurally

A parameter binding is either a value -- slang resolves it to a `ConstantValue` of any type,
including unpacked aggregates (LRM 6.20.2) -- or a type -- a data type, recursive, including a
parameterized class with its own bindings (LRM 6.20.3). The identity encodes the structural content
of these, and excludes arena ids, source names, and source spans, because identity follows structure
not position (`identity_and_ownership.md`) and a fingerprint captures semantic meaning not spelling
(`incremental_build.md` inv 3). Two structurally identical bindings encode identically; an arena id,
which is a unit-local insertion position, is never part of the encoding.

### F4. Generic-language precedent points to injective mangling for a reason that does not bind us

C++ (Itanium ABI) and Rust (v0) encode template / generic arguments into an injective mangled symbol
name. They do so because the name must be demanglable for debuggers and must be self-contained for a
linker that matches symbols across separately compiled units with no global view, and they accept
that the name grows with argument complexity. Lyra needs neither property: its readable surface is
the MIR dump, not a demangled symbol, and determinism alone makes the producer and consumer agree on
a name without a global view. So Lyra can content-address (hash) the binding encoding where C++ and
Rust cannot.

## The decision

1. **Identity is a deterministic name = module definition name + a content hash of a canonical
   serialization of the selected parameter bindings.** The producer and the consumer both compute it
   from the bindings; they match by name.

2. **The serializer canonically encodes the binding structure.** It walks `ConstantValue` on the
   value side and the data-type structure on the type side, recursively, and excludes arena ids,
   source names, and source spans. Ordering is normalized so the result does not depend on traversal
   or enumeration order (`specialization_model.md` inv 6).

3. **The serializer is subset-agnostic; which bindings feed it is policy.** Functional
   specialization (the current requirement, `docs/progress/hierarchy.md` Stage A) feeds all
   parameters. The deferred dedup optimization (`docs/progress/performance.md`) feeds only
   code-shape-affecting bindings and demotes value-only parameters to constructor inputs resolved at
   construction. The identity mechanism does not change across that shift; only the input subset
   does.

4. **The identity is computed at AST-to-HIR and carried by name thereafter.** HIR owns identity and
   frontend ids end there (`hir.md`); the cross-unit reference and the construct carry the name, and
   the backend renders it as the artifact's name.

5. **Readability is a separable, non-load-bearing decoration.** A human-readable prefix or comment
   may be added later for the emitted artifact; it never gates the identity, never has to be
   collision-free, and degrades gracefully on bindings it cannot render. The hash is the
   load-bearing identity.

## Consequences

- Distinct bindings produce distinct names, hence distinct artifacts; the current name-collision
  collapse is fixed, including value parameters of any type (unpacked aggregates included) and type
  parameters.
- The hash's only failure mode is collision; a wide content hash makes it not a practical concern,
  and no global view is needed to guarantee that the producer and consumer agree.
- The deferred dedup optimization reuses the same serializer with a narrower input subset, so it
  does not discard this work.
- Type-parameter identity reuses the recursive data-type structure already produced by type
  lowering; no parallel structure is introduced.

## Open hardening: an incremental-grade fingerprint

The hash must be self-owned, not the frontend's. slang exposes a value/type hash, but it folds raw
pointers for some type kinds (a virtual-interface type hashes the interface address) and ties the
result to a frontend-internal algorithm; identity is owned past AST-to-HIR (`hir.md`), so the
specialization hash is computed here, over content, with a fixed algorithm that is stable across
sessions (`incremental_build.md` forbids a pointer-derived or process-seeded key).

The first implementation feeds the hash a textual rendering of each binding (the value's and type's
`toString`). This is deterministic and session-stable, but it is not yet an incremental-grade
fingerprint: a type rendering can carry source spelling (a typedef name), so a rename would shift
the key even though the meaning is unchanged, which `incremental_build.md` invariant 3 rules out;
and a value rendering can be lossy. When aggressive incremental build lands, the encoding is
hardened to a structural fingerprint that mirrors slang's `==` / `isMatching` equivalence and
excludes source spelling, so the key changes only when the compiled meaning does. The hash being
self-owned and the identity being `f(def, bindings)` do not change; only what is fed to the hash
does. Deferred until incremental build exists, because the encoding's exact obligations are fixed by
that machinery.

## Alternatives considered

**Injective mangling (C++ / Rust style): the binding encoding is the name.** Rejected. The name
grows without bound with binding complexity -- an unpacked array of structs or a deeply
parameterized type produces an enormous name -- and that cost is certain and always present. Lyra
gains nothing from the self-containedness that motivates it in C++ / Rust, because determinism
already makes the two sides agree, and it does not need demangling. Trading a certain, ever-present
cost (name growth) for a vanishing one (hash collision under a wide hash) favors the hash.

**Fingerprint the lowered body.** Rejected. The consumer that must compute the child's identity
cannot see the child's body under independent compilation (F1).

**Opaque key carried in a design-global map (the previous iteration's `ModuleSpecId`).** Rejected.
It resolves cross-unit references through a design-global table rather than by name, which
`compilation_unit_model.md` and `emission_model.md` forbid. The previous iteration's classification
insight -- fingerprint only code-shape facts, value parameters flow in as runtime data -- is
retained as the deferred dedup policy, but its mechanism is not.

**A readable name with scalar values baked in (e.g. `Reg__INIT_3`).** Rejected. It is specialized to
scalar value parameters and cannot express an aggregate value or a type parameter, so it breaks the
moment a non-scalar binding appears and forces a throwaway rewrite. Readability is recovered as a
non-load-bearing decoration on top of the general identity instead.

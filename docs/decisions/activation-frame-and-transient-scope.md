# Activation frame vs transient scope: naming and the escape invariant

Date: 2026-07-15 Status: accepted

## Context

The execution backend represents a runtime value as an opaque handle into runtime-owned storage
([jit-value-realization](jit-value-realization.md)). Storage for those values grew in two lifetimes
-- one released at every suspension, one that spans an activation -- and the word "activation" was
attached to both the activation's control identity and its value storage, plus to a single local's
storage cell. Distinct concepts drifted onto one name, which a codebase audit surfaced. This entry
fixes the vocabulary against the concepts the architecture already defines, and states the invariant
that keeps the two storage lifetimes from mixing. It renames; it does not change behavior.

The concepts already exist and are already separate in the architecture:

- `activation.md` owns the **activation** -- the in-flight execution instance's control face (the
  token the scheduler holds, the completion slot, the ownership/continuation/cancellation relations,
  the lineage, the registration set). It is backend-neutral, and its "Does Not Own" is explicit that
  storage layout and frame allocation are below it.
- `object_lifetime.md` owns the **activation frame** -- the Lyra-owned storage holding the
  language-visible values that live with an activation across a suspension. Same lifetime as the
  activation, different responsibility, realized per backend.

## Decision

Four concepts, one name each. Storage is named after the architecture's **activation frame**; the
word **activation** is reserved for the control identity; and the per-stretch transient store keeps
its own name.

- **`RuntimeProcess` is the lineage / scheduler node.** It carries process identity and the
  parent-child process lineage (LRM 9.5), and it owns the frames of the activation it currently
  runs. `activation.md` forbids a lineage node whose lifetime is its execution's, so the node
  outlives the frame it once held -- the code already does this. `RuntimeProcess` is not a
  value-storage concept.

- **"activation" names the control identity only.** The scheduler token (a `PromiseBase*`, the field
  `Registration::activation`) is the activation reached through its token, and `Scope::Activate` is
  the elaboration phase that creates processes (`elaboration_lifecycle.md`). These uses are correct
  and are left as they are. The word does not name any value storage.

- **"activation frame" names the cross-suspension value storage.** The storage a value-typed local
  needs to survive a suspension is the activation frame (`object_lifetime.md`). Its runtime home is
  `ActivationFrameStorage`, owned by the adapter that drives a generated process for the
  activation's whole life; a value living in it is an `ActivationValueCell<T>`. The LIR operation
  that allocates, reads, and writes such a value is `ActivationFrameTarget`, realized through the
  `lyra_rt_activation_frame_*` ABI. `ActivationFrameStorage` is a distinct type from the per-stretch
  arena even though both allocate the same way, so the two lifetimes cannot be passed for each
  other.

- **`GeneratedCallScope` is the transient, per-stretch store only.** It owns the values one stretch
  of generated code materializes and releases them when the stretch returns. It is never activation
  storage; a scope over a stretch of a suspending body borrows the enclosing
  `ActivationFrameStorage` but keeps its own transient arena separate.

## The escape invariant

> A value owned by a `GeneratedCallScope` (a transient) may not escape the stretch that produced it.
> Every store that hands a value to longer-lived storage -- an activation frame value, an observable
> cell, a member, a closure capture, a deferred effect -- copies or promotes the value; a raw
> transient handle is never installed into longer-lived storage.

This holds today: a frame-value store copies (`ActivationValueCell::Store`), a cell set copies
(`Var::Set`), a closure capture snapshots. The one path that does not copy is a **method's return
value**: it is returned as-is, and it stays valid only because a generated method call runs in the
caller's own `GeneratedCallScope` -- no scope is pushed per call, so a returned transient's arena is
the caller's, still live. This is a load-bearing assumption, recorded here so it is not silently
broken: **a per-call `GeneratedCallScope` must not be introduced without promoting return values out
of the callee's scope first.** Only the construct entry, each lifecycle body, and each resumed
process push a scope; an ordinary call does not.

Enforcement is by construction and by name, not yet by a verifier pass: the longer-lived stores are
copy-by-contract, and the ABI/LIR operations that install into longer-lived storage are named so a
raw-handle install reads as wrong. A verifier check for transient escape is deferred until a
concrete need makes it worth its cost.

## Rejected

- **A single fused `RuntimeActivation` owning control, storage, and the GC root.** It re-merges the
  activation (control, backend-neutral) and the activation frame (storage, per-backend) that
  `activation.md` and `object_lifetime.md` deliberately keep apart; storage realization differs per
  backend (the C++ backend's activation is a coroutine frame with no separate storage object), so
  folding storage into the neutral concept couples it to one realization.

- **A generic typed slot schema, trace hooks, or a GC-root shape now.** The frame has one consumer
  today (value-typed procedural locals in packed/string), no second value domain and no managed
  value on the execution backend, and GC is doc-only. Building the general storage schema ahead of
  those consumers is speculative infrastructure
  ([lifetime-extended-automatic-scope](lifetime-extended-automatic-scope.md): no facility ahead of a
  second consumer); its shape is learned from the consumers that force it.

## Consequences

- The storage-side vocabulary converges on "activation frame"; the word "activation" no longer names
  a value store. `ActivationFrameStorage` and the per-stretch arena are distinct types.
- The escape invariant and its one non-copying path (method return) are written down; a future
  change to per-call scoping must honor it.
- The slot schema, tracing, and the GC root remain future work, taken up when a value domain beyond
  packed/string or a managed value reaches the execution backend.

## Cross-references

- `architecture/activation.md` -- the activation (control) concept and the lineage-vs-execution
  forbidden shape.
- `architecture/object_lifetime.md` -- the activation frame as traceable storage, and the deferred
  GC model.
- [jit-value-realization](jit-value-realization.md) -- the opaque-handle baseline and the
  per-stretch transient scope this names.
- [cross-suspension-value-storage](cross-suspension-value-storage.md) -- the activation-frame value
  cell mechanism, one instance of the storage concept named here.

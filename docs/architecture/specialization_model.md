# Specialization Model

## Purpose

Define what a specialization is, when a new specialization is created, and what stays shared across
instances that match a specialization. Specialization is the compile-time **sharing optimization**:
the correctness baseline is concrete elaboration -- every distinct parameter binding resolved to its
own concrete artifact -- and a shared specialization is admitted only when distinct concrete forms
are proven to behave identically under one artifact. Parameter classification is the **eligibility
filter** for that sharing, never a correctness requirement; where sharing cannot be proven, concrete
artifacts are retained, which is a correct outcome (an optimization miss, not a failure).

## Owns

- The definition of a specialization: a compilation unit together with the set of inputs that change
  compiled code shape.
- The classification of parameters into code-shape-affecting inputs and constructor/config inputs.
- The rule that one specialization produces one compile-time artifact, shared by every instance that
  matches.
- The rule for what may vary within one specialization vs what forces a new specialization.

## Does Not Own

- The internal shape of a compilation unit (see `compilation_unit_model.md`).
- Identity kinds inside a specialization (see `identity_and_ownership.md`).
- Cache keys for incremental reuse (see `incremental_build.md`). Specialization keys feed into that
  cache, but this doc does not define the cache's contract.

## Core Invariants

1. **Concrete elaboration is the correctness baseline; sharing is the optimization.** Every accepted
   program has a correct concrete lowering in which each distinct parameter binding is its own
   artifact. A specialization that shares one artifact across distinct bindings is admitted only
   when proven behavior-preserving; absent that proof, the concrete artifacts are retained.
   Correctness never depends on classification or on proving sharing.
2. **Specialization is driven by code-shape differences, not by instance count.** A new
   specialization is created only when compiled code shape actually differs between two potential
   instances. The number of instances that match a specialization does not affect how many
   specializations exist.
3. **Parameters are classified as a sharing-eligibility filter.** Every parameter is either a
   **code-shape-affecting input** (enters the specialization key) or a **constructor/config input**
   (flows in at runtime construction, does not enter the key). The classification is explicit and
   stable, and it widens what may share one artifact; it is never a precondition for lowering a
   program correctly.
4. **Code-shape-affecting inputs are exactly those that change generated code.** Packed bit widths
   used in types, type substitutions for `parameter type`, and structural decisions that change the
   set of emitted instructions are code-shape-affecting. Nothing else.
5. **Constructor/config inputs do not fork the specialization.** Initial values, counts that only
   steer runtime state, enable/disable flags that do not change generated code, and values consumed
   only by the runtime constructor flow through as inputs. They do not produce additional
   compile-time artifacts.
6. **One specialization compiles once.** Every instance that matches a specialization shares the
   same compile-time artifact. Two instances with identical specialization keys are
   indistinguishable at compile time.
7. **Specialization keys are stable and deterministic.** Given a compilation unit and its
   code-shape-affecting inputs, the specialization key is fully determined. Keys do not depend on
   traversal order, instance enumeration, or the order in which instances are encountered.

## Boundary to Adjacent Layers

- `compilation_unit_model.md` defines the compilation unit. This doc defines how parameter values
  refine a compilation unit into specializations.
- `incremental_build.md` uses specialization keys as cache keys. The rules in this doc are upstream
  of incremental reuse.
- Each IR layer's shape is per-specialization, not per-instance. See the individual IR docs for how
  a specialization manifests in HIR, MIR, and LIR.

## Forbidden Shapes

- A specialization keyed on a parameter that does not change compiled code shape.
- In the optimized steady state, leaving every distinct parameter binding as its own specialization
  without checking whether the values change code shape. The concrete baseline of invariant 1 does
  exactly this and is correct; the sharing optimization must not stop there.
- Using runtime-only state (counter initial values, enable flags, sizes that the runtime constructor
  handles) as part of the specialization key.
- Specialization keys derived from instance path, instance ordinal, or instance enumeration.
- Silently reclassifying a constructor input as a code-shape input to simplify codegen.
- An input that appears on both axes (code-shape and constructor) simultaneously. Each input has
  exactly one classification.
- Forking a specialization per instance, or allowing instance count to drive the number of
  specializations.
- Rejecting a program because a parameter cannot be classified. The correct fallback is concrete
  specialization, never rejection.

## Notes / Examples

`parameter int W = 8` used as `logic [W-1:0] data`: W changes the packed width of generated code. W
is code-shape-affecting. Instances with different W values belong to different specializations.

`parameter int INIT = 0` used only as the initial value of a register: INIT does not change
generated code. INIT is a constructor input. Instances with different INIT values share one
specialization and differ only in constructor inputs.

`parameter int N` controlling a `generate for` that instantiates N children: the compiled
constructor loops N times, producing N child objects. The compilation unit's compiled code does not
depend on N. N is a constructor input; the unit compiles once for any N.

`parameter type T = int` substituting a type parameter: T changes emitted types and operations. T is
code-shape-affecting. Distinct types produce distinct specializations.

# Specialization Model

## Purpose

Define what a specialization is, when a new specialization is created, and what stays shared across
instances that match a specialization.

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

1. **Specialization is driven by code-shape differences, not by instance count.** A new
   specialization is created only when compiled code shape actually differs between two potential
   instances. The number of instances that match a specialization does not affect how many
   specializations exist.
2. **Parameters are classified.** Every parameter is either a **code-shape-affecting input** (enters
   the specialization key) or a **constructor/config input** (flows in at runtime construction, does
   not enter the key). The classification is explicit and stable.
3. **Code-shape-affecting inputs are exactly those that change generated code.** Packed bit widths
   used in types, type substitutions for `parameter type`, and structural decisions that change the
   set of emitted instructions are code-shape-affecting. Nothing else.
4. **Constructor/config inputs do not fork the specialization.** Initial values, counts that only
   steer runtime state, enable/disable flags that do not change generated code, and values consumed
   only by the runtime constructor flow through as inputs. They do not produce additional
   compile-time artifacts.
5. **One specialization compiles once.** Every instance that matches a specialization shares the
   same compile-time artifact. Two instances with identical specialization keys are
   indistinguishable at compile time.
6. **Specialization keys are stable and deterministic.** Given a compilation unit and its
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
- Treating every distinct parameter binding as a new specialization without checking whether the
  values change code shape.
- Using runtime-only state (counter initial values, enable flags, sizes that the runtime constructor
  handles) as part of the specialization key.
- Specialization keys derived from instance path, instance ordinal, or instance enumeration.
- Silently reclassifying a constructor input as a code-shape input to simplify codegen.
- An input that appears on both axes (code-shape and constructor) simultaneously. Each input has
  exactly one classification.
- Forking a specialization per instance, or allowing instance count to drive the number of
  specializations.

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

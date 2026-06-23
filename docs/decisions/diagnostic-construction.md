# Diagnostic Metadata Derives From the Code; Construction Is Infallible

## Date

2026-06-23

## Status

Accepted

## Why this decision matters

A diagnostic's intrinsic metadata -- its severity kind and its classification -- was being
re-supplied at every emission site and then cross-checked against a central table by guards that
throw `InternalError` on mismatch. The guards run on the unsupported / error path, which is the
graceful-degradation path and the least-tested branch, so a call site that names the wrong
classification turns a clean user-facing diagnostic into a compiler crash. Eleven such drifts were
found and corrected pointwise, but the design kept re-arming the landmine because the call-site
value is a second source of truth that can diverge from the table.

This record surveys how mature compilers structure diagnostic metadata, derives the principle they
share, and applies it: metadata is a property of the code, derived at construction, never
re-supplied and never validated at the report site; and the one classification axis with no consumer
is removed rather than carried as a dead field.

## Findings that shaped the design

### F1. Every surveyed compiler derives metadata from the code identity, never from the call site

| Compiler | Registry (single source)                                                                                     | What the emission site supplies                                           |
| -------- | ------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------- |
| Clang    | `Diagnostic*Kinds.td` record (tblgen-generated id); severity baked into the id                               | `Diag(Loc, diag::err_xxx) << args` -- location and message args only      |
| Roslyn   | `DiagnosticDescriptor` (defined once, static); `Id`, `Title`, `MessageFormat`, `Category`, `DefaultSeverity` | `Diagnostic.Create(descriptor, location, args)` -- location and args only |
| rustc    | `#[derive(Diagnostic)]` struct / lint declaration; `Level` on the definition                                 | `struct_span_err(span, msg).emit()` -- the level is not re-supplied       |

In none of the three does the emission site restate the severity and have it cross-checked. The
severity is a property of the diagnostic's identity. Clang makes it readable at the call site
through the id _name_ (`err_` / `warn_` / `ext_` prefix), not through a per-severity factory the
author selects.

**Consequence:** the prior shape -- a per-kind factory (`Error` / `Unsupported` / `Warning` /
`HostError`) selected by the author, plus a `RequireKind` guard cross-checking the table -- is the
outlier. The kind belongs to the code, emitted through one entry, with the code name carrying the
kind for readability.

### F2. A classification axis exists in mature systems only where an end-user surface consumes it

Roslyn's `Category` is a free-form string (`"Design"`, `"Naming"`, `"Security"`) that earns its
place by being consumed: `.editorconfig` sets severity in bulk per category
(`dotnet_analyzer_diagnostic.category-<C>.severity`), and IDEs group analyzers by it. Clang's
category is surfaced by `-fdiagnostics-show-category`. rustc has no per-error category at all; it
classifies _lints_ through lint groups, again an end-user configuration surface
(`#[allow(<group>)]`).

The pattern is uniform: a classification axis is carried only when an end-user-facing consumer
(severity configuration, grouping, `--explain`) reads it. None of the three carries a classification
that nothing reads.

**Consequence:** a classification axis is justified by its consumer, not by its plausibility.

### F3. The `UnsupportedCategory` axis has no consumer and does not fit the consumers it mimics

`UnsupportedCategory` (`kType` / `kOperation` / `kFeature`) is read by exactly two things: the
`RequireCategory` guard (the landmine this decision removes) and a test that asserts the factory
stored the value the test passed it. The render layer never reads it; there is no severity
configuration surface, no grouping, no `--explain`. It is a write-only field.

The three classic category consumers were each checked against this compiler's reality and none
fits:

- **Per-category severity configuration** (Roslyn's `.editorconfig`) presupposes that downgrading a
  diagnostic lets compilation proceed. An `unsupported` diagnostic means the construct cannot be
  lowered; downgrading it to a warning produces no IR to continue with. The axis Roslyn's category
  configures does not exist here.
- **`--explain <code>`** is served by the code name and message, which already state the nature of
  the gap; a `[category: type]` tag beside `unsupported_associative_array_type` is redundant.
- **A maturity / triage summary** ("8 unsupported: 4 type, 3 feature, 1 operation") is the one
  surface that would fit, but it does not exist and is not on the near-term roadmap.

**Consequence:** the category concept is removed in full. When a real consumer appears (a triage
summary, or an external user base large enough to want `--explain` grouping), the axis is
re-introduced on the code registry, derived at construction, with values designed for that consumer
-- not the present internal three-way split.

### F4. The severity kind is the only call-site distinction that is not already on the code

Two call-site distinctions look like they encode kind but do not need to:

- **Short-circuit versus report-and-continue.** A recoverable failure returns
  `std::unexpected<Diagnostic>` and unwinds the lowering; a warning is reported to the sink and
  lowering continues. This is a control-flow distinction, orthogonal to severity, and it is the real
  reason a call site picks one helper over another.
- **Span versus no span.** Host-level failures and the no-location unsupported form carry
  `UnknownSpan`; everything else carries a `SourceSpan`. This is an overload of one entry, not a
  separate kind.

The severity kind itself (`error` / `unsupported` / `host error` / `warning`) is fully determined by
the code and is surfaced at the call site by the code name. The production code emits no warning
today; the only warning code is unreferenced outside tests, confirming that the per-kind factory
surface was modeling distinctions the call sites do not actually make.

**Consequence:** the construction surface collapses to two kind-neutral helpers split by control
flow, not four split by severity.

## Decision

The diagnostic subsystem follows these invariants:

1. **The code is the single registry of intrinsic metadata.** Each `DiagCode` carries its `DiagKind`
   and its `name` in one table. Construction reads the kind from the table
   (`.kind = DiagCodeKind(code)`); no kind is passed at the call site.

2. **Construction is infallible.** No guard validates a call-site value against the table, because
   no call-site value is supplied to validate. `RequireKind` and `RequireCategory` are removed.
   Diagnostic construction, being the graceful-degradation path, never throws.

3. **One kind-neutral construction surface, split by control flow.** `diag::Fail(...)` returns
   `std::unexpected<Diagnostic>` for the recoverable-failure path; `diag::Make(...)` returns a
   `Diagnostic` for the report-and-continue path. Each has a with-span and a no-span overload. The
   severity is not in the helper name; it is on the code. `.WithNote(...)` chaining is unchanged.

4. **The code name carries the kind.** A code's enum name encodes its kind for call-site
   readability, following Clang's `err_` / `warn_` convention -- the unsupported, host, and warning
   families are prefixed accordingly, and the error family is prefixed `kError`.

5. **No classification axis without a consumer.** `UnsupportedCategory`, the `category` field on the
   code table and on `PrimaryDiagItem`, and `DiagCodeCategory` are removed. A future classification
   axis is added only alongside the end-user surface that reads it, on the code registry, derived at
   construction.

## Consequences

- `include/lyra/diag/diagnostic.hpp` loses the `detail::RequireKind` / `detail::RequireCategory`
  guards and the four per-kind factories; it gains `Make` (pure factory, sets kind from the code)
  and `Fail` (the `unexpected`-returning sugar). The header no longer throws.
- `PrimaryDiagItem` loses its `category` field; `DiagCodeInfo` loses its `category` field; the
  `UnsupportedCategory` enum and `DiagCodeCategory` are deleted.
- Every emission site drops the verb-as-kind selection and, for the unsupported family, the trailing
  category argument: `diag::Unsupported(span, code, msg, cat)` and `diag::Error(span, code, msg)`
  both become `diag::Fail(span, code, msg)`. Argument order is unchanged.
- The render layer is untouched: it already consumes only `kind`, `span`, and `message`.
- The sink's unused per-kind convenience methods (`Error` / `Unsupported` / `Warning`) are removed;
  `Report(Diagnostic)` -- the only method production code calls -- stays.
- The category-round-trip test is removed; a test asserting that a constructed diagnostic's kind
  matches its code's table kind replaces the role it served.
- Adding a diagnostic is: one table row (kind + name) and call sites that name the code. There is no
  second place to keep in sync and no way for a call site to crash the compiler by disagreeing with
  the table.

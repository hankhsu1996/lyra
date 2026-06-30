# Array method runtime semantics

Date: 2026-06-04 Status: dispatch portion superseded by
[builtin-call-identity](builtin-call-identity.md); runtime semantics below remain in force

## Context

LRM 7.5 / 7.10 / 7.12 define the array method families that apply uniformly across the unpacked
array containers (fixed unpacked, dynamic, queue, associative). Two runtime-side semantics need to
be pinned regardless of how the dispatch is modelled in IR.

## Decisions

### Empty-array reduction returns element-shape zero

LRM 7.12.3 is silent on the empty-input case. The runtime returns an integral value of the element's
bit width and signedness filled with zero, regardless of which reduction (`sum`, `product`, `and`,
`or`, `xor`). The implementation re-uses the receiver's canonical-shape shield slot: copy the slot,
default-initialise the copy, return.

### Sort uses selection sort, not the standard-library introsort

The SV comparison over 4-state keys is not a strict weak ordering. A key carrying an `x` / `z`
compares indeterminate, so `a < b` and `b < a` can both be false while a definite `a < c` still
holds -- the equivalence the comparator induces is not transitive. `std::ranges::sort` (libstdc++
introsort) is undefined behaviour on such a comparator: it relies on a strict weak ordering for its
partition bounds and can read out of range. A pairwise in-place selection sort makes no such
assumption -- it yields a defined order with no UB. The asymptotic O(n^2) cost is acceptable because
the realistic SV workload runs sort on small arrays.

(An earlier version of this decision rested instead on the packed-array move-assign preserving the
destination's shape, which `std::ranges::sort`'s element eviction would have tripped. The
value-model reset made every value pure -- assignment adopts the whole value, see
[value-store-discipline](value-store-discipline.md) -- so that reason no longer applies; the
strict-weak-ordering reason above is what keeps selection sort in place.)

## Rejected alternatives

- Switching to `std::ranges::sort` now that the move-assign no longer preserves shape. The shape
  obstacle is gone, but the 4-state comparator is still not a strict weak ordering, so the standard
  sort would be undefined behaviour. A robust pairwise sort is required regardless.

## Notes

The dispatch shape this decision originally pinned -- a `BuiltinMethodRef` variant carrying a
per-family kind enum (`ArrayMethodKind`, `StringMethodKind`, `EnumMethodKind`, `EventMethodKind`,
`QueueMethodKind`, `AssociativeMethodKind`) -- was superseded by
[builtin-call-identity](builtin-call-identity.md). Both layers now share one flat closed-namespace
identifier for every built-in call; the receiver-type dispatch in AST-to-HIR routes by container
kind to the right name, but the resolved callee is a single id rather than a per-family arm.

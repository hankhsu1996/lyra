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

The packed-array move-assign operator preserves the destination's declared shape rather than
adopting the source's; this is the correct semantic for SV-language assignments. The standard
library's `std::ranges::sort` (libstdc++ introsort) internally evicts an element into a temporary
via move-construct, then move-assigns from another element into the now-empty slot; that move-
assign tripping the shape check produces a runtime error. Reverse works because it uses `iter_swap`,
which is ADL-hooked with a friend `swap` that exchanges storage directly. Sort cannot be ADL-hooked
the same way -- it touches elements via `=`, not `swap`. The runtime therefore implements `Sort` /
`Rsort` as in-place selection sort using ADL `swap`. The asymptotic O(n^2) cost is acceptable
because (a) the realistic SV workload runs sort on small arrays and (b) the alternative -- relaxing
the move-assign shape check -- would silently weaken the shape-preservation safety net used by
user-facing assignments.

## Rejected alternatives

- Relaxing the packed-array move-assign to adopt the source shape. Would let `std::ranges::sort`
  work directly, but would silently reshape destinations on SV assignments if any backend conversion
  path failed to insert the matching conversion. Keeping the shape-preservation guarantee in place
  is worth the O(n^2) sort.

## Notes

The dispatch shape this decision originally pinned -- a `BuiltinMethodRef` variant carrying a
per-family kind enum (`ArrayMethodKind`, `StringMethodKind`, `EnumMethodKind`, `EventMethodKind`,
`QueueMethodKind`, `AssociativeMethodKind`) -- was superseded by
[builtin-call-identity](builtin-call-identity.md). Both layers now share one flat closed-namespace
identifier for every built-in call; the receiver-type dispatch in AST-to-HIR routes by container
kind to the right name, but the resolved callee is a single id rather than a per-family arm.

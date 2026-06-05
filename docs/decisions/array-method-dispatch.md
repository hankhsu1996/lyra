# Array method dispatch

Date: 2026-06-04 Status: accepted

## Context

LRM 7.5.2 / 7.5.3 attach two methods (`size`, `delete`) to the dynamic-array type, and LRM 7.12
defines a much larger family (`sort`, `rsort`, `reverse`, `shuffle`, `sum`, `product`, `and`, `or`,
`xor`, `find` and its variants, `min`, `max`, `unique`, `unique_index`, `map`) that applies
uniformly to any unpacked array container (fixed unpacked, dynamic, queue, associative). Slang
implements all of these as `SystemSubroutine` instances registered against each container's
`SymbolKind`. At the SV call site they share the surface syntax of method dispatch
(`receiver.method(...)`); at the slang AST they surface as `CallExpression` with
`isSystemCall() == true`.

Lyra already has a first-class home for receiver-type-bound built-in methods:
`hir::BuiltinMethodRef` (`include/lyra/hir/method.hpp`), which carries the enum / string / event
method kinds and is found via receiver-type detection in `LowerCallExprProc`
(`src/lyra/lowering/ast_to_hir/expression/lower.cpp`). The system-subroutine path
(`SystemSubroutineRef`) is reserved for global `$xxx` functions whose dispatch is name-based.

This decision pins where the array-method family lives and how the surface area splits across cuts.

## Decision

**Type-bound built-in array methods route through `hir::BuiltinMethodRef`.**

A new `ArrayMethodKind` enum joins the existing variant arms (`EnumMethodKind`, `StringMethodKind`,
`EventMethodKind`). AST -> HIR detects the receiver type in `LowerCallExprProc` and routes the call
to `BuiltinMethodRef{ArrayMethodKind::kXxx}`. HIR -> MIR translates the kind through the existing
`Overloaded` visitor on the `BuiltinMethodRef` callee. The backend renders `(receiver).MethodName()`
and wraps the result in `lyra::value::PackedArray::Int(...)` for the one method whose C++ return
type is `std::size_t` (`size`).

The enum is named `ArrayMethodKind` (not `DynArrayMethodKind`) because LRM 7.12 defines the
semantics uniformly across container kinds; only the AST -> HIR receiver-detection block differs per
container. Fixed-unpacked, queue, and associative array workstreams extend the routing block, not
the kind enum.

**Empty-array reduction returns element-shape zero.**

LRM 7.12.3 is silent on the empty-input case. Slang's `ArrayReductionMethod::eval` returns an
`SVInt` of the element's bit width and signedness filled with zero, regardless of which reduction
(`sum`, `product`, `and`, `or`, `xor`). The runtime implementation re-uses the receiver's
`oob_slot_` (which already carries the canonical element shape from the DA1 shield protocol): copy
the slot, call `ResetToDefault()` on the copy, return.

**The method family ships in three cuts.**

The full LRM 7.12 surface mixes three independent architectural foundations: pure method-dispatch
infrastructure, the `with` clause / iterator binding, and the `Queue<T>` runtime (needed by methods
whose return type is `queue<elem>` or `queue<int>`). Folding all three into one PR is unmanageable;
folding none means each follow-up reopens the dispatch plumbing. The sequencing:

- **DA6-a** (this cut): pure method dispatch + the no-`with`, no-queue-return subset (`size`,
  `delete`, `reverse`, `sort`, `rsort`, `sum`, `product`, `and`, `or`, `xor`).
- **DA6-b**: `with` clause + iterator binding + the non-queue-return `with` variants (`sort with`,
  `sum with`, ...).
- **DA6-c + Q1**: `Queue<T>` runtime + locator family (`find*`, `min`, `max`, `unique*`).

`shuffle()` (RNG) and SV2023 `map()` ship as standalone follow-ups after DA6-c lands.

**Sort uses selection sort, not `std::ranges::sort`.**

`PackedArray::operator=(PackedArray&&)` enforces destination-shape preservation via `AssignFrom`,
which is the correct semantic for SV-language assignments. `std::ranges::sort` (libstdc++ introsort)
internally evicts an element into a temporary via move-construct, then move-assigns from another
element into the now-empty slot; that move-assign tripping the shape check produces an
`InternalError`. Reverse works (it uses `iter_swap`, which we ADL-hook with a friend `swap` on
`PackedArray` that exchanges storage directly). Sort cannot be ADL-hooked the same way -- it touches
elements via `=`, not `swap`. The runtime therefore implements `Sort` / `Rsort` as in-place
selection sort using ADL `swap`. The asymptotic O(n^2) cost is acceptable because (a) DA arrays in
the realistic SV workload are small, (b) the alternative (relaxing PackedArray's move-assign) would
silently weaken the shape-preservation safety net used by user-facing assignments. A future
PackedArray refactor can introduce an explicit "moved-from" state and a smarter sort if needed.

## Rejected alternatives

- **Routing array methods through `SystemSubroutineRef`.** That path is keyed on a name in the
  global `$xxx` namespace; `arr.size()` is keyed on the receiver type. The existing enum / string /
  event arms already establish the pattern. Forcing all three into one shared system-subroutine
  registry would conflate two distinct dispatch axes (global function name vs receiver-type method).
- **Per-container kind enums** (`DynArrayMethodKind`, `UnpackedArrayMethodKind`, ...). The methods'
  semantics are LRM-uniform across containers; only the receiver-detection block differs. A single
  `ArrayMethodKind` keeps the kind enum stable as other workstreams open.
- **Shipping all 18+ methods in one PR.** Even the no-queue-return no-`with` subset is ten methods
  and ships in one cut here; pulling in queue runtime or `with`-clause infrastructure on top would
  push the review surface past the point where bisection helps.
- **Relaxing `PackedArray::operator=(PackedArray&&)` to adopt source shape.** Would let
  `std::ranges::sort` work directly, but would silently reshape destinations on SV assignments if
  any backend conversion path failed to insert the matching `Conversion`. Keeping the
  shape-preservation guarantee in place is worth the O(n^2) sort.

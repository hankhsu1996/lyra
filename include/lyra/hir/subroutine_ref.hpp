#pragma once

#include <variant>

#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/subroutine_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::hir {

// Calls a structural subroutine declared in the unit (or one of its
// enclosing scopes, reached through `hops`).
struct StructuralSubroutineRef {
  StructuralHops hops;
  StructuralSubroutineId subroutine;
};

// Calls a `$xxx` system subroutine. The id resolves through
// `support::LookupSystemSubroutine` to the descriptor that drives lowering.
struct SystemSubroutineRef {
  support::SystemSubroutineId id;
};

// Calls a built-in runtime method (LRM 6.16 string, 6.19.5 enum, 7.9
// associative, 7.10 queue, 7.12 unpacked-array shared family, 15.5 named
// event). The id is the flat closed namespace `support::BuiltinFn`,
// shared with MIR (see `docs/decisions/builtin-call-identity.md`).
struct BuiltinMethodRef {
  support::BuiltinFn method;
};

// LRM 7.12.4 `item.index` (also written `item.<user-named-index>`). SV
// dresses it as a method on the iterator binding, but no runtime function
// exists -- the receiver value is discarded and the call resolves to the
// closure's index parameter. HIR-to-MIR rewrites it to a `LocalRef`; no
// MIR `CallExpr` survives. Kept as its own callee arm so the translation
// behaviour ("rewrites away") is visible in the type rather than hidden
// inside `BuiltinMethodRef`.
struct IteratorIndexRef {};

using SubroutineRef = std::variant<
    StructuralSubroutineRef, SystemSubroutineRef, BuiltinMethodRef,
    IteratorIndexRef>;

}  // namespace lyra::hir

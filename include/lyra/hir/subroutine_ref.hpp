#pragma once

#include <cstdint>
#include <variant>

#include "lyra/hir/class_id.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/foreign_import_id.hpp"
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

// Calls a DPI-C import (LRM 35.4) declared in the unit (or an enclosing scope,
// reached through `hops`). A bodyless external callable, resolved separately
// from a body-bearing structural subroutine.
struct ForeignImportRef {
  StructuralHops hops;
  ForeignImportId id;
};

// Calls an instance method (LRM 8.6) on `receiver`, an expression yielding the
// class handle the call dispatches through. `class_id` names the declaring
// class and `method_index` is the method's declaration-order position in it.
struct MethodCallRef {
  ExprId receiver;
  ClassId class_id;
  std::uint32_t method_index;
};

// Calls a `$xxx` system subroutine. The id resolves through
// `support::LookupSystemSubroutine` to the descriptor that drives lowering.
struct SystemSubroutineRef {
  support::SystemSubroutineId id;
};

// Calls a built-in runtime method (LRM 6.16 string, 6.19.5 enum, 7.9
// associative, 7.10 queue, 7.12 unpacked-array shared family, 15.5 named
// event). The id is the flat closed namespace `support::BuiltinFn`, shared
// with MIR.
struct BuiltinMethodRef {
  support::BuiltinFn method;
};

using SubroutineRef = std::variant<
    StructuralSubroutineRef, MethodCallRef, SystemSubroutineRef,
    BuiltinMethodRef, ForeignImportRef>;

}  // namespace lyra::hir

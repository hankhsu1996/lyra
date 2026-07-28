#pragma once

// Lowering of the LRM 6.19.5 enum methods. An enum value is its base integral,
// so the methods are type-owned operations resolved above the backend:
// `first` / `last` / `num` fold to constants, `name` / `next` / `prev` to
// synthesized per-enum callables.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Lowers `first` / `last` / `num` to their compile-time constant value: a
// member-value constant at the enum's base shape for `first` / `last`, the
// member count as an `int` for `num`. The meaning is independent of the
// enclosing scope, so one template over the pass class serves both contexts;
// explicit instantiations live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerEnumConstantMethod(
    Lowerer& lowerer, const hir::CallExpr& c, const hir::BuiltinMethodRef& b,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// Lowers `name` / `next` / `prev` to a call of a per-enum callable synthesized
// once per unit (its body is generic MIR primitives -- a case-equality chain
// for `name`, index arithmetic for the shared `next` / `prev` step). The
// callable homes on the enclosing scope class; a package context has no such
// class and is not yet supported.
template <ExprLowerer Lowerer>
auto LowerEnumMethodCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::BuiltinMethodRef& b, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir

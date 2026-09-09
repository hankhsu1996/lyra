#pragma once

// Lowering of the LRM 6.19.5 enumerated type methods. An enum value is its base
// integral and the enumeration's declared members are a compile-time fact, so
// every one of them is answered here: `first` / `last` / `num` fold to
// constants, `name` / `next` / `prev` to synthesized per-enumeration callables.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Answers one call against the member table its enumeration declares. A
// constant answer is a member-value literal at the enumeration's base shape or
// the member count as an `int`; a searched answer is a call to a callable
// synthesized once per unit, whose body is generic MIR primitives -- a
// case-equality chain for `name`, index arithmetic for the shared `next` /
// `prev` step. That callable homes on the enclosing scope class; a package
// context has no such class and is not yet supported.
//
// The meaning is independent of the enclosing scope, so one template over the
// pass class serves both contexts; explicit instantiations live in the
// implementation file.
template <ExprLowerer Lowerer>
auto LowerEnumMethod(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    hir::EnumMethodRef ref, mir::TypeId result_type) -> diag::Result<mir::Expr>;

// The declared name of an enumeration value, as the string `name` answers with
// (LRM 6.19.5): the member's name where the type declares one for the value,
// and the empty string where it does not. Shared with the assignment-pattern
// rendering of an enumeration (LRM 21.2.1.6), which prints that name when there
// is one, so both reach one synthesized callable per enum rather than each
// building its own.
template <ExprLowerer Lowerer>
auto BuildEnumNameCallExpr(
    Lowerer& lowerer, WalkFrame frame, mir::ExprId value_id,
    mir::TypeId enum_type, diag::SourceSpan span) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir

#pragma once

// Lowering of the LRM 6.19.5 enumerated type methods, and of the one other
// question the member list answers. An enum value is its base integral and the
// enumeration's declared members are a compile-time fact: `first` / `last` /
// `num` fold to constants, while `name`, `next` / `prev`, and whether a value
// is a member at all are questions put to the member list the unit states once
// for the enumeration.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Answers one call against the members its enumeration declares. A constant
// answer is a member's value or the member count as an `int`; every other one
// is a question put to the enumeration's member list about the value.
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
// is one.
auto BuildEnumNameCallExpr(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId value_id,
    hir::TypeId enum_type) -> mir::Expr;

// Whether a value is a member of the enumeration (LRM 6.24.2), which is what a
// dynamic cast into a variable of that type answers with. The value asked about
// is the one such an assignment would store.
auto BuildEnumMembershipCallExpr(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId value_id,
    hir::TypeId enum_type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir

#include "lyra/lowering/hir_to_mir/lower_expr.hpp"

#include <variant>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/net_decl.hpp"
#include "lyra/hir/param_decl.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/hir/var_decl.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/support/overloaded.hpp"
#include "lyra/support/unsupported.hpp"

namespace lyra::lowering::hir_to_mir {

auto TranslateValueDeclRefToMember(
    const UnitLoweringState& unit_state, const hir::ValueDeclRef& ref)
    -> mir::MemberId {
  return std::visit(
      support::Overloaded{
          [&](const hir::VarDeclId& id) -> mir::MemberId {
            return unit_state.var_map[id.value];
          },
          [](const hir::NetDeclId&) -> mir::MemberId {
            support::Unsupported(
                "HIR->MIR lowering for NetDeclId is not implemented");
          },
          [](const hir::ParamDeclId&) -> mir::MemberId {
            support::Unsupported(
                "HIR->MIR lowering for ParamDeclId is not implemented");
          },
      },
      ref);
}

auto LowerPrimary(
    const UnitLoweringState& unit_state, const hir::Primary& primary)
    -> mir::ExprData {
  return std::visit(
      support::Overloaded{
          [](const hir::IntegerLiteral& p) -> mir::ExprData {
            return mir::IntegerLiteral{p.value};
          },
          [&](const hir::LocalValueRef& p) -> mir::ExprData {
            return mir::MemberRef{
                TranslateValueDeclRefToMember(unit_state, p.target)};
          },
      },
      primary);
}

auto LowerExprData(
    const UnitLoweringState& unit_state, const hir::ExprData& data)
    -> mir::ExprData {
  return std::visit(
      support::Overloaded{
          [&](const hir::Primary& p) -> mir::ExprData {
            return LowerPrimary(unit_state, p);
          },
      },
      data);
}

}  // namespace lyra::lowering::hir_to_mir

#include "lyra/lowering/hir_to_mir/lower_expr.hpp"

#include <variant>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/lvalue.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/support/overloaded.hpp"
#include "lyra/support/unsupported.hpp"

namespace lyra::lowering::hir_to_mir {

auto ResolveVarRef(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const hir::VarDeclRef& ref) -> mir::MemberId {
  const auto& home = stack.Resolve(ref.parent_scope_hops);
  if (&home != &unit_facts.RootScope()) {
    support::Unsupported(
        "HIR->MIR: variable reference to declaration outside the root scope "
        "is not supported in this cut");
  }
  return unit_state.TranslateRootVar(ref.local_id);
}

auto TranslateLvalueTarget(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const hir::Lvalue& lvalue) -> mir::MemberId {
  return std::visit(
      support::Overloaded{
          [&](const hir::LocalValueRef& l) -> mir::MemberId {
            return std::visit(
                support::Overloaded{
                    [&](const hir::VarDeclRef& r) -> mir::MemberId {
                      return ResolveVarRef(unit_facts, unit_state, stack, r);
                    },
                },
                l.target);
          },
      },
      lvalue);
}

auto LowerPrimary(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const hir::Primary& primary) -> mir::ExprData {
  return std::visit(
      support::Overloaded{
          [](const hir::IntegerLiteral& p) -> mir::ExprData {
            return mir::IntegerLiteral{.value = p.value};
          },
          [&](const hir::LocalValueRef& p) -> mir::ExprData {
            return std::visit(
                support::Overloaded{
                    [&](const hir::VarDeclRef& r) -> mir::ExprData {
                      return mir::MemberRef{
                          .target =
                              ResolveVarRef(unit_facts, unit_state, stack, r)};
                    },
                },
                p.target);
          },
      },
      primary);
}

auto LowerExprData(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const hir::ExprData& data) -> mir::ExprData {
  return std::visit(
      support::Overloaded{
          [&](const hir::Primary& p) -> mir::ExprData {
            return LowerPrimary(unit_facts, unit_state, stack, p);
          },
      },
      data);
}

}  // namespace lyra::lowering::hir_to_mir

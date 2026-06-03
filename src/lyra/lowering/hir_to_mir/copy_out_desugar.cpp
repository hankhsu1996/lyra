#include "lyra/lowering/hir_to_mir/copy_out_desugar.hpp"

#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/procedural_scope_helpers.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildOutputArgSlot(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, ProceduralScopeLoweringState& wrapper,
    const hir::ProceduralBody& hir_proc, hir::ExprId actual_hir,
    std::string_view temp_name) -> diag::Result<OutputArgSlot> {
  auto actual_or = LowerExpr(
      unit_state, scope_state, proc_state, wrapper, hir_proc,
      hir_proc.exprs.at(actual_hir.value));
  if (!actual_or) return std::unexpected(std::move(actual_or.error()));
  const mir::TypeId actual_type = actual_or->type;
  const mir::ExprId actual_id = wrapper.AddExpr(*std::move(actual_or));
  const mir::ProceduralVarRef temp = wrapper.AppendLocal(
      mir::ProceduralVarDecl{
          .name = std::string{temp_name}, .type = actual_type},
      actual_id);
  return OutputArgSlot{.actual = actual_id, .temp = temp, .type = actual_type};
}

auto BuildCopyOutBlock(
    ProceduralScopeLoweringState& wrapper, const hir::Stmt& stmt,
    mir::TypeId result_type, mir::Expr call_expr,
    std::optional<mir::ExprId> assign_target_id,
    const std::vector<OutputArgSlot>& slots) -> mir::Stmt {
  const mir::TypeId call_type = call_expr.type;
  const mir::ExprId call_id = wrapper.AddExpr(std::move(call_expr));

  if (assign_target_id.has_value()) {
    mir::ExprId value_id = call_id;
    if (call_type != result_type) {
      value_id = wrapper.AddExpr(
          mir::Expr{
              .data =
                  mir::ConversionExpr{
                      .operand = call_id,
                      .kind = mir::ConversionKind::kImplicit},
              .type = result_type});
    }
    const mir::ExprId assign_id = wrapper.AddExpr(
        mir::Expr{
            .data =
                mir::AssignExpr{.target = *assign_target_id, .value = value_id},
            .type = result_type});
    wrapper.AppendStmt(mir::ExprStmt{.expr = assign_id});
  } else {
    wrapper.AppendStmt(mir::ExprStmt{.expr = call_id});
  }

  for (const OutputArgSlot& slot : slots) {
    const mir::ExprId temp_read =
        wrapper.AddExpr(mir::Expr{.data = slot.temp, .type = slot.type});
    const mir::ExprId copy_out = wrapper.AddExpr(
        mir::Expr{
            .data = mir::AssignExpr{.target = slot.actual, .value = temp_read},
            .type = slot.type});
    wrapper.AppendStmt(mir::ExprStmt{.expr = copy_out});
  }

  std::vector<mir::ProceduralScope> child_scopes;
  const mir::ProceduralScopeId scope_id =
      AddChildProceduralScope(child_scopes, wrapper.Finish());
  return mir::Stmt{
      .label = stmt.label,
      .data = mir::BlockStmt{.scope = scope_id},
      .child_procedural_scopes = std::move(child_scopes)};
}

}  // namespace lyra::lowering::hir_to_mir

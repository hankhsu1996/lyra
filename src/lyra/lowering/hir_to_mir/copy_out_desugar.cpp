#include "lyra/lowering/hir_to_mir/copy_out_desugar.hpp"

#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildOutputArgSlot(
    ProcessLowerer& proc, WalkFrame frame, hir::ExprId actual_hir,
    std::string_view temp_name) -> diag::Result<OutputArgSlot> {
  const auto& hir_body = proc.HirBody();
  auto& wrapper = *frame.current_procedural_scope;
  // The actual is written to after the call returns: lower it as a cell-
  // typed lvalue so an observable destination's writeback routes through
  // `Var<T>::Set`. For the copy-in initializer the temp wants the current
  // value, so a separate `LowerExpr` lowering wraps the cell in `Get`.
  auto target_or =
      proc.LowerLhsExpr(hir_body.exprs.at(actual_hir.value), frame);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  const mir::TypeId actual_type =
      proc.Module().TranslateType(hir_body.exprs.at(actual_hir.value).type);
  const mir::ExprId actual_id = wrapper.AddExpr(*std::move(target_or));
  auto value_or = proc.LowerExpr(hir_body.exprs.at(actual_hir.value), frame);
  if (!value_or) return std::unexpected(std::move(value_or.error()));
  const mir::ExprId init_id = wrapper.AddExpr(*std::move(value_or));
  const mir::ProceduralVarRef temp = wrapper.AppendLocal(
      mir::ProceduralVarDecl{
          .name = std::string{temp_name}, .type = actual_type},
      init_id);
  return OutputArgSlot{.actual = actual_id, .temp = temp, .type = actual_type};
}

auto BuildCopyOutBlock(
    const mir::CompilationUnit& unit, mir::ExprId services_id,
    WalkFrame parent_frame, mir::ProceduralScope wrapper,
    std::optional<std::string> label, mir::TypeId result_type,
    mir::Expr call_expr, bool call_suspends,
    std::optional<mir::ExprId> assign_target_id,
    const std::vector<OutputArgSlot>& slots) -> mir::Stmt {
  const mir::TypeId call_type = call_expr.type;
  const mir::ExprId call_id = wrapper.AddExpr(std::move(call_expr));
  const mir::TypeId void_type = unit.builtins.void_type;

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
    const mir::Expr assign_expr = BuildObservableAssignExpr(
        unit, wrapper, services_id, *assign_target_id, value_id, std::nullopt,
        result_type, void_type);
    const mir::ExprId assign_id = wrapper.AddExpr(assign_expr);
    wrapper.AppendStmt(mir::ExprStmt{.expr = assign_id});
  } else if (call_suspends) {
    wrapper.AppendStmt(mir::AwaitStmt{.awaitable = call_id});
  } else {
    wrapper.AppendStmt(mir::ExprStmt{.expr = call_id});
  }

  for (const OutputArgSlot& slot : slots) {
    const mir::ExprId temp_read =
        wrapper.AddExpr(mir::Expr{.data = slot.temp, .type = slot.type});
    const mir::Expr copy_out_expr = BuildObservableAssignExpr(
        unit, wrapper, services_id, slot.actual, temp_read, std::nullopt,
        slot.type, void_type);
    const mir::ExprId copy_out = wrapper.AddExpr(copy_out_expr);
    wrapper.AppendStmt(mir::ExprStmt{.expr = copy_out});
  }

  const mir::ProceduralScopeId scope_id =
      parent_frame.current_procedural_scope->AddChildScope(std::move(wrapper));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

}  // namespace lyra::lowering::hir_to_mir

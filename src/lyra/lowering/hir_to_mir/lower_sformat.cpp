#include "lyra/lowering/hir_to_mir/lower_sformat.hpp"

#include <cstddef>
#include <expected>
#include <format>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_sformat.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto BuildSFormatCallExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::SFormatSystemSubroutineInfo& info, std::size_t arg_offset,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  const FormatStringRequirement fmt_req =
      info.expects_format_string ? FormatStringRequirement::kRequired
                                 : FormatStringRequirement::kOptional;
  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc, call,
      info.radix, arg_offset, fmt_req, span);
  if (!items_or) return std::unexpected(std::move(items_or.error()));

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeSFormatCall(std::move(*items_or))},
      .type = unit_state.Builtins().string};
}

auto RejectNonStringOutput(
    const support::SystemSubroutineDesc& desc, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  return diag::Unsupported(
      span, diag::DiagCode::kUnsupportedSubroutineArgument,
      std::format(
          "{} output_var must be string-typed in this build (LRM 21.3.3 "
          "allows integral and unpacked-byte-array outputs via LRM 5.9 "
          "assignment rules; deferred)",
          std::string{desc.name}),
      diag::UnsupportedCategory::kFeature);
}

}  // namespace

auto LowerSFormatSystemSubroutineCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::SFormatSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  if (info.has_output_arg) {
    throw InternalError(
        "LowerSFormatSystemSubroutineCall: $sformat / $swrite reached "
        "expression-position lowering; slang's task binding should reject "
        "them outside statement position");
  }
  return BuildSFormatCallExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc, call,
      info, 0, span);
}

auto LowerSFormatSystemSubroutineCallStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::Stmt& stmt,
    diag::SourceSpan span, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc,
    const support::SFormatSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt> {
  if (!info.has_output_arg) {
    auto call_expr_or = BuildSFormatCallExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc, call,
        info, 0, span);
    if (!call_expr_or) return std::unexpected(std::move(call_expr_or.error()));
    const mir::ExprId expr_id =
        proc_scope_state.AddExpr(*std::move(call_expr_or));
    return mir::Stmt{
        .label = stmt.label,
        .data = mir::ExprStmt{.expr = expr_id},
        .child_procedural_scopes = {}};
  }

  if (call.arguments.empty()) {
    throw InternalError(
        "LowerSFormatSystemSubroutineCallStmt: $sformat / $swrite requires "
        "an output_var argument; slang's arg-count check should have "
        "rejected the call");
  }

  if (!call.arguments[0].has_value()) {
    throw InternalError(
        "LowerSFormatSystemSubroutineCallStmt: output_var arg unexpectedly "
        "elided");
  }
  auto out_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0]->value));
  if (!out_or) return std::unexpected(std::move(out_or.error()));
  const mir::TypeId out_type = out_or->type;
  if (unit_state.GetType(out_type).Kind() != mir::TypeKind::kString) {
    return RejectNonStringOutput(desc, span);
  }
  const mir::ExprId out_id = proc_scope_state.AddExpr(*std::move(out_or));

  auto call_expr_or = BuildSFormatCallExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc, call,
      info, 1, span);
  if (!call_expr_or) return std::unexpected(std::move(call_expr_or.error()));
  const mir::ExprId call_id =
      proc_scope_state.AddExpr(*std::move(call_expr_or));

  const mir::ExprId assign_id = proc_scope_state.AddExpr(
      mir::Expr{
          .data = mir::AssignExpr{.target = out_id, .value = call_id},
          .type = out_type});

  return mir::Stmt{
      .label = stmt.label,
      .data = mir::ExprStmt{.expr = assign_id},
      .child_procedural_scopes = {}};
}

}  // namespace lyra::lowering::hir_to_mir

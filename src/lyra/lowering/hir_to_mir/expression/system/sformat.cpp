#include "lyra/lowering/hir_to_mir/expression/system/sformat.hpp"

#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_sformat.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto BuildSFormatCallExpr(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::SFormatSystemSubroutineInfo& info, std::size_t arg_offset,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  const FormatStringRequirement fmt_req =
      info.expects_format_string ? FormatStringRequirement::kRequired
                                 : FormatStringRequirement::kOptional;
  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      process, frame, call, info.radix, arg_offset, fmt_req, span);
  if (!items_or) return std::unexpected(std::move(items_or.error()));

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeSFormatCall(std::move(*items_or))},
      .type = process.Module().Unit().builtins.string};
}

auto RejectNonStringOutput(std::string_view name, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  return diag::Unsupported(
      span, diag::DiagCode::kUnsupportedSubroutineArgument,
      std::format(
          "{} output_var must be string-typed in this build (LRM 21.3.3 "
          "allows integral and unpacked-byte-array outputs via LRM 5.9 "
          "assignment rules; deferred)",
          std::string{name}),
      diag::UnsupportedCategory::kFeature);
}

}  // namespace

auto LowerSFormatSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::SFormatSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  if (info.has_output_arg) {
    throw InternalError(
        "LowerSFormatSystemSubroutineCall: $sformat / $swrite reached "
        "expression-position lowering; slang's task binding should reject "
        "them outside statement position");
  }
  return BuildSFormatCallExpr(process, frame, call, info, 0, span);
}

auto LowerSFormatSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    diag::SourceSpan span, const hir::CallExpr& call, std::string_view name,
    const support::SFormatSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;

  if (!info.has_output_arg) {
    auto call_expr_or =
        BuildSFormatCallExpr(process, frame, call, info, 0, span);
    if (!call_expr_or) return std::unexpected(std::move(call_expr_or.error()));
    const mir::ExprId expr_id = proc_scope.AddExpr(*std::move(call_expr_or));
    return mir::Stmt{
        .label = std::move(label), .data = mir::ExprStmt{.expr = expr_id}};
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
  auto out_or =
      process.LowerLhsExpr(hir_proc.exprs.at(call.arguments[0]->value), frame);
  if (!out_or) return std::unexpected(std::move(out_or.error()));
  const mir::TypeId out_type = process.Module().TranslateType(
      hir_proc.exprs.at(call.arguments[0]->value).type);
  if (process.Module().Unit().GetType(out_type).Kind() !=
      mir::TypeKind::kString) {
    return RejectNonStringOutput(name, span);
  }
  const mir::ExprId out_id = proc_scope.AddExpr(*std::move(out_or));

  auto call_expr_or = BuildSFormatCallExpr(process, frame, call, info, 1, span);
  if (!call_expr_or) return std::unexpected(std::move(call_expr_or.error()));
  const mir::ExprId call_id = proc_scope.AddExpr(*std::move(call_expr_or));

  const mir::ExprId services_id =
      proc_scope.AddExpr(BuildServicesCallExpr(process, frame));
  const mir::Expr assign_expr = BuildObservableAssignExpr(
      process.Module().Unit(), proc_scope, services_id, out_id, call_id,
      std::nullopt, out_type, process.Module().Unit().builtins.void_type);
  const mir::ExprId assign_id = proc_scope.AddExpr(assign_expr);

  return mir::Stmt{
      .label = std::move(label), .data = mir::ExprStmt{.expr = assign_id}};
}

}  // namespace lyra::lowering::hir_to_mir

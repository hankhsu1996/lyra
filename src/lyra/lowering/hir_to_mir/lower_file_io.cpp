#include "lyra/lowering/hir_to_mir/lower_file_io.hpp"

#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_file_io.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerFileOpenCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  if (call.arguments.empty() || call.arguments.size() > 2) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        std::format(
            "{} expects 1 or 2 arguments (filename [, mode])",
            std::string{desc.name}),
        diag::UnsupportedCategory::kFeature);
  }

  auto name_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0].value));
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  const mir::ExprId name_id = proc_scope_state.AddExpr(*std::move(name_or));

  std::optional<mir::ExprId> mode_id = std::nullopt;
  if (call.arguments.size() == 2) {
    auto mode_or = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        hir_proc.exprs.at(call.arguments[1].value));
    if (!mode_or) return std::unexpected(std::move(mode_or.error()));
    mode_id = proc_scope_state.AddExpr(*std::move(mode_or));
  }

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call =
                  mir::RuntimeFileOpenCall{.name = name_id, .mode = mode_id}},
      .type = unit_state.Builtins().int32};
}

auto LowerFileCloseCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  if (call.arguments.size() != 1) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        std::format(
            "{} expects exactly 1 argument (descriptor)",
            std::string{desc.name}),
        diag::UnsupportedCategory::kFeature);
  }

  auto desc_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0].value));
  if (!desc_or) return std::unexpected(std::move(desc_or.error()));
  const mir::ExprId descriptor_id =
      proc_scope_state.AddExpr(*std::move(desc_or));

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeFileCloseCall{.descriptor = descriptor_id}},
      .type = unit_state.Builtins().void_type};
}

}  // namespace

auto LowerFileIOSystemSubroutineCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc,
    const support::FileIOSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  switch (info.kind) {
    case support::FileIOKind::kOpen:
      return LowerFileOpenCall(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc, call,
          desc, span);
    case support::FileIOKind::kClose:
      return LowerFileCloseCall(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc, call,
          desc, span);
  }
  throw InternalError("LowerFileIOSystemSubroutineCall: unknown FileIOKind");
}

}  // namespace lyra::lowering::hir_to_mir

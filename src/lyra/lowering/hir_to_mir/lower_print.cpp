#include "lyra/lowering/hir_to_mir/lower_print.hpp"

#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ToMirPrintKind(const support::PrintSystemSubroutineInfo& info)
    -> mir::PrintKind {
  if (info.sink_kind == support::PrintSinkKind::kStdout) {
    return info.append_newline ? mir::PrintKind::kDisplay
                               : mir::PrintKind::kWrite;
  }
  return info.append_newline ? mir::PrintKind::kFDisplay
                             : mir::PrintKind::kFWrite;
}

}  // namespace

auto LowerPrintSystemSubroutineCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc,
    const support::PrintSystemSubroutineInfo& print, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  if (print.sink_kind == support::PrintSinkKind::kFile) {
    return diag::Unsupported(
        span, diag::DiagCode::kFileDisplayNotImplemented,
        std::format(
            "{} is not implemented in this build", std::string{desc.name}),
        diag::UnsupportedCategory::kFeature);
  }
  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc, call,
      span);
  if (!items_or) return std::unexpected(std::move(items_or.error()));

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimePrintCall(
                  ToMirPrintKind(print), std::nullopt, std::move(*items_or))},
      .type = unit_state.Builtins().void_type};
}

}  // namespace lyra::lowering::hir_to_mir

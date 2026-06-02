#include "lyra/lowering/hir_to_mir/lower_diagnostic.hpp"

#include <expected>
#include <optional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_diagnostic.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ToMirDiagnosticSeverity(support::DiagnosticSeverityKind k)
    -> mir::DiagnosticSeverity {
  switch (k) {
    case support::DiagnosticSeverityKind::kInfo:
      return mir::DiagnosticSeverity::kInfo;
    case support::DiagnosticSeverityKind::kWarning:
      return mir::DiagnosticSeverity::kWarning;
    case support::DiagnosticSeverityKind::kError:
      return mir::DiagnosticSeverity::kError;
  }
  throw InternalError("ToMirDiagnosticSeverity: unknown severity kind");
}

}  // namespace

auto LowerDiagnosticSystemSubroutineCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc,
    const support::DiagnosticSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  (void)desc;
  // $info/$warning/$error use display-style format (LRM 20.10) with decimal
  // as the bare-arg default radix; sink/newline fields are not consulted by
  // the item builder for diagnostic calls (they drive separate Diagnostic
  // dispatch downstream).
  constexpr support::PrintSystemSubroutineInfo kDiagnosticPrintInfo{
      .radix = support::PrintRadix::kDecimal,
      .append_newline = true,
      .is_strobe = false,
      .sink_kind = support::PrintSinkKind::kStdout};
  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc, call,
      kDiagnosticPrintInfo, 0, span);
  if (!items_or) return std::unexpected(std::move(items_or.error()));

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeDiagnosticCall(
                  ToMirDiagnosticSeverity(info.severity), std::nullopt,
                  std::move(*items_or))},
      .type = unit_state.Builtins().void_type};
}

}  // namespace lyra::lowering::hir_to_mir

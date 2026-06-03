#include "lyra/lowering/hir_to_mir/lower_system_subroutine.hpp"

#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/lower_diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/lower_file_io.hpp"
#include "lyra/lowering/hir_to_mir/lower_finish.hpp"
#include "lyra/lowering/hir_to_mir/lower_print.hpp"
#include "lyra/lowering/hir_to_mir/lower_scan.hpp"
#include "lyra/lowering/hir_to_mir/lower_sformat.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerSystemSubroutineCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const hir::SystemSubroutineRef& ref, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  const auto& desc = support::LookupSystemSubroutine(ref.id);
  return std::visit(
      Overloaded{
          [&](const support::PrintSystemSubroutineInfo& print)
              -> diag::Result<mir::Expr> {
            return LowerPrintSystemSubroutineCall(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                call, desc, print, span);
          },
          [&](const support::TerminationSystemSubroutineInfo& term)
              -> diag::Result<mir::Expr> {
            return LowerFinishSystemSubroutineCall(
                unit_state, hir_proc, call, desc, term, span);
          },
          [&](const support::DiagnosticSystemSubroutineInfo& diag_info)
              -> diag::Result<mir::Expr> {
            return LowerDiagnosticSystemSubroutineCall(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                call, desc, diag_info, span);
          },
          [&](const support::FileIOSystemSubroutineInfo& file_io)
              -> diag::Result<mir::Expr> {
            return LowerFileIOSystemSubroutineCall(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                call, desc, file_io, span);
          },
          [&](const support::ScanSystemSubroutineInfo& scan)
              -> diag::Result<mir::Expr> {
            return LowerScanSystemSubroutineCall(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                call, desc, scan, span);
          },
          [&](const support::SFormatSystemSubroutineInfo& sformat)
              -> diag::Result<mir::Expr> {
            return LowerSFormatSystemSubroutineCall(
                unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
                call, sformat, span);
          },
      },
      desc.semantic);
}

}  // namespace lyra::lowering::hir_to_mir

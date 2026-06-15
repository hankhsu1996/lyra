#include "lyra/lowering/hir_to_mir/lower_system_subroutine.hpp"

#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/lower_diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/lower_file_io.hpp"
#include "lyra/lowering/hir_to_mir/lower_finish.hpp"
#include "lyra/lowering/hir_to_mir/lower_print.hpp"
#include "lyra/lowering/hir_to_mir/lower_scan.hpp"
#include "lyra/lowering/hir_to_mir/lower_sformat.hpp"
#include "lyra/lowering/hir_to_mir/lower_timescale.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const hir::SystemSubroutineRef& ref, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  const auto& desc = support::LookupSystemSubroutine(ref.id);
  return std::visit(
      Overloaded{
          [&](const support::PrintSystemSubroutineInfo& print)
              -> diag::Result<mir::Expr> {
            return LowerPrintSystemSubroutineCall(
                process, frame, call, print, span);
          },
          [&](const support::TerminationSystemSubroutineInfo& term)
              -> diag::Result<mir::Expr> {
            return LowerFinishSystemSubroutineCall(
                process, call, desc.name, term, span);
          },
          [&](const support::DiagnosticSystemSubroutineInfo& diag_info)
              -> diag::Result<mir::Expr> {
            return LowerDiagnosticSystemSubroutineCall(
                process, frame, call, diag_info, span);
          },
          [&](const support::FileIOSystemSubroutineInfo& file_io)
              -> diag::Result<mir::Expr> {
            return LowerFileIOSystemSubroutineCall(
                process, frame, call, desc.name, file_io, span);
          },
          [&](const support::ScanSystemSubroutineInfo& scan_info)
              -> diag::Result<mir::Expr> {
            return LowerScanSystemSubroutineCall(
                process, frame, call, scan_info, span);
          },
          [&](const support::SFormatSystemSubroutineInfo& sformat)
              -> diag::Result<mir::Expr> {
            return LowerSFormatSystemSubroutineCall(
                process, frame, call, sformat, span);
          },
          [&](const support::TimeSystemSubroutineInfo& time_info)
              -> diag::Result<mir::Expr> {
            // No arguments to lower (LRM 20.3 takes none); the scope-unit
            // scaling happens at render time against the emitted
            // `kTimeUnitPower`, so the node carries only the kind. The result
            // type is fixed by the function (LRM 20.3.1/.2/.3).
            mir::TypeId result_type = process.Module().Unit().builtins.time;
            switch (time_info.kind) {
              case support::TimeKind::kTime:
                result_type = process.Module().Unit().builtins.time;
                break;
              case support::TimeKind::kStime:
                result_type = process.Module().Unit().builtins.int32;
                break;
              case support::TimeKind::kRealtime:
                result_type = process.Module().Unit().builtins.realtime;
                break;
            }
            return mir::Expr{
                .data =
                    mir::RuntimeCallExpr{
                        .call = mir::RuntimeTimeCall{.kind = time_info.kind}},
                .type = result_type};
          },
          [&](const support::TimeFormatSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            return LowerTimeFormatSystemSubroutineCall(
                process, frame, call, span);
          },
          [&](const support::PrintTimescaleSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            return LowerPrintTimescaleSystemSubroutineCall(process);
          },
      },
      desc.semantic);
}

}  // namespace lyra::lowering::hir_to_mir

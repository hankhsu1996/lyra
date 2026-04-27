#include "lyra/lowering/hir_to_mir/lower_system_subroutine.hpp"

#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/lower_print.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerSystemSubroutineStmt(
    const UnitLoweringState& unit_state, const ProcessLoweringState& proc_state,
    const hir::Process& hir_proc, const BodyLoweringState& body_state,
    const hir::CallExpr& call, const hir::SystemSubroutineRef& ref,
    diag::SourceSpan span) -> diag::Result<mir::StmtData> {
  const auto& desc = support::LookupSystemSubroutine(ref.id);
  return std::visit(
      Overloaded{
          [&](const support::PrintSystemSubroutineInfo& print)
              -> diag::Result<mir::StmtData> {
            return LowerPrintSystemSubroutineStmt(
                unit_state, proc_state, hir_proc, body_state, call, desc, print,
                span);
          },
          [&](const auto&) -> diag::Result<mir::StmtData> {
            return diag::Unsupported(
                span, diag::DiagCode::kSystemSubroutineExecutionNotImplemented,
                "this system subroutine is not executable in this build",
                diag::UnsupportedCategory::kFeature);
          },
      },
      desc.semantic);
}

}  // namespace lyra::lowering::hir_to_mir

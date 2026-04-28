#include "lyra/lowering/hir_to_mir/lower_system_subroutine.hpp"

#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/lower_print.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerSystemSubroutineCall(
    const UnitLoweringState& unit_state, const hir::Process& hir_proc,
    const BodyLoweringState& body_state, const hir::CallExpr& call,
    const hir::SystemSubroutineRef& ref, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  const auto& desc = support::LookupSystemSubroutine(ref.id);
  return std::visit(
      Overloaded{
          [&](const support::PrintSystemSubroutineInfo& print)
              -> diag::Result<mir::Expr> {
            return LowerPrintSystemSubroutineCall(
                unit_state, hir_proc, body_state, call, desc, print, span);
          },
      },
      desc.semantic);
}

}  // namespace lyra::lowering::hir_to_mir

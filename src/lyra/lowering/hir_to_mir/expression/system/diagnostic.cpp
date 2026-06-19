#include "lyra/lowering/hir_to_mir/expression/system/diagnostic.hpp"

#include <cstdint>
#include <expected>
#include <utility>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerDiagnosticSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::SystemSubroutineId id, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  // $info/$warning/$error use display-style format (LRM 20.10) with decimal as
  // the bare-arg default radix; the diagnostic sink runs separately. Severity
  // is carried by `id` -- the backend selects the matching runtime entry.
  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      process, frame, call, support::PrintRadix::kDecimal, 0,
      FormatStringRequirement::kOptional, span);
  if (!items_or) return std::unexpected(std::move(items_or.error()));

  auto& unit = process.Module().Unit();
  auto& proc_scope = *frame.current_procedural_scope;
  const auto time_unit_power =
      static_cast<std::int64_t>(process.Resolution().unit_power);
  const mir::ExprId items_array = proc_scope.AddExpr(
      BuildPrintItemsArray(unit, proc_scope, *items_or, time_unit_power));

  std::vector<mir::ExprId> args;
  args.push_back(proc_scope.AddExpr(BuildServicesCallExpr(process, frame)));
  args.push_back(items_array);

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::SystemSubroutineCallee{.id = id},
              .arguments = std::move(args)},
      .type = unit.builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir

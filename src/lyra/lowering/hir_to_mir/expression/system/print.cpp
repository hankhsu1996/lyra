#include "lyra/lowering/hir_to_mir/expression/system/print.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ToValuePrintKind(const support::PrintSystemSubroutineInfo& info)
    -> value::PrintKind {
  if (info.sink_kind == support::PrintSinkKind::kStdout) {
    return info.append_newline ? value::PrintKind::kDisplay
                               : value::PrintKind::kWrite;
  }
  return info.append_newline ? value::PrintKind::kFDisplay
                             : value::PrintKind::kFWrite;
}

}  // namespace

auto LowerPrintSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::SystemSubroutineId id,
    const support::PrintSystemSubroutineInfo& print, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& block = *frame.current_block;
  const mir::TypeId void_type = process.Module().Unit().builtins.void_type;

  std::optional<mir::ExprId> descriptor = std::nullopt;
  std::size_t arg_offset = 0;
  if (print.sink_kind == support::PrintSinkKind::kFile) {
    // LRM 21.3.2: the first argument of $fdisplay / $fwrite is an MCD/FD
    // descriptor; remaining arguments are the print payload. The runtime
    // decodes the bit pattern (MCD vs FD per LRM 21.3.1) at dispatch time.
    if (!call.arguments[0].has_value()) {
      throw InternalError("$f-print descriptor argument unexpectedly elided");
    }
    auto lowered_or =
        process.LowerExpr(hir_proc.exprs.at(call.arguments[0]->value), frame);
    if (!lowered_or) return std::unexpected(std::move(lowered_or.error()));
    descriptor = block.AddExpr(*std::move(lowered_or));
    arg_offset = 1;
  }

  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      process, frame, call, print.radix, arg_offset,
      FormatStringRequirement::kOptional, span);
  if (!items_or) return std::unexpected(std::move(items_or.error()));

  // LRM 21.2.2: $strobe defers the same print to the postponed region. The
  // items keep their outer-scope ExprIds; the backend wraps the rendered print
  // in a postponed-region lambda whose init-capture list snapshots any
  // procedural-local operands.
  if (print.is_strobe) {
    mir::RuntimePrintCall print_call(
        ToValuePrintKind(print), descriptor, std::move(*items_or));
    return mir::Expr{
        .data =
            mir::RuntimeCallExpr{
                .call =
                    mir::RuntimeSubmitPostponedCall{
                        .print = std::move(print_call)}},
        .type = void_type};
  }

  auto& unit = process.Module().Unit();
  const auto time_unit_power =
      static_cast<std::int64_t>(process.Resolution().unit_power);
  const mir::ExprId items_array = block.AddExpr(
      BuildPrintItemsArray(unit, block, *items_or, time_unit_power));

  std::vector<mir::ExprId> args;
  args.push_back(block.AddExpr(BuildServicesCallExpr(process, frame)));
  if (descriptor.has_value()) {
    args.push_back(*descriptor);
  }
  args.push_back(items_array);

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::SystemSubroutineCallee{.id = id},
              .arguments = std::move(args)},
      .type = void_type};
}

}  // namespace lyra::lowering::hir_to_mir

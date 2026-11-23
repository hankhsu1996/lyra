#include "lyra/lowering/hir_to_mir/expression/system/timescale.hpp"

#include <cstddef>
#include <expected>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/file_descriptor.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerTimeFormatSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  mir::Builder body = frame.Builder();
  const auto& args = call.arguments;
  if (!args.empty() && args.size() != 4) {
    // LRM 20.4.3.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedSubroutineArgument,
        "'$timeformat' takes either no arguments or exactly four");
  }

  const auto& unit = process.Owner().Unit();
  const mir::ExprId runtime_id =
      body.Add(BuildCurrentRuntimeCallExpr(process.Owner().Unit()));
  // Every argument but the suffix is a number the runtime keeps as its own
  // setting rather than a value of the design.
  constexpr std::size_t kSuffixPosition = 2;
  std::vector<mir::ExprId> call_args;
  for (std::size_t i = 0; i < args.size(); ++i) {
    if (!args[i].has_value()) {
      throw InternalError(
          "$timeformat positional argument unexpectedly elided");
    }
    auto lowered = process.LowerExpr(hir_proc.exprs.Get(*args[i]), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    const mir::ExprId value = body.Add(*std::move(lowered));
    call_args.push_back(
        i == kSuffixPosition ? value : BuildToInt64Call(unit, body, value));
  }

  const support::BuiltinFn builtin = args.empty()
                                         ? support::BuiltinFn::kResetTimeFormat
                                         : support::BuiltinFn::kSetTimeFormat;
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = builtin, .receiver = runtime_id},
              .arguments = std::move(call_args)},
      .type = unit.builtins.void_type};
}

auto LowerPrintTimescaleSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& unit = process.Owner().Unit();
  mir::Builder body = frame.Builder();
  if (call.arguments.size() != 1 || !call.arguments.front().has_value()) {
    throw InternalError(
        "$printtimescale reaches MIR holding the line it writes, which the "
        "elaboration settles");
  }
  auto line = process.LowerExpr(
      process.HirBody().exprs.Get(*call.arguments.front()), frame);
  if (!line) return std::unexpected(std::move(line.error()));
  const mir::ExprId text_id = body.Add(*std::move(line));
  const mir::ExprId fd_id =
      mir::BuildMachineIntLiteral(unit, body, support::kStdoutFd);
  const mir::ExprId files_id =
      body.Add(BuildFilesCallExpr(process.Owner(), body));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kWriteln,
                      .receiver = files_id},
              .arguments = {fd_id, text_id}},
      .type = unit.builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir

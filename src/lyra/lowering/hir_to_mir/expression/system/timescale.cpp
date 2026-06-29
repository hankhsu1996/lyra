#include "lyra/lowering/hir_to_mir/expression/system/timescale.hpp"

#include <expected>
#include <format>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/file_descriptor.hpp"
#include "lyra/value/format.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerTimeFormatSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& body = *frame.current_block;
  const auto& args = call.arguments;
  if (!args.empty() && args.size() != 4) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedSubroutineArgument,
        "$timeformat takes either no arguments or exactly four (LRM 20.4.3)");
  }

  std::vector<mir::ExprId> call_args;
  call_args.push_back(
      body.exprs.Add(BuildServicesCallExpr(process.Module(), frame)));
  for (const auto& arg : args) {
    if (!arg.has_value()) {
      throw InternalError(
          "$timeformat positional argument unexpectedly elided");
    }
    auto lowered = process.LowerExpr(hir_proc.exprs.Get(*arg), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    call_args.push_back(body.exprs.Add(*std::move(lowered)));
  }

  const support::BuiltinFn builtin = args.empty()
                                         ? support::BuiltinFn::kResetTimeFormat
                                         : support::BuiltinFn::kSetTimeFormat;
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = builtin},
              .arguments = std::move(call_args)},
      .type = process.Module().Unit().builtins.void_type};
}

auto LowerPrintTimescaleSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame)
    -> diag::Result<mir::Expr> {
  const auto& builtins = process.Module().Unit().builtins;
  auto& body = *frame.current_block;
  const auto resolution = process.Resolution();

  // LRM 20.4.2 fixed format -- the scope name and the two powers are all
  // compile-time facts of the enclosing scope, so the message string is
  // assembled here once and the runtime only sees the same sink write that
  // $display lands on.
  const std::string message = std::format(
      "Time scale of ({}) is {} / {}", process.EnclosingScopeLowerer().Name(),
      value::TimeUnitText(resolution.unit_power),
      value::TimeUnitText(resolution.precision_power));
  const mir::ExprId text_lit = body.exprs.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = message},
          .type = builtins.string});
  const mir::ExprId text_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{}, .arguments = {text_lit}},
          .type = builtins.string});

  const mir::ExprId fd_id =
      body.exprs.Add(mir::MakeInt32Literal(builtins.int32, support::kStdoutFd));
  const mir::ExprId files_id =
      body.exprs.Add(BuildFilesCallExpr(process.Module(), frame));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kWriteln},
              .arguments = {files_id, fd_id, text_id}},
      .type = builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir

#include "lyra/lowering/hir_to_mir/expression/system/timescale.hpp"

#include <cstddef>
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
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
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

  const auto& unit = process.Owner().Unit();
  const mir::ExprId runtime_id =
      body.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
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
    const mir::ExprId value = body.exprs.Add(*std::move(lowered));
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
    const ProcessLowerer& process, const WalkFrame& frame)
    -> diag::Result<mir::Expr> {
  const auto& builtins = process.Owner().Unit().builtins;
  auto& body = *frame.current_block;
  const auto resolution = process.Resolution();

  // LRM 20.4.2 fixed format -- the scope name and the two powers are all
  // compile-time facts of the enclosing scope, so the message string is
  // assembled here once and the runtime only sees the same sink write that
  // $display lands on.
  // TODO(hankhsu): LRM 20.4.2 names the scope by its hierarchical path, which
  // exists only once the tree is built; the unit's own name is what this layer
  // has.
  const std::string message = std::format(
      "Time scale of ({}) is {} / {}", process.Owner().Unit().name,
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
      BuildMachineIntLiteral(process.Owner().Unit(), body, support::kStdoutFd);
  const mir::ExprId files_id =
      body.exprs.Add(BuildFilesCallExpr(process.Owner(), body));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kWriteln,
                      .receiver = files_id},
              .arguments = {fd_id, text_id}},
      .type = builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir

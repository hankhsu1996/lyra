#include "lyra/lowering/hir_to_mir/expression/system/control.hpp"

#include <expected>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The finish number a call that gives none takes (LRM 20.2).
constexpr int kDefaultFinishNumber = 1;

}  // namespace

auto BuildFinishNumber(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call)
    -> diag::Result<mir::ExprId> {
  const auto& unit = process.Owner().Unit();
  mir::Block& block = *frame.current_block;
  if (call.arguments.empty()) {
    return BuildMachineIntLiteral(unit, block, kDefaultFinishNumber);
  }
  if (!call.arguments.front().has_value()) {
    throw InternalError("an elided finish number reached lowering");
  }
  auto lowered = process.LowerExpr(
      process.HirBody().exprs.Get(*call.arguments.front()), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  const mir::ExprId value = block.exprs.Add(*std::move(lowered));
  const mir::Type& type = unit.types.Get(block.exprs.Get(value).type);
  // A real is rounded (LRM 6.12.1) and a string read as the bits of its
  // characters (LRM 5.9), the conversions a cast to `int` makes.
  if (type.IsIntegral() || type.IsRealFamily() || type.Is<mir::StringType>()) {
    return BuildToInt64Call(
        unit, block, ConvertToType(unit, block, value, unit.builtins.int_type));
  }
  block.AppendStmt(mir::ExprStmt{.expr = value});
  return BuildMachineIntLiteral(unit, block, kDefaultFinishNumber);
}

auto BuildEndRunCall(
    const ProcessLowerer& process, mir::Block& block, support::BuiltinFn entry,
    mir::ExprId finish_number, diag::SourceSpan span) -> mir::Expr {
  const auto& unit = process.Owner().Unit();
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId origin_id = BuildStringValueExpr(
      unit, block,
      diag::RuntimeOriginOf(span, process.Owner().SourceManager()));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = entry},
              .arguments = {runtime_id, origin_id, finish_number}},
      .type = unit.builtins.void_type};
}

auto LowerTerminationSystemSubroutineCall(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call,
    const support::TerminationSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  auto finish_number = BuildFinishNumber(process, frame, call);
  if (!finish_number) return std::unexpected(std::move(finish_number.error()));
  return BuildEndRunCall(
      process, *frame.current_block, info.builtin_fn, *finish_number, span);
}

}  // namespace lyra::lowering::hir_to_mir

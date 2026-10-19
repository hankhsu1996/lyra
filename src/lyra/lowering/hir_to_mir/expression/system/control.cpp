#include "lyra/lowering/hir_to_mir/expression/system/control.hpp"

#include <expected>
#include <initializer_list>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The finish number a call that gives none takes (LRM 20.2).
constexpr int kDefaultFinishNumber = 1;

// A call to `entry` on the runtime handle, carrying where the call was written
// and then `more`: every control task reports where it was called.
auto ControlCallAt(
    const ProcessLowerer& process, mir::Block& block, support::BuiltinFn entry,
    diag::SourceSpan span, std::initializer_list<mir::ExprId> more)
    -> mir::Expr {
  const auto& unit = process.Owner().Unit();
  std::vector<mir::ExprId> arguments = {
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner())),
      BuildStringValueExpr(
          unit, block,
          diag::RuntimeOriginOf(span, process.Owner().SourceManager()))};
  arguments.insert(arguments.end(), more);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = entry},
              .arguments = std::move(arguments)},
      .type = unit.builtins.void_type};
}

}  // namespace

auto BuildFinishNumber(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call)
    -> diag::Result<mir::ExprId> {
  const auto& unit = process.Owner().Unit();
  mir::Block& block = *frame.current_block;
  if (call.arguments.empty()) {
    return mir::BuildMachineIntLiteral(unit, block, kDefaultFinishNumber);
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
  return mir::BuildMachineIntLiteral(unit, block, kDefaultFinishNumber);
}

auto BuildEndRunCall(
    const ProcessLowerer& process, mir::Block& block, support::BuiltinFn entry,
    mir::ExprId finish_number, diag::SourceSpan span) -> mir::Expr {
  return ControlCallAt(process, block, entry, span, {finish_number});
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

auto LowerProgramControlSystemSubroutineCall(
    const ProcessLowerer& process, mir::Block& block, diag::SourceSpan span)
    -> mir::Expr {
  return ControlCallAt(process, block, support::BuiltinFn::kExit, span, {});
}

}  // namespace lyra::lowering::hir_to_mir

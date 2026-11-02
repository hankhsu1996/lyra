#include "lyra/lowering/hir_to_mir/expression/system/control.hpp"

#include <expected>
#include <initializer_list>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The finish number a call that gives none takes (LRM 20.2).
constexpr int kDefaultFinishNumber = 1;

// A call to `entry` on the runtime handle, carrying `more`.
auto ControlCall(
    const ProcessLowerer& process, mir::Builder block, support::BuiltinFn entry,
    std::initializer_list<mir::ExprId> more) -> mir::Expr {
  const auto& unit = process.Owner().Unit();
  std::vector<mir::ExprId> arguments = {
      block.Add(BuildCurrentRuntimeCallExpr(process.Owner().Unit()))};
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
  mir::Builder block = frame.Builder();
  if (call.arguments.empty()) {
    return mir::BuildMachineIntLiteral(unit, block, kDefaultFinishNumber);
  }
  if (!call.arguments.front().has_value()) {
    throw InternalError("an elided finish number reached lowering");
  }
  auto lowered = process.LowerExpr(
      process.HirBody().exprs.Get(*call.arguments.front()), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  const mir::ExprId value = block.Add(*std::move(lowered));
  const mir::Type& type = unit.types.Get(block.Get(value).type);
  // A real is rounded (LRM 6.12.1) and a string read as the bits of its
  // characters (LRM 5.9), the conversions a cast to `int` makes.
  if (type.IsIntegral() || type.Is<mir::RealType>() ||
      type.IsDeclaredBy(support::LibraryDecl::kString)) {
    return BuildToInt64Call(
        unit, block, ConvertToType(unit, block, value, unit.builtins.int_type));
  }
  block.AppendStmt(mir::ExprStmt{.expr = value});
  return mir::BuildMachineIntLiteral(unit, block, kDefaultFinishNumber);
}

auto BuildEndRunCall(
    const ProcessLowerer& process, mir::Builder block, support::BuiltinFn entry,
    mir::ExprId finish_number) -> mir::Expr {
  return ControlCall(process, block, entry, {finish_number});
}

auto LowerTerminationSystemSubroutineCall(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call,
    const support::TerminationSystemSubroutineInfo& info)
    -> diag::Result<mir::Expr> {
  auto finish_number = BuildFinishNumber(process, frame, call);
  if (!finish_number) return std::unexpected(std::move(finish_number.error()));
  return BuildEndRunCall(
      process, frame.Builder(), info.builtin_fn, *finish_number);
}

auto LowerProgramControlSystemSubroutineCall(
    const ProcessLowerer& process, mir::Builder block) -> mir::Expr {
  return ControlCall(process, block, support::BuiltinFn::kExit, {});
}

}  // namespace lyra::lowering::hir_to_mir

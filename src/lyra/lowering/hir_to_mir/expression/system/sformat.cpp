#include "lyra/lowering/hir_to_mir/expression/system/sformat.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

template <ExprLowerer Lowerer>
auto BuildSFormatCallExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::SFormatSystemSubroutineInfo& info, std::size_t arg_offset)
    -> diag::Result<mir::Expr> {
  // LRM 21.3.3: `$sformat` / `$sformatf` always take a format string, but it
  // need not be constant. A literal is parsed now, binding every operand to its
  // conversion at compile time; anything else carries its text only at
  // simulation time, so the parse and the binding happen there.
  if (info.expects_format_string &&
      !HasLiteralFormatString(lowerer, call, arg_offset)) {
    return BuildRuntimeFormatCallExpr(lowerer, frame, call, arg_offset);
  }

  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      lowerer, frame, call, info.radix, arg_offset);
  if (!items_or) return std::unexpected(std::move(items_or.error()));

  auto& unit = lowerer.Owner().Unit();
  auto& block = *frame.current_block;
  const auto time_unit_power =
      static_cast<std::int64_t>(lowerer.Resolution().unit_power);
  const mir::ExprId items_array = block.exprs.Add(
      BuildPrintItemsArray(unit, block, *items_or, time_unit_power));

  const mir::ExprId services_id =
      block.exprs.Add(BuildServicesCallExpr(lowerer.Owner(), frame));

  return BuildFormatCallExpr(unit, block, services_id, items_array);
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerSFormatSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::SFormatSystemSubroutineInfo& info)
    -> diag::Result<mir::Expr> {
  if (info.has_output_arg) {
    throw InternalError(
        "LowerSFormatSystemSubroutineCall: $sformat / $swrite reached "
        "expression-position lowering; slang's task binding should reject "
        "them outside statement position");
  }
  return BuildSFormatCallExpr(lowerer, frame, call, info, 0);
}

auto LowerSFormatSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const support::SFormatSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt> {
  const auto& hir_proc = process.HirBody();
  auto& block = *frame.current_block;

  if (!info.has_output_arg) {
    auto call_expr_or = BuildSFormatCallExpr(process, frame, call, info, 0);
    if (!call_expr_or) return std::unexpected(std::move(call_expr_or.error()));
    const mir::ExprId expr_id = block.exprs.Add(*std::move(call_expr_or));
    return mir::Stmt{
        .label = std::move(label), .data = mir::ExprStmt{.expr = expr_id}};
  }

  if (call.arguments.empty()) {
    throw InternalError(
        "LowerSFormatSystemSubroutineCallStmt: $sformat / $swrite requires "
        "an output_var argument; slang's arg-count check should have "
        "rejected the call");
  }

  if (!call.arguments[0].has_value()) {
    throw InternalError(
        "LowerSFormatSystemSubroutineCallStmt: output_var arg unexpectedly "
        "elided");
  }
  auto out_or =
      process.LowerLhsExpr(hir_proc.exprs.Get(*call.arguments[0]), frame);
  if (!out_or) return std::unexpected(std::move(out_or.error()));
  const mir::TypeId out_type = process.Owner().TranslateType(
      hir_proc.exprs.Get(*call.arguments[0]).type);
  const mir::ExprId out_id = block.exprs.Add(*std::move(out_or));

  auto call_expr_or = BuildSFormatCallExpr(process, frame, call, info, 1);
  if (!call_expr_or) return std::unexpected(std::move(call_expr_or.error()));
  const mir::ExprId call_id = block.exprs.Add(*std::move(call_expr_or));

  // LRM 21.3.3: the formatted text reaches output_var under the LRM 5.9
  // string-literal assignment rules, so an integral or unpacked-byte-array
  // destination conforms the string value to its own representation. A
  // string-typed destination already matches and passes through unchanged.
  const mir::ExprId value_id =
      ConvertToType(process.Owner().Unit(), block, call_id, out_type);

  const mir::ExprId services_id =
      block.exprs.Add(BuildServicesCallExpr(process.Owner(), frame));
  const mir::Expr assign_expr = BuildObservableAssignExpr(
      process.Owner().Unit(), block, services_id, out_id, value_id,
      std::nullopt, out_type, process.Owner().Unit().builtins.void_type);
  const mir::ExprId assign_id = block.exprs.Add(assign_expr);

  return mir::Stmt{
      .label = std::move(label), .data = mir::ExprStmt{.expr = assign_id}};
}

template auto LowerSFormatSystemSubroutineCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&,
    const support::SFormatSystemSubroutineInfo&) -> diag::Result<mir::Expr>;
template auto LowerSFormatSystemSubroutineCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&,
    const support::SFormatSystemSubroutineInfo&) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir

#include "lyra/lowering/hir_to_mir/expression/system/plusargs.hpp"

#include <array>
#include <expected>
#include <optional>
#include <span>
#include <utility>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"  // IWYU pragma: keep
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"  // IWYU pragma: keep
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

template <ExprLowerer Lowerer>
auto LowerTestPlusargs(
    Lowerer& lowerer, WalkFrame frame, std::span<const hir::ExprId> operands)
    -> diag::Result<mir::Expr> {
  const auto& hir_exprs = lowerer.HirExprs();
  auto& unit = lowerer.Owner().Unit();
  auto& body = *frame.current_block;

  auto user_or = lowerer.LowerExpr(hir_exprs.Get(operands[0]), frame);
  if (!user_or) return std::unexpected(std::move(user_or.error()));
  const mir::ExprId raw_user_id = body.exprs.Add(*std::move(user_or));
  // A packed literal / integral variable is a legal user_string here
  // (LRM 21.6); the runtime signature is on SV `string`, so the operand
  // gets the value-layer conversion.
  const mir::ExprId user_id =
      ConvertToType(unit, body, raw_user_id, unit.builtins.string);
  const mir::ExprId runtime_id =
      body.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner()));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{.target = support::BuiltinFn::kTestPlusargs},
              .arguments = {runtime_id, user_id}},
      .type = unit.builtins.int_type};
}

template <ExprLowerer Lowerer>
auto LowerValuePlusargs(
    Lowerer& lowerer, WalkFrame frame, std::span<const hir::ExprId> operands)
    -> diag::Result<mir::Expr> {
  const auto& hir_exprs = lowerer.HirExprs();
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();

  // A match converts the plusarg's remainder into the caller's lvalue and
  // answers 1; a miss answers 0 and hands the lvalue back as it was, which is
  // what the clause requires of it (LRM 21.6). Both are steps of one block
  // expression, so the call stands wherever the source wrote it.
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const WalkFrame& step_frame = steps.Frame();

  auto user_or = lowerer.LowerExpr(hir_exprs.Get(operands[0]), step_frame);
  if (!user_or) return std::unexpected(std::move(user_or.error()));
  const mir::ExprId raw_user_id = body.exprs.Add(*std::move(user_or));
  // A packed literal / integral variable is a legal user_string here
  // (LRM 21.6); the runtime signature is on SV `string`, so the operand
  // gets the value-layer conversion.
  const mir::ExprId user_id =
      ConvertToType(unit, body, raw_user_id, unit.builtins.string);

  // The destination is an `inout`: a miss leaves the variable it names as it
  // was, and its size decides whether a match is zero-padded or truncated, so
  // its value crosses in as well as riding the completion back (LRM 13.5,
  // 21.6). Its place is bound here, which is the once it is evaluated.
  const hir::Expr& target_hir = hir_exprs.Get(operands[1]);
  const mir::TypeId target_type = unit_lowerer.TranslateType(target_hir.type);
  auto place_or = lowerer.LowerLhsExpr(target_hir, step_frame);
  if (!place_or) return std::unexpected(std::move(place_or.error()));
  const mir::ExprId target_place = body.exprs.Add(*std::move(place_or));
  auto incoming_or = lowerer.LowerExpr(target_hir, step_frame);
  if (!incoming_or) return std::unexpected(std::move(incoming_or.error()));
  const mir::ExprId incoming_id = body.exprs.Add(*std::move(incoming_or));

  const CompletionLayout layout = BuildCompletionLayout(
      {CalleeFormal{
          .direction = hir::ParamDirection::kInOut, .type = target_type}},
      unit.builtins.int_type);
  const mir::TypeId payload = CompletionPayloadType(unit, layout.components);
  const mir::ExprId runtime_id =
      body.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  const std::array writebacks{CompletionWriteback{
      .place = target_place,
      .component = *layout.formals.front().component,
      .type = target_type}};
  const mir::LocalId completion = BindCompletion(
      unit, step_frame,
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kValuePlusargs},
                  .arguments = {runtime_id, user_id, incoming_id}},
          .type = payload},
      payload, writebacks);
  return steps.Build(ProjectCompletionComponent(
      body, completion, payload, base::ComponentIndex{},
      unit.builtins.int_type));
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerPlusargsSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::PlusargsSystemSubroutineInfo& info)
    -> diag::Result<mir::Expr> {
  switch (info.kind) {
    case support::PlusargsKind::kTest: {
      // $test$plusargs(user_string) -- LRM 21.6.
      return LowerTestPlusargs(lowerer, frame, RequiredOperands(call, 1));
    }
    case support::PlusargsKind::kValue: {
      // $value$plusargs(user_string, variable) -- LRM 21.6.
      return LowerValuePlusargs(lowerer, frame, RequiredOperands(call, 2));
    }
  }
  throw InternalError(
      "LowerPlusargsSystemSubroutineCall: unknown PlusargsKind");
}

template auto LowerPlusargsSystemSubroutineCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&,
    const support::PlusargsSystemSubroutineInfo&) -> diag::Result<mir::Expr>;
template auto LowerPlusargsSystemSubroutineCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&,
    const support::PlusargsSystemSubroutineInfo&) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir

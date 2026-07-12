#include "lyra/lowering/hir_to_mir/expression/system/plusargs.hpp"

#include <cstdint>
#include <expected>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerTestPlusargs(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& unit = process.Module().Unit();
  auto& body = *frame.current_block;

  auto user_or =
      process.LowerExpr(hir_proc.exprs.Get(*call.arguments[0]), frame);
  if (!user_or) return std::unexpected(std::move(user_or.error()));
  const mir::ExprId raw_user_id = body.exprs.Add(*std::move(user_or));
  // A packed literal / integral variable is a legal user_string here
  // (LRM 21.6); the runtime signature is on SV `string`, so the operand
  // gets the value-layer conversion.
  const mir::ExprId user_id =
      ConvertToType(unit, body, raw_user_id, unit.builtins.string);
  const mir::ExprId services_id =
      body.exprs.Add(BuildServicesCallExpr(process.Module(), frame));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{.target = support::BuiltinFn::kTestPlusargs},
              .arguments = {services_id, user_id}},
      .type = unit.builtins.int32};
}

auto LowerValuePlusargs(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& module = process.Module();
  auto& unit = module.Unit();
  const mir::TypeId int32_t_id = unit.builtins.int32;
  const mir::TypeId bit_t = unit.builtins.bit1;
  const mir::TypeId void_t = unit.builtins.void_type;

  const auto& hir_target = hir_proc.exprs.Get(*call.arguments[1]);
  const mir::TypeId target_type = module.TranslateType(hir_target.type);

  // A match writes the parsed remainder into the caller's lvalue and returns
  // 1; a no-match leaves the lvalue untouched and returns 0. Both effects
  // sit in an IIFE so the call fits in expression position (LRM 21.6).
  ClosureBuilder closure(unit, frame);
  mir::Block& body = closure.Body();
  const WalkFrame& closure_frame = closure.Frame();

  auto user_or =
      process.LowerExpr(hir_proc.exprs.Get(*call.arguments[0]), closure_frame);
  if (!user_or) return std::unexpected(std::move(user_or.error()));
  const mir::ExprId raw_user_id = body.exprs.Add(*std::move(user_or));
  // A packed literal / integral variable is a legal user_string here
  // (LRM 21.6); the runtime signature is on SV `string`, so the operand
  // gets the value-layer conversion.
  const mir::ExprId user_id =
      ConvertToType(unit, body, raw_user_id, unit.builtins.string);

  const mir::ExprId temp_init =
      body.exprs.Add(BuildDefaultValueExpr(module, closure_frame, target_type));
  const mir::LocalId temp_var = closure.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_plusargs_out", .type = target_type});
  body.AppendStmt(mir::LocalDeclStmt{.target = temp_var, .init = temp_init});

  const mir::ExprId services_id =
      body.exprs.Add(BuildServicesCallExpr(module, closure_frame));
  const mir::ExprId temp_ref =
      body.exprs.Add(mir::MakeLocalRefExpr(temp_var, target_type));
  const mir::ExprId hit_call = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kValuePlusargs},
                  .arguments = {services_id, user_id, temp_ref}},
          .type = int32_t_id});
  const mir::LocalId hit_var = closure.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_plusargs_hit", .type = int32_t_id});
  body.AppendStmt(mir::LocalDeclStmt{.target = hit_var, .init = hit_call});

  const mir::ExprId hit_read =
      body.exprs.Add(mir::MakeLocalRefExpr(hit_var, int32_t_id));
  const mir::ExprId one_lit = body.exprs.Add(
      mir::MakeInt32Literal(int32_t_id, static_cast<std::int64_t>(1)));
  const mir::ExprId cond_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kGreaterEqual,
                  .lhs = hit_read,
                  .rhs = one_lit},
          .type = bit_t});

  mir::Block then_body;
  const WalkFrame then_frame = closure_frame.WithBlock(&then_body);
  auto lvalue_or =
      process.LowerLhsExpr(hir_proc.exprs.Get(*call.arguments[1]), then_frame);
  if (!lvalue_or) return std::unexpected(std::move(lvalue_or.error()));
  const mir::ExprId lvalue_id = then_body.exprs.Add(*std::move(lvalue_or));
  const mir::ExprId temp_read_then =
      then_body.exprs.Add(mir::MakeLocalRefExpr(temp_var, target_type));
  const mir::ExprId services_id_then =
      then_body.exprs.Add(BuildServicesCallExpr(module, then_frame));
  const mir::Expr assign_expr = BuildObservableAssignExpr(
      unit, then_body, services_id_then, lvalue_id, temp_read_then,
      std::nullopt, target_type, void_t);
  const mir::ExprId assign_id = then_body.exprs.Add(assign_expr);
  then_body.AppendStmt(mir::ExprStmt{.expr = assign_id});

  body.AppendIfThen(
      ReduceToCondition(body, cond_id, unit.builtins.bit1),
      std::move(then_body));

  const mir::ExprId final_hit =
      body.exprs.Add(mir::MakeLocalRefExpr(hit_var, int32_t_id));
  return BuildClosureCallExpr(
      unit, *frame.current_block, closure.Build(final_hit));
}

}  // namespace

auto LowerPlusargsSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::PlusargsSystemSubroutineInfo& info)
    -> diag::Result<mir::Expr> {
  switch (info.kind) {
    case support::PlusargsKind::kTest: {
      if (call.arguments.size() != 1 || !call.arguments[0].has_value()) {
        throw InternalError(
            "LowerPlusargsSystemSubroutineCall: $test$plusargs expects exactly "
            "one non-elided argument");
      }
      return LowerTestPlusargs(process, frame, call);
    }
    case support::PlusargsKind::kValue: {
      if (call.arguments.size() != 2 || !call.arguments[0].has_value() ||
          !call.arguments[1].has_value()) {
        throw InternalError(
            "LowerPlusargsSystemSubroutineCall: $value$plusargs expects two "
            "non-elided arguments");
      }
      return LowerValuePlusargs(process, frame, call);
    }
  }
  throw InternalError(
      "LowerPlusargsSystemSubroutineCall: unknown PlusargsKind");
}

}  // namespace lyra::lowering::hir_to_mir

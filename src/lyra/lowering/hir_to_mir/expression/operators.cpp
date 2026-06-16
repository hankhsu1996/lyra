#include "lyra/lowering/hir_to_mir/expression/operators.hpp"

#include <expected>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp {
  switch (op) {
    case hir::BinaryOp::kAdd:
      return mir::BinaryOp::kAdd;
    case hir::BinaryOp::kSub:
      return mir::BinaryOp::kSub;
    case hir::BinaryOp::kMul:
      return mir::BinaryOp::kMul;
    case hir::BinaryOp::kDiv:
      return mir::BinaryOp::kDiv;
    case hir::BinaryOp::kMod:
      return mir::BinaryOp::kMod;
    case hir::BinaryOp::kPower:
      return mir::BinaryOp::kPower;
    case hir::BinaryOp::kBitwiseAnd:
      return mir::BinaryOp::kBitwiseAnd;
    case hir::BinaryOp::kBitwiseOr:
      return mir::BinaryOp::kBitwiseOr;
    case hir::BinaryOp::kBitwiseXor:
      return mir::BinaryOp::kBitwiseXor;
    case hir::BinaryOp::kBitwiseXnor:
      return mir::BinaryOp::kBitwiseXnor;
    case hir::BinaryOp::kEquality:
      return mir::BinaryOp::kEquality;
    case hir::BinaryOp::kInequality:
      return mir::BinaryOp::kInequality;
    case hir::BinaryOp::kCaseEquality:
      return mir::BinaryOp::kCaseEquality;
    case hir::BinaryOp::kCaseInequality:
      return mir::BinaryOp::kCaseInequality;
    case hir::BinaryOp::kWildcardEquality:
      return mir::BinaryOp::kWildcardEquality;
    case hir::BinaryOp::kWildcardInequality:
      return mir::BinaryOp::kWildcardInequality;
    case hir::BinaryOp::kGreaterEqual:
      return mir::BinaryOp::kGreaterEqual;
    case hir::BinaryOp::kGreaterThan:
      return mir::BinaryOp::kGreaterThan;
    case hir::BinaryOp::kLessEqual:
      return mir::BinaryOp::kLessEqual;
    case hir::BinaryOp::kLessThan:
      return mir::BinaryOp::kLessThan;
    case hir::BinaryOp::kLogicalAnd:
      return mir::BinaryOp::kLogicalAnd;
    case hir::BinaryOp::kLogicalOr:
      return mir::BinaryOp::kLogicalOr;
    case hir::BinaryOp::kLogicalImplication:
      return mir::BinaryOp::kLogicalImplication;
    case hir::BinaryOp::kLogicalEquivalence:
      return mir::BinaryOp::kLogicalEquivalence;
    case hir::BinaryOp::kLogicalShiftLeft:
    case hir::BinaryOp::kArithmeticShiftLeft:
      return mir::BinaryOp::kShiftLeft;
    case hir::BinaryOp::kLogicalShiftRight:
      return mir::BinaryOp::kLogicalShiftRight;
    case hir::BinaryOp::kArithmeticShiftRight:
      return mir::BinaryOp::kArithmeticShiftRight;
  }
  throw InternalError("LowerBinaryOp: unknown HIR BinaryOp");
}

namespace {

auto LowerUnaryOp(hir::UnaryOp op) -> mir::UnaryOp {
  switch (op) {
    case hir::UnaryOp::kPlus:
      return mir::UnaryOp::kPlus;
    case hir::UnaryOp::kMinus:
      return mir::UnaryOp::kMinus;
    case hir::UnaryOp::kBitwiseNot:
      return mir::UnaryOp::kBitwiseNot;
    case hir::UnaryOp::kLogicalNot:
      return mir::UnaryOp::kLogicalNot;
    case hir::UnaryOp::kReductionAnd:
      return mir::UnaryOp::kReductionAnd;
    case hir::UnaryOp::kReductionOr:
      return mir::UnaryOp::kReductionOr;
    case hir::UnaryOp::kReductionXor:
      return mir::UnaryOp::kReductionXor;
    case hir::UnaryOp::kReductionNand:
      return mir::UnaryOp::kReductionNand;
    case hir::UnaryOp::kReductionNor:
      return mir::UnaryOp::kReductionNor;
    case hir::UnaryOp::kReductionXnor:
      return mir::UnaryOp::kReductionXnor;
  }
  throw InternalError("LowerUnaryOp: unknown HIR UnaryOp");
}

auto LowerIncDecOp(hir::IncDecOp op) -> mir::IncDecOp {
  switch (op) {
    case hir::IncDecOp::kPreInc:
      return mir::IncDecOp::kPreInc;
    case hir::IncDecOp::kPostInc:
      return mir::IncDecOp::kPostInc;
    case hir::IncDecOp::kPreDec:
      return mir::IncDecOp::kPreDec;
    case hir::IncDecOp::kPostDec:
      return mir::IncDecOp::kPostDec;
  }
  throw InternalError("LowerIncDecOp: unknown HIR IncDecOp");
}

auto LowerHirConversionKind(hir::ConversionKind k) -> mir::ConversionKind {
  switch (k) {
    case hir::ConversionKind::kImplicit:
      return mir::ConversionKind::kImplicit;
    case hir::ConversionKind::kPropagated:
      return mir::ConversionKind::kPropagated;
    case hir::ConversionKind::kStreamingConcat:
      return mir::ConversionKind::kStreamingConcat;
    case hir::ConversionKind::kExplicit:
      return mir::ConversionKind::kExplicit;
    case hir::ConversionKind::kBitstreamCast:
      return mir::ConversionKind::kBitstreamCast;
  }
  throw InternalError("LowerHirConversionKind: unknown HIR ConversionKind");
}

}  // namespace

auto LowerHirUnaryExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::UnaryExpr& u,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto operand_or =
      process.LowerExpr(hir_process.exprs.at(u.operand.value), frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      frame.current_procedural_scope->AddExpr(*std::move(operand_or));
  return mir::Expr{
      .data = mir::UnaryExpr{.op = LowerUnaryOp(u.op), .operand = operand_id},
      .type = result_type};
}

auto LowerHirBinaryExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::BinaryExpr& b,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto lhs_or = process.LowerExpr(hir_process.exprs.at(b.lhs.value), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id =
      frame.current_procedural_scope->AddExpr(*std::move(lhs_or));
  auto rhs_or = process.LowerExpr(hir_process.exprs.at(b.rhs.value), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id =
      frame.current_procedural_scope->AddExpr(*std::move(rhs_or));
  return mir::Expr{
      .data =
          mir::BinaryExpr{
              .op = LowerBinaryOp(b.op), .lhs = lhs_id, .rhs = rhs_id},
      .type = result_type};
}

auto LowerHirConditionalExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::ConditionalExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto cond_or =
      process.LowerExpr(hir_process.exprs.at(c.condition.value), frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id =
      frame.current_procedural_scope->AddExpr(*std::move(cond_or));
  auto then_or =
      process.LowerExpr(hir_process.exprs.at(c.then_value.value), frame);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const mir::ExprId then_id =
      frame.current_procedural_scope->AddExpr(*std::move(then_or));
  auto else_or =
      process.LowerExpr(hir_process.exprs.at(c.else_value.value), frame);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const mir::ExprId else_id =
      frame.current_procedural_scope->AddExpr(*std::move(else_or));
  return mir::Expr{
      .data =
          mir::ConditionalExpr{
              .condition = cond_id,
              .then_value = then_id,
              .else_value = else_id},
      .type = result_type};
}

auto LowerHirIncDecExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto target_or =
      process.LowerExpr(hir_process.exprs.at(inc.target.value), frame);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  const mir::ExprId target_id =
      frame.current_procedural_scope->AddExpr(*std::move(target_or));
  return mir::Expr{
      .data = mir::IncDecExpr{.op = LowerIncDecOp(inc.op), .target = target_id},
      .type = result_type};
}

auto LowerHirConversionExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::ConversionExpr& cv,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto operand_or =
      process.LowerExpr(hir_process.exprs.at(cv.operand.value), frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      frame.current_procedural_scope->AddExpr(*std::move(operand_or));
  return mir::Expr{
      .data =
          mir::ConversionExpr{
              .operand = operand_id, .kind = LowerHirConversionKind(cv.kind)},
      .type = result_type};
}

auto LowerHirUnaryExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::UnaryExpr& u, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_scope = scope.HirScope();
  auto operand_or = scope.LowerExpr(hir_scope.GetExpr(u.operand), frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      frame.current_procedural_scope->AddExpr(*std::move(operand_or));
  return mir::Expr{
      .data = mir::UnaryExpr{.op = LowerUnaryOp(u.op), .operand = operand_id},
      .type = result_type};
}

auto LowerHirBinaryExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::BinaryExpr& b, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_scope = scope.HirScope();
  auto& proc_scope = *frame.current_procedural_scope;
  auto lhs_or = scope.LowerExpr(hir_scope.GetExpr(b.lhs), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = proc_scope.AddExpr(*std::move(lhs_or));
  auto rhs_or = scope.LowerExpr(hir_scope.GetExpr(b.rhs), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = proc_scope.AddExpr(*std::move(rhs_or));
  return mir::Expr{
      .data =
          mir::BinaryExpr{
              .op = LowerBinaryOp(b.op), .lhs = lhs_id, .rhs = rhs_id},
      .type = result_type};
}

auto LowerHirConditionalExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::ConditionalExpr& c, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_scope = scope.HirScope();
  auto& proc_scope = *frame.current_procedural_scope;
  auto cond_or = scope.LowerExpr(hir_scope.GetExpr(c.condition), frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = proc_scope.AddExpr(*std::move(cond_or));
  auto then_or = scope.LowerExpr(hir_scope.GetExpr(c.then_value), frame);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const mir::ExprId then_id = proc_scope.AddExpr(*std::move(then_or));
  auto else_or = scope.LowerExpr(hir_scope.GetExpr(c.else_value), frame);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const mir::ExprId else_id = proc_scope.AddExpr(*std::move(else_or));
  return mir::Expr{
      .data =
          mir::ConditionalExpr{
              .condition = cond_id,
              .then_value = then_id,
              .else_value = else_id},
      .type = result_type};
}

auto LowerHirConversionExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::ConversionExpr& cv, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_scope = scope.HirScope();
  auto operand_or = scope.LowerExpr(hir_scope.GetExpr(cv.operand), frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      frame.current_procedural_scope->AddExpr(*std::move(operand_or));
  return mir::Expr{
      .data =
          mir::ConversionExpr{
              .operand = operand_id, .kind = LowerHirConversionKind(cv.kind)},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir

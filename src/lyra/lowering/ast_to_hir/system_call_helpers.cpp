#include "lyra/lowering/ast_to_hir/system_call_helpers.hpp"

#include <variant>

#include <slang/ast/expressions/AssignmentExpressions.h>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/timescale.hpp"

namespace lyra::lowering::ast_to_hir {

auto ComputeTimeDivisor(const LoweringFrame& frame) -> uint64_t {
  int exponent = frame.unit_power - frame.global_precision_power;
  if (exponent < 0) {
    throw common::InternalError(
        "time scaling",
        "negative exponent - global precision coarser than timeunit");
  }
  if (exponent == 0) {
    return 1;
  }
  auto result = IntegerPow10(exponent);
  if (!result) {
    throw common::InternalError("time scaling", "divisor overflow");
  }
  return *result;
}

auto EmitRawTicksQuery(SourceSpan span, Context* ctx) -> hir::ExpressionId {
  TypeId tick_type = ctx->GetTickType();
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kSystemCall,
          .type = tick_type,
          .span = span,
          .data = hir::SystemCallExpressionData{
              hir::RuntimeQueryData{.kind = RuntimeQueryKind::kTimeRawTicks}}});
}

auto MakeConstant(uint64_t value, TypeId type, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  IntegralConstant constant;
  constant.value.push_back(value);
  constant.unknown.push_back(0);
  ConstId cid = ctx->constant_arena->Intern(type, std::move(constant));
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kConstant,
          .type = type,
          .span = span,
          .data = hir::ConstantExpressionData{.constant = cid}});
}

auto EmitIntegerTimeScaling(
    hir::ExpressionId raw_ticks, uint64_t divisor, TypeId result_type,
    SourceSpan span, Context* ctx) -> hir::ExpressionId {
  TypeId tick_type = ctx->GetTickType();
  hir::ExpressionId scaled = raw_ticks;

  if (divisor > 1) {
    hir::ExpressionId divisor_expr =
        MakeConstant(divisor, tick_type, span, ctx);
    scaled = ctx->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kBinaryOp,
            .type = tick_type,
            .span = span,
            .data = hir::BinaryExpressionData{
                .op = hir::BinaryOp::kDivide,
                .lhs = raw_ticks,
                .rhs = divisor_expr}});
  }

  if (tick_type == result_type) {
    return scaled;
  }
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kCast,
          .type = result_type,
          .span = span,
          .data = hir::CastExpressionData{.operand = scaled}});
}

auto EmitRealTimeScaling(
    hir::ExpressionId raw_ticks, uint64_t divisor, SourceSpan span,
    Context* ctx) -> hir::ExpressionId {
  TypeId tick_type = ctx->GetTickType();
  TypeId real_type = ctx->type_arena->Intern(TypeKind::kReal, std::monostate{});

  // Cast raw ticks to real first
  hir::ExpressionId raw_real = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kCast,
          .type = real_type,
          .span = span,
          .data = hir::CastExpressionData{.operand = raw_ticks}});

  if (divisor == 1) {
    return raw_real;
  }

  // Real division
  hir::ExpressionId divisor_int = MakeConstant(divisor, tick_type, span, ctx);
  hir::ExpressionId divisor_real = ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kCast,
          .type = real_type,
          .span = span,
          .data = hir::CastExpressionData{.operand = divisor_int}});

  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kBinaryOp,
          .type = real_type,
          .span = span,
          .data = hir::BinaryExpressionData{
              .op = hir::BinaryOp::kDivide,
              .lhs = raw_real,
              .rhs = divisor_real}});
}

auto UnwrapOutputArgument(
    const slang::ast::Expression* arg, ExpressionLoweringView view)
    -> hir::ExpressionId {
  if (arg->kind == slang::ast::ExpressionKind::Assignment) {
    const auto& assign = arg->as<slang::ast::AssignmentExpression>();
    return LowerExpression(assign.left(), view);
  }
  return LowerExpression(*arg, view);
}

auto RequireModuleFrame(
    std::string_view func_name, SourceSpan span, ExpressionLoweringView view)
    -> const LoweringFrame* {
  if (view.frame != nullptr) {
    return view.frame;
  }
  view.context->ErrorFmt(
      span,
      "{} requires module scope (cannot be used in port bindings "
      "or parameter contexts)",
      func_name);
  return nullptr;
}

}  // namespace lyra::lowering::ast_to_hir

#include "lyra/lowering/ast_to_hir/system_function_desugar.hpp"

#include <string_view>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"
#include "lyra/semantic/bitcast_validation.hpp"
#include "lyra/semantic/signedness_cast.hpp"

namespace lyra::lowering::ast_to_hir {

auto ClassifyPureSystemFunction(const slang::ast::CallExpression& call)
    -> std::optional<PureSysFnKind> {
  if (!call.isSystemCall()) {
    return std::nullopt;
  }

  std::string_view name = call.getSubroutineName();

  // Value conversion functions -> kCast
  if (name == "$signed") {
    return PureSysFnKind::kSigned;
  }
  if (name == "$unsigned") {
    return PureSysFnKind::kUnsigned;
  }
  if (name == "$itor") {
    return PureSysFnKind::kItor;
  }
  if (name == "$rtoi") {
    return PureSysFnKind::kRtoi;
  }

  // Bit reinterpretation functions -> kBitCast
  if (name == "$realtobits") {
    return PureSysFnKind::kRealToBits;
  }
  if (name == "$bitstoreal") {
    return PureSysFnKind::kBitsToReal;
  }
  if (name == "$shortrealtobits") {
    return PureSysFnKind::kShortRealToBits;
  }
  if (name == "$bitstoshortreal") {
    return PureSysFnKind::kBitsToShortReal;
  }

  return std::nullopt;
}

namespace {

// Create kCast expression node
auto MakeCast(
    hir::ExpressionId operand, TypeId target_type, SourceSpan span,
    Context* ctx) -> hir::ExpressionId {
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kCast,
          .type = target_type,
          .span = span,
          .data = hir::CastExpressionData{.operand = operand}});
}

// Create kBitCast expression node with validation
// Returns kInvalidExpressionId on validation failure (error already emitted)
auto MakeBitCast(
    hir::ExpressionId operand, TypeId target_type, SourceSpan span,
    Context* ctx) -> hir::ExpressionId {
  const hir::Expression& operand_expr = (*ctx->hir_arena)[operand];
  TypeId source_type = operand_expr.type;

  const Type& src = (*ctx->type_arena)[source_type];
  const Type& dst = (*ctx->type_arena)[target_type];

  auto err = semantic::ValidateBitCast(src, dst, *ctx->type_arena);
  if (err) {
    ctx->sink->Error(span, *err);
    return hir::kInvalidExpressionId;
  }

  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kBitCast,
          .type = target_type,
          .span = span,
          .data = hir::BitCastExpressionData{.operand = operand}});
}

}  // namespace

auto LowerPureSystemFunction(
    const slang::ast::CallExpression& call, PureSysFnKind kind,
    SymbolRegistrar& registrar, Context* ctx) -> hir::ExpressionId {
  SourceSpan span = ctx->SpanOf(call.sourceRange);

  // All pure system functions take exactly one argument
  if (call.arguments().size() != 1) {
    ctx->ErrorFmt(
        span, "{}() requires exactly one argument", call.getSubroutineName());
    return hir::kInvalidExpressionId;
  }

  // Lower the operand
  hir::ExpressionId operand =
      LowerExpression(*call.arguments()[0], registrar, ctx);
  if (!operand) {
    return hir::kInvalidExpressionId;
  }

  const hir::Expression& operand_expr = (*ctx->hir_arena)[operand];

  switch (kind) {
    case PureSysFnKind::kSigned: {
      auto result =
          semantic::MakeSignedVariant(operand_expr.type, *ctx->type_arena);
      if (!result) {
        ctx->sink->Error(span, result.error());
        return hir::kInvalidExpressionId;
      }
      return MakeCast(operand, *result, span, ctx);
    }

    case PureSysFnKind::kUnsigned: {
      auto result =
          semantic::MakeUnsignedVariant(operand_expr.type, *ctx->type_arena);
      if (!result) {
        ctx->sink->Error(span, result.error());
        return hir::kInvalidExpressionId;
      }
      return MakeCast(operand, *result, span, ctx);
    }

    case PureSysFnKind::kItor: {
      // $itor: integer to real
      TypeId real_type =
          ctx->type_arena->Intern(TypeKind::kReal, std::monostate{});
      return MakeCast(operand, real_type, span, ctx);
    }

    case PureSysFnKind::kRtoi: {
      // $rtoi: real to integer (32-bit signed 4-state per SV spec)
      TypeId integer_type = ctx->type_arena->Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 32, .is_signed = true, .is_four_state = true});
      return MakeCast(operand, integer_type, span, ctx);
    }

    case PureSysFnKind::kRealToBits: {
      // $realtobits: real -> bit[63:0]
      TypeId target = semantic::GetBitVectorType(*ctx->type_arena, 64);
      return MakeBitCast(operand, target, span, ctx);
    }

    case PureSysFnKind::kBitsToReal: {
      // $bitstoreal: bit[63:0] -> real
      TypeId real_type =
          ctx->type_arena->Intern(TypeKind::kReal, std::monostate{});
      return MakeBitCast(operand, real_type, span, ctx);
    }

    case PureSysFnKind::kShortRealToBits: {
      // $shortrealtobits: shortreal -> bit[31:0]
      TypeId target = semantic::GetBitVectorType(*ctx->type_arena, 32);
      return MakeBitCast(operand, target, span, ctx);
    }

    case PureSysFnKind::kBitsToShortReal: {
      // $bitstoshortreal: bit[31:0] -> shortreal
      TypeId shortreal_type =
          ctx->type_arena->Intern(TypeKind::kShortReal, std::monostate{});
      return MakeBitCast(operand, shortreal_type, span, ctx);
    }
  }

  // Should not reach here
  ctx->ErrorFmt(
      span, "unhandled pure system function: {}", call.getSubroutineName());
  return hir::kInvalidExpressionId;
}

}  // namespace lyra::lowering::ast_to_hir

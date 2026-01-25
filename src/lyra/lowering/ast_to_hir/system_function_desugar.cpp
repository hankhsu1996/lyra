#include "lyra/lowering/ast_to_hir/system_function_desugar.hpp"

#include <string_view>

#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/numeric/Time.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/timescale.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"
#include "lyra/semantic/bitcast_validation.hpp"
#include "lyra/semantic/signedness_cast.hpp"

namespace lyra::lowering::ast_to_hir {

auto ClassifyPureSystemFunction(const slang::ast::CallExpression& call)
    -> std::optional<PureSysFnClassification> {
  if (!call.isSystemCall()) {
    return std::nullopt;
  }

  std::string_view name = call.getSubroutineName();

  // Conversion functions
  if (name == "$signed") {
    return ConversionSysFnKind::kSigned;
  }
  if (name == "$unsigned") {
    return ConversionSysFnKind::kUnsigned;
  }
  if (name == "$itor") {
    return ConversionSysFnKind::kItor;
  }
  if (name == "$rtoi") {
    return ConversionSysFnKind::kRtoi;
  }
  if (name == "$realtobits") {
    return ConversionSysFnKind::kRealToBits;
  }
  if (name == "$bitstoreal") {
    return ConversionSysFnKind::kBitsToReal;
  }
  if (name == "$shortrealtobits") {
    return ConversionSysFnKind::kShortRealToBits;
  }
  if (name == "$bitstoshortreal") {
    return ConversionSysFnKind::kBitsToShortReal;
  }

  // Math functions -> MathSysFn
  if (name == "$ln") {
    return MathSysFn{MathFn::kLn};
  }
  if (name == "$log10") {
    return MathSysFn{MathFn::kLog10};
  }
  if (name == "$exp") {
    return MathSysFn{MathFn::kExp};
  }
  if (name == "$sqrt") {
    return MathSysFn{MathFn::kSqrt};
  }
  if (name == "$floor") {
    return MathSysFn{MathFn::kFloor};
  }
  if (name == "$ceil") {
    return MathSysFn{MathFn::kCeil};
  }
  if (name == "$sin") {
    return MathSysFn{MathFn::kSin};
  }
  if (name == "$cos") {
    return MathSysFn{MathFn::kCos};
  }
  if (name == "$tan") {
    return MathSysFn{MathFn::kTan};
  }
  if (name == "$asin") {
    return MathSysFn{MathFn::kAsin};
  }
  if (name == "$acos") {
    return MathSysFn{MathFn::kAcos};
  }
  if (name == "$atan") {
    return MathSysFn{MathFn::kAtan};
  }
  if (name == "$sinh") {
    return MathSysFn{MathFn::kSinh};
  }
  if (name == "$cosh") {
    return MathSysFn{MathFn::kCosh};
  }
  if (name == "$tanh") {
    return MathSysFn{MathFn::kTanh};
  }
  if (name == "$asinh") {
    return MathSysFn{MathFn::kAsinh};
  }
  if (name == "$acosh") {
    return MathSysFn{MathFn::kAcosh};
  }
  if (name == "$atanh") {
    return MathSysFn{MathFn::kAtanh};
  }
  if (name == "$clog2") {
    return MathSysFn{MathFn::kClog2};
  }
  if (name == "$pow") {
    return MathSysFn{MathFn::kPow};
  }
  if (name == "$atan2") {
    return MathSysFn{MathFn::kAtan2};
  }
  if (name == "$hypot") {
    return MathSysFn{MathFn::kHypot};
  }

  // Timescale query functions -> constant
  if (name == "$timeunit") {
    return TimeScaleSysFnKind::kTimeunit;
  }
  if (name == "$timeprecision") {
    return TimeScaleSysFnKind::kTimeprecision;
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

auto LowerConversion(
    const slang::ast::CallExpression& call, ConversionSysFnKind kind,
    hir::ExpressionId operand, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  const hir::Expression& operand_expr = (*ctx->hir_arena)[operand];

  switch (kind) {
    case ConversionSysFnKind::kSigned: {
      auto result =
          semantic::MakeSignedVariant(operand_expr.type, *ctx->type_arena);
      if (!result) {
        ctx->sink->Error(span, result.error());
        return hir::kInvalidExpressionId;
      }
      return MakeCast(operand, *result, span, ctx);
    }

    case ConversionSysFnKind::kUnsigned: {
      auto result =
          semantic::MakeUnsignedVariant(operand_expr.type, *ctx->type_arena);
      if (!result) {
        ctx->sink->Error(span, result.error());
        return hir::kInvalidExpressionId;
      }
      return MakeCast(operand, *result, span, ctx);
    }

    case ConversionSysFnKind::kItor: {
      TypeId real_type =
          ctx->type_arena->Intern(TypeKind::kReal, std::monostate{});
      return MakeCast(operand, real_type, span, ctx);
    }

    case ConversionSysFnKind::kRtoi: {
      TypeId integer_type = ctx->type_arena->Intern(
          TypeKind::kIntegral,
          IntegralInfo{
              .bit_width = 32, .is_signed = true, .is_four_state = true});
      return MakeCast(operand, integer_type, span, ctx);
    }

    case ConversionSysFnKind::kRealToBits: {
      TypeId target = semantic::GetBitVectorType(*ctx->type_arena, 64);
      return MakeBitCast(operand, target, span, ctx);
    }

    case ConversionSysFnKind::kBitsToReal: {
      TypeId real_type =
          ctx->type_arena->Intern(TypeKind::kReal, std::monostate{});
      return MakeBitCast(operand, real_type, span, ctx);
    }

    case ConversionSysFnKind::kShortRealToBits: {
      TypeId target = semantic::GetBitVectorType(*ctx->type_arena, 32);
      return MakeBitCast(operand, target, span, ctx);
    }

    case ConversionSysFnKind::kBitsToShortReal: {
      TypeId shortreal_type =
          ctx->type_arena->Intern(TypeKind::kShortReal, std::monostate{});
      return MakeBitCast(operand, shortreal_type, span, ctx);
    }
  }

  ctx->ErrorFmt(
      span, "unhandled conversion function: {}", call.getSubroutineName());
  return hir::kInvalidExpressionId;
}

auto MakeIntConstant(int32_t value, SourceSpan span, Context* ctx)
    -> hir::ExpressionId {
  TypeId int_type = ctx->type_arena->Intern(
      TypeKind::kIntegral,
      IntegralInfo{.bit_width = 32, .is_signed = true, .is_four_state = false});
  auto bits = static_cast<uint32_t>(value);
  IntegralConstant ic;
  ic.value = {static_cast<uint64_t>(bits)};
  ic.unknown = {0};
  ConstId cid = ctx->constant_arena->Intern(int_type, std::move(ic));
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kConstant,
          .type = int_type,
          .span = span,
          .data = hir::ConstantExpressionData{.constant = cid}});
}

}  // namespace

auto LowerPureSystemFunction(
    const slang::ast::CallExpression& call,
    const PureSysFnClassification& classification, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ExpressionId {
  SourceSpan span = ctx->SpanOf(call.sourceRange);

  return std::visit(
      common::Overloaded{
          [&](ConversionSysFnKind kind) -> hir::ExpressionId {
            if (call.arguments().size() != 1) {
              ctx->ErrorFmt(
                  span, "{}() requires exactly one argument",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }
            hir::ExpressionId operand =
                LowerExpression(*call.arguments()[0], registrar, ctx);
            if (!operand) {
              return hir::kInvalidExpressionId;
            }
            return LowerConversion(call, kind, operand, span, ctx);
          },
          [&](UnaryOpSysFn fn) -> hir::ExpressionId {
            if (call.arguments().size() != 1) {
              ctx->ErrorFmt(
                  span, "{}() requires exactly one argument",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }
            hir::ExpressionId operand =
                LowerExpression(*call.arguments()[0], registrar, ctx);
            if (!operand) {
              return hir::kInvalidExpressionId;
            }
            TypeId result_type = LowerType(*call.type, span, ctx);
            return ctx->hir_arena->AddExpression(
                hir::Expression{
                    .kind = hir::ExpressionKind::kUnaryOp,
                    .type = result_type,
                    .span = span,
                    .data = hir::UnaryExpressionData{
                        .op = fn.op, .operand = operand}});
          },
          [&](BinaryOpSysFn fn) -> hir::ExpressionId {
            if (call.arguments().size() != 2) {
              ctx->ErrorFmt(
                  span, "{}() requires exactly two arguments",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }
            hir::ExpressionId lhs =
                LowerExpression(*call.arguments()[0], registrar, ctx);
            if (!lhs) {
              return hir::kInvalidExpressionId;
            }
            hir::ExpressionId rhs =
                LowerExpression(*call.arguments()[1], registrar, ctx);
            if (!rhs) {
              return hir::kInvalidExpressionId;
            }
            TypeId result_type = LowerType(*call.type, span, ctx);
            return ctx->hir_arena->AddExpression(
                hir::Expression{
                    .kind = hir::ExpressionKind::kBinaryOp,
                    .type = result_type,
                    .span = span,
                    .data = hir::BinaryExpressionData{
                        .op = fn.op, .lhs = lhs, .rhs = rhs}});
          },
          [&](TimeScaleSysFnKind kind) -> hir::ExpressionId {
            using slang::ast::ArbitrarySymbolExpression;
            using slang::ast::CompilationUnitSymbol;
            using slang::ast::InstanceSymbol;

            if (call.arguments().size() > 1) {
              ctx->ErrorFmt(
                  span, "{}() takes at most one argument",
                  call.getSubroutineName());
              return hir::kInvalidExpressionId;
            }

            const auto& sys_info =
                std::get<slang::ast::CallExpression::SystemCallInfo>(
                    call.subroutine);

            auto extract_power = [&](std::optional<slang::TimeScale> ts) {
              if (!ts) {
                return kDefaultTimeScalePower;
              }
              return TimeScaleValueToPower(
                  kind == TimeScaleSysFnKind::kTimeunit ? ts->base
                                                        : ts->precision);
            };

            int power = kDefaultTimeScalePower;
            if (call.arguments().empty()) {
              auto ts = sys_info.scope->getTimeScale();
              power = extract_power(ts);
            } else {
              const auto* arg_expr = call.arguments()[0];
              if (arg_expr->kind !=
                  slang::ast::ExpressionKind::ArbitrarySymbol) {
                ctx->ErrorFmt(
                    span,
                    "{}() argument must be a module/interface "
                    "instance, $root, or $unit",
                    call.getSubroutineName());
                return hir::kInvalidExpressionId;
              }
              const auto& arg = arg_expr->as<ArbitrarySymbolExpression>();
              const auto& sym = *arg.symbol;

              if (sym.kind == slang::ast::SymbolKind::Root) {
                power =
                    ComputeGlobalPrecision(sys_info.scope->getCompilation());
              } else if (sym.kind == slang::ast::SymbolKind::CompilationUnit) {
                auto ts = sym.as<CompilationUnitSymbol>().getTimeScale();
                power = extract_power(ts);
              } else if (sym.kind == slang::ast::SymbolKind::Instance) {
                auto ts = sym.as<InstanceSymbol>().body.getTimeScale();
                power = extract_power(ts);
              } else {
                ctx->ErrorFmt(
                    span, "{}() unexpected symbol kind in argument",
                    call.getSubroutineName());
                return hir::kInvalidExpressionId;
              }
            }

            return MakeIntConstant(power, span, ctx);
          },
          [&](MathSysFn fn) -> hir::ExpressionId {
            int expected_arity = GetMathFnArity(fn.fn);
            if (static_cast<int>(call.arguments().size()) != expected_arity) {
              ctx->ErrorFmt(
                  span, "{}() requires exactly {} argument(s)",
                  call.getSubroutineName(), expected_arity);
              return hir::kInvalidExpressionId;
            }
            std::vector<hir::ExpressionId> args;
            args.reserve(static_cast<size_t>(expected_arity));
            for (int i = 0; i < expected_arity; ++i) {
              hir::ExpressionId arg =
                  LowerExpression(*call.arguments()[i], registrar, ctx);
              if (!arg) {
                return hir::kInvalidExpressionId;
              }
              args.push_back(arg);
            }
            TypeId result_type = LowerType(*call.type, span, ctx);
            return ctx->hir_arena->AddExpression(
                hir::Expression{
                    .kind = hir::ExpressionKind::kMathCall,
                    .type = result_type,
                    .span = span,
                    .data = hir::MathCallExpressionData{
                        .fn = fn.fn, .args = std::move(args)}});
          }},
      classification);
}

}  // namespace lyra::lowering::ast_to_hir

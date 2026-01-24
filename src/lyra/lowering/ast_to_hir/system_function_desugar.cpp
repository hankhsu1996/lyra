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

  // Math unary functions -> UnaryOpSysFn
  if (name == "$ln") {
    return UnaryOpSysFn{hir::UnaryOp::kLn};
  }
  if (name == "$log10") {
    return UnaryOpSysFn{hir::UnaryOp::kLog10};
  }
  if (name == "$exp") {
    return UnaryOpSysFn{hir::UnaryOp::kExp};
  }
  if (name == "$sqrt") {
    return UnaryOpSysFn{hir::UnaryOp::kSqrt};
  }
  if (name == "$floor") {
    return UnaryOpSysFn{hir::UnaryOp::kFloor};
  }
  if (name == "$ceil") {
    return UnaryOpSysFn{hir::UnaryOp::kCeil};
  }
  if (name == "$sin") {
    return UnaryOpSysFn{hir::UnaryOp::kSin};
  }
  if (name == "$cos") {
    return UnaryOpSysFn{hir::UnaryOp::kCos};
  }
  if (name == "$tan") {
    return UnaryOpSysFn{hir::UnaryOp::kTan};
  }
  if (name == "$asin") {
    return UnaryOpSysFn{hir::UnaryOp::kAsin};
  }
  if (name == "$acos") {
    return UnaryOpSysFn{hir::UnaryOp::kAcos};
  }
  if (name == "$atan") {
    return UnaryOpSysFn{hir::UnaryOp::kAtan};
  }
  if (name == "$sinh") {
    return UnaryOpSysFn{hir::UnaryOp::kSinh};
  }
  if (name == "$cosh") {
    return UnaryOpSysFn{hir::UnaryOp::kCosh};
  }
  if (name == "$tanh") {
    return UnaryOpSysFn{hir::UnaryOp::kTanh};
  }
  if (name == "$asinh") {
    return UnaryOpSysFn{hir::UnaryOp::kAsinh};
  }
  if (name == "$acosh") {
    return UnaryOpSysFn{hir::UnaryOp::kAcosh};
  }
  if (name == "$atanh") {
    return UnaryOpSysFn{hir::UnaryOp::kAtanh};
  }
  if (name == "$clog2") {
    return UnaryOpSysFn{hir::UnaryOp::kClog2};
  }

  // Math binary functions -> BinaryOpSysFn
  if (name == "$pow") {
    return BinaryOpSysFn{hir::BinaryOp::kPower};
  }
  if (name == "$atan2") {
    return BinaryOpSysFn{hir::BinaryOp::kAtan2};
  }
  if (name == "$hypot") {
    return BinaryOpSysFn{hir::BinaryOp::kHypot};
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

constexpr int kDefaultTimeScalePower = -12;  // 1ps

auto TimeScaleValueToPower(const slang::TimeScaleValue& tsv) -> int {
  int base = 0;
  switch (tsv.unit) {
    case slang::TimeUnit::Seconds:
      base = 0;
      break;
    case slang::TimeUnit::Milliseconds:
      base = -3;
      break;
    case slang::TimeUnit::Microseconds:
      base = -6;
      break;
    case slang::TimeUnit::Nanoseconds:
      base = -9;
      break;
    case slang::TimeUnit::Picoseconds:
      base = -12;
      break;
    case slang::TimeUnit::Femtoseconds:
      base = -15;
      break;
  }

  int magnitude_offset = 0;
  switch (tsv.magnitude) {
    case slang::TimeScaleMagnitude::One:
      magnitude_offset = 0;
      break;
    case slang::TimeScaleMagnitude::Ten:
      magnitude_offset = 1;
      break;
    case slang::TimeScaleMagnitude::Hundred:
      magnitude_offset = 2;
      break;
  }

  return base + magnitude_offset;
}

auto ExtractPower(std::optional<slang::TimeScale> ts, TimeScaleSysFnKind kind)
    -> int {
  if (!ts) {
    return kDefaultTimeScalePower;
  }
  return TimeScaleValueToPower(
      kind == TimeScaleSysFnKind::kTimeunit ? ts->base : ts->precision);
}

void CollectMinPrecision(
    const slang::ast::InstanceSymbol& inst, int& min_power, bool& found) {
  auto ts = inst.body.getTimeScale();
  if (ts) {
    int prec = TimeScaleValueToPower(ts->precision);
    if (!found || prec < min_power) {
      min_power = prec;
      found = true;
    }
  }
  for (const auto& child :
       inst.body.membersOfType<slang::ast::InstanceSymbol>()) {
    CollectMinPrecision(child, min_power, found);
  }
}

auto ComputeGlobalPrecision(slang::ast::Compilation& comp) -> int {
  int min_power = 0;
  bool found = false;
  for (const auto* inst : comp.getRoot().topInstances) {
    CollectMinPrecision(*inst, min_power, found);
  }
  return found ? min_power : kDefaultTimeScalePower;
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
      Overloaded{
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

            int power = kDefaultTimeScalePower;
            if (call.arguments().empty()) {
              auto ts = sys_info.scope->getTimeScale();
              power = ExtractPower(ts, kind);
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
                power = ExtractPower(ts, kind);
              } else if (sym.kind == slang::ast::SymbolKind::Instance) {
                auto ts = sym.as<InstanceSymbol>().body.getTimeScale();
                power = ExtractPower(ts, kind);
              } else {
                ctx->ErrorFmt(
                    span, "{}() unexpected symbol kind in argument",
                    call.getSubroutineName());
                return hir::kInvalidExpressionId;
              }
            }

            return MakeIntConstant(power, span, ctx);
          }},
      classification);
}

}  // namespace lyra::lowering::ast_to_hir

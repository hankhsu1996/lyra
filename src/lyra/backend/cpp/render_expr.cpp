#include "lyra/backend/cpp/render_expr.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/backend/cpp/render_print.hpp"
#include "lyra/backend/cpp/string_literal.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderBinaryOp(
    mir::BinaryOp op, const std::string& lhs, const std::string& rhs)
    -> diag::Result<std::string> {
  std::string_view tok;
  switch (op) {
    case mir::BinaryOp::kAdd:
      tok = " + ";
      break;
    case mir::BinaryOp::kSub:
      tok = " - ";
      break;
    case mir::BinaryOp::kMul:
      tok = " * ";
      break;
    case mir::BinaryOp::kDiv:
      tok = " / ";
      break;
    case mir::BinaryOp::kMod:
      tok = " % ";
      break;
    case mir::BinaryOp::kBitwiseAnd:
      tok = " & ";
      break;
    case mir::BinaryOp::kBitwiseOr:
      tok = " | ";
      break;
    case mir::BinaryOp::kBitwiseXor:
      tok = " ^ ";
      break;
    case mir::BinaryOp::kEquality:
      tok = " == ";
      break;
    case mir::BinaryOp::kInequality:
      tok = " != ";
      break;
    case mir::BinaryOp::kLessThan:
      tok = " < ";
      break;
    case mir::BinaryOp::kLessEqual:
      tok = " <= ";
      break;
    case mir::BinaryOp::kGreaterThan:
      tok = " > ";
      break;
    case mir::BinaryOp::kGreaterEqual:
      tok = " >= ";
      break;
    case mir::BinaryOp::kLogicalAnd:
      return "(" + lhs + ").LogicalAnd(" + rhs + ")";
    case mir::BinaryOp::kLogicalOr:
      return "(" + lhs + ").LogicalOr(" + rhs + ")";
    case mir::BinaryOp::kBitwiseXnor:
      return "(" + lhs + ").BitwiseXnor(" + rhs + ")";
    case mir::BinaryOp::kPower:
      return "(" + lhs + ").Power(" + rhs + ")";
    case mir::BinaryOp::kShiftLeft:
      return "(" + lhs + ").ShiftLeft(" + rhs + ")";
    case mir::BinaryOp::kLogicalShiftRight:
      return "(" + lhs + ").LogicalShiftRight(" + rhs + ")";
    case mir::BinaryOp::kArithmeticShiftRight:
      return "(" + lhs + ").ArithmeticShiftRight(" + rhs + ")";
    case mir::BinaryOp::kCaseEquality:
    case mir::BinaryOp::kCaseInequality:
    case mir::BinaryOp::kWildcardEquality:
    case mir::BinaryOp::kWildcardInequality:
    case mir::BinaryOp::kLogicalImplication:
    case mir::BinaryOp::kLogicalEquivalence:
      return diag::Unsupported(
          diag::DiagCode::kCppEmitBinaryOpNotImplemented,
          "this binary operator is not yet implemented in cpp emit",
          diag::UnsupportedCategory::kFeature);
  }
  return "(" + lhs + std::string{tok} + rhs + ")";
}

auto RenderUnaryOp(mir::UnaryOp op, const std::string& operand)
    -> diag::Result<std::string> {
  switch (op) {
    case mir::UnaryOp::kPlus:
      return "(" + operand + ")";
    case mir::UnaryOp::kMinus:
      return "(-" + operand + ")";
    case mir::UnaryOp::kBitwiseNot:
      return "(~" + operand + ")";
    case mir::UnaryOp::kLogicalNot:
      return "(" + operand + ").LogicalNot()";
    case mir::UnaryOp::kReductionAnd:
      return "(" + operand + ").ReductionAnd()";
    case mir::UnaryOp::kReductionOr:
      return "(" + operand + ").ReductionOr()";
    case mir::UnaryOp::kReductionXor:
      return "(" + operand + ").ReductionXor()";
    case mir::UnaryOp::kReductionNand:
      return "(" + operand + ").ReductionNand()";
    case mir::UnaryOp::kReductionNor:
      return "(" + operand + ").ReductionNor()";
    case mir::UnaryOp::kReductionXnor:
      return "(" + operand + ").ReductionXnor()";
  }
  throw InternalError("RenderUnaryOp: unknown MIR UnaryOp");
}

auto LookupProceduralVarName(
    const RenderContext& ctx, const mir::ProceduralVarRef& ref)
    -> const std::string& {
  return ctx.ProceduralScopeAtHops(ref.hops).vars.at(ref.var.value).name;
}

auto RenderStructuralVarName(
    const RenderContext& ctx, const mir::StructuralVarRef& ref)
    -> diag::Result<std::string> {
  if (ref.hops.value != 0) {
    return diag::Unsupported(
        diag::DiagCode::kCppEmitExpressionFormNotImplemented,
        "cross-scope structural var access is not yet implemented in cpp "
        "emit",
        diag::UnsupportedCategory::kFeature);
  }
  return ctx.StructuralScope().GetStructuralVar(ref.var).name;
}

auto IntegralConstantToInt64(const mir::IntegralConstant& c) -> std::int64_t {
  if (c.width == 0U) {
    throw InternalError("IntegralConstantToInt64: zero width");
  }
  if (c.width > 64U) {
    throw InternalError(
        "IntegralConstantToInt64: width > 64 not yet implemented");
  }
  std::uint64_t raw = c.value_words.empty() ? 0U : c.value_words[0];
  // 4-state X/Z bits collapse to 0 in the numeric int64 form.
  if (!c.state_words.empty()) {
    raw &= ~c.state_words[0];
  }
  const std::uint64_t mask =
      c.width >= 64U ? ~std::uint64_t{0} : (std::uint64_t{1} << c.width) - 1U;
  const std::uint64_t masked = raw & mask;
  if (c.signedness == mir::Signedness::kSigned && c.width < 64U) {
    const std::uint64_t sign_bit = std::uint64_t{1} << (c.width - 1U);
    if ((masked & sign_bit) != 0U) {
      return static_cast<std::int64_t>(masked | ~mask);
    }
  }
  return static_cast<std::int64_t>(masked);
}

auto RenderWordList(std::span<const std::uint64_t> words, std::size_t n)
    -> std::string {
  std::string out = "{";
  for (std::size_t i = 0; i < n; ++i) {
    if (i != 0U) {
      out += ", ";
    }
    const std::uint64_t w = i < words.size() ? words[i] : std::uint64_t{0};
    out += std::format("0x{:x}ULL", w);
  }
  out += "}";
  return out;
}

auto RenderPackedArrayIntegerLiteral(
    const mir::PackedArrayType& pa, const mir::IntegralConstant& c)
    -> std::string {
  const bool is_four_state = pa.atom != mir::BitAtom::kBit;
  const char* signed_lit =
      pa.signedness == mir::Signedness::kSigned ? "true" : "false";
  const char* four_state_lit = is_four_state ? "true" : "false";

  // Narrow + no X/Z fits a single int64 carrier and reads better as
  // PackedArray::Int / FromInt. Wide literals or X/Z-bearing literals must
  // round-trip through explicit value/unknown word planes via FromWords.
  const bool literal_has_xz = !c.state_words.empty() && c.state_words[0] != 0U;
  const bool needs_word_planes = c.width > 64U || literal_has_xz;

  if (!needs_word_planes) {
    const auto value = IntegralConstantToInt64(c);
    if (pa.BitWidth() == 32U && pa.signedness == mir::Signedness::kSigned &&
        pa.atom == mir::BitAtom::kBit) {
      return std::format("lyra::value::PackedArray::Int({})", value);
    }
    return std::format(
        "lyra::value::PackedArray::FromInt({}LL, {}, {}, {})", value,
        pa.BitWidth(), signed_lit, four_state_lit);
  }

  if (literal_has_xz && !is_four_state) {
    throw InternalError(
        "RenderPackedArrayIntegerLiteral: 2-state PackedArray cannot hold "
        "X/Z literal");
  }
  const std::size_t n = (pa.BitWidth() + 63U) / 64U;
  const std::string value_init = RenderWordList(c.value_words, n);
  const std::string unknown_init =
      is_four_state ? RenderWordList(c.state_words, n) : std::string{"{}"};
  return std::format(
      "lyra::value::PackedArray::FromWords({}, {}, {}, {}, {})", value_init,
      unknown_init, pa.BitWidth(), signed_lit, four_state_lit);
}

}  // namespace

auto RenderLvalue(const RenderContext& ctx, const mir::Lvalue& target)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::StructuralVarRef& m) -> diag::Result<std::string> {
            return RenderStructuralVarName(ctx, m);
          },
          [&](const mir::ProceduralVarRef& l) -> diag::Result<std::string> {
            return LookupProceduralVarName(ctx, l);
          },
      },
      target);
}

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::IntegerLiteral& lit) -> diag::Result<std::string> {
            const auto& ty = ctx.Unit().GetType(expr.type);
            if (!ty.IsPackedArray()) {
              throw InternalError(
                  "RenderExpr: IntegerLiteral not typed as "
                  "PackedArrayType");
            }
            return RenderPackedArrayIntegerLiteral(
                ty.AsPackedArray(), lit.value);
          },
          [&](const mir::StringLiteral& s) -> diag::Result<std::string> {
            return RenderStdStringLiteral(s.value);
          },
          [](const mir::TimeLiteral&) -> diag::Result<std::string> {
            return diag::Unsupported(
                diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                "TimeLiteral is not yet implemented in cpp emit",
                diag::UnsupportedCategory::kFeature);
          },
          [&](const mir::StructuralParamRef& r) -> diag::Result<std::string> {
            if (r.hops.value != 0) {
              return diag::Unsupported(
                  diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                  "cross-scope structural parameter access is not yet "
                  "implemented in cpp emit",
                  diag::UnsupportedCategory::kFeature);
            }
            return ctx.StructuralScope().GetStructuralParam(r.param).name;
          },
          [&](const mir::StructuralVarRef& m) -> diag::Result<std::string> {
            return RenderStructuralVarName(ctx, m);
          },
          [&](const mir::ProceduralVarRef& l) -> diag::Result<std::string> {
            return LookupProceduralVarName(ctx, l);
          },
          [&](const mir::UnaryExpr& u) -> diag::Result<std::string> {
            auto operand_or = RenderExpr(ctx, ctx.Expr(u.operand));
            if (!operand_or) {
              return std::unexpected(std::move(operand_or.error()));
            }
            return RenderUnaryOp(u.op, *operand_or);
          },
          [&](const mir::BinaryExpr& b) -> diag::Result<std::string> {
            auto lhs_or = RenderExpr(ctx, ctx.Expr(b.lhs));
            if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
            auto rhs_or = RenderExpr(ctx, ctx.Expr(b.rhs));
            if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
            return RenderBinaryOp(b.op, *lhs_or, *rhs_or);
          },
          [&](const mir::ConditionalExpr& c) -> diag::Result<std::string> {
            auto cond_or = RenderConditionAsBool(ctx, ctx.Expr(c.condition));
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            auto then_or = RenderExpr(ctx, ctx.Expr(c.then_value));
            if (!then_or) return std::unexpected(std::move(then_or.error()));
            auto else_or = RenderExpr(ctx, ctx.Expr(c.else_value));
            if (!else_or) return std::unexpected(std::move(else_or.error()));
            return "(" + *cond_or + " ? " + *then_or + " : " + *else_or + ")";
          },
          [&](const mir::AssignExpr& a) -> diag::Result<std::string> {
            auto target_or = RenderLvalue(ctx, a.target);
            if (!target_or) {
              return std::unexpected(std::move(target_or.error()));
            }
            auto value_or = RenderExpr(ctx, ctx.Expr(a.value));
            if (!value_or) return std::unexpected(std::move(value_or.error()));
            return "(" + *target_or + " = " + *value_or + ")";
          },
          [&](const mir::ConversionExpr& cv) -> diag::Result<std::string> {
            const auto& src_expr = ctx.Expr(cv.operand);
            auto operand_or = RenderExpr(ctx, src_expr);
            if (!operand_or) {
              return std::unexpected(std::move(operand_or.error()));
            }
            const auto& src_ty = ctx.Unit().GetType(src_expr.type);
            const auto& dst_ty = ctx.Unit().GetType(expr.type);
            // Same-shape conversion (slang's spurious promotions): pass
            // through. Cross-shape: route through PackedArray::ConvertFrom.
            if (src_ty.IsPackedArray() && dst_ty.IsPackedArray()) {
              const auto& src_pa = src_ty.AsPackedArray();
              const auto& dst_pa = dst_ty.AsPackedArray();
              if (src_pa.BitWidth() == dst_pa.BitWidth() &&
                  src_pa.signedness == dst_pa.signedness &&
                  src_pa.atom == dst_pa.atom) {
                return *operand_or;
              }
              const char* signed_lit =
                  dst_pa.signedness == mir::Signedness::kSigned ? "true"
                                                                : "false";
              const char* four_state_lit =
                  dst_pa.atom != mir::BitAtom::kBit ? "true" : "false";
              return std::format(
                  "lyra::value::PackedArray::ConvertFrom({}, {}, {}, {})",
                  *operand_or, dst_pa.BitWidth(), signed_lit, four_state_lit);
            }
            return *operand_or;
          },
          [](const mir::CallExpr&) -> diag::Result<std::string> {
            return diag::Unsupported(
                diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                "user subroutine call is not yet implemented in cpp emit",
                diag::UnsupportedCategory::kFeature);
          },
          [&](const mir::RuntimeCallExpr& rc) -> diag::Result<std::string> {
            return RenderRuntimeCallExpr(ctx, rc);
          },
          [](const mir::ClosureExpr&) -> diag::Result<std::string> {
            return diag::Unsupported(
                diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                "ClosureExpr is not yet implemented in cpp emit",
                diag::UnsupportedCategory::kFeature);
          },
      },
      expr.data);
}

auto RenderConditionAsBool(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  auto text_or = RenderExpr(ctx, expr);
  if (!text_or) return std::unexpected(std::move(text_or.error()));
  const auto& ty = ctx.Unit().GetType(expr.type);
  if (ty.IsPackedArray()) {
    return "(" + *text_or + ").IsTruthy()";
  }
  return *text_or;
}

}  // namespace lyra::backend::cpp

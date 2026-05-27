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
#include "lyra/backend/cpp/render_stmt.hpp"
#include "lyra/backend/cpp/render_type.hpp"
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
    case mir::BinaryOp::kLogicalImplication:
      return "(" + lhs + ").LogicalImplication(" + rhs + ")";
    case mir::BinaryOp::kLogicalEquivalence:
      return "(" + lhs + ").LogicalEquivalence(" + rhs + ")";
    case mir::BinaryOp::kWildcardEquality:
      return "(" + lhs + ").WildcardEquals(" + rhs + ")";
    case mir::BinaryOp::kWildcardInequality:
      return "(" + lhs + ").WildcardEquals(" + rhs + ").LogicalNot()";
    case mir::BinaryOp::kCaseEquality:
      return "(" + lhs + ").CaseEqual(" + rhs + ")";
    case mir::BinaryOp::kCaseInequality:
      return "(" + lhs + ").CaseEqual(" + rhs + ").LogicalNot()";
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

auto RenderIntegerLiteralExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::IntegerLiteral& lit) -> diag::Result<std::string> {
  const auto& ty = ctx.Unit().GetType(expr.type);
  if (!ty.IsPackedArray()) {
    throw InternalError(
        "RenderExpr: IntegerLiteral not typed as PackedArrayType");
  }
  return RenderPackedArrayIntegerLiteral(ty.AsPackedArray(), lit.value);
}

auto RenderStructuralParamExpr(
    const RenderContext& ctx, const mir::StructuralParamRef& r)
    -> diag::Result<std::string> {
  if (r.hops.value != 0) {
    return diag::Unsupported(
        diag::DiagCode::kCppEmitExpressionFormNotImplemented,
        "cross-scope structural parameter access is not yet implemented in "
        "cpp emit",
        diag::UnsupportedCategory::kFeature);
  }
  return ctx.StructuralScope().GetStructuralParam(r.param).name;
}

}  // namespace

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

namespace {

auto RenderClosureExpr(
    const RenderContext& ctx, const mir::ClosureExpr& closure)
    -> diag::Result<std::string>;

auto RenderUnaryExpr(const RenderContext& ctx, const mir::UnaryExpr& u)
    -> diag::Result<std::string> {
  auto operand_or = RenderExpr(ctx, ctx.Expr(u.operand));
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  return RenderUnaryOp(u.op, *operand_or);
}

auto RenderBinaryExprNode(const RenderContext& ctx, const mir::BinaryExpr& b)
    -> diag::Result<std::string> {
  auto lhs_or = RenderExpr(ctx, ctx.Expr(b.lhs));
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  auto rhs_or = RenderExpr(ctx, ctx.Expr(b.rhs));
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  return RenderBinaryOp(b.op, *lhs_or, *rhs_or);
}

auto RenderConditionalExprNode(
    const RenderContext& ctx, const mir::ConditionalExpr& c)
    -> diag::Result<std::string> {
  auto cond_or = RenderConditionAsBool(ctx, ctx.Expr(c.condition));
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  auto then_or = RenderExpr(ctx, ctx.Expr(c.then_value));
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  auto else_or = RenderExpr(ctx, ctx.Expr(c.else_value));
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  return "(" + *cond_or + " ? " + *then_or + " : " + *else_or + ")";
}

auto RenderAssignExprNode(const RenderContext& ctx, const mir::AssignExpr& a)
    -> diag::Result<std::string> {
  auto target_or = RenderLvalue(ctx, a.target);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  auto value_or = RenderExpr(ctx, ctx.Expr(a.value));
  if (!value_or) return std::unexpected(std::move(value_or.error()));
  if (const auto* svr = std::get_if<mir::StructuralVarRef>(&a.target)) {
    const auto& var_decl = ctx.StructuralScope().GetStructuralVar(svr->var);
    if (IsObservableScalarType(ctx.Unit().GetType(var_decl.type))) {
      return "lyra::runtime::WriteVar(*services_, " + *target_or + ", " +
             *value_or + ")";
    }
  }
  return "(" + *target_or + " = " + *value_or + ")";
}

auto RenderConversionExprNode(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::ConversionExpr& cv) -> diag::Result<std::string> {
  const auto& src_expr = ctx.Expr(cv.operand);
  auto operand_or = RenderExpr(ctx, src_expr);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const auto& src_ty = ctx.Unit().GetType(src_expr.type);
  const auto& dst_ty = ctx.Unit().GetType(expr.type);
  // Same-shape conversion (slang's spurious promotions): pass through.
  // Cross-shape: route through PackedArray::ConvertFrom.
  if (src_ty.IsPackedArray() && dst_ty.IsPackedArray()) {
    const auto& src_pa = src_ty.AsPackedArray();
    const auto& dst_pa = dst_ty.AsPackedArray();
    if (src_pa.BitWidth() == dst_pa.BitWidth() &&
        src_pa.signedness == dst_pa.signedness && src_pa.atom == dst_pa.atom) {
      return *operand_or;
    }
    const char* signed_lit =
        dst_pa.signedness == mir::Signedness::kSigned ? "true" : "false";
    const char* four_state_lit =
        dst_pa.atom != mir::BitAtom::kBit ? "true" : "false";
    return std::format(
        "lyra::value::PackedArray::ConvertFrom({}, {}, {}, {})", *operand_or,
        dst_pa.BitWidth(), signed_lit, four_state_lit);
  }
  return *operand_or;
}

}  // namespace

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::IntegerLiteral& lit) -> diag::Result<std::string> {
            return RenderIntegerLiteralExpr(ctx, expr, lit);
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
            return RenderStructuralParamExpr(ctx, r);
          },
          [&](const mir::StructuralVarRef& m) -> diag::Result<std::string> {
            auto name_or = RenderStructuralVarName(ctx, m);
            if (!name_or) return std::unexpected(std::move(name_or.error()));
            const auto& ty = ctx.Unit().GetType(expr.type);
            if (ty.IsPackedArray()) {
              return *name_or + ".Get()";
            }
            return *name_or;
          },
          [&](const mir::ProceduralVarRef& l) -> diag::Result<std::string> {
            return LookupProceduralVarName(ctx, l);
          },
          [&](const mir::UnaryExpr& u) -> diag::Result<std::string> {
            return RenderUnaryExpr(ctx, u);
          },
          [&](const mir::BinaryExpr& b) -> diag::Result<std::string> {
            return RenderBinaryExprNode(ctx, b);
          },
          [&](const mir::ConditionalExpr& c) -> diag::Result<std::string> {
            return RenderConditionalExprNode(ctx, c);
          },
          [&](const mir::AssignExpr& a) -> diag::Result<std::string> {
            return RenderAssignExprNode(ctx, a);
          },
          [&](const mir::ConversionExpr& cv) -> diag::Result<std::string> {
            return RenderConversionExprNode(ctx, expr, cv);
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
          [&](const mir::ClosureExpr& cl) -> diag::Result<std::string> {
            return RenderClosureExpr(ctx, cl);
          },
          [&](const mir::ElementSelectExpr& sel) -> diag::Result<std::string> {
            auto base_or = RenderExpr(ctx, ctx.Expr(sel.base_value));
            if (!base_or) return std::unexpected(std::move(base_or.error()));
            auto idx_or = RenderExpr(ctx, ctx.Expr(sel.index));
            if (!idx_or) return std::unexpected(std::move(idx_or.error()));
            return std::format("({}).Select({}, 1)", *base_or, *idx_or);
          },
      },
      expr.data);
}

namespace {

auto RenderClosureExpr(
    const RenderContext& ctx, const mir::ClosureExpr& closure)
    -> diag::Result<std::string> {
  if (closure.body == nullptr) {
    throw InternalError("RenderClosureExpr: closure has no body");
  }

  // Always capture `this` so the closure body can reach module-scope members
  // (the emitted module class's services_ pointer, structural vars, the
  // SubmitObserved / DrainObserved interface on the Module base). The
  // explicit by-value captures from the MIR list go after.
  std::string captures_text = "this";
  for (std::size_t i = 0; i < closure.captures.size(); ++i) {
    captures_text += ", ";
    auto rendered_or = std::visit(
        Overloaded{
            [&](const mir::ByValueCapture& bv) -> diag::Result<std::string> {
              const std::string& bind_name =
                  closure.body->vars.at(bv.binding.value).name;
              auto value_or = RenderExpr(ctx, ctx.Expr(bv.value));
              if (!value_or) {
                return std::unexpected(std::move(value_or.error()));
              }
              return bind_name + " = " + *value_or;
            },
            [](const mir::ByReferenceCapture&) -> diag::Result<std::string> {
              return diag::Unsupported(
                  diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                  "by-reference capture is not yet implemented in cpp emit",
                  diag::UnsupportedCategory::kFeature);
            }},
        closure.captures[i]);
    if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
    captures_text += *rendered_or;
  }

  const RenderContext body_ctx =
      RenderContext::ForRoot(ctx.Unit(), ctx.StructuralScope(), *closure.body);
  auto body_or = RenderProceduralScopeStatements(body_ctx, 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));

  return "[" + captures_text + "]() {\n" + *body_or + "}";
}

}  // namespace

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

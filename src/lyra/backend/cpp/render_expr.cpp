#include "lyra/backend/cpp/render_expr.hpp"

#include <cstdint>
#include <format>
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

namespace lyra::backend::cpp {

namespace {

auto BinaryOpToken(mir::BinaryOp op) -> diag::Result<std::string_view> {
  switch (op) {
    case mir::BinaryOp::kAdd:
      return std::string_view{" + "};
    case mir::BinaryOp::kLessThan:
      return std::string_view{" < "};
    case mir::BinaryOp::kSub:
    case mir::BinaryOp::kMul:
    case mir::BinaryOp::kDiv:
    case mir::BinaryOp::kMod:
    case mir::BinaryOp::kPower:
    case mir::BinaryOp::kBitwiseAnd:
    case mir::BinaryOp::kBitwiseOr:
    case mir::BinaryOp::kBitwiseXor:
    case mir::BinaryOp::kBitwiseXnor:
    case mir::BinaryOp::kEquality:
    case mir::BinaryOp::kInequality:
    case mir::BinaryOp::kCaseEquality:
    case mir::BinaryOp::kCaseInequality:
    case mir::BinaryOp::kWildcardEquality:
    case mir::BinaryOp::kWildcardInequality:
    case mir::BinaryOp::kGreaterEqual:
    case mir::BinaryOp::kGreaterThan:
    case mir::BinaryOp::kLessEqual:
    case mir::BinaryOp::kLogicalAnd:
    case mir::BinaryOp::kLogicalOr:
    case mir::BinaryOp::kLogicalImplication:
    case mir::BinaryOp::kLogicalEquivalence:
    case mir::BinaryOp::kShiftLeft:
    case mir::BinaryOp::kLogicalShiftRight:
    case mir::BinaryOp::kArithmeticShiftRight:
      return diag::Unsupported(
          diag::DiagCode::kCppEmitBinaryOpNotImplemented,
          "this binary operator is not yet implemented in cpp emit",
          diag::UnsupportedCategory::kFeature);
  }
  throw InternalError("BinaryOpToken: unknown MIR BinaryOp");
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

auto IsPackedRuntime(const mir::CompilationUnit& unit, mir::TypeId type_id)
    -> bool {
  const auto& t = unit.GetType(type_id);
  return t.IsPackedArray() &&
         t.AsPackedArray().form == mir::PackedArrayForm::kExplicit;
}

auto MirTypeOfLvalue(const RenderContext& ctx, const mir::Lvalue& lv)
    -> diag::Result<mir::TypeId> {
  return std::visit(
      Overloaded{
          [&](const mir::StructuralVarRef& m) -> diag::Result<mir::TypeId> {
            if (m.hops.value != 0) {
              return diag::Unsupported(
                  diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                  "cross-scope structural var access is not yet implemented "
                  "in cpp emit",
                  diag::UnsupportedCategory::kFeature);
            }
            return ctx.StructuralScope().GetStructuralVar(m.var).type;
          },
          [&](const mir::ProceduralVarRef& l) -> diag::Result<mir::TypeId> {
            return ctx.ProceduralScopeAtHops(l.hops).vars.at(l.var.value).type;
          },
      },
      lv);
}

auto IntegralConstantToInt32(const mir::IntegralConstant& c) -> std::int32_t {
  if (c.state_kind == mir::IntegralStateKind::kFourState) {
    throw InternalError(
        "IntegralConstantToInt32: 4-state literal targeting native int");
  }
  if (c.width == 0U || c.width > 32U) {
    throw InternalError(
        "IntegralConstantToInt32: invalid native int literal width");
  }
  if (c.value_words.empty()) {
    throw InternalError(
        "IntegralConstantToInt32: integral constant has no value word");
  }
  const std::uint64_t raw = c.value_words[0];
  if (c.signedness == mir::Signedness::kSigned && c.width < 32U) {
    const std::uint32_t sign_bit = 1U << (c.width - 1U);
    const std::uint32_t low =
        static_cast<std::uint32_t>(raw) & ((std::uint32_t{1} << c.width) - 1U);
    if ((low & sign_bit) != 0U) {
      const std::uint32_t fill = ~((1U << c.width) - 1U);
      return static_cast<std::int32_t>(low | fill);
    }
    return static_cast<std::int32_t>(low);
  }
  return static_cast<std::int32_t>(static_cast<std::uint32_t>(raw));
}

auto RenderNativeIntegerLiteral(const mir::IntegralConstant& c) -> std::string {
  return std::format("{}", IntegralConstantToInt32(c));
}

auto PackedRuntimeUnsupported() -> diag::Result<std::string> {
  return diag::Unsupported(
      diag::DiagCode::kCppEmitPackedRuntimeNotSupported,
      "packed runtime storage is not yet supported in cpp emit",
      diag::UnsupportedCategory::kFeature);
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

auto RenderExprAsNative(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::IntegerLiteral& lit) -> diag::Result<std::string> {
            return RenderNativeIntegerLiteral(lit.value);
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
          [](const mir::UnaryExpr&) -> diag::Result<std::string> {
            return diag::Unsupported(
                diag::DiagCode::kCppEmitUnaryOpNotImplemented,
                "this unary operator is not yet implemented in cpp emit",
                diag::UnsupportedCategory::kFeature);
          },
          [&](const mir::BinaryExpr& b) -> diag::Result<std::string> {
            auto token_or = BinaryOpToken(b.op);
            if (!token_or) {
              return std::unexpected(std::move(token_or.error()));
            }
            auto lhs_or = RenderExprAsNative(ctx, ctx.Expr(b.lhs));
            if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
            auto rhs_or = RenderExprAsNative(ctx, ctx.Expr(b.rhs));
            if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
            return "(" + *lhs_or + std::string{*token_or} + *rhs_or + ")";
          },
          [&](const mir::AssignExpr& a) -> diag::Result<std::string> {
            auto target_type_or = MirTypeOfLvalue(ctx, a.target);
            if (!target_type_or) {
              return std::unexpected(std::move(target_type_or.error()));
            }
            const auto target_type = *target_type_or;
            auto target_or = RenderLvalue(ctx, a.target);
            if (!target_or) {
              return std::unexpected(std::move(target_or.error()));
            }
            if (IsPackedRuntime(ctx.Unit(), target_type)) {
              return PackedRuntimeUnsupported();
            }
            auto value_or = RenderExprAsNative(ctx, ctx.Expr(a.value));
            if (!value_or) return std::unexpected(std::move(value_or.error()));
            return "(" + *target_or + " = " + *value_or + ")";
          },
          [&](const mir::ConversionExpr& cv) -> diag::Result<std::string> {
            return RenderExprAsNative(ctx, ctx.Expr(cv.operand));
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
      },
      expr.data);
}

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  if (IsPackedRuntime(ctx.Unit(), expr.type)) {
    return PackedRuntimeUnsupported();
  }
  return RenderExprAsNative(ctx, expr);
}

}  // namespace lyra::backend::cpp

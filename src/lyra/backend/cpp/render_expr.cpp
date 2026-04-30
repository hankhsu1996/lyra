#include "lyra/backend/cpp/render_expr.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/backend/cpp/render_print.hpp"
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

auto LookupLocalName(const mir::Body& body, const mir::LocalVarRef& ref)
    -> const std::string& {
  return body.local_scopes.at(ref.scope.value).locals.at(ref.local.value).name;
}

auto SignednessLiteral(mir::Signedness s) -> std::string_view {
  switch (s) {
    case mir::Signedness::kSigned:
      return "lyra::runtime::Signedness::kSigned";
    case mir::Signedness::kUnsigned:
      return "lyra::runtime::Signedness::kUnsigned";
  }
  throw InternalError("SignednessLiteral: unknown MIR Signedness");
}

auto IsPackedRuntime(const mir::CompilationUnit& unit, mir::TypeId type_id)
    -> bool {
  const auto& t = unit.GetType(type_id);
  return t.IsPackedArray() &&
         t.AsPackedArray().form == mir::PackedArrayForm::kExplicit;
}

auto MirTypeOfLvalue(const RenderContext& ctx, const mir::Lvalue& lv)
    -> mir::TypeId {
  return std::visit(
      Overloaded{
          [&](const mir::MemberVarRef& m) -> mir::TypeId {
            return ctx.Class().GetMemberVar(m.target).type;
          },
          [&](const mir::LocalVarRef& l) -> mir::TypeId {
            return ctx.Body()
                .local_scopes.at(l.scope.value)
                .locals.at(l.local.value)
                .type;
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

auto RenderIntegerLiteralAsView(
    const mir::CompilationUnit& unit, mir::TypeId type_id,
    const mir::IntegralConstant& c) -> std::string {
  const auto& pa = unit.GetType(type_id).AsPackedArray();
  const bool target_4state = pa.IsFourState();
  const std::uint64_t target_width = pa.BitWidth();

  if (c.width != static_cast<std::uint32_t>(target_width)) {
    throw InternalError(
        "RenderIntegerLiteralAsView: literal width does not match target "
        "type width");
  }
  if (!target_4state && c.state_kind == mir::IntegralStateKind::kFourState) {
    throw InternalError(
        "RenderIntegerLiteralAsView: 4-state literal targeting 2-state type");
  }

  const std::size_t target_words =
      (static_cast<std::size_t>(target_width) + 63U) / 64U;

  std::string value_init;
  for (std::size_t i = 0; i < target_words; ++i) {
    if (i != 0) value_init += ", ";
    const std::uint64_t w = i < c.value_words.size() ? c.value_words[i] : 0U;
    value_init += std::format("0x{:x}ULL", w);
  }

  std::string out = "([&]() -> lyra::runtime::";
  out += target_4state ? "ConstLogicView" : "ConstBitView";
  out += " {\n";
  out += std::format(
      "  static constexpr std::array<std::uint64_t, {}> kValueWords = "
      "{{{}}};\n",
      target_words, value_init);
  if (target_4state) {
    std::string state_init;
    for (std::size_t i = 0; i < target_words; ++i) {
      if (i != 0) state_init += ", ";
      const std::uint64_t w =
          (c.state_kind == mir::IntegralStateKind::kFourState &&
           i < c.state_words.size())
              ? c.state_words[i]
              : 0U;
      state_init += std::format("0x{:x}ULL", w);
    }
    out += std::format(
        "  static constexpr std::array<std::uint64_t, {}> kStateWords = "
        "{{{}}};\n",
        target_words, state_init);
    out += std::format(
        "  return lyra::runtime::ConstLogicView{{kValueWords, kStateWords, 0, "
        "{}}};\n",
        target_width);
  } else {
    out += std::format(
        "  return lyra::runtime::ConstBitView{{kValueWords, 0, {}}};\n",
        target_width);
  }
  out += "}())";
  return out;
}

auto IsPackedExplicitMatching(
    const mir::CompilationUnit& unit, mir::TypeId operand_type,
    const mir::PackedArrayType& result_pa) -> bool {
  const auto& ot = unit.GetType(operand_type);
  if (!ot.IsPackedArray()) {
    return false;
  }
  const auto& opa = ot.AsPackedArray();
  return opa.form == mir::PackedArrayForm::kExplicit &&
         opa.IsFourState() == result_pa.IsFourState() &&
         opa.BitWidth() == result_pa.BitWidth();
}

auto IsPackedRuntimeUnaryOp(mir::UnaryOp op) -> bool {
  switch (op) {
    case mir::UnaryOp::kBitwiseNot:
    case mir::UnaryOp::kReductionAnd:
    case mir::UnaryOp::kReductionOr:
    case mir::UnaryOp::kReductionXor:
    case mir::UnaryOp::kReductionNand:
    case mir::UnaryOp::kReductionNor:
    case mir::UnaryOp::kReductionXnor:
      return true;
    case mir::UnaryOp::kPlus:
    case mir::UnaryOp::kMinus:
    case mir::UnaryOp::kLogicalNot:
      return false;
  }
  throw InternalError("IsPackedRuntimeUnaryOp: unknown MIR UnaryOp");
}

auto IsReductionUnaryOp(mir::UnaryOp op) -> bool {
  switch (op) {
    case mir::UnaryOp::kReductionAnd:
    case mir::UnaryOp::kReductionOr:
    case mir::UnaryOp::kReductionXor:
    case mir::UnaryOp::kReductionNand:
    case mir::UnaryOp::kReductionNor:
    case mir::UnaryOp::kReductionXnor:
      return true;
    case mir::UnaryOp::kBitwiseNot:
    case mir::UnaryOp::kPlus:
    case mir::UnaryOp::kMinus:
    case mir::UnaryOp::kLogicalNot:
      return false;
  }
  throw InternalError("IsReductionUnaryOp: unknown MIR UnaryOp");
}

auto ReductionRuntimeFunctionName(mir::UnaryOp op) -> std::string_view {
  switch (op) {
    case mir::UnaryOp::kReductionAnd:
      return "lyra::runtime::ReductionAnd";
    case mir::UnaryOp::kReductionOr:
      return "lyra::runtime::ReductionOr";
    case mir::UnaryOp::kReductionXor:
      return "lyra::runtime::ReductionXor";
    case mir::UnaryOp::kReductionNand:
      return "lyra::runtime::ReductionNand";
    case mir::UnaryOp::kReductionNor:
      return "lyra::runtime::ReductionNor";
    case mir::UnaryOp::kReductionXnor:
      return "lyra::runtime::ReductionXnor";
    case mir::UnaryOp::kBitwiseNot:
    case mir::UnaryOp::kPlus:
    case mir::UnaryOp::kMinus:
    case mir::UnaryOp::kLogicalNot:
      throw InternalError(
          "ReductionRuntimeFunctionName: not a reduction unary op");
  }
  throw InternalError("ReductionRuntimeFunctionName: unknown MIR UnaryOp");
}

auto IsPackedRuntimeReductionShape(
    const mir::CompilationUnit& unit, mir::TypeId operand_type,
    const mir::PackedArrayType& result_pa) -> bool {
  if (!result_pa.dims.empty()) {
    return false;
  }
  if (result_pa.BitWidth() != 1U) {
    return false;
  }
  const auto& ot = unit.GetType(operand_type);
  if (!ot.IsPackedArray()) {
    return false;
  }
  const auto& opa = ot.AsPackedArray();
  if (opa.form != mir::PackedArrayForm::kExplicit) {
    return false;
  }
  return opa.IsFourState() == result_pa.IsFourState();
}

auto RenderPackedRuntimeUnaryCall(
    const RenderContext& ctx, mir::TypeId result_type, mir::UnaryOp op,
    const mir::Expr& operand) -> diag::Result<std::string> {
  if (!IsPackedRuntimeUnaryOp(op)) {
    throw InternalError(
        "RenderPackedRuntimeUnaryCall: not a packed runtime unary op");
  }
  if (!IsPackedRuntime(ctx.Unit(), operand.type)) {
    throw InternalError(
        "RenderPackedRuntimeUnaryCall: operand not packed runtime");
  }
  if (!IsPackedRuntime(ctx.Unit(), result_type)) {
    throw InternalError(
        "RenderPackedRuntimeUnaryCall: result not packed runtime");
  }
  const auto& result_pa = ctx.Unit().GetType(result_type).AsPackedArray();

  if (IsReductionUnaryOp(op)) {
    if (!IsPackedRuntimeReductionShape(ctx.Unit(), operand.type, result_pa)) {
      throw InternalError(
          "RenderPackedRuntimeUnaryCall: reduction result must be a scalar "
          "1-bit packed type whose state-kind matches the operand");
    }
    auto operand_view_or = RenderExprAsRuntimeView(ctx, operand);
    if (!operand_view_or) {
      return std::unexpected(std::move(operand_view_or.error()));
    }
    return std::format(
        "{}({})", ReductionRuntimeFunctionName(op), *operand_view_or);
  }

  // Bitwise-not: operand and result must share the same explicit packed shape.
  if (!IsPackedExplicitMatching(ctx.Unit(), operand.type, result_pa)) {
    throw InternalError(
        "RenderPackedRuntimeUnaryCall: operand type not normalized to result");
  }
  const std::string shape = RenderPackedShapeLiteral(result_pa.dims);
  const std::string_view signedness = SignednessLiteral(result_pa.signedness);
  auto operand_view_or = RenderExprAsRuntimeView(ctx, operand);
  if (!operand_view_or) {
    return std::unexpected(std::move(operand_view_or.error()));
  }
  return std::format(
      "lyra::runtime::BitwiseNot<{}, {}>({})", shape, signedness,
      *operand_view_or);
}

auto RenderPackedBitwiseBinaryCall(
    const RenderContext& ctx, mir::TypeId result_type, mir::BinaryOp op,
    const mir::Expr& lhs, const mir::Expr& rhs) -> diag::Result<std::string> {
  const char* fn = nullptr;
  switch (op) {
    case mir::BinaryOp::kBitwiseAnd:
      fn = "BitwiseAnd";
      break;
    case mir::BinaryOp::kBitwiseOr:
      fn = "BitwiseOr";
      break;
    case mir::BinaryOp::kBitwiseXor:
      fn = "BitwiseXor";
      break;
    case mir::BinaryOp::kBitwiseXnor:
      fn = "BitwiseXnor";
      break;
    default:
      throw InternalError(
          "RenderPackedBitwiseBinaryCall: non-bitwise binary op");
  }
  if (!IsPackedRuntime(ctx.Unit(), result_type)) {
    throw InternalError(
        "RenderPackedBitwiseBinaryCall: result not packed runtime");
  }
  const auto& result_pa = ctx.Unit().GetType(result_type).AsPackedArray();
  if (!IsPackedExplicitMatching(ctx.Unit(), lhs.type, result_pa)) {
    throw InternalError(
        "RenderPackedBitwiseBinaryCall: lhs not normalized to result");
  }
  if (!IsPackedExplicitMatching(ctx.Unit(), rhs.type, result_pa)) {
    throw InternalError(
        "RenderPackedBitwiseBinaryCall: rhs not normalized to result");
  }
  const std::string shape = RenderPackedShapeLiteral(result_pa.dims);
  const std::string_view signedness = SignednessLiteral(result_pa.signedness);
  auto lhs_view_or = RenderExprAsRuntimeView(ctx, lhs);
  if (!lhs_view_or) {
    return std::unexpected(std::move(lhs_view_or.error()));
  }
  auto rhs_view_or = RenderExprAsRuntimeView(ctx, rhs);
  if (!rhs_view_or) {
    return std::unexpected(std::move(rhs_view_or.error()));
  }
  return std::format(
      "lyra::runtime::{}<{}, {}>({}, {})", fn, shape, signedness, *lhs_view_or,
      *rhs_view_or);
}

auto RenderConversionCall(
    const RenderContext& ctx, mir::TypeId target_type,
    const mir::ConversionExpr& conv) -> diag::Result<std::string> {
  const auto& target = ctx.Unit().GetType(target_type).AsPackedArray();
  const auto& operand_expr = ctx.Expr(conv.operand);
  const auto& source_type = ctx.Unit().GetType(operand_expr.type);
  if (!source_type.IsPackedArray() ||
      source_type.AsPackedArray().form != mir::PackedArrayForm::kExplicit) {
    throw InternalError(
        "RenderConversionCall: ConversionExpr operand must be a packed "
        "explicit type");
  }
  const auto& source = source_type.AsPackedArray();

  auto operand_view_or = RenderExprAsRuntimeView(ctx, operand_expr);
  if (!operand_view_or) {
    return std::unexpected(std::move(operand_view_or.error()));
  }
  const std::string target_shape = RenderPackedShapeLiteral(target.dims);

  const char* fn = target.IsFourState() ? "lyra::runtime::ConvertToLogic"
                                        : "lyra::runtime::ConvertToBit";
  return std::format(
      "{}<{}, {}>({}, {})", fn, target_shape,
      SignednessLiteral(target.signedness), *operand_view_or,
      SignednessLiteral(source.signedness));
}

}  // namespace

auto RenderLvalue(const RenderContext& ctx, const mir::Lvalue& target)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::MemberVarRef& m) -> std::string {
            return ctx.Class().GetMemberVar(m.target).name;
          },
          [&](const mir::LocalVarRef& l) -> std::string {
            return LookupLocalName(ctx.Body(), l);
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
          [&](const mir::MemberVarRef& m) -> diag::Result<std::string> {
            return ctx.Class().GetMemberVar(m.target).name;
          },
          [&](const mir::LocalVarRef& l) -> diag::Result<std::string> {
            return LookupLocalName(ctx.Body(), l);
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
            const auto target_type = MirTypeOfLvalue(ctx, a.target);
            if (IsPackedRuntime(ctx.Unit(), target_type)) {
              auto value_or = RenderExprAsRuntimeView(ctx, ctx.Expr(a.value));
              if (!value_or) {
                return std::unexpected(std::move(value_or.error()));
              }
              return std::format(
                  "{}.Assign({})", RenderLvalue(ctx, a.target), *value_or);
            }
            auto value_or = RenderExprAsNative(ctx, ctx.Expr(a.value));
            if (!value_or) return std::unexpected(std::move(value_or.error()));
            return "(" + RenderLvalue(ctx, a.target) + " = " + *value_or + ")";
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

auto RenderExprAsRuntimeView(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::IntegerLiteral& lit) -> diag::Result<std::string> {
            return RenderIntegerLiteralAsView(ctx.Unit(), expr.type, lit.value);
          },
          [&](const mir::MemberVarRef& m) -> diag::Result<std::string> {
            return std::format(
                "{}.View()", ctx.Class().GetMemberVar(m.target).name);
          },
          [&](const mir::LocalVarRef& l) -> diag::Result<std::string> {
            return std::format("{}.View()", LookupLocalName(ctx.Body(), l));
          },
          [&](const mir::ConversionExpr& cv) -> diag::Result<std::string> {
            auto inner_or = RenderConversionCall(ctx, expr.type, cv);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            return std::format("{}.View()", *inner_or);
          },
          [&](const mir::UnaryExpr& u) -> diag::Result<std::string> {
            if (!IsPackedRuntimeUnaryOp(u.op) ||
                !IsPackedRuntime(ctx.Unit(), expr.type)) {
              return diag::Unsupported(
                  diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                  "this unary expression is not yet renderable as a runtime "
                  "view in cpp emit",
                  diag::UnsupportedCategory::kFeature);
            }
            auto inner_or = RenderPackedRuntimeUnaryCall(
                ctx, expr.type, u.op, ctx.Expr(u.operand));
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            return std::format("{}.View()", *inner_or);
          },
          [&](const mir::BinaryExpr& b) -> diag::Result<std::string> {
            switch (b.op) {
              case mir::BinaryOp::kBitwiseAnd:
              case mir::BinaryOp::kBitwiseOr:
              case mir::BinaryOp::kBitwiseXor:
              case mir::BinaryOp::kBitwiseXnor: {
                if (!IsPackedRuntime(ctx.Unit(), expr.type)) {
                  return diag::Unsupported(
                      diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                      "this binary expression is not yet renderable as a "
                      "runtime view in cpp emit",
                      diag::UnsupportedCategory::kFeature);
                }
                auto inner_or = RenderPackedBitwiseBinaryCall(
                    ctx, expr.type, b.op, ctx.Expr(b.lhs), ctx.Expr(b.rhs));
                if (!inner_or) {
                  return std::unexpected(std::move(inner_or.error()));
                }
                return std::format("{}.View()", *inner_or);
              }
              default:
                return diag::Unsupported(
                    diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                    "this binary expression is not yet renderable as a "
                    "runtime view in cpp emit",
                    diag::UnsupportedCategory::kFeature);
            }
          },
          [](const mir::AssignExpr&) -> diag::Result<std::string> {
            return diag::Unsupported(
                diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                "AssignExpr in a runtime-view context is not yet implemented "
                "in cpp emit",
                diag::UnsupportedCategory::kFeature);
          },
          [](const auto&) -> diag::Result<std::string> {
            return diag::Unsupported(
                diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                "this expression form is not yet renderable as a runtime view "
                "in cpp emit",
                diag::UnsupportedCategory::kFeature);
          },
      },
      expr.data);
}

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  // Some expressions slang types as packed but read as native rvalues
  // (e.g. comparisons over native-int operands). Mode is picked per-variant.
  return std::visit(
      Overloaded{
          [&](const mir::IntegerLiteral&) -> diag::Result<std::string> {
            return IsPackedRuntime(ctx.Unit(), expr.type)
                       ? RenderExprAsRuntimeView(ctx, expr)
                       : RenderExprAsNative(ctx, expr);
          },
          [&](const mir::MemberVarRef&) -> diag::Result<std::string> {
            return IsPackedRuntime(ctx.Unit(), expr.type)
                       ? RenderExprAsRuntimeView(ctx, expr)
                       : RenderExprAsNative(ctx, expr);
          },
          [&](const mir::LocalVarRef&) -> diag::Result<std::string> {
            return IsPackedRuntime(ctx.Unit(), expr.type)
                       ? RenderExprAsRuntimeView(ctx, expr)
                       : RenderExprAsNative(ctx, expr);
          },
          [&](const mir::ConversionExpr&) -> diag::Result<std::string> {
            return IsPackedRuntime(ctx.Unit(), expr.type)
                       ? RenderExprAsRuntimeView(ctx, expr)
                       : RenderExprAsNative(ctx, expr);
          },
          [&](const mir::UnaryExpr&) -> diag::Result<std::string> {
            return IsPackedRuntime(ctx.Unit(), expr.type)
                       ? RenderExprAsRuntimeView(ctx, expr)
                       : RenderExprAsNative(ctx, expr);
          },
          [&](const mir::BinaryExpr&) -> diag::Result<std::string> {
            return IsPackedRuntime(ctx.Unit(), expr.type)
                       ? RenderExprAsRuntimeView(ctx, expr)
                       : RenderExprAsNative(ctx, expr);
          },
          [&](const auto&) -> diag::Result<std::string> {
            return RenderExprAsNative(ctx, expr);
          },
      },
      expr.data);
}

}  // namespace lyra::backend::cpp

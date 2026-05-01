#include "lyra/backend/cpp/render_expr.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
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

auto IsPackedExplicit(const mir::CompilationUnit& unit, mir::TypeId type_id)
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
      "this packed expression form is not yet supported in cpp emit",
      diag::UnsupportedCategory::kFeature);
}

auto RenderWordArrayInitializer(
    std::span<const std::uint64_t> words, std::size_t n) -> std::string {
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

auto RenderPackedLiteralView(
    const mir::PackedArrayType& pa, const mir::IntegralConstant& c)
    -> std::string {
  const std::uint64_t width = pa.BitWidth();
  const std::size_t n = (width + 63U) / 64U;
  const std::string vw = RenderWordArrayInitializer(c.value_words, n);
  if (pa.atom == mir::BitAtom::kBit) {
    return std::format(
        "([&]() -> lyra::runtime::ConstBitView {{ "
        "static constexpr std::array<std::uint64_t, {0}> kV = {1}; "
        "return lyra::runtime::ConstBitView{{kV, 0, {2}}}; }}())",
        n, vw, width);
  }
  std::string uw;
  if (c.state_words.empty()) {
    uw = RenderWordArrayInitializer({}, n);
  } else {
    uw = RenderWordArrayInitializer(c.state_words, n);
  }
  return std::format(
      "([&]() -> lyra::runtime::ConstLogicView {{ "
      "static constexpr std::array<std::uint64_t, {0}> kV = {1}; "
      "static constexpr std::array<std::uint64_t, {0}> kU = {2}; "
      "return lyra::runtime::ConstLogicView{{kV, kU, 0, {3}}}; }}())",
      n, vw, uw, width);
}

auto RenderPackedAssign(const RenderContext& ctx, const mir::AssignExpr& a)
    -> diag::Result<std::string>;

auto RenderExprAsPackedTopLevel(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::AssignExpr& a) -> diag::Result<std::string> {
            return RenderPackedAssign(ctx, a);
          },
          [&](const auto&) -> diag::Result<std::string> {
            return PackedRuntimeUnsupported();
          },
      },
      expr.data);
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

auto RenderPackedExprAsView(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  const auto& ty = ctx.Unit().GetType(expr.type);
  if (!ty.IsPackedArray() ||
      ty.AsPackedArray().form != mir::PackedArrayForm::kExplicit) {
    throw InternalError("RenderPackedExprAsView: not packed-explicit");
  }
  const auto& pa = ty.AsPackedArray();
  return std::visit(
      Overloaded{
          [&](const mir::StructuralVarRef& m) -> diag::Result<std::string> {
            auto name_or = RenderStructuralVarName(ctx, m);
            if (!name_or) return std::unexpected(std::move(name_or.error()));
            return *name_or + ".View()";
          },
          [&](const mir::ProceduralVarRef& l) -> diag::Result<std::string> {
            return LookupProceduralVarName(ctx, l) + ".View()";
          },
          [&](const mir::IntegerLiteral& lit) -> diag::Result<std::string> {
            return RenderPackedLiteralView(pa, lit.value);
          },
          [&](const auto&) -> diag::Result<std::string> {
            return PackedRuntimeUnsupported();
          },
      },
      expr.data);
}

namespace {

auto SignednessLiteral(mir::Signedness s) -> std::string {
  return s == mir::Signedness::kSigned ? "lyra::runtime::Signedness::kSigned"
                                       : "lyra::runtime::Signedness::kUnsigned";
}

auto BitwiseRuntimeFunctionName(mir::UnaryOp op)
    -> std::optional<std::string_view> {
  if (op == mir::UnaryOp::kBitwiseNot) {
    return std::string_view{"lyra::runtime::BitwiseNot"};
  }
  return std::nullopt;
}

auto BitwiseRuntimeFunctionName(mir::BinaryOp op)
    -> std::optional<std::string_view> {
  switch (op) {
    case mir::BinaryOp::kBitwiseAnd:
      return std::string_view{"lyra::runtime::BitwiseAnd"};
    case mir::BinaryOp::kBitwiseOr:
      return std::string_view{"lyra::runtime::BitwiseOr"};
    case mir::BinaryOp::kBitwiseXor:
      return std::string_view{"lyra::runtime::BitwiseXor"};
    case mir::BinaryOp::kBitwiseXnor:
      return std::string_view{"lyra::runtime::BitwiseXnor"};
    default:
      return std::nullopt;
  }
}

auto ReductionRuntimeFunctionName(mir::UnaryOp op)
    -> std::optional<std::string_view> {
  switch (op) {
    case mir::UnaryOp::kReductionAnd:
      return std::string_view{"lyra::runtime::ReductionAnd"};
    case mir::UnaryOp::kReductionOr:
      return std::string_view{"lyra::runtime::ReductionOr"};
    case mir::UnaryOp::kReductionXor:
      return std::string_view{"lyra::runtime::ReductionXor"};
    case mir::UnaryOp::kReductionNand:
      return std::string_view{"lyra::runtime::ReductionNand"};
    case mir::UnaryOp::kReductionNor:
      return std::string_view{"lyra::runtime::ReductionNor"};
    case mir::UnaryOp::kReductionXnor:
      return std::string_view{"lyra::runtime::ReductionXnor"};
    default:
      return std::nullopt;
  }
}

auto IsTwoStatePacked(const mir::PackedArrayType& a) -> bool {
  return a.atom == mir::BitAtom::kBit;
}

auto IsSamePackedBitwiseShape(
    const mir::PackedArrayType& a, const mir::PackedArrayType& b) -> bool {
  if (a.form != mir::PackedArrayForm::kExplicit ||
      b.form != mir::PackedArrayForm::kExplicit) {
    return false;
  }
  if (a.BitWidth() != b.BitWidth()) {
    return false;
  }
  return IsTwoStatePacked(a) == IsTwoStatePacked(b);
}

auto RenderPackedBitwiseOperand(
    const RenderContext& ctx, const mir::Expr& operand_expr,
    const mir::PackedArrayType& lhs_pa) -> diag::Result<std::string> {
  const auto& op_ty = ctx.Unit().GetType(operand_expr.type);
  if (!op_ty.IsPackedArray()) {
    throw InternalError(
        "RenderPackedBitwiseAssign: operand is not a packed array");
  }
  const auto& op_pa = op_ty.AsPackedArray();
  if (!IsSamePackedBitwiseShape(op_pa, lhs_pa)) {
    throw InternalError(
        "RenderPackedBitwiseAssign: operand shape does not match LHS");
  }
  return RenderPackedExprAsView(ctx, operand_expr);
}

auto RenderPackedBitwiseAssign(
    const RenderContext& ctx, const mir::AssignExpr& a,
    const mir::PackedArrayType& lhs_pa, std::string_view lhs_view)
    -> diag::Result<std::optional<std::string>> {
  const mir::Expr& value_expr = ctx.Expr(a.value);

  if (std::holds_alternative<mir::ConversionExpr>(value_expr.data)) {
    return std::optional<std::string>{};
  }

  std::optional<std::string_view> fn_name;
  bool is_unary = false;
  if (const auto* u = std::get_if<mir::UnaryExpr>(&value_expr.data)) {
    fn_name = BitwiseRuntimeFunctionName(u->op);
    is_unary = true;
  } else if (const auto* b = std::get_if<mir::BinaryExpr>(&value_expr.data)) {
    fn_name = BitwiseRuntimeFunctionName(b->op);
  }
  if (!fn_name) {
    return std::optional<std::string>{};
  }

  const auto& result_ty = ctx.Unit().GetType(value_expr.type);
  if (!result_ty.IsPackedArray() ||
      !IsSamePackedBitwiseShape(result_ty.AsPackedArray(), lhs_pa)) {
    throw InternalError(
        "RenderPackedBitwiseAssign: result type does not match LHS");
  }

  if (is_unary) {
    const auto& u = std::get<mir::UnaryExpr>(value_expr.data);
    auto operand_or =
        RenderPackedBitwiseOperand(ctx, ctx.Expr(u.operand), lhs_pa);
    if (!operand_or) {
      return std::unexpected(std::move(operand_or.error()));
    }
    return std::optional<std::string>{
        std::format("{}({}, {})", *fn_name, *operand_or, lhs_view)};
  }

  const auto& b = std::get<mir::BinaryExpr>(value_expr.data);
  auto lhs_op_or = RenderPackedBitwiseOperand(ctx, ctx.Expr(b.lhs), lhs_pa);
  if (!lhs_op_or) {
    return std::unexpected(std::move(lhs_op_or.error()));
  }
  auto rhs_op_or = RenderPackedBitwiseOperand(ctx, ctx.Expr(b.rhs), lhs_pa);
  if (!rhs_op_or) {
    return std::unexpected(std::move(rhs_op_or.error()));
  }
  return std::optional<std::string>{std::format(
      "{}({}, {}, {})", *fn_name, *lhs_op_or, *rhs_op_or, lhs_view)};
}

auto RenderPackedReductionAssign(
    const RenderContext& ctx, const mir::AssignExpr& a,
    const mir::PackedArrayType& lhs_pa, std::string_view lhs_view)
    -> diag::Result<std::optional<std::string>> {
  const mir::Expr& value_expr = ctx.Expr(a.value);

  if (std::holds_alternative<mir::ConversionExpr>(value_expr.data)) {
    return std::optional<std::string>{};
  }

  const auto* u = std::get_if<mir::UnaryExpr>(&value_expr.data);
  if (u == nullptr) {
    return std::optional<std::string>{};
  }
  const auto fn_name = ReductionRuntimeFunctionName(u->op);
  if (!fn_name) {
    return std::optional<std::string>{};
  }

  if (lhs_pa.form != mir::PackedArrayForm::kExplicit ||
      lhs_pa.BitWidth() != 1U) {
    throw InternalError(
        "RenderPackedReductionAssign: LHS must be 1-bit kExplicit");
  }
  const auto& result_ty = ctx.Unit().GetType(value_expr.type);
  if (!result_ty.IsPackedArray()) {
    throw InternalError(
        "RenderPackedReductionAssign: result is not a packed array");
  }
  const auto& result_pa = result_ty.AsPackedArray();
  if (result_pa.form != mir::PackedArrayForm::kExplicit ||
      result_pa.BitWidth() != 1U ||
      IsTwoStatePacked(result_pa) != IsTwoStatePacked(lhs_pa)) {
    throw InternalError(
        "RenderPackedReductionAssign: result type does not match LHS");
  }

  const mir::Expr& operand_expr = ctx.Expr(u->operand);
  const auto& operand_ty = ctx.Unit().GetType(operand_expr.type);
  if (!operand_ty.IsPackedArray()) {
    return std::optional<std::string>{};
  }
  const auto& operand_pa = operand_ty.AsPackedArray();
  if (operand_pa.form != mir::PackedArrayForm::kExplicit) {
    return std::optional<std::string>{};
  }
  if (IsTwoStatePacked(operand_pa) != IsTwoStatePacked(result_pa)) {
    throw InternalError(
        "RenderPackedReductionAssign: operand state-kind does not match "
        "result");
  }

  auto operand_or = RenderPackedExprAsView(ctx, operand_expr);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  return std::optional<std::string>{
      std::format("{}({}, {})", *fn_name, *operand_or, lhs_view)};
}

auto RenderPackedAssign(const RenderContext& ctx, const mir::AssignExpr& a)
    -> diag::Result<std::string> {
  auto lhs_t_or = MirTypeOfLvalue(ctx, a.target);
  if (!lhs_t_or) return std::unexpected(std::move(lhs_t_or.error()));
  const auto& lhs = ctx.Unit().GetType(*lhs_t_or).AsPackedArray();
  auto lhs_name_or = RenderLvalue(ctx, a.target);
  if (!lhs_name_or) return std::unexpected(std::move(lhs_name_or.error()));
  const std::string lhs_view = *lhs_name_or + ".View()";

  auto bitwise_or = RenderPackedBitwiseAssign(ctx, a, lhs, lhs_view);
  if (!bitwise_or) {
    return std::unexpected(std::move(bitwise_or.error()));
  }
  if (bitwise_or->has_value()) {
    return std::move(**bitwise_or);
  }

  auto reduction_or = RenderPackedReductionAssign(ctx, a, lhs, lhs_view);
  if (!reduction_or) {
    return std::unexpected(std::move(reduction_or.error()));
  }
  if (reduction_or->has_value()) {
    return std::move(**reduction_or);
  }

  const mir::Expr* src_expr = &ctx.Expr(a.value);
  while (const auto* cv = std::get_if<mir::ConversionExpr>(&src_expr->data)) {
    const mir::Expr& op_expr = ctx.Expr(cv->operand);
    const auto& op_ty = ctx.Unit().GetType(op_expr.type);
    if (!op_ty.IsPackedArray() ||
        op_ty.AsPackedArray().form != mir::PackedArrayForm::kExplicit) {
      return PackedRuntimeUnsupported();
    }
    src_expr = &op_expr;
  }

  const auto& src_ty = ctx.Unit().GetType(src_expr->type);
  if (!src_ty.IsPackedArray() ||
      src_ty.AsPackedArray().form != mir::PackedArrayForm::kExplicit) {
    return PackedRuntimeUnsupported();
  }
  const auto& src_pa = src_ty.AsPackedArray();

  auto src_view_or = RenderPackedExprAsView(ctx, *src_expr);
  if (!src_view_or) {
    return std::unexpected(std::move(src_view_or.error()));
  }

  const std::string convert_fn = (lhs.atom == mir::BitAtom::kBit)
                                     ? "lyra::runtime::ConvertToBit"
                                     : "lyra::runtime::ConvertToLogic";
  return std::format(
      "{}({}, {}, {})", convert_fn, *src_view_or, lhs_view,
      SignednessLiteral(src_pa.signedness));
}

}  // namespace

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
            if (IsPackedExplicit(ctx.Unit(), target_type)) {
              return RenderPackedAssign(ctx, a);
            }
            auto target_or = RenderLvalue(ctx, a.target);
            if (!target_or) {
              return std::unexpected(std::move(target_or.error()));
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
  if (IsPackedExplicit(ctx.Unit(), expr.type)) {
    return RenderExprAsPackedTopLevel(ctx, expr);
  }
  return RenderExprAsNative(ctx, expr);
}

}  // namespace lyra::backend::cpp

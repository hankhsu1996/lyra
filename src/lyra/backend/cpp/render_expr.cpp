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

auto RenderPackedArrayIntegerLiteral(
    const mir::PackedArrayType& pa, const mir::IntegralConstant& c)
    -> std::string {
  if (c.width > 64U) {
    throw InternalError(
        "RenderPackedArrayIntegerLiteral: width > 64 not yet implemented");
  }
  const auto value = IntegralConstantToInt64(c);
  // Default-int shape (`int`, `bit signed [31:0]`, etc.): emit the concise
  // factory that mirrors SV's natural int literal promotion.
  if (pa.BitWidth() == 32U && pa.signedness == mir::Signedness::kSigned &&
      pa.atom == mir::BitAtom::kBit) {
    return std::format("lyra::value::PackedArray::Int({})", value);
  }
  const char* signed_lit =
      pa.signedness == mir::Signedness::kSigned ? "true" : "false";
  const char* four_state_lit = pa.atom != mir::BitAtom::kBit ? "true" : "false";
  return std::format(
      "lyra::value::PackedArray::FromInt({}LL, {}, {}, {})", value,
      pa.BitWidth(), signed_lit, four_state_lit);
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
        "([&]() -> lyra::value::ConstBitView {{ "
        "static constexpr std::array<std::uint64_t, {0}> kV = {1}; "
        "return lyra::value::ConstBitView{{kV, 0, {2}}}; }}())",
        n, vw, width);
  }
  std::string uw;
  if (c.state_words.empty()) {
    uw = RenderWordArrayInitializer({}, n);
  } else {
    uw = RenderWordArrayInitializer(c.state_words, n);
  }
  return std::format(
      "([&]() -> lyra::value::ConstLogicView {{ "
      "static constexpr std::array<std::uint64_t, {0}> kV = {1}; "
      "static constexpr std::array<std::uint64_t, {0}> kU = {2}; "
      "return lyra::value::ConstLogicView{{kV, kU, 0, {3}}}; }}())",
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
  if (!ty.IsPackedArray()) {
    throw InternalError("RenderPackedExprAsView: type is not a packed array");
  }
  const auto& pa = ty.AsPackedArray();
  const std::string view_call =
      pa.atom == mir::BitAtom::kBit ? ".AsBitView()" : ".AsLogicView()";
  return std::visit(
      Overloaded{
          [&](const mir::StructuralVarRef& m) -> diag::Result<std::string> {
            auto name_or = RenderStructuralVarName(ctx, m);
            if (!name_or) return std::unexpected(std::move(name_or.error()));
            return *name_or + view_call;
          },
          [&](const mir::ProceduralVarRef& l) -> diag::Result<std::string> {
            return LookupProceduralVarName(ctx, l) + view_call;
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
  return s == mir::Signedness::kSigned ? "lyra::value::Signedness::kSigned"
                                       : "lyra::value::Signedness::kUnsigned";
}

auto BitwiseRuntimeFunctionName(mir::UnaryOp op)
    -> std::optional<std::string_view> {
  if (op == mir::UnaryOp::kBitwiseNot) {
    return std::string_view{"lyra::value::BitwiseNot"};
  }
  return std::nullopt;
}

auto BitwiseRuntimeFunctionName(mir::BinaryOp op)
    -> std::optional<std::string_view> {
  switch (op) {
    case mir::BinaryOp::kBitwiseAnd:
      return std::string_view{"lyra::value::BitwiseAnd"};
    case mir::BinaryOp::kBitwiseOr:
      return std::string_view{"lyra::value::BitwiseOr"};
    case mir::BinaryOp::kBitwiseXor:
      return std::string_view{"lyra::value::BitwiseXor"};
    case mir::BinaryOp::kBitwiseXnor:
      return std::string_view{"lyra::value::BitwiseXnor"};
    default:
      return std::nullopt;
  }
}

auto ReductionRuntimeFunctionName(mir::UnaryOp op)
    -> std::optional<std::string_view> {
  switch (op) {
    case mir::UnaryOp::kReductionAnd:
      return std::string_view{"lyra::value::ReductionAnd"};
    case mir::UnaryOp::kReductionOr:
      return std::string_view{"lyra::value::ReductionOr"};
    case mir::UnaryOp::kReductionXor:
      return std::string_view{"lyra::value::ReductionXor"};
    case mir::UnaryOp::kReductionNand:
      return std::string_view{"lyra::value::ReductionNand"};
    case mir::UnaryOp::kReductionNor:
      return std::string_view{"lyra::value::ReductionNor"};
    case mir::UnaryOp::kReductionXnor:
      return std::string_view{"lyra::value::ReductionXnor"};
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
  const std::string lhs_view =
      *lhs_name_or +
      (lhs.atom == mir::BitAtom::kBit ? ".AsBitView()" : ".AsLogicView()");

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
                                     ? "lyra::value::ConvertToBit"
                                     : "lyra::value::ConvertToLogic";
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
            const auto& ty = ctx.Unit().GetType(expr.type);
            if (!ty.IsPackedArray()) {
              throw InternalError(
                  "RenderExprAsNative: IntegerLiteral not typed as "
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
            auto operand_or = RenderExprAsNative(ctx, ctx.Expr(u.operand));
            if (!operand_or) {
              return std::unexpected(std::move(operand_or.error()));
            }
            return RenderUnaryOp(u.op, *operand_or);
          },
          [&](const mir::BinaryExpr& b) -> diag::Result<std::string> {
            auto lhs_or = RenderExprAsNative(ctx, ctx.Expr(b.lhs));
            if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
            auto rhs_or = RenderExprAsNative(ctx, ctx.Expr(b.rhs));
            if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
            return RenderBinaryOp(b.op, *lhs_or, *rhs_or);
          },
          [&](const mir::ConditionalExpr& c) -> diag::Result<std::string> {
            auto cond_or = RenderConditionAsBool(ctx, ctx.Expr(c.condition));
            if (!cond_or) return std::unexpected(std::move(cond_or.error()));
            auto then_or = RenderExprAsNative(ctx, ctx.Expr(c.then_value));
            if (!then_or) return std::unexpected(std::move(then_or.error()));
            auto else_or = RenderExprAsNative(ctx, ctx.Expr(c.else_value));
            if (!else_or) return std::unexpected(std::move(else_or.error()));
            return "(" + *cond_or + " ? " + *then_or + " : " + *else_or + ")";
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
            const auto& src_expr = ctx.Expr(cv.operand);
            auto operand_or = RenderExprAsNative(ctx, src_expr);
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

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  if (IsPackedExplicit(ctx.Unit(), expr.type)) {
    return RenderExprAsPackedTopLevel(ctx, expr);
  }
  return RenderExprAsNative(ctx, expr);
}

auto RenderConditionAsBool(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  auto text_or = RenderExprAsNative(ctx, expr);
  if (!text_or) return std::unexpected(std::move(text_or.error()));
  const auto& ty = ctx.Unit().GetType(expr.type);
  if (ty.IsPackedArray()) {
    return "(" + *text_or + ").IsTruthy()";
  }
  return *text_or;
}

}  // namespace lyra::backend::cpp

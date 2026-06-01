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
#include "lyra/mir/builtin_method.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"

namespace lyra::backend::cpp {

// Renders `expr` without forcing a `PackedArrayRef` chain to materialize.
// Public `RenderExpr` wraps any leftover ref in `.Clone()` so consumers see
// an owning value.
auto RenderExprNatural(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string>;

namespace {

// Type-kind predicate. `real`, `shortreal`, and `realtime` (LRM 6.12)
// collectively form the "real family"; arithmetic / relational / logical
// operator dispatch all branch on this. Taking the full `mir::Type` instead
// of `TypeKind` keeps the call-site spelling consistent with `IsIntegralPacked`
// / `IsEnum` on `mir::Type` itself.
auto IsRealFamilyType(const mir::Type& ty) -> bool {
  return ty.Kind() == mir::TypeKind::kReal ||
         ty.Kind() == mir::TypeKind::kShortReal ||
         ty.Kind() == mir::TypeKind::kRealTime;
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
  // Narrow + no X/Z fits a single int64 carrier and reads better as
  // PackedArray::Int / FromInt. Wide literals or X/Z-bearing literals must
  // round-trip through explicit value/unknown word planes via FromWords.
  const bool literal_has_xz = !c.state_words.empty() && c.state_words[0] != 0U;

  // FromInt / FromWords both take a single `bit_width`, so a multi-dim PA
  // shape cannot ride those paths. The only multi-dim literal the synthesiser
  // produces is the type's LRM Table 6-7 default (all zeros for 2-state, all
  // x for 4-state), which the dim-list constructor materialises directly --
  // call it instead of pretending the literal is single-dim.
  if (pa.dims.size() > 1U) {
    return std::format(
        "lyra::value::PackedArray({})", RenderPackedArrayCtorArgs(pa));
  }

  const bool needs_word_planes = c.width > 64U || literal_has_xz;

  if (!needs_word_planes) {
    const auto value = IntegralConstantToInt64(c);
    if (pa.BitWidth() == 32U && pa.signedness == mir::Signedness::kSigned &&
        pa.atom == mir::BitAtom::kBit) {
      return std::format("lyra::value::PackedArray::Int({})", value);
    }
    return std::format(
        "lyra::value::PackedArray::FromInt({}LL, {})", value,
        RenderPackedArrayCtorArgs(pa));
  }

  const bool is_four_state = pa.atom != mir::BitAtom::kBit;
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
      "lyra::value::PackedArray::FromWords({}, {}, {})", value_init,
      unknown_init, RenderPackedArrayCtorArgs(pa));
}

auto RenderIntegerLiteralExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::IntegerLiteral& lit) -> diag::Result<std::string> {
  const auto& ty = ctx.Unit().GetType(expr.type);
  if (!ty.IsIntegralPacked()) {
    throw InternalError(
        "RenderIntegerLiteralExpr: IntegerLiteral not typed as "
        "PackedArrayType or EnumType");
  }
  auto body = RenderPackedArrayIntegerLiteral(ty.AsIntegralPacked(), lit.value);
  if (ty.IsEnum()) {
    return std::format(
        "{}{{{}}}", RenderEnumClassName(ctx.StructuralScope(), expr.type),
        body);
  }
  return body;
}

auto RenderRealLiteralExpr(
    const RenderContext& ctx, const mir::Expr& expr, const mir::RealLiteral& r)
    -> std::string {
  // `std::format` with `{:.{}g}` and precision=17 round-trips a double;
  // precision=9 round-trips a float (IEEE 754 minimum representable-pair
  // widths). The trailing 'f' suffix on the float form keeps the C++ literal
  // type matched to the destination `float` so overload resolution and
  // initialization both land on the shortreal path.
  //
  // `g` strips trailing zeros and the decimal point for whole-number values,
  // which would produce literals like `0f` or `42e3f` that the C++ lexer
  // rejects (a digit sequence followed by `f` is not a valid float suffix).
  // Force a decimal point when the formatted body has neither `.` nor an
  // exponent, so `0` -> `0.0`, `42` -> `42.0`.
  const auto& ty = ctx.Unit().GetType(expr.type);
  const bool is_short = ty.Kind() == mir::TypeKind::kShortReal;
  std::string body = is_short ? std::format("{:.9g}", r.value)
                              : std::format("{:.17g}", r.value);
  if (body.find_first_of(".eE") == std::string::npos) {
    body += ".0";
  }
  if (is_short) {
    body += "f";
  }
  return body;
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

auto LookupProceduralVarName(
    const RenderContext& ctx, const mir::ProceduralVarRef& ref)
    -> const std::string& {
  return ctx.ProceduralScopeAtHops(ref.hops).vars.at(ref.var.value).name;
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

namespace {

// Read-side render for a structural var ref. Integral packed vars wrap in
// `Var<PackedArray>` whose held value is observed via `.Get()`; all other
// storage types (real-family, string, ObjectType) expose the value directly,
// so the bare field name is the C++ read expression. Writes use
// `RenderStructuralVarName` directly (no `.Get()`); the read/write split is
// intentional and lives at this boundary.
auto RenderStructuralVarReadExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::StructuralVarRef& m) -> diag::Result<std::string> {
  auto name_or = RenderStructuralVarName(ctx, m);
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  if (ctx.Unit().GetType(expr.type).IsIntegralPacked()) {
    return *name_or + ".Get()";
  }
  return *name_or;
}

// Builds the C++ literal that materializes a 1-bit PackedArray of the SV-typed
// shape carried by `result_ty`. Used to wrap the C++ `bool` result of a
// real-operand relational / equality / logical operator into the 1-bit
// integral PackedArray result type LRM 11.3.1 prescribes.
auto WrapBoolAsResultShape(
    const mir::Type& result_ty, std::string_view bool_expr) -> std::string {
  if (!result_ty.IsIntegralPacked()) {
    throw InternalError(
        "WrapBoolAsResultShape: real comparison / logical result type is "
        "not an integral PackedArray; MIR invariant violated");
  }
  return std::format(
      "lyra::value::PackedArray::FromInt(({}) ? 1LL : 0LL, {})", bool_expr,
      RenderPackedArrayCtorArgs(result_ty.AsIntegralPacked()));
}

auto RenderBinaryOpString(
    mir::BinaryOp op, const std::string& lhs, const std::string& rhs,
    const mir::Type& result_ty) -> diag::Result<std::string> {
  // LRM 6.16 Table 6-9: equality compares contents; relational ops use
  // lexicographic ordering (str.compare semantics). std::string's operator==
  // and operator< match these directly.
  switch (op) {
    case mir::BinaryOp::kEquality:
      return WrapBoolAsResultShape(result_ty, lhs + " == " + rhs);
    case mir::BinaryOp::kInequality:
      return WrapBoolAsResultShape(result_ty, lhs + " != " + rhs);
    case mir::BinaryOp::kLessThan:
      return WrapBoolAsResultShape(result_ty, lhs + " < " + rhs);
    case mir::BinaryOp::kLessEqual:
      return WrapBoolAsResultShape(result_ty, lhs + " <= " + rhs);
    case mir::BinaryOp::kGreaterThan:
      return WrapBoolAsResultShape(result_ty, lhs + " > " + rhs);
    case mir::BinaryOp::kGreaterEqual:
      return WrapBoolAsResultShape(result_ty, lhs + " >= " + rhs);
    default:
      break;
  }
  return diag::Unsupported(
      diag::DiagCode::kCppEmitExpressionFormNotImplemented,
      "binary operator is not legal on string operands per LRM 6.16",
      diag::UnsupportedCategory::kFeature);
}

auto RenderBinaryOpReal(
    mir::BinaryOp op, const std::string& lhs, const std::string& rhs,
    const mir::Type& result_ty) -> diag::Result<std::string> {
  // LRM 11.3.1 + Table 11-1: arithmetic on real produces real; relational /
  // logical / equality produce a 1-bit integral that the runtime sees as a
  // PackedArray view. `std::pow` overload resolution picks (float, float) ->
  // float and (double, double) -> double, matching LRM 11.3.1's result type
  // for shortreal-only vs any-real expressions.
  switch (op) {
    case mir::BinaryOp::kAdd:
      return "(" + lhs + " + " + rhs + ")";
    case mir::BinaryOp::kSub:
      return "(" + lhs + " - " + rhs + ")";
    case mir::BinaryOp::kMul:
      return "(" + lhs + " * " + rhs + ")";
    case mir::BinaryOp::kDiv:
      return "(" + lhs + " / " + rhs + ")";
    case mir::BinaryOp::kPower:
      return "std::pow(" + lhs + ", " + rhs + ")";
    case mir::BinaryOp::kLessThan:
      return WrapBoolAsResultShape(result_ty, lhs + " < " + rhs);
    case mir::BinaryOp::kLessEqual:
      return WrapBoolAsResultShape(result_ty, lhs + " <= " + rhs);
    case mir::BinaryOp::kGreaterThan:
      return WrapBoolAsResultShape(result_ty, lhs + " > " + rhs);
    case mir::BinaryOp::kGreaterEqual:
      return WrapBoolAsResultShape(result_ty, lhs + " >= " + rhs);
    case mir::BinaryOp::kEquality:
      return WrapBoolAsResultShape(result_ty, lhs + " == " + rhs);
    case mir::BinaryOp::kInequality:
      return WrapBoolAsResultShape(result_ty, lhs + " != " + rhs);
    case mir::BinaryOp::kLogicalAnd:
      return WrapBoolAsResultShape(
          result_ty, "bool(" + lhs + ") && bool(" + rhs + ")");
    case mir::BinaryOp::kLogicalOr:
      return WrapBoolAsResultShape(
          result_ty, "bool(" + lhs + ") || bool(" + rhs + ")");
    case mir::BinaryOp::kLogicalImplication:
      return WrapBoolAsResultShape(
          result_ty, "!bool(" + lhs + ") || bool(" + rhs + ")");
    case mir::BinaryOp::kLogicalEquivalence:
      return WrapBoolAsResultShape(
          result_ty, "bool(" + lhs + ") == bool(" + rhs + ")");
    default:
      break;
  }
  return diag::Unsupported(
      diag::DiagCode::kCppEmitExpressionFormNotImplemented,
      "binary operator is not legal on real operands per LRM 11.3.1",
      diag::UnsupportedCategory::kFeature);
}

auto RenderBinaryOpIntegral(
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
      return "(" + lhs + " && " + rhs + ")";
    case mir::BinaryOp::kLogicalOr:
      return "(" + lhs + " || " + rhs + ")";
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
      return "(!(" + lhs + ").WildcardEquals(" + rhs + "))";
    case mir::BinaryOp::kCaseEquality:
      return "(" + lhs + ").CaseEqual(" + rhs + ")";
    case mir::BinaryOp::kCaseInequality:
      return "(!(" + lhs + ").CaseEqual(" + rhs + "))";
    case mir::BinaryOp::kCasezEquality:
      return "(" + lhs + ").CasezEquals(" + rhs + ")";
    case mir::BinaryOp::kCasexEquality:
      return "(" + lhs + ").CasexEquals(" + rhs + ")";
  }
  return "(" + lhs + std::string{tok} + rhs + ")";
}

auto RenderUnaryOpReal(
    mir::UnaryOp op, const std::string& operand, const mir::Type& result_ty)
    -> diag::Result<std::string> {
  switch (op) {
    case mir::UnaryOp::kPlus:
      return "(" + operand + ")";
    case mir::UnaryOp::kMinus:
      return "(-(" + operand + "))";
    case mir::UnaryOp::kLogicalNot:
      return WrapBoolAsResultShape(result_ty, "!bool(" + operand + ")");
    default:
      break;
  }
  return diag::Unsupported(
      diag::DiagCode::kCppEmitExpressionFormNotImplemented,
      "unary operator is not legal on real operands per LRM 11.3.1",
      diag::UnsupportedCategory::kFeature);
}

auto RenderUnaryOpIntegral(mir::UnaryOp op, const std::string& operand)
    -> diag::Result<std::string> {
  switch (op) {
    case mir::UnaryOp::kPlus:
      return "(" + operand + ")";
    case mir::UnaryOp::kMinus:
      return "(-" + operand + ")";
    case mir::UnaryOp::kBitwiseNot:
      return "(~" + operand + ")";
    case mir::UnaryOp::kLogicalNot:
      return "(!(" + operand + "))";
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
  throw InternalError("RenderUnaryOpIntegral: unknown MIR UnaryOp");
}

auto RenderUnaryExpr(
    const RenderContext& ctx, const mir::Expr& expr, const mir::UnaryExpr& u)
    -> diag::Result<std::string> {
  const auto& operand_expr = ctx.Expr(u.operand);
  auto operand_or = RenderExpr(ctx, operand_expr);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  if (IsRealFamilyType(ctx.Unit().GetType(operand_expr.type))) {
    return RenderUnaryOpReal(u.op, *operand_or, ctx.Unit().GetType(expr.type));
  }
  return RenderUnaryOpIntegral(u.op, *operand_or);
}

auto RenderBinaryExpr(
    const RenderContext& ctx, const mir::Expr& expr, const mir::BinaryExpr& b)
    -> diag::Result<std::string> {
  const auto& lhs_expr = ctx.Expr(b.lhs);
  const auto& rhs_expr = ctx.Expr(b.rhs);
  auto lhs_or = RenderExpr(ctx, lhs_expr);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  auto rhs_or = RenderExpr(ctx, rhs_expr);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  // Slang inserts a propagated `ConversionExpr` on the narrower operand so by
  // the time we reach a binary op both sides share a precision class. Dispatch
  // by operand kind: real-family (LRM 11.3.1), string (LRM 6.16 Table 6-9),
  // else integral PackedArray ops.
  const auto& lhs_ty = ctx.Unit().GetType(lhs_expr.type);
  const auto& rhs_ty = ctx.Unit().GetType(rhs_expr.type);
  if (IsRealFamilyType(lhs_ty) || IsRealFamilyType(rhs_ty)) {
    return RenderBinaryOpReal(
        b.op, *lhs_or, *rhs_or, ctx.Unit().GetType(expr.type));
  }
  if (lhs_ty.Kind() == mir::TypeKind::kString &&
      rhs_ty.Kind() == mir::TypeKind::kString) {
    return RenderBinaryOpString(
        b.op, *lhs_or, *rhs_or, ctx.Unit().GetType(expr.type));
  }
  return RenderBinaryOpIntegral(b.op, *lhs_or, *rhs_or);
}

auto RenderConditionalExpr(
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

// SV LRM 6.12.1 + 6.22: real-family conversions are pure float-precision
// reshape. `realtime` is a synonym for `real`, so both share `double`. The
// only emit-time work is on shortreal <-> real: insert an explicit
// `static_cast` so the C++ compiler does not flag the narrowing direction.
auto RenderRealConversion(
    std::string operand, const mir::Type& src_ty, const mir::Type& dst_ty)
    -> std::string {
  const bool src_is_short = src_ty.Kind() == mir::TypeKind::kShortReal;
  const bool dst_is_short = dst_ty.Kind() == mir::TypeKind::kShortReal;
  if (src_is_short == dst_is_short) {
    return operand;
  }
  return std::format(
      "static_cast<{}>({})", dst_is_short ? "float" : "double", operand);
}

// Integral-to-integral conversions reshape a PackedArray to a possibly-
// different width / signedness / state. Same-shape is pass-through (slang
// emits spurious self-conversions). Enum endpoints layer two extra wraps on
// top of the integral body so the C++ static type matches LRM 6.19.3's
// explicit-cast requirement: integral -> enum wraps with the enum class's
// converting ctor; enum -> integral slices the enum subobject into a plain
// PackedArray for downstream template deduction.
auto RenderIntegralConversion(
    const RenderContext& ctx, mir::TypeId dst_type_id, std::string operand,
    const mir::PackedArrayType& src_pa, const mir::PackedArrayType& dst_pa,
    bool src_is_enum, bool dst_is_enum) -> std::string {
  std::string body;
  if (src_pa.BitWidth() == dst_pa.BitWidth() &&
      src_pa.signedness == dst_pa.signedness && src_pa.atom == dst_pa.atom) {
    body = std::move(operand);
  } else {
    body = std::format(
        "lyra::value::PackedArray::ConvertFrom({}, {})", operand,
        RenderPackedArrayCtorArgs(dst_pa));
  }
  if (dst_is_enum) {
    return std::format(
        "{}{{{}}}", RenderEnumClassName(ctx.StructuralScope(), dst_type_id),
        body);
  }
  if (src_is_enum) {
    return std::format("lyra::value::PackedArray{{{}}}", body);
  }
  return body;
}

// LRM 6.12.1: integer-to-real implicit conversion treats X/Z bits as 0.
// `PackedArray::ToInt64` already collapses X/Z bits to 0 (packed_array.hpp
// docstring), so a plain `static_cast` to the destination real precision is
// the full conversion. `shortreal` casts to `float`; `real` and `realtime`
// (LRM 6.12 synonyms) cast to `double`.
auto RenderIntegralToRealConversion(
    std::string operand, const mir::Type& dst_ty) -> std::string {
  const auto* cpp_ty =
      dst_ty.Kind() == mir::TypeKind::kShortReal ? "float" : "double";
  return std::format("static_cast<{}>(({}).ToInt64())", cpp_ty, operand);
}

// LRM 6.12.1: real-to-integer implicit conversion rounds the real to the
// nearest integer with ties rounded away from zero (35.5 -> 36, -1.5 -> -2).
// `std::llround` is the standard library function with exactly that rounding
// rule; it returns `long long` (>= 64 bits), which feeds `PackedArray::FromInt`
// to land the rounded value into the destination shape. Destination widths >
// 64 bits would need a wide-int conversion path; that is currently caught by
// `FromInt`'s own width invariant.
auto RenderRealToIntegralConversion(
    std::string operand, const mir::PackedArrayType& dst_pa) -> std::string {
  return std::format(
      "lyra::value::PackedArray::FromInt(std::llround({}), {})", operand,
      RenderPackedArrayCtorArgs(dst_pa));
}

auto RenderConversionExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::ConversionExpr& cv) -> diag::Result<std::string> {
  const auto& src_expr = ctx.Expr(cv.operand);
  auto operand_or = RenderExpr(ctx, src_expr);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  const auto& src_ty = ctx.Unit().GetType(src_expr.type);
  const auto& dst_ty = ctx.Unit().GetType(expr.type);
  if (IsRealFamilyType(src_ty) && IsRealFamilyType(dst_ty)) {
    return RenderRealConversion(*std::move(operand_or), src_ty, dst_ty);
  }
  if (src_ty.IsIntegralPacked() && dst_ty.IsIntegralPacked()) {
    return RenderIntegralConversion(
        ctx, expr.type, *std::move(operand_or), src_ty.AsIntegralPacked(),
        dst_ty.AsIntegralPacked(), src_ty.IsEnum(), dst_ty.IsEnum());
  }
  if (src_ty.IsIntegralPacked() && IsRealFamilyType(dst_ty)) {
    return RenderIntegralToRealConversion(*std::move(operand_or), dst_ty);
  }
  if (IsRealFamilyType(src_ty) && dst_ty.IsIntegralPacked()) {
    return RenderRealToIntegralConversion(
        *std::move(operand_or), dst_ty.AsIntegralPacked());
  }
  // Fall-through covers conversions whose source and destination already share
  // a compatible C++ representation, so no transformation is needed at this
  // boundary: same-kind identity conversions (e.g., string -> string), and
  // slang's narrow `bit[N-1:0]` string-literal -> `string` lift where the
  // operand already renders as the C++ string literal.
  return *std::move(operand_or);
}

auto EnumMethodMemberName(mir::EnumMethodKind k) -> std::string_view {
  switch (k) {
    case mir::EnumMethodKind::kFirst:
      return "First";
    case mir::EnumMethodKind::kLast:
      return "Last";
    case mir::EnumMethodKind::kNum:
      return "Num";
    case mir::EnumMethodKind::kName:
      return "Name";
    case mir::EnumMethodKind::kNext:
      return "Next";
    case mir::EnumMethodKind::kPrev:
      return "Prev";
  }
  throw InternalError("EnumMethodMemberName: unknown kind");
}

auto StringMethodMemberName(mir::StringMethodKind k) -> std::string_view {
  switch (k) {
    case mir::StringMethodKind::kLen:
      return "Len";
    case mir::StringMethodKind::kGetc:
      return "Getc";
    case mir::StringMethodKind::kPutc:
      return "Putc";
    case mir::StringMethodKind::kToupper:
      return "Toupper";
    case mir::StringMethodKind::kTolower:
      return "Tolower";
    case mir::StringMethodKind::kCompare:
      return "Compare";
    case mir::StringMethodKind::kIcompare:
      return "Icompare";
    case mir::StringMethodKind::kSubstr:
      return "Substr";
    case mir::StringMethodKind::kAtoi:
      return "Atoi";
    case mir::StringMethodKind::kAtohex:
      return "Atohex";
    case mir::StringMethodKind::kAtooct:
      return "Atooct";
    case mir::StringMethodKind::kAtobin:
      return "Atobin";
    case mir::StringMethodKind::kAtoreal:
      return "Atoreal";
    case mir::StringMethodKind::kItoa:
      return "Itoa";
    case mir::StringMethodKind::kHextoa:
      return "Hextoa";
    case mir::StringMethodKind::kOcttoa:
      return "Octtoa";
    case mir::StringMethodKind::kBintoa:
      return "Bintoa";
    case mir::StringMethodKind::kRealtoa:
      return "Realtoa";
  }
  throw InternalError("StringMethodMemberName: unknown kind");
}

auto EventMethodMemberName(mir::EventMethodKind k) -> std::string_view {
  switch (k) {
    case mir::EventMethodKind::kTrigger:
      return "Trigger";
    case mir::EventMethodKind::kAwait:
      return "Await";
    case mir::EventMethodKind::kTriggered:
      return "Triggered";
  }
  throw InternalError("EventMethodMemberName: unknown kind");
}

// LRM 6.19.5: first/last/num are static on the enum class; name/next/prev
// dispatch on the receiver. num returns std::int32_t and wraps into the SV
// `int` shape via PackedArray::Int.
auto RenderEnumMethodCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    const mir::EnumMethodInfo& m) -> diag::Result<std::string> {
  const auto member = EnumMethodMemberName(m.kind);
  const auto class_name =
      RenderEnumClassName(ctx.StructuralScope(), m.enum_type);
  const bool is_static = m.kind == mir::EnumMethodKind::kFirst ||
                         m.kind == mir::EnumMethodKind::kLast ||
                         m.kind == mir::EnumMethodKind::kNum;
  const bool takes_step = m.kind == mir::EnumMethodKind::kNext ||
                          m.kind == mir::EnumMethodKind::kPrev;
  std::string raw_call;
  if (is_static) {
    raw_call = std::format("{}::{}()", class_name, member);
  } else {
    if (call.arguments.empty()) {
      throw InternalError(
          "RenderEnumMethodCall: instance method expects a receiver "
          "argument");
    }
    auto receiver_or = RenderExpr(ctx, ctx.Expr(call.arguments[0]));
    if (!receiver_or) {
      return std::unexpected(std::move(receiver_or.error()));
    }
    // next / prev have an optional step. Omitted at the SV call site ->
    // omit at the C++ call site too; the Enum<Derived> method's default
    // argument supplies 1.
    std::string step_arg;
    if (takes_step && call.arguments.size() >= 2) {
      auto step_or = RenderExpr(ctx, ctx.Expr(call.arguments[1]));
      if (!step_or) {
        return std::unexpected(std::move(step_or.error()));
      }
      step_arg = std::format("static_cast<unsigned>(({}).ToInt64())", *step_or);
    }
    raw_call = std::format("({}).{}({})", *receiver_or, member, step_arg);
  }
  if (m.kind == mir::EnumMethodKind::kNum) {
    return std::format("lyra::value::PackedArray::Int({})", raw_call);
  }
  return raw_call;
}

// LRM 6.16: dispatch on the receiver (arguments[0]); subsequent SV args are
// the method parameters. Integral-typed args (SV int / integer / byte) live
// in a PackedArray at the call site and project to std::int32_t / std::int64_t
// via ToInt64. Integral return values are wrapped back into the appropriate
// PackedArray shape.
auto RenderStringMethodCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    const mir::StringMethodInfo& m) -> diag::Result<std::string> {
  if (call.arguments.empty()) {
    throw InternalError(
        "RenderStringMethodCall: instance method expects a receiver "
        "argument");
  }
  auto receiver_or = RenderExpr(ctx, ctx.Expr(call.arguments[0]));
  if (!receiver_or) {
    return std::unexpected(std::move(receiver_or.error()));
  }
  std::string args_text;
  for (std::size_t i = 1; i < call.arguments.size(); ++i) {
    auto arg_or = RenderExpr(ctx, ctx.Expr(call.arguments[i]));
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    const auto& arg_ty = ctx.Unit().GetType(ctx.Expr(call.arguments[i]).type);
    std::string rendered = arg_ty.IsIntegralPacked()
                               ? std::format(
                                     "static_cast<std::int32_t>("
                                     "({}).ToInt64())",
                                     *arg_or)
                               : std::move(*arg_or);
    if (i > 1) args_text += ", ";
    args_text += rendered;
  }
  const std::string raw_call = std::format(
      "({}).{}({})", *receiver_or, StringMethodMemberName(m.kind), args_text);
  switch (m.kind) {
    case mir::StringMethodKind::kLen:
    case mir::StringMethodKind::kCompare:
    case mir::StringMethodKind::kIcompare:
      // Returns SV int (32-bit signed 2-state).
      return std::format("lyra::value::PackedArray::Int({})", raw_call);
    case mir::StringMethodKind::kGetc:
      // Returns SV byte (8-bit signed 2-state).
      return std::format("lyra::value::PackedArray::Byte({})", raw_call);
    case mir::StringMethodKind::kAtoi:
    case mir::StringMethodKind::kAtohex:
    case mir::StringMethodKind::kAtooct:
    case mir::StringMethodKind::kAtobin:
      // Returns SV integer (32-bit signed 4-state).
      return std::format("lyra::value::PackedArray::Integer({})", raw_call);
    // Atoreal returns C++ double, matching SV real.
    // Toupper / Tolower / Substr return lyra::value::String.
    // Putc / Itoa / Hextoa / Octtoa / Bintoa / Realtoa are void.
    default:
      return raw_call;
  }
}

// LRM 15.5: Trigger and Triggered reach into RuntimeServices; Await is
// suspending and must be wrapped in co_await.
auto RenderEventMethodCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    const mir::EventMethodInfo& m) -> diag::Result<std::string> {
  if (call.arguments.empty()) {
    throw InternalError(
        "RenderEventMethodCall: event method expects a receiver argument");
  }
  auto receiver_or = RenderExpr(ctx, ctx.Expr(call.arguments[0]));
  if (!receiver_or) {
    return std::unexpected(std::move(receiver_or.error()));
  }
  const std::string args = (m.kind == mir::EventMethodKind::kTrigger ||
                            m.kind == mir::EventMethodKind::kTriggered)
                               ? "*services_"
                               : "";
  const std::string raw_call = std::format(
      "({}).{}({})", *receiver_or, EventMethodMemberName(m.kind), args);
  if (mir::IsSuspending(m)) return "co_await " + raw_call;
  return raw_call;
}

auto RenderCallExpr(const RenderContext& ctx, const mir::CallExpr& call)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [](const mir::SystemSubroutineCallee&) -> diag::Result<std::string> {
            return diag::Unsupported(
                diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                "system subroutine call as expression is not yet implemented "
                "in cpp emit",
                diag::UnsupportedCategory::kFeature);
          },
          [&](const mir::StructuralSubroutineRef& ref)
              -> diag::Result<std::string> {
            if (ref.hops.value != 0) {
              return diag::Unsupported(
                  diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                  "subroutine call across structural scopes is not yet "
                  "implemented in cpp emit",
                  diag::UnsupportedCategory::kFeature);
            }
            const auto& scope = ctx.StructuralScopeAtHops(
                mir::StructuralHops{.value = ref.hops.value});
            const std::string& name =
                scope.GetStructuralSubroutine(ref.subroutine).name;
            std::string args;
            for (std::size_t i = 0; i < call.arguments.size(); ++i) {
              auto arg_or = RenderExpr(ctx, ctx.Expr(call.arguments[i]));
              if (!arg_or) return std::unexpected(std::move(arg_or.error()));
              if (i != 0) args += ", ";
              args += *arg_or;
            }
            return std::format("{}({})", name, args);
          },
          [&](const mir::BuiltinMethodCallee& b) -> diag::Result<std::string> {
            return std::visit(
                Overloaded{
                    [&](const mir::EnumMethodInfo& m) {
                      return RenderEnumMethodCall(ctx, call, m);
                    },
                    [&](const mir::StringMethodInfo& m) {
                      return RenderStringMethodCall(ctx, call, m);
                    },
                    [&](const mir::EventMethodInfo& m) {
                      return RenderEventMethodCall(ctx, call, m);
                    },
                },
                b.method);
          },
      },
      call.callee);
}

auto RenderSameShapeLiteral(
    const mir::PackedArrayType& shape, std::int64_t value) -> std::string {
  return std::format(
      "lyra::value::PackedArray::FromInt({}LL, {})", value,
      RenderPackedArrayCtorArgs(shape));
}

auto RenderElementSelectExpr(
    const RenderContext& ctx, const mir::ElementSelectExpr& sel)
    -> diag::Result<std::string> {
  const auto& base_expr = ctx.Expr(sel.base_value);
  auto base_or = RenderExprNatural(ctx, base_expr);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  auto idx_or = RenderExpr(ctx, ctx.Expr(sel.index));
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const auto& base_ty = ctx.Unit().GetType(base_expr.type);
  // Unpacked vectors are zero-based; the SV declared-range translation
  // already lives in `sel.index` (HIR-to-MIR rewrites it). Packed arrays
  // keep their range-aware ElementAt path inside PackedArray.
  if (std::holds_alternative<mir::UnpackedArrayType>(base_ty.data)) {
    return std::format(
        "({})[static_cast<std::size_t>(({}).ToInt64())]", *base_or, *idx_or);
  }
  return std::format("({}).ElementAt({})", *base_or, *idx_or);
}

// Constant integer extracted from a MIR expression. Caller is responsible for
// ensuring the expression is a constant (slang guarantees this for constant
// bounds and indexed widths per LRM 11.5.1).
auto ConstantIntFromExpr(const mir::Expr& expr) -> std::int64_t {
  if (const auto* lit = std::get_if<mir::IntegerLiteral>(&expr.data)) {
    return IntegralConstantToInt64(lit->value);
  }
  throw InternalError(
      "ConstantIntFromExpr: expression is not an IntegerLiteral");
}

// Renders an LHS-shaped expression for use as an assignment target. The
// `mutate_adapter` string (typically `.Mutate(*services_)` or empty) is
// inserted immediately after the structural-var root so an observable
// partial write enters a ScopedMutation before the selector chain begins.
// Output shape:
//   <root>{<mutate_adapter>}{.ElementAt(idx) | .Slice(lsb, w)}*
auto RenderLhsExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    std::string_view mutate_adapter) -> diag::Result<std::string>;

auto RenderRangeBoundsAsSlice(
    const RenderContext& ctx, const mir::RangeBounds& bounds)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::RangeConstantBounds& b) -> diag::Result<std::string> {
            auto lsb = RenderExpr(ctx, ctx.Expr(b.lsb_expr));
            if (!lsb) return std::unexpected(std::move(lsb.error()));
            const auto msb_val = ConstantIntFromExpr(ctx.Expr(b.msb_expr));
            const auto lsb_val = ConstantIntFromExpr(ctx.Expr(b.lsb_expr));
            const auto count = static_cast<std::uint32_t>(
                (msb_val >= lsb_val ? msb_val - lsb_val : lsb_val - msb_val) +
                1);
            return std::format(".Slice({}, {}U)", *lsb, count);
          },
          [&](const mir::RangeIndexedUpBounds& b) -> diag::Result<std::string> {
            auto base = RenderExpr(ctx, ctx.Expr(b.base_index));
            if (!base) return std::unexpected(std::move(base.error()));
            const auto count = static_cast<std::uint32_t>(
                ConstantIntFromExpr(ctx.Expr(b.width)));
            return std::format(".Slice({}, {}U)", *base, count);
          },
          [&](const mir::RangeIndexedDownBounds& b)
              -> diag::Result<std::string> {
            const auto& base_mir = ctx.Expr(b.base_index);
            auto base = RenderExpr(ctx, base_mir);
            if (!base) return std::unexpected(std::move(base.error()));
            const auto& base_ty = ctx.Unit().GetType(base_mir.type);
            if (!base_ty.IsPackedArray()) {
              throw InternalError(
                  "RenderRangeBoundsAsSlice: IndexedDown base_index "
                  "type is not PackedArrayType");
            }
            const auto count = static_cast<std::uint32_t>(
                ConstantIntFromExpr(ctx.Expr(b.width)));
            const auto sub_lit = RenderSameShapeLiteral(
                base_ty.AsPackedArray(), static_cast<std::int64_t>(count) - 1);
            const auto lsb = std::format("(({}) - {})", *base, sub_lit);
            return std::format(".Slice({}, {}U)", lsb, count);
          },
      },
      bounds);
}

auto RenderLhsExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    std::string_view mutate_adapter) -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::StructuralVarRef& r) -> diag::Result<std::string> {
            auto name = RenderStructuralVarName(ctx, r);
            if (!name) return std::unexpected(std::move(name.error()));
            return *name + std::string{mutate_adapter};
          },
          [&](const mir::ProceduralVarRef& l) -> diag::Result<std::string> {
            return LookupProceduralVarName(ctx, l);
          },
          [&](const mir::ElementSelectExpr& s) -> diag::Result<std::string> {
            auto base =
                RenderLhsExpr(ctx, ctx.Expr(s.base_value), mutate_adapter);
            if (!base) return std::unexpected(std::move(base.error()));
            auto idx = RenderExpr(ctx, ctx.Expr(s.index));
            if (!idx) return std::unexpected(std::move(idx.error()));
            return std::format("{}.ElementAt({})", *base, *idx);
          },
          [&](const mir::RangeSelectExpr& s) -> diag::Result<std::string> {
            auto base =
                RenderLhsExpr(ctx, ctx.Expr(s.base_value), mutate_adapter);
            if (!base) return std::unexpected(std::move(base.error()));
            auto suffix = RenderRangeBoundsAsSlice(ctx, s.bounds);
            if (!suffix) return std::unexpected(std::move(suffix.error()));
            return *base + *suffix;
          },
          [&](const auto&) -> diag::Result<std::string> {
            throw InternalError(
                "RenderLhsExpr: expression form is not addressable; "
                "ValidateAssignableProcExpr should have rejected it");
          },
      },
      expr.data);
}

// Walks an LHS expression to its root PrimaryExpr and returns the
// StructuralVarRef there iff that's what the root is, else nullptr. Used
// to detect whether the assignment touches an observable scalar.
auto LhsRootStructuralVarForObservable(
    const RenderContext& ctx, const mir::Expr& expr)
    -> const mir::StructuralVarRef* {
  const mir::Expr* current = &expr;
  while (true) {
    const mir::Expr& e = *current;
    if (const auto* svr = std::get_if<mir::StructuralVarRef>(&e.data)) {
      return svr;
    }
    if (const auto* sel = std::get_if<mir::ElementSelectExpr>(&e.data)) {
      current = &ctx.Expr(sel->base_value);
      continue;
    }
    if (const auto* sel = std::get_if<mir::RangeSelectExpr>(&e.data)) {
      current = &ctx.Expr(sel->base_value);
      continue;
    }
    return nullptr;
  }
}

auto IsLhsBarePrimary(const mir::Expr& expr) -> bool {
  return std::holds_alternative<mir::StructuralVarRef>(expr.data) ||
         std::holds_alternative<mir::ProceduralVarRef>(expr.data);
}

// Render a compound op suffix for the SV `op=` family. Arithmetic /
// bitwise compounds use the C++ operator tokens directly because
// `PackedArray`/`PackedArrayRef`/`ScopedMutation` overload `operator+=`,
// etc. Shifts route through method-style `XxxAssign(rhs)` calls because
// the binary form is already method-style (no native C++ token for SV's
// arithmetic / logical shift distinction). Returns either the operator
// token (e.g. " += ") with caller-supplied rhs appended, or the full
// method form `.XxxAssign(rhs)`.
auto RenderCompoundAssign(
    mir::BinaryOp op, const std::string& chain, const std::string& rhs)
    -> std::string {
  switch (op) {
    case mir::BinaryOp::kAdd:
      return chain + " += " + rhs;
    case mir::BinaryOp::kSub:
      return chain + " -= " + rhs;
    case mir::BinaryOp::kMul:
      return chain + " *= " + rhs;
    case mir::BinaryOp::kDiv:
      return chain + " /= " + rhs;
    case mir::BinaryOp::kMod:
      return chain + " %= " + rhs;
    case mir::BinaryOp::kBitwiseAnd:
      return chain + " &= " + rhs;
    case mir::BinaryOp::kBitwiseOr:
      return chain + " |= " + rhs;
    case mir::BinaryOp::kBitwiseXor:
      return chain + " ^= " + rhs;
    case mir::BinaryOp::kShiftLeft:
      return chain + ".ShiftLeftAssign(" + rhs + ")";
    case mir::BinaryOp::kLogicalShiftRight:
      return chain + ".LogicalShiftRightAssign(" + rhs + ")";
    case mir::BinaryOp::kArithmeticShiftRight:
      return chain + ".ArithmeticShiftRightAssign(" + rhs + ")";
    default:
      throw InternalError(
          "RenderCompoundAssign: BinaryOp is not a legal SV compound "
          "assignment operator (LRM 11.4 only allows arithmetic, bitwise, "
          "and shift compounds)");
  }
}

auto RenderAssignExpr(const RenderContext& ctx, const mir::AssignExpr& a)
    -> diag::Result<std::string> {
  auto value_or = RenderExpr(ctx, ctx.Expr(a.value));
  if (!value_or) return std::unexpected(std::move(value_or.error()));

  const mir::Expr& lhs_expr = ctx.Expr(a.target);
  const mir::StructuralVarRef* observable_root =
      LhsRootStructuralVarForObservable(ctx, lhs_expr);
  const bool observable = observable_root != nullptr && [&] {
    const auto& var_decl =
        ctx.StructuralScope().GetStructuralVar(observable_root->var);
    return IsObservableScalarType(ctx.Unit().GetType(var_decl.type));
  }();

  // Whole-var write: route observable structural targets through WriteVar
  // (which fires subscribers), procedural locals get a direct C++ assignment.
  if (IsLhsBarePrimary(lhs_expr)) {
    auto root_or = RenderLhsExpr(ctx, lhs_expr, std::string_view{});
    if (!root_or) return std::unexpected(std::move(root_or.error()));
    if (a.compound_op.has_value()) {
      // For observable whole-var compound we still route through Mutate so
      // the destructor commit fires subscribers exactly once -- the
      // ScopedMutation overloads `op=` to mutate the snapshot in place.
      // Procedural-local compounds operate on the underlying PackedArray
      // directly via its own `op=` overloads.
      const std::string chain =
          observable ? *root_or + ".Mutate(*services_)" : *root_or;
      return "(" + RenderCompoundAssign(*a.compound_op, chain, *value_or) + ")";
    }
    if (observable) {
      return "lyra::runtime::WriteVar(*services_, " + *root_or + ", " +
             *value_or + ")";
    }
    return "(" + *root_or + " = " + *value_or + ")";
  }

  // Selector chain: compose PackedArrayRef method calls. Per-layer element
  // bit widths are derived by the runtime PackedArray / PackedArrayRef from
  // their own dim stack; render emits source-form positions only.
  //
  // Procedural-local target: chain mutates the variable in place. Chain base
  // is the variable name itself.
  // Observable target: enter a partial-write scope via `Var<T>::Mutate(svc)`,
  // which returns a `ScopedMutation` RAII handle that snapshots the current
  // value. The chain runs against the snapshot via the same forwarding
  // methods as on PackedArray; when the full expression ends, the handle's
  // destructor commits the snapshot through `WriteVar` so subscribers fire.
  // Same shape inside or outside an NBA closure body -- no nested lambda.
  const std::string_view mutate_adapter =
      observable ? std::string_view{".Mutate(*services_)"} : std::string_view{};
  auto chain_or = RenderLhsExpr(ctx, lhs_expr, mutate_adapter);
  if (!chain_or) return std::unexpected(std::move(chain_or.error()));
  if (a.compound_op.has_value()) {
    return "(" + RenderCompoundAssign(*a.compound_op, *chain_or, *value_or) +
           ")";
  }
  return *chain_or + " = " + *value_or;
}

auto RenderIncDecExpr(const RenderContext& ctx, const mir::IncDecExpr& inc)
    -> diag::Result<std::string> {
  const mir::Expr& target_expr = ctx.Expr(inc.target);
  const mir::StructuralVarRef* observable_root =
      LhsRootStructuralVarForObservable(ctx, target_expr);
  const bool observable = observable_root != nullptr && [&] {
    const auto& var_decl =
        ctx.StructuralScope().GetStructuralVar(observable_root->var);
    return IsObservableScalarType(ctx.Unit().GetType(var_decl.type));
  }();

  std::string lhs;
  if (IsLhsBarePrimary(target_expr)) {
    auto root_or = RenderLhsExpr(ctx, target_expr, std::string_view{});
    if (!root_or) return std::unexpected(std::move(root_or.error()));
    lhs = observable ? *root_or + ".Mutate(*services_)" : *root_or;
  } else {
    const std::string_view mutate_adapter =
        observable ? std::string_view{".Mutate(*services_)"}
                   : std::string_view{};
    auto chain_or = RenderLhsExpr(ctx, target_expr, mutate_adapter);
    if (!chain_or) return std::unexpected(std::move(chain_or.error()));
    lhs = *std::move(chain_or);
  }

  switch (inc.op) {
    case mir::IncDecOp::kPreInc:
      return "(++" + lhs + ")";
    case mir::IncDecOp::kPostInc:
      return "(" + lhs + "++)";
    case mir::IncDecOp::kPreDec:
      return "(--" + lhs + ")";
    case mir::IncDecOp::kPostDec:
      return "(" + lhs + "--)";
  }
  throw InternalError("RenderIncDecExpr: unknown IncDecOp");
}

auto RenderRangeSelectExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::RangeSelectExpr& sel) -> diag::Result<std::string> {
  const auto& operand_expr = ctx.Expr(sel.base_value);
  auto base_or = RenderExprNatural(ctx, operand_expr);
  if (!base_or) return std::unexpected(std::move(base_or.error()));

  const auto& result_ty = ctx.Unit().GetType(expr.type);
  if (!result_ty.IsPackedArray()) {
    throw InternalError(
        "RenderRangeSelectExpr: result must be PackedArrayType");
  }
  // The slice's element-count is the outer dim of the result type. For a
  // 1D operand this is bit count; for a multi-dim operand it is the
  // selected outer-element count. PackedArray's `Slice` scales internally.
  if (result_ty.AsPackedArray().dims.empty()) {
    throw InternalError(
        "RenderRangeSelectExpr: result PackedArrayType has no dims");
  }
  const auto& result_pa = result_ty.AsPackedArray();
  const auto count =
      static_cast<std::uint32_t>(result_pa.dims.front().ElementCount());

  // PackedArray::Slice() always produces an unsigned result (LRM 7.4.1
  // part-select default). When the MIR result type carries signed semantics
  // -- e.g. member access of a `logic signed` packed-struct/union field --
  // re-tag the slice via ConvertFrom so a downstream sign-extending
  // conversion sees the correct source signedness.
  const auto wrap_for_sign = [&](std::string slice_expr) -> std::string {
    if (result_pa.signedness == mir::Signedness::kSigned) {
      return std::format(
          "lyra::value::PackedArray::ConvertFrom({}, {})", slice_expr,
          RenderPackedArrayCtorArgs(result_pa));
    }
    return slice_expr;
  };

  return std::visit(
      Overloaded{
          [&](const mir::RangeConstantBounds& b) -> diag::Result<std::string> {
            auto lsb_or = RenderExpr(ctx, ctx.Expr(b.lsb_expr));
            if (!lsb_or) return std::unexpected(std::move(lsb_or.error()));
            return wrap_for_sign(
                std::format("({}).Slice({}, {}U)", *base_or, *lsb_or, count));
          },
          [&](const mir::RangeIndexedUpBounds& b) -> diag::Result<std::string> {
            auto base = RenderExpr(ctx, ctx.Expr(b.base_index));
            if (!base) return std::unexpected(std::move(base.error()));
            return wrap_for_sign(
                std::format("({}).Slice({}, {}U)", *base_or, *base, count));
          },
          [&](const mir::RangeIndexedDownBounds& b)
              -> diag::Result<std::string> {
            const auto& base_mir = ctx.Expr(b.base_index);
            auto base = RenderExpr(ctx, base_mir);
            if (!base) return std::unexpected(std::move(base.error()));
            const auto& base_ty = ctx.Unit().GetType(base_mir.type);
            if (!base_ty.IsPackedArray()) {
              throw InternalError(
                  "RenderRangeSelectExpr: IndexedDown base_index type is not "
                  "PackedArrayType");
            }
            const auto sub_lit = RenderSameShapeLiteral(
                base_ty.AsPackedArray(), static_cast<std::int64_t>(count) - 1);
            const auto lsb = std::format("(({}) - {})", *base, sub_lit);
            return wrap_for_sign(
                std::format("({}).Slice({}, {}U)", *base_or, lsb, count));
          },
      },
      sel.bounds);
}

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

auto RenderConcatExpr(
    const RenderContext& ctx, const mir::Expr& expr, const mir::ConcatExpr& c)
    -> diag::Result<std::string> {
  if (c.operands.empty()) {
    throw InternalError("RenderConcatExpr: hir lowering produced empty concat");
  }
  const auto& result_ty = ctx.Unit().GetType(expr.type);
  if (result_ty.IsIntegralPacked()) {
    std::string out = "lyra::value::PackedArray::Concat(";
    for (std::size_t i = 0; i < c.operands.size(); ++i) {
      auto rendered = RenderExpr(ctx, ctx.Expr(c.operands[i]));
      if (!rendered) return std::unexpected(std::move(rendered.error()));
      if (i != 0) out += ", ";
      out += *rendered;
    }
    out += ")";
    return out;
  }
  // String mode (LRM 6.16): operator+ on std::string joins contents.
  std::string out = "(";
  for (std::size_t i = 0; i < c.operands.size(); ++i) {
    auto rendered = RenderExpr(ctx, ctx.Expr(c.operands[i]));
    if (!rendered) return std::unexpected(std::move(rendered.error()));
    if (i != 0) out += " + ";
    out += *rendered;
  }
  out += ")";
  return out;
}

auto RenderArrayLiteralExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::ArrayLiteralExpr& a) -> diag::Result<std::string> {
  auto vec_type_or =
      RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), expr.type);
  if (!vec_type_or) return std::unexpected(std::move(vec_type_or.error()));
  std::string out = *vec_type_or + "{";
  for (std::size_t i = 0; i < a.elements.size(); ++i) {
    auto rendered = RenderExpr(ctx, ctx.Expr(a.elements[i]));
    if (!rendered) return std::unexpected(std::move(rendered.error()));
    if (i != 0) out += ", ";
    out += *rendered;
  }
  out += "}";
  return out;
}

auto RenderReplicationExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::ReplicationExpr& r) -> diag::Result<std::string> {
  auto count = RenderExpr(ctx, ctx.Expr(r.count));
  if (!count) return std::unexpected(std::move(count.error()));
  auto concat = RenderExpr(ctx, ctx.Expr(r.concat));
  if (!concat) return std::unexpected(std::move(concat.error()));
  const auto& count_ty = ctx.Unit().GetType(ctx.Expr(r.count).type);
  std::string count_text = count_ty.IsIntegralPacked()
                               ? std::format("({}).ToInt64()", *count)
                               : *count;
  const auto& result_ty = ctx.Unit().GetType(expr.type);
  if (result_ty.IsIntegralPacked()) {
    return std::format(
        "lyra::value::PackedArray::Replicate({}, "
        "static_cast<std::uint64_t>({}))",
        *concat, count_text);
  }
  return std::format(
      "lyra::value::ReplicateString({}, {})", *concat, count_text);
}

auto ProducesPackedArrayRef(const mir::Expr& expr) -> bool {
  return std::holds_alternative<mir::ElementSelectExpr>(expr.data) ||
         std::holds_alternative<mir::RangeSelectExpr>(expr.data);
}

}  // namespace

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  auto rendered_or = RenderExprNatural(ctx, expr);
  if (!rendered_or) return rendered_or;
  if (ProducesPackedArrayRef(expr)) {
    return "(" + *std::move(rendered_or) + ").Clone()";
  }
  return rendered_or;
}

auto RenderExprNatural(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::IntegerLiteral& lit) -> diag::Result<std::string> {
            return RenderIntegerLiteralExpr(ctx, expr, lit);
          },
          [&](const mir::StringLiteral& s) -> diag::Result<std::string> {
            return RenderSvStringLiteral(s.value);
          },
          [](const mir::TimeLiteral&) -> diag::Result<std::string> {
            return diag::Unsupported(
                diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                "TimeLiteral is not yet implemented in cpp emit",
                diag::UnsupportedCategory::kFeature);
          },
          [&](const mir::RealLiteral& r) -> diag::Result<std::string> {
            return RenderRealLiteralExpr(ctx, expr, r);
          },
          [&](const mir::StructuralParamRef& r) -> diag::Result<std::string> {
            return RenderStructuralParamExpr(ctx, r);
          },
          [&](const mir::StructuralVarRef& m) -> diag::Result<std::string> {
            return RenderStructuralVarReadExpr(ctx, expr, m);
          },
          [&](const mir::ProceduralVarRef& l) -> diag::Result<std::string> {
            return LookupProceduralVarName(ctx, l);
          },
          [&](const mir::UnaryExpr& u) -> diag::Result<std::string> {
            return RenderUnaryExpr(ctx, expr, u);
          },
          [&](const mir::BinaryExpr& b) -> diag::Result<std::string> {
            return RenderBinaryExpr(ctx, expr, b);
          },
          [&](const mir::ConditionalExpr& c) -> diag::Result<std::string> {
            return RenderConditionalExpr(ctx, c);
          },
          [&](const mir::AssignExpr& a) -> diag::Result<std::string> {
            return RenderAssignExpr(ctx, a);
          },
          [&](const mir::IncDecExpr& inc) -> diag::Result<std::string> {
            return RenderIncDecExpr(ctx, inc);
          },
          [&](const mir::ConversionExpr& cv) -> diag::Result<std::string> {
            return RenderConversionExpr(ctx, expr, cv);
          },
          [&](const mir::CallExpr& call) -> diag::Result<std::string> {
            return RenderCallExpr(ctx, call);
          },
          [&](const mir::RuntimeCallExpr& rc) -> diag::Result<std::string> {
            return RenderRuntimeCallExpr(ctx, rc);
          },
          [&](const mir::ClosureExpr& cl) -> diag::Result<std::string> {
            return RenderClosureExpr(ctx, cl);
          },
          [&](const mir::ElementSelectExpr& sel) -> diag::Result<std::string> {
            return RenderElementSelectExpr(ctx, sel);
          },
          [&](const mir::RangeSelectExpr& sel) -> diag::Result<std::string> {
            return RenderRangeSelectExpr(ctx, expr, sel);
          },
          [&](const mir::ConcatExpr& c) -> diag::Result<std::string> {
            return RenderConcatExpr(ctx, expr, c);
          },
          [&](const mir::ReplicationExpr& r) -> diag::Result<std::string> {
            return RenderReplicationExpr(ctx, expr, r);
          },
          [&](const mir::ArrayLiteralExpr& a) -> diag::Result<std::string> {
            return RenderArrayLiteralExpr(ctx, expr, a);
          },
      },
      expr.data);
}

auto RenderConditionAsBool(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  auto text_or = RenderExpr(ctx, expr);
  if (!text_or) return std::unexpected(std::move(text_or.error()));
  // PackedArray's `explicit operator bool` fires in any boolean context (if /
  // while / for / ternary cond / `&&` / `||` / `!`), so no wrapping needed.
  return *text_or;
}

}  // namespace lyra::backend::cpp

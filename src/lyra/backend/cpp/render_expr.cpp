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
#include "lyra/support/system_subroutine.hpp"

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
    // 32-bit signed has the named shorthands Int (2-state) / Integer
    // (4-state); both read far better than the explicit FromInt for the
    // ubiquitous `int` / `integer` literal.
    if (pa.BitWidth() == 32U && pa.signedness == mir::Signedness::kSigned) {
      return std::format(
          "lyra::value::PackedArray::{}({})",
          pa.atom == mir::BitAtom::kBit ? "Int" : "Integer", value);
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
  const std::string& name =
      ctx.StructuralScope().GetStructuralParam(r.param).name;
  return "self->" + name;
}

auto LookupProceduralVarName(
    const RenderContext& ctx, const mir::ProceduralVarRef& ref) -> std::string {
  return ctx.ProceduralScopeAtHops(ref.hops).vars.at(ref.var.value).name;
}

// True iff the procedural var is a `ref` / `const ref` formal, rendered as a
// `Ref<T>` whose read is `.Get()` and whose write is `.Set(Services(), ...)`
// (LRM 13.5.2), the same surface as a structural Var.
auto IsReferenceProceduralVar(
    const RenderContext& ctx, const mir::ProceduralVarRef& ref) -> bool {
  return ctx.ProceduralScopeAtHops(ref.hops).vars.at(ref.var.value).binding ==
         mir::VariableBinding::kReference;
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
  return "self->" + ctx.StructuralScope().GetStructuralVar(ref.var).name;
}

namespace {

// True iff the type exposes the observable cell surface (`Set` / `Get` /
// `Mutate`) to the backend -- an explicit `mir::ObservableType` wrapper or an
// `mir::ExternalRefType` (intrinsic upward-reference cell). Transitional
// residue: every callsite uses the result to inject a `.Get()` / `.Mutate()`
// at render time, which the Phase D follow-up retires by lifting those calls
// into MIR.
[[nodiscard]] auto IsObservableCell(const mir::Type& ty) -> bool {
  return std::holds_alternative<mir::ObservableType>(ty.data) ||
         std::holds_alternative<mir::ExternalRefType>(ty.data);
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

// LRM 11.2.2 + 11.4.5: aggregate equality / inequality / case-equality /
// case-inequality. Both `UnpackedArray` and `DynamicArray` overload `==` /
// `!=` to return a 1-bit `PackedArray` (X / Z propagating) and expose
// `CaseEqual` as a method returning a 1-bit `PackedArray` (0 / 1 only). The
// dynamic-array overloads additionally short-circuit on runtime size
// mismatch -> 0 (industry convention, LRM 11.2.2 is silent). Relational
// operators are not defined on aggregate operands.
auto RenderBinaryOpArray(
    mir::BinaryOp op, const std::string& lhs, const std::string& rhs)
    -> diag::Result<std::string> {
  switch (op) {
    case mir::BinaryOp::kEquality:
      return "(" + lhs + " == " + rhs + ")";
    case mir::BinaryOp::kInequality:
      return "(" + lhs + " != " + rhs + ")";
    case mir::BinaryOp::kCaseEquality:
      return "(" + lhs + ").CaseEqual(" + rhs + ")";
    case mir::BinaryOp::kCaseInequality:
      return "!(" + lhs + ").CaseEqual(" + rhs + ")";
    default:
      break;
  }
  return diag::Unsupported(
      diag::DiagCode::kCppEmitExpressionFormNotImplemented,
      "binary operator is not legal on aggregate operands per LRM 11.2.2",
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
  const bool lhs_is_array =
      std::holds_alternative<mir::UnpackedArrayType>(lhs_ty.data) ||
      std::holds_alternative<mir::DynamicArrayType>(lhs_ty.data);
  const bool rhs_is_array =
      std::holds_alternative<mir::UnpackedArrayType>(rhs_ty.data) ||
      std::holds_alternative<mir::DynamicArrayType>(rhs_ty.data);
  if (lhs_is_array && rhs_is_array) {
    return RenderBinaryOpArray(b.op, *lhs_or, *rhs_or);
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
  const auto& src_ty = ctx.Unit().GetType(src_expr.type);
  const auto& dst_ty = ctx.Unit().GetType(expr.type);
  auto operand_or = RenderExpr(ctx, src_expr);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));

  // Dispatch by (source category, destination category). Each branch is
  // one (src, dst) emit form.
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
  // LRM 21.3.4.3: $sscanf unpacked-array-of-byte source lifts to string.
  // The lowering layer inserts the ConversionExpr at the call boundary,
  // leaving this site as the single emit point.
  if (src_ty.Kind() == mir::TypeKind::kUnpackedArray &&
      dst_ty.Kind() == mir::TypeKind::kString) {
    return std::format(
        "lyra::value::String::FromByteArray({})", *std::move(operand_or));
  }
  // LRM 5.9 / 21.3.4.3: packed integral source / format lifts to string by
  // viewing the bit vector as an MSB-first byte sequence. The $sscanf /
  // $fscanf body emits an unknown-bits guard before this conversion runs,
  // so x/z is never observed in the scan path; $display "%s" on an x/z-
  // bearing packed operand sees `'\0'` per the policy documented at the
  // String::FromPackedArray declaration.
  //
  // SV string literals are typed by slang as `bit[N:0]` packed (LRM 5.9)
  // but the emitter renders them directly as `lyra::value::String{"..."}`,
  // so the operand at C++ level is already a String when the source is a
  // StringLiteral node -- the conversion is a slang-typing artifact, not a
  // real byte-unpacking need. Skip the wrap in that case; the rendered
  // operand is the lift's result.
  if (src_ty.IsIntegralPacked() && dst_ty.Kind() == mir::TypeKind::kString) {
    if (std::holds_alternative<mir::StringLiteral>(src_expr.data)) {
      return *std::move(operand_or);
    }
    return std::format(
        "lyra::value::String::FromPackedArray({})", *std::move(operand_or));
  }
  // Identity / no-op rendering: the conversion exists in MIR but the
  // operand's already-rendered C++ matches the destination shape. Covers
  // string -> string identity, and slang's `bit[N-1:0]` string literal ->
  // `string` lift where RenderExpr already returns a `value::String{...}`.
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

auto ArrayMethodMemberName(mir::ArrayMethodKind k) -> std::string_view {
  switch (k) {
    case mir::ArrayMethodKind::kSize:
      return "Size";
    case mir::ArrayMethodKind::kDelete:
      return "Delete";
    case mir::ArrayMethodKind::kReverse:
      return "Reverse";
    case mir::ArrayMethodKind::kSort:
      return "Sort";
    case mir::ArrayMethodKind::kRsort:
      return "Rsort";
    case mir::ArrayMethodKind::kSum:
      return "Sum";
    case mir::ArrayMethodKind::kProduct:
      return "Product";
    case mir::ArrayMethodKind::kAnd:
      return "And";
    case mir::ArrayMethodKind::kOr:
      return "Or";
    case mir::ArrayMethodKind::kXor:
      return "Xor";
    case mir::ArrayMethodKind::kFind:
      return "Find";
    case mir::ArrayMethodKind::kFindIndex:
      return "FindIndex";
    case mir::ArrayMethodKind::kFindFirst:
      return "FindFirst";
    case mir::ArrayMethodKind::kFindFirstIndex:
      return "FindFirstIndex";
    case mir::ArrayMethodKind::kFindLast:
      return "FindLast";
    case mir::ArrayMethodKind::kFindLastIndex:
      return "FindLastIndex";
    case mir::ArrayMethodKind::kMin:
      return "Min";
    case mir::ArrayMethodKind::kMax:
      return "Max";
    case mir::ArrayMethodKind::kUnique:
      return "Unique";
    case mir::ArrayMethodKind::kUniqueIndex:
      return "UniqueIndex";
  }
  throw InternalError("ArrayMethodMemberName: unknown kind");
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

auto RenderMethodCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    std::string_view member_name, bool mutates) -> diag::Result<std::string>;

// LRM 6.19.5: first / last / num are static on the enum class (no receiver);
// next / prev dispatch on the receiver and render through the generic method
// call. num() yields the SV `int` shape from the runtime, so no wrap here; the
// optional next / prev step is a plain SV argument the runtime takes by value.
auto RenderEnumMethodCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    const mir::EnumMethodInfo& m) -> diag::Result<std::string> {
  const auto member = EnumMethodMemberName(m.kind);
  const bool is_static = m.kind == mir::EnumMethodKind::kFirst ||
                         m.kind == mir::EnumMethodKind::kLast ||
                         m.kind == mir::EnumMethodKind::kNum;
  if (is_static) {
    return std::format(
        "{}::{}()", RenderEnumClassName(ctx.StructuralScope(), m.enum_type),
        member);
  }
  return RenderMethodCall(ctx, call, member, false);
}

// LRM 6.16: `Putc` and the integer-to-string family (`Itoa`, `Hextoa`,
// `Octtoa`, `Bintoa`, `Realtoa`) mutate the receiver string in place.
auto StringMethodMutatesReceiver(mir::StringMethodKind k) -> bool {
  return k == mir::StringMethodKind::kPutc ||
         k == mir::StringMethodKind::kItoa ||
         k == mir::StringMethodKind::kHextoa ||
         k == mir::StringMethodKind::kOcttoa ||
         k == mir::StringMethodKind::kBintoa ||
         k == mir::StringMethodKind::kRealtoa;
}

auto IsLhsBarePrimary(const mir::Expr& expr) -> bool;

// Render the receiver of an instance-method call. When the method mutates the
// receiver in place and the receiver is an observable cell, the call must
// route through `Var<T>::Mutate(svc)` so the destructor commits exactly once
// and subscribers fire; the snapshot is reached through `operator->`. Returns
// the receiver text and a flag selecting the C++ member-access syntax (`->`
// vs `.`).
struct MethodReceiverText {
  std::string text;
  std::string_view sep;
};
auto RenderMethodReceiver(
    const RenderContext& ctx, const mir::Expr& receiver_expr, bool mutates)
    -> diag::Result<MethodReceiverText> {
  if (mutates && LhsRootIsObservableScalar(ctx, receiver_expr)) {
    auto lhs_or =
        RenderLhsExpr(ctx, receiver_expr, ".Mutate(self->Services())");
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    // The Mutate adapter yields a `ScopedMutation` whose members are reached
    // via `operator->`. Selector chains past it (`.ElementRef(...)`,
    // `.ElementAt(...)`) decay back to the wrapped value's reference type, so
    // the trailing method call dots through C++ as usual.
    const std::string_view sep = IsLhsBarePrimary(receiver_expr) ? "->" : ".";
    return MethodReceiverText{.text = std::move(*lhs_or), .sep = sep};
  }
  auto receiver_or = RenderExpr(ctx, receiver_expr);
  if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
  return MethodReceiverText{.text = std::move(*receiver_or), .sep = "."};
}

// A method call renders generically as `(receiver).name(args)`: the receiver is
// arguments[0]; every following argument renders uniformly. Nothing here is
// method-family specific -- the only per-call input beyond the operands is the
// member name and whether the call mutates its receiver. Any engine handle a
// method needs, and any representation shaping of arguments or result, lives in
// the runtime method's signature; the backend reads the call and emits it.
// When `mutates` is set and the receiver is an observable cell, the receiver
// routes through `Var<T>::Mutate(svc)` so the destructor commits once.
auto RenderMethodCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    std::string_view member_name, bool mutates) -> diag::Result<std::string> {
  if (call.arguments.empty()) {
    throw InternalError(
        "RenderMethodCall: a method call expects a receiver argument");
  }
  auto recv_or =
      RenderMethodReceiver(ctx, ctx.Expr(call.arguments[0]), mutates);
  if (!recv_or) return std::unexpected(std::move(recv_or.error()));
  std::string args;
  for (std::size_t i = 1; i < call.arguments.size(); ++i) {
    auto arg_or = RenderExpr(ctx, ctx.Expr(call.arguments[i]));
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    if (i != 1) args += ", ";
    args += *std::move(arg_or);
  }
  return std::format(
      "({}){}{}({})", recv_or->text, recv_or->sep, member_name, args);
}

// LRM 7.5.3 / 7.12.1: array methods that mutate the receiver in place. The
// rest of the family is read-only (queries, reductions, locators).
auto ArrayMethodMutatesReceiver(mir::ArrayMethodKind k) -> bool {
  return k == mir::ArrayMethodKind::kDelete ||
         k == mir::ArrayMethodKind::kReverse ||
         k == mir::ArrayMethodKind::kSort || k == mir::ArrayMethodKind::kRsort;
}

auto QueueMethodMemberName(mir::QueueMethodKind k) -> std::string_view {
  switch (k) {
    case mir::QueueMethodKind::kSize:
      return "Size";
    case mir::QueueMethodKind::kInsert:
      return "Insert";
    case mir::QueueMethodKind::kDelete:
      return "Delete";
    case mir::QueueMethodKind::kPopFront:
      return "PopFront";
    case mir::QueueMethodKind::kPopBack:
      return "PopBack";
    case mir::QueueMethodKind::kPushFront:
      return "PushFront";
    case mir::QueueMethodKind::kPushBack:
      return "PushBack";
    case mir::QueueMethodKind::kElementAt:
      return "ElementAt";
    case mir::QueueMethodKind::kWriteRef:
      return "WriteRef";
    case mir::QueueMethodKind::kSlice:
      return "Slice";
  }
  throw InternalError("QueueMethodMemberName: unknown kind");
}

// The queue methods that write the receiver (LRM 7.10.2 insert / delete / pop /
// push, and the LRM 7.10.1 element-write `WriteRef`) take the mutating receiver
// path; the read accessors (`Size`, `ElementAt`, `Slice`) do not.
auto QueueMethodMutatesReceiver(mir::QueueMethodKind k) -> bool {
  switch (k) {
    case mir::QueueMethodKind::kSize:
    case mir::QueueMethodKind::kElementAt:
    case mir::QueueMethodKind::kSlice:
      return false;
    case mir::QueueMethodKind::kInsert:
    case mir::QueueMethodKind::kDelete:
    case mir::QueueMethodKind::kPopFront:
    case mir::QueueMethodKind::kPopBack:
    case mir::QueueMethodKind::kPushFront:
    case mir::QueueMethodKind::kPushBack:
    case mir::QueueMethodKind::kWriteRef:
      return true;
  }
  throw InternalError("QueueMethodMutatesReceiver: unknown kind");
}

auto AssociativeMethodMemberName(mir::AssociativeMethodKind k)
    -> std::string_view {
  switch (k) {
    case mir::AssociativeMethodKind::kNum:
    case mir::AssociativeMethodKind::kSize:
      return "Size";
    case mir::AssociativeMethodKind::kExists:
      return "Exists";
    case mir::AssociativeMethodKind::kDelete:
      return "Delete";
    case mir::AssociativeMethodKind::kFirst:
    case mir::AssociativeMethodKind::kLast:
    case mir::AssociativeMethodKind::kNext:
    case mir::AssociativeMethodKind::kPrev:
      throw InternalError(
          "AssociativeMethodMemberName: traversal methods render as runtime "
          "calls, not member calls");
  }
  throw InternalError("AssociativeMethodMemberName: unknown kind");
}

auto AssociativeTraversalFunctionName(mir::AssociativeMethodKind k)
    -> std::optional<std::string_view> {
  switch (k) {
    case mir::AssociativeMethodKind::kFirst:
      return "AssocFirst";
    case mir::AssociativeMethodKind::kLast:
      return "AssocLast";
    case mir::AssociativeMethodKind::kNext:
      return "AssocNext";
    case mir::AssociativeMethodKind::kPrev:
      return "AssocPrev";
    case mir::AssociativeMethodKind::kNum:
    case mir::AssociativeMethodKind::kSize:
    case mir::AssociativeMethodKind::kExists:
    case mir::AssociativeMethodKind::kDelete:
      return std::nullopt;
  }
  throw InternalError("AssociativeTraversalFunctionName: unknown kind");
}

// LRM 7.9.4 -- 7.9.7 traversal: `m.first(idx)` etc. The index argument is a
// `ref` that the method writes the visited key into and whose observable
// update event must fire (LRM 4.3), so it lowers to a runtime call carrying
// the index lvalue as a `Ref<K>` -- the same by-reference shape a user `ref`
// argument uses -- and `Services()` as the leading argument. The receiver is
// read-only (traversal methods are `const`): a plain read renders it through
// `Var<T>::Get` for observable members and through `Read(...)` for nested
// receivers such as `mm[i]`, so the call lands on the stored sub-map without a
// detached clone.
auto RenderAssociativeTraversalCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    std::string_view function_name) -> diag::Result<std::string> {
  if (call.arguments.size() != 2) {
    throw InternalError(
        "RenderAssociativeTraversalCall: traversal method expects a receiver "
        "and an index argument");
  }
  auto receiver_or = RenderExpr(ctx, ctx.Expr(call.arguments[0]));
  if (!receiver_or) {
    return std::unexpected(std::move(receiver_or.error()));
  }
  const mir::Expr& index = ctx.Expr(call.arguments[1]);
  auto index_or = RenderLhsExpr(ctx, index, std::string_view{});
  if (!index_or) {
    return std::unexpected(std::move(index_or.error()));
  }
  auto key_type_or =
      RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), index.type);
  if (!key_type_or) {
    return std::unexpected(std::move(key_type_or.error()));
  }
  return std::format(
      "lyra::runtime::{}(self->Services(), {}, lyra::runtime::Ref<{}>({}))",
      function_name, *receiver_or, *key_type_or, *index_or);
}

// LRM 7.9 associative-array methods. The receiver is arguments[0] and the key
// (exists / delete-by-index) follows as a real C++ argument, so the overload
// set on `AssociativeArray<K, V>` resolves arity (`Delete()` vs `Delete(key)`).
auto RenderAssociativeMethodCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    const mir::AssociativeMethodInfo& m) -> diag::Result<std::string> {
  if (auto traversal = AssociativeTraversalFunctionName(m.kind)) {
    return RenderAssociativeTraversalCall(ctx, call, *traversal);
  }
  // LRM 7.9.2: only `delete` mutates the associative array; the rest are
  // read-only queries (traversal methods are routed above through a runtime
  // shim).
  const bool mutates = m.kind == mir::AssociativeMethodKind::kDelete;
  return RenderMethodCall(
      ctx, call, AssociativeMethodMemberName(m.kind), mutates);
}

// Side-effect-free per-value queries. Dispatch by the receiver's MIR
// type because the answer is type-static for everything except 4-state
// integral packed: a string (LRM 6.16) and a byte-element unpacked array
// have no unknown plane, and a 2-state packed has its unknown plane fixed
// at zero. Emitting `Bit(false)` for those cases keeps the closure-IIFE
// body shape uniform (`if (.IsUnknown()) return -1;`) while making the
// guard a trivial constant the C++ optimizer dead-code-eliminates.
auto RenderValueMethodCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    const mir::ValueMethodInfo& m) -> diag::Result<std::string> {
  if (call.arguments.empty()) {
    throw InternalError(
        "RenderValueMethodCall: value method expects a receiver argument");
  }
  const mir::Expr& receiver = ctx.Expr(call.arguments[0]);
  switch (m.kind) {
    case mir::ValueMethodKind::kIsUnknown: {
      const auto& ty = ctx.Unit().GetType(receiver.type);
      if (ty.IsIntegralPacked() && ty.AsIntegralPacked().IsFourState()) {
        auto receiver_or = RenderExpr(ctx, receiver);
        if (!receiver_or) {
          return std::unexpected(std::move(receiver_or.error()));
        }
        // Runtime helper retains its historical `HasUnknown` spelling;
        // the MIR enum tracks LRM 20.9 `$isunknown` naming.
        return std::format(
            "lyra::value::PackedArray::Bit(({}).HasUnknown())", *receiver_or);
      }
      return std::string{"lyra::value::PackedArray::Bit(false)"};
    }
  }
  throw InternalError("RenderValueMethodCall: unknown kind");
}

// `self.Services()` -- reaches the engine facade from the scope handle. The
// receiver (arguments[0]) is the `self` pointer, so the call is `->`. This is
// the engine handle every runtime-effect call threads as a plain argument.
auto RenderScopeMethodCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    const mir::ScopeMethodInfo& m) -> diag::Result<std::string> {
  if (call.arguments.empty()) {
    throw InternalError(
        "RenderScopeMethodCall: scope method expects a receiver argument");
  }
  auto receiver_or = RenderExpr(ctx, ctx.Expr(call.arguments[0]));
  if (!receiver_or) {
    return std::unexpected(std::move(receiver_or.error()));
  }
  switch (m.kind) {
    case mir::ScopeMethodKind::kServices:
      return std::format("({})->Services()", *receiver_or);
  }
  throw InternalError("RenderScopeMethodCall: unknown ScopeMethodKind");
}

// The C++ runtime entry this backend realizes a system subroutine with. Pure
// representation -- this backend's spelling of the stated id; a LIR -> LLVM
// backend resolves the same id to its own entry. An id whose family is not yet
// on the generic-call shape returns a feature diagnostic.
auto RenderSystemSubroutineEntryName(const support::SystemSubroutineDesc& desc)
    -> diag::Result<std::string_view> {
  return std::visit(
      Overloaded{
          [](const support::TerminationSystemSubroutineInfo&)
              -> diag::Result<std::string_view> {
            return std::string_view{"lyra::runtime::Finish"};
          },
          [](const support::PrintSystemSubroutineInfo& print)
              -> diag::Result<std::string_view> {
            if (print.sink_kind == support::PrintSinkKind::kStdout) {
              return print.append_newline
                         ? std::string_view{"lyra::runtime::LyraDisplay"}
                         : std::string_view{"lyra::runtime::LyraWrite"};
            }
            return print.append_newline
                       ? std::string_view{"lyra::runtime::LyraFDisplay"}
                       : std::string_view{"lyra::runtime::LyraFWrite"};
          },
          [](const support::DiagnosticSystemSubroutineInfo& diagnostic)
              -> diag::Result<std::string_view> {
            switch (diagnostic.severity) {
              case support::DiagnosticSeverityKind::kInfo:
                return std::string_view{"lyra::runtime::LyraInfo"};
              case support::DiagnosticSeverityKind::kWarning:
                return std::string_view{"lyra::runtime::LyraWarning"};
              case support::DiagnosticSeverityKind::kError:
                return std::string_view{"lyra::runtime::LyraError"};
            }
            throw InternalError(
                "RenderSystemSubroutineEntryName: unknown DiagnosticSeverity");
          },
          [](const support::TimeSystemSubroutineInfo& time)
              -> diag::Result<std::string_view> {
            switch (time.kind) {
              case support::TimeKind::kTime:
                return std::string_view{"lyra::runtime::SimTimeInUnit"};
              case support::TimeKind::kStime:
                return std::string_view{"lyra::runtime::STimeInUnit"};
              case support::TimeKind::kRealtime:
                return std::string_view{"lyra::runtime::RealTimeInUnit"};
            }
            throw InternalError(
                "RenderSystemSubroutineEntryName: unknown TimeKind");
          },
          [](const support::FileIOSystemSubroutineInfo& file_io)
              -> diag::Result<std::string_view> {
            switch (file_io.kind) {
              case support::FileIOKind::kOpen:
                return std::string_view{"lyra::runtime::LyraFOpen"};
              case support::FileIOKind::kClose:
                return std::string_view{"lyra::runtime::LyraFClose"};
              case support::FileIOKind::kGetc:
                return std::string_view{"lyra::runtime::LyraFGetc"};
              case support::FileIOKind::kUngetc:
                return std::string_view{"lyra::runtime::LyraFUngetc"};
              case support::FileIOKind::kGets:
                return std::string_view{"lyra::runtime::LyraFGets"};
              case support::FileIOKind::kSeek:
                return std::string_view{"lyra::runtime::LyraFSeek"};
              case support::FileIOKind::kRewind:
                return std::string_view{"lyra::runtime::LyraFRewind"};
              case support::FileIOKind::kTell:
                return std::string_view{"lyra::runtime::LyraFTell"};
              case support::FileIOKind::kEof:
                return std::string_view{"lyra::runtime::LyraFEof"};
              case support::FileIOKind::kError:
                return std::string_view{"lyra::runtime::LyraFError"};
              case support::FileIOKind::kFlush:
                return std::string_view{"lyra::runtime::LyraFFlush"};
              case support::FileIOKind::kRead:
                return std::string_view{"lyra::runtime::LyraFRead"};
            }
            throw InternalError(
                "RenderSystemSubroutineEntryName: unknown FileIOKind");
          },
          [&](const auto&) -> diag::Result<std::string_view> {
            return diag::Unsupported(
                diag::DiagCode::kCppEmitExpressionFormNotImplemented,
                std::format(
                    "system subroutine '{}' is not yet lowered to a generic "
                    "call",
                    desc.name),
                diag::UnsupportedCategory::kFeature);
          },
      },
      desc.semantic);
}

// A system subroutine renders as one generic call: the runtime entry name
// followed by every argument rendered uniformly. The engine handle is not
// injected here -- it is `arguments[0]` (a `self.Services()` expression) and
// renders like any other argument.
auto RenderSystemSubroutineCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    const mir::SystemSubroutineCallee& callee) -> diag::Result<std::string> {
  const support::SystemSubroutineDesc& desc =
      support::LookupSystemSubroutine(callee.id);
  auto name_or = RenderSystemSubroutineEntryName(desc);
  if (!name_or) return std::unexpected(std::move(name_or.error()));

  std::string out;
  out += *name_or;
  out += "(";
  for (std::size_t i = 0; i < call.arguments.size(); ++i) {
    auto arg_or = RenderExpr(ctx, ctx.Expr(call.arguments[i]));
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    if (i != 0) out += ", ";
    out += *std::move(arg_or);
  }
  out += ")";
  return out;
}

auto RenderCallExpr(
    const RenderContext& ctx, const mir::CallExpr& call,
    mir::TypeId result_type) -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::SystemSubroutineCallee& s)
              -> diag::Result<std::string> {
            return RenderSystemSubroutineCall(ctx, call, s);
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
            const auto& decl = scope.GetStructuralSubroutine(ref.subroutine);
            std::string args;
            for (std::size_t i = 0; i < call.arguments.size(); ++i) {
              const mir::Expr& actual = ctx.Expr(call.arguments[i]);
              std::string rendered;
              if (i == 0) {
                // arguments[0] is the callee's `self` instance handle (mir.md
                // invariant 11) -- a plain pointer value with no formal.
                auto self_or = RenderExpr(ctx, actual);
                if (!self_or)
                  return std::unexpected(std::move(self_or.error()));
                rendered = *std::move(self_or);
              } else {
                const mir::ParamDirection dir = decl.params[i - 1].direction;
                if (dir == mir::ParamDirection::kRef ||
                    dir == mir::ParamDirection::kConstRef) {
                  // A ref / const ref actual binds the variable's cell: render
                  // its lvalue and wrap it in a `Ref<T>`. Constructor overload
                  // picks the observable (`Var<T>`), plain (`T`), or
                  // unpacked-element backing; a ref formal passed on forwards
                  // via the copy ctor (LRM 13.5.2). When the actual is rooted
                  // in an observable cell and points at an element (not the
                  // whole cell), route through `Var<T>::Mutate(svc)` so the
                  // ScopedMutation snapshot lives for the call's full
                  // expression and commits via `Var::Set` on return.
                  const bool needs_mutate =
                      LhsRootIsObservableScalar(ctx, actual) &&
                      !IsLhsBarePrimary(actual);
                  auto lhs_or = RenderLhsExpr(
                      ctx, actual,
                      needs_mutate
                          ? std::string_view{".Mutate(self->Services())"}
                          : std::string_view{});
                  if (!lhs_or)
                    return std::unexpected(std::move(lhs_or.error()));
                  auto type_or = RenderTypeAsCpp(
                      ctx.Unit(), ctx.StructuralScope(),
                      decl.params[i - 1].type);
                  if (!type_or)
                    return std::unexpected(std::move(type_or.error()));
                  rendered =
                      "lyra::runtime::Ref<" + *type_or + ">(" + *lhs_or + ")";
                } else {
                  auto arg_or = RenderExpr(ctx, actual);
                  if (!arg_or)
                    return std::unexpected(std::move(arg_or.error()));
                  rendered = *std::move(arg_or);
                }
              }
              if (i != 0) args += ", ";
              args += rendered;
            }
            return std::format("{}({})", decl.name, args);
          },
          [&](const mir::BuiltinMethodCallee& b) -> diag::Result<std::string> {
            return std::visit(
                Overloaded{
                    [&](const mir::EnumMethodInfo& m) {
                      return RenderEnumMethodCall(ctx, call, m);
                    },
                    [&](const mir::StringMethodInfo& m) {
                      return RenderMethodCall(
                          ctx, call, StringMethodMemberName(m.kind),
                          StringMethodMutatesReceiver(m.kind));
                    },
                    [&](const mir::EventMethodInfo& m) {
                      return RenderMethodCall(
                          ctx, call, EventMethodMemberName(m.kind), false);
                    },
                    [&](const mir::ArrayMethodInfo& m) {
                      return RenderMethodCall(
                          ctx, call, ArrayMethodMemberName(m.kind),
                          ArrayMethodMutatesReceiver(m.kind));
                    },
                    [&](const mir::QueueMethodInfo& m) {
                      return RenderMethodCall(
                          ctx, call, QueueMethodMemberName(m.kind),
                          QueueMethodMutatesReceiver(m.kind));
                    },
                    [&](const mir::AssociativeMethodInfo& m) {
                      return RenderAssociativeMethodCall(ctx, call, m);
                    },
                    [&](const mir::ValueMethodInfo& m) {
                      return RenderValueMethodCall(ctx, call, m);
                    },
                    [](const mir::IteratorMethodInfo&)
                        -> diag::Result<std::string> {
                      // LRM 7.12.4 `item.index` is resolved at HIR -> MIR to
                      // a `ProceduralVarRef` on the closure's `index`
                      // parameter binding; reaching the backend means
                      // closure synthesis failed to rewrite it.
                      throw InternalError(
                          "RenderCallExpr: IteratorMethodInfo reached the "
                          "backend (LRM 7.12.4 -- HIR -> MIR should have "
                          "rewritten `item.index` to a ProceduralVarRef on "
                          "the closure parameter binding)");
                    },
                    [&](const mir::ScopeMethodInfo& m) {
                      return RenderScopeMethodCall(ctx, call, m);
                    },
                },
                b.method);
          },
          [&](const mir::ClosureRef& cr) -> diag::Result<std::string> {
            auto closure_or = RenderExpr(ctx, ctx.Expr(cr.closure));
            if (!closure_or) {
              return std::unexpected(std::move(closure_or.error()));
            }
            std::string args;
            for (std::size_t i = 0; i < call.arguments.size(); ++i) {
              auto arg_or = RenderExpr(ctx, ctx.Expr(call.arguments[i]));
              if (!arg_or) return std::unexpected(std::move(arg_or.error()));
              if (i != 0) args += ", ";
              args += *std::move(arg_or);
            }
            return "(" + *closure_or + ")(" + args + ")";
          },
          [&](const mir::RuntimeNavCallee& nav) -> diag::Result<std::string> {
            auto base_or = RenderExpr(ctx, ctx.Expr(call.arguments.at(0)));
            if (!base_or) return std::unexpected(std::move(base_or.error()));
            switch (nav.fn) {
              case mir::RuntimeFn::kGetChild: {
                std::string indices = "{}";
                if (call.arguments.size() > 1) {
                  indices = "std::array{";
                  for (std::size_t i = 1; i < call.arguments.size(); ++i) {
                    auto idx_or =
                        RenderExpr(ctx, ctx.Expr(call.arguments.at(i)));
                    if (!idx_or)
                      return std::unexpected(std::move(idx_or.error()));
                    if (i != 1) indices += ", ";
                    indices +=
                        "std::size_t((" + *std::move(idx_or) + ").ToInt64())";
                  }
                  indices += "}";
                }
                return *base_or + "->GetChild(\"" + nav.name + "\", " +
                       indices + ")";
              }
              case mir::RuntimeFn::kGetSignal: {
                // GetSignal returns an untyped storage pointer; the call's
                // result type names the cell, so the cast is fixed by that
                // type.
                auto cell_or = RenderTypeAsCpp(
                    ctx.Unit(), ctx.StructuralScope(), result_type);
                if (!cell_or)
                  return std::unexpected(std::move(cell_or.error()));
                return "static_cast<" + *cell_or + ">(" + *base_or +
                       "->GetSignal(\"" + nav.name + "\"))";
              }
              case mir::RuntimeFn::kRegisterSignal: {
                // Registers the signal's own cell address under its name; the
                // var argument renders as a bare lvalue so `&` takes the cell.
                auto var_or = RenderLhsExpr(
                    ctx, ctx.Expr(call.arguments.at(1)), std::string_view{});
                if (!var_or) return std::unexpected(std::move(var_or.error()));
                return *base_or + "->RegisterSignal(\"" + nav.name + "\", &" +
                       *var_or + ")";
              }
            }
            throw InternalError("RenderCallExpr: unknown RuntimeFn");
          },
      },
      call.callee);
}

enum class ElementAccessMode : std::uint8_t { kRead, kWrite };

// Indexed element access shared by rvalue `RenderElementSelectExpr` and the
// lvalue `RenderLhsExpr` ElementSelect arm. `PackedArray`, `UnpackedArray`,
// `DynamicArray`, and `Queue` expose a symmetric `ElementAt(const
// PackedArray&)` so the emit string is substrate-agnostic; declared-range
// translation and `ToInt64` canonicalize inside the runtime type.
//
// The associative array splits read from write (LRM 7.8.6 / 7.8.7): a read
// never allocates and returns the element default for a missing key, while a
// write allocates the entry. The access mode selects `Read` vs `ElementRef`.
auto RenderIndexedElementAccess(
    const mir::Type& base_ty, std::string_view base, std::string_view idx,
    ElementAccessMode mode) -> std::string {
  if (std::holds_alternative<mir::AssociativeArrayType>(base_ty.data)) {
    const std::string_view member =
        mode == ElementAccessMode::kWrite ? "ElementRef" : "Read";
    return std::format("({}).{}({})", base, member, idx);
  }
  if (!std::holds_alternative<mir::PackedArrayType>(base_ty.data) &&
      !std::holds_alternative<mir::UnpackedArrayType>(base_ty.data) &&
      !std::holds_alternative<mir::DynamicArrayType>(base_ty.data)) {
    throw InternalError(
        "RenderIndexedElementAccess: base type must be PackedArrayType, "
        "UnpackedArrayType, DynamicArrayType, or AssociativeArrayType");
  }
  return std::format("({}).ElementAt({})", base, idx);
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
  return RenderIndexedElementAccess(
      base_ty, *base_or, *idx_or, ElementAccessMode::kRead);
}

// Renders an LHS-shaped expression for use as an assignment target. The
// `mutate_adapter` string (typically `.Mutate(Services())` or empty) is
// inserted immediately after the structural-var root so an observable
// partial write enters a ScopedMutation before the selector chain begins.
auto RenderRangeSliceSuffix(
    const RenderContext& ctx, mir::ExprId offset_expr, std::uint32_t count)
    -> diag::Result<std::string> {
  auto offset = RenderExpr(ctx, ctx.Expr(offset_expr));
  if (!offset) return std::unexpected(std::move(offset.error()));
  return std::format(".Slice({}, {}U)", *offset, count);
}

}  // namespace

// Output shape:
//   <root>{<mutate_adapter>}{.ElementAt(idx) | .Slice(offset, count)}*
auto RenderLhsExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    std::string_view mutate_adapter) -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::MemberAccessExpr& m) -> diag::Result<std::string> {
            auto receiver_or = RenderExpr(ctx, ctx.Expr(m.receiver));
            if (!receiver_or) {
              return std::unexpected(std::move(receiver_or.error()));
            }
            const auto& scope = ctx.StructuralScopeAtHops(m.member.hops);
            const auto& var = scope.GetStructuralVar(m.member.var);
            return *receiver_or + "->" + var.name + std::string{mutate_adapter};
          },
          [&](const mir::ProceduralVarRef& l) -> diag::Result<std::string> {
            return LookupProceduralVarName(ctx, l);
          },
          [&](const mir::ElementSelectExpr& s) -> diag::Result<std::string> {
            const mir::Expr& base_expr = ctx.Expr(s.base_value);
            auto base = RenderLhsExpr(ctx, base_expr, mutate_adapter);
            if (!base) return std::unexpected(std::move(base.error()));
            auto idx = RenderExpr(ctx, ctx.Expr(s.index));
            if (!idx) return std::unexpected(std::move(idx.error()));
            const auto& base_ty = ctx.Unit().GetType(base_expr.type);
            return RenderIndexedElementAccess(
                base_ty, *base, *idx, ElementAccessMode::kWrite);
          },
          [&](const mir::RangeSelectExpr& s) -> diag::Result<std::string> {
            auto base =
                RenderLhsExpr(ctx, ctx.Expr(s.base_value), mutate_adapter);
            if (!base) return std::unexpected(std::move(base.error()));
            auto suffix = RenderRangeSliceSuffix(ctx, s.offset_expr, s.count);
            if (!suffix) return std::unexpected(std::move(suffix.error()));
            return *base + *suffix;
          },
          [&](const mir::DerefExpr& d) -> diag::Result<std::string> {
            auto ptr = RenderExpr(ctx, ctx.Expr(d.pointer));
            if (!ptr) return std::unexpected(std::move(ptr.error()));
            return "(*" + *ptr + ")" + std::string{mutate_adapter};
          },
          [&](const mir::CallExpr& call) -> diag::Result<std::string> {
            // LRM 7.10.1: a queue element write lowers to a `WriteRef` built-in
            // method call that returns the element reference, so the call site
            // is itself the assignable lvalue.
            return RenderCallExpr(ctx, call, expr.type);
          },
          [&](const auto&) -> diag::Result<std::string> {
            throw InternalError(
                "RenderLhsExpr: expression form is not addressable; the "
                "assignment target lowering should have produced an "
                "addressable form");
          },
      },
      expr.data);
}

namespace {

// Walks an LHS expression through element / range selects to its root primary.
// Whether a write touches an observable cell or a reference formal is decided
// by what that root is.
auto LhsRootPrimary(const RenderContext& ctx, const mir::Expr& expr)
    -> const mir::Expr& {
  const mir::Expr* current = &expr;
  while (true) {
    if (const auto* sel = std::get_if<mir::ElementSelectExpr>(&current->data)) {
      current = &ctx.Expr(sel->base_value);
      continue;
    }
    if (const auto* sel = std::get_if<mir::RangeSelectExpr>(&current->data)) {
      current = &ctx.Expr(sel->base_value);
      continue;
    }
    return *current;
  }
}

auto IsLhsBarePrimary(const mir::Expr& expr) -> bool {
  return std::holds_alternative<mir::MemberAccessExpr>(expr.data) ||
         std::holds_alternative<mir::DerefExpr>(expr.data) ||
         std::holds_alternative<mir::ProceduralVarRef>(expr.data);
}

// The root procedural var iff the LHS root is a `ref` / `const ref` formal,
// else nullptr. A write whose root is a reference formal must route through
// `Ref::Set` (LRM 13.5.2).
auto LhsRootReferenceProceduralVar(
    const RenderContext& ctx, const mir::Expr& expr)
    -> const mir::ProceduralVarRef* {
  const auto* pvr =
      std::get_if<mir::ProceduralVarRef>(&LhsRootPrimary(ctx, expr).data);
  return pvr != nullptr && IsReferenceProceduralVar(ctx, *pvr) ? pvr : nullptr;
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

  // A `ref` / `const ref` formal aliases the actual's cell; a whole write
  // routes through `Ref::Set` so the actual's update-event path fires (LRM
  // 13.5.2). Compound and partial writes through a ref formal are not yet
  // supported.
  if (const auto* ref_root = LhsRootReferenceProceduralVar(ctx, lhs_expr)) {
    if (!IsLhsBarePrimary(lhs_expr) || a.compound_op.has_value()) {
      return diag::Unsupported(
          diag::DiagCode::kCppEmitExpressionFormNotImplemented,
          "compound or partial assignment through a ref / const ref formal is "
          "not yet implemented in cpp emit",
          diag::UnsupportedCategory::kFeature);
    }
    return LookupProceduralVarName(ctx, *ref_root) + ".Set(" +
           "self->Services()" + ", " + *value_or + ")";
  }

  const bool observable = LhsRootIsObservableScalar(ctx, lhs_expr);

  // Whole-var write: route observable structural targets through Var::Set
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
          observable ? *root_or + ".Mutate(" + "self->Services()" + ")"
                     : *root_or;
      return "(" + RenderCompoundAssign(*a.compound_op, chain, *value_or) + ")";
    }
    if (observable) {
      return *root_or + ".Set(" + "self->Services()" + ", " + *value_or + ")";
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
  // destructor commits the snapshot through `Var::Set` so subscribers fire.
  // Same shape inside or outside an NBA closure body -- no nested lambda.
  const std::string mutate_adapter =
      observable ? std::string{".Mutate(self->Services())"} : std::string{};
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
  if (LhsRootReferenceProceduralVar(ctx, target_expr) != nullptr) {
    return diag::Unsupported(
        diag::DiagCode::kCppEmitExpressionFormNotImplemented,
        "increment / decrement of a ref / const ref formal is not yet "
        "implemented in cpp emit",
        diag::UnsupportedCategory::kFeature);
  }
  const bool observable = LhsRootIsObservableScalar(ctx, target_expr);

  std::string lhs;
  if (IsLhsBarePrimary(target_expr)) {
    auto root_or = RenderLhsExpr(ctx, target_expr, std::string_view{});
    if (!root_or) return std::unexpected(std::move(root_or.error()));
    lhs = observable ? *root_or + ".Mutate(self->Services())" : *root_or;
  } else {
    const std::string mutate_adapter =
        observable ? std::string{".Mutate(self->Services())"} : std::string{};
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

  auto offset_or = RenderExpr(ctx, ctx.Expr(sel.offset_expr));
  if (!offset_or) return std::unexpected(std::move(offset_or.error()));
  auto slice_expr =
      std::format("({}).Slice({}, {}U)", *base_or, *offset_or, sel.count);

  // PackedArray::Slice() always produces an unsigned result (LRM 7.4.1
  // part-select default). When the MIR result type carries signed semantics
  // -- e.g. member access of a `logic signed` packed-struct/union field --
  // re-tag the slice via ConvertFrom so a downstream sign-extending
  // conversion sees the correct source signedness. Unpacked slice produces
  // an UnpackedArray<T> with no top-level signedness attribute, so the
  // wrap is packed-only.
  const auto& result_ty = ctx.Unit().GetType(expr.type);
  if (result_ty.IsPackedArray() &&
      result_ty.AsPackedArray().signedness == mir::Signedness::kSigned) {
    return std::format(
        "lyra::value::PackedArray::ConvertFrom({}, {})", slice_expr,
        RenderPackedArrayCtorArgs(result_ty.AsPackedArray()));
  }
  return slice_expr;
}

auto RenderClosureExpr(
    const RenderContext& ctx, const mir::ClosureExpr& closure)
    -> diag::Result<std::string> {
  if (closure.body == nullptr) {
    throw InternalError("RenderClosureExpr: closure has no body");
  }

  // Every captured value flows through `closure.captures` as a named
  // by-value or by-reference entry; the receiver `self` is captures[0] (mir.md
  // invariant 11). The clause never contains `[this]`, `[=]`, or `[&]`.
  std::string captures_text;
  for (std::size_t i = 0; i < closure.captures.size(); ++i) {
    if (i != 0) captures_text += ", ";
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
            [&](const mir::ByReferenceCapture& br)
                -> diag::Result<std::string> {
              const std::string& bind_name =
                  closure.body->vars.at(br.binding.value).name;
              auto lvalue_or =
                  RenderLhsExpr(ctx, ctx.Expr(br.target), std::string_view{});
              if (!lvalue_or) {
                return std::unexpected(std::move(lvalue_or.error()));
              }
              return "&" + bind_name + " = " + *lvalue_or;
            }},
        closure.captures[i]);
    if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
    captures_text += *rendered_or;
  }

  // LRM 7.12.4 with-clause closures (or any closure built with a non-empty
  // `params` list) render as a value-returning lambda whose parameter
  // clause names the bindings in declaration order and whose body ends with
  // `return <value>;` (lowered from a tail `ReturnStmt`). Closures with no
  // `params` (fork branches, NBA submit) keep the existing `()` form.
  std::string params_text;
  for (std::size_t i = 0; i < closure.params.size(); ++i) {
    const auto& bind = closure.body->vars.at(closure.params[i].binding.value);
    auto type_or =
        RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), bind.type);
    if (!type_or) return std::unexpected(std::move(type_or.error()));
    if (i != 0) params_text += ", ";
    params_text += "const " + *type_or + "& " + bind.name;
  }

  // Derive the lambda return type from the tail `ReturnStmt`'s value
  // expression -- closures synthesized for LRM 7.12.2 / 7.12.3 always end
  // there. An empty-params closure (fork branch / NBA submit) carries no
  // return value; leave the lambda return type unspecified so C++ deduces
  // `void`.
  std::string return_clause;
  if (!closure.params.empty()) {
    const auto& root_stmts = closure.body->root_stmts;
    if (root_stmts.empty()) {
      throw InternalError(
          "RenderClosureExpr: with-clause closure body has no root "
          "statements (expected a tail ReturnStmt)");
    }
    const auto& tail = closure.body->stmts.at(root_stmts.back().value);
    const auto* ret = std::get_if<mir::ReturnStmt>(&tail.data);
    if (ret == nullptr || !ret->value.has_value()) {
      throw InternalError(
          "RenderClosureExpr: with-clause closure body does not end with a "
          "value-returning `ReturnStmt`");
    }
    const auto& ret_expr = closure.body->GetExpr(*ret->value);
    auto ret_ty_or =
        RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), ret_expr.type);
    if (!ret_ty_or) return std::unexpected(std::move(ret_ty_or.error()));
    return_clause = " -> " + *ret_ty_or;
  }

  const RenderContext body_ctx =
      RenderContext::ForRoot(ctx.Unit(), ctx.StructuralScope(), *closure.body);
  auto body_or = RenderProceduralScopeStatements(body_ctx, 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));

  return "[" + captures_text + "](" + params_text + ")" + return_clause +
         " {\n" + *body_or + "}";
}

// LRM 10.10 unpacked array concatenation into a queue. Each operand is spliced
// (an unpacked container of the element type) or appended as a single element
// (anything else); the empty `{}` is the empty queue. The result is unseeded --
// the assignment destination keeps its own element shape (LRM 10.6.1) -- so no
// element default is threaded.
auto RenderQueueConcat(
    const RenderContext& ctx, const mir::Type& result_ty,
    const mir::ConcatExpr& c) -> diag::Result<std::string> {
  const mir::TypeId elem_type_id =
      std::get<mir::QueueType>(result_ty.data).element_type;
  auto elem_cpp =
      RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), elem_type_id);
  if (!elem_cpp) return std::unexpected(std::move(elem_cpp.error()));
  std::string out = "lyra::value::MakeQueueConcat<" + *elem_cpp + ">(";
  for (std::size_t i = 0; i < c.operands.size(); ++i) {
    const auto& op_expr = ctx.Expr(c.operands[i]);
    const auto& op_ty = ctx.Unit().GetType(op_expr.type);
    const bool spread =
        std::holds_alternative<mir::QueueType>(op_ty.data) ||
        std::holds_alternative<mir::DynamicArrayType>(op_ty.data) ||
        std::holds_alternative<mir::UnpackedArrayType>(op_ty.data);
    auto rendered =
        spread ? RenderExprNatural(ctx, op_expr) : RenderExpr(ctx, op_expr);
    if (!rendered) return std::unexpected(std::move(rendered.error()));
    if (i != 0) out += ", ";
    out += spread ? "lyra::value::QSpread(" + *rendered + ")"
                  : "lyra::value::QElem(" + *rendered + ")";
  }
  out += ")";
  return out;
}

auto RenderConcatExpr(
    const RenderContext& ctx, const mir::Expr& expr, const mir::ConcatExpr& c)
    -> diag::Result<std::string> {
  const auto& result_ty = ctx.Unit().GetType(expr.type);
  if (std::holds_alternative<mir::QueueType>(result_ty.data)) {
    return RenderQueueConcat(ctx, result_ty, c);
  }
  if (c.operands.empty()) {
    throw InternalError("RenderConcatExpr: hir lowering produced empty concat");
  }
  const auto join = [&](std::string_view open, std::string_view sep,
                        std::string_view close) -> diag::Result<std::string> {
    std::string out{open};
    for (std::size_t i = 0; i < c.operands.size(); ++i) {
      auto rendered = RenderExpr(ctx, ctx.Expr(c.operands[i]));
      if (!rendered) return std::unexpected(std::move(rendered.error()));
      if (i != 0) out += sep;
      out += *rendered;
    }
    out += close;
    return out;
  };
  if (result_ty.IsIntegralPacked()) {
    return join("lyra::value::PackedArray::Concat(", ", ", ")");
  }
  if (result_ty.Kind() == mir::TypeKind::kString) {
    // LRM 6.16: string concat joins contents via std::string `operator+`.
    return join("(", " + ", ")");
  }
  throw InternalError(
      "RenderConcatExpr: result type must be PackedArrayType or string");
}

// LRM 10.9.1 array assignment pattern: renders as `std::array<T, N>{e1, ...}`.
// `expr.type` is the parent container type (`UnpackedArrayType` /
// `DynamicArrayType` / `QueueType`); the element type is read off it and
// emitted as the `std::array` element parameter. The resulting
// `std::array<T, N>` implicitly
// converts to the container ctor's `std::span<const T>` parameter, so this
// rendering is context-independent: the same string is correct standalone
// and as a `ConstructExpr` argument.
auto RenderArrayLiteralExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::ArrayLiteralExpr& a) -> diag::Result<std::string> {
  const auto& container_ty = ctx.Unit().GetType(expr.type);
  const mir::TypeId elem_type_id = std::visit(
      [](const auto& ty) -> mir::TypeId {
        using TyT = std::decay_t<decltype(ty)>;
        if constexpr (
            std::same_as<TyT, mir::UnpackedArrayType> ||
            std::same_as<TyT, mir::DynamicArrayType> ||
            std::same_as<TyT, mir::QueueType>) {
          return ty.element_type;
        } else {
          throw InternalError(
              "RenderArrayLiteralExpr: result type must be UnpackedArrayType, "
              "DynamicArrayType, or QueueType");
        }
      },
      container_ty.data);
  auto elem_type_or =
      RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), elem_type_id);
  if (!elem_type_or) return std::unexpected(std::move(elem_type_or.error()));
  std::string out =
      std::format("std::array<{}, {}>{{", *elem_type_or, a.elements.size());
  for (std::size_t i = 0; i < a.elements.size(); ++i) {
    auto rendered = RenderExpr(ctx, ctx.Expr(a.elements[i]));
    if (!rendered) return std::unexpected(std::move(rendered.error()));
    if (i != 0) out += ", ";
    out += *rendered;
  }
  out += "}";
  return out;
}

// LRM 7.5.1 / 7.6: constructor invocation. Emits `RenderType(type)(args...)`
// -- parenthesised so brace-vs-paren overload resolution is unambiguous and
// the runtime ctor is selected by arg-list shape.
auto RenderConstructExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::ConstructExpr& c) -> diag::Result<std::string> {
  // A pointer value-initializes to null; `nullptr` is the valid spelling of
  // that (the functional-cast form `T*()` is ill-formed for a raw pointer
  // type).
  if (c.args.empty() && std::holds_alternative<mir::PointerType>(
                            ctx.Unit().GetType(expr.type).data)) {
    return std::string{"nullptr"};
  }
  auto type_or = RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), expr.type);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  std::string out = *type_or + "(";
  for (std::size_t i = 0; i < c.args.size(); ++i) {
    auto r = RenderExpr(ctx, ctx.Expr(c.args[i]));
    if (!r) return std::unexpected(std::move(r.error()));
    if (i != 0) out += ", ";
    out += *r;
  }
  out += ")";
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
  if (result_ty.Kind() == mir::TypeKind::kString) {
    return std::format(
        "lyra::value::ReplicateString({}, {})", *concat, count_text);
  }
  throw InternalError(
      "RenderReplicationExpr: result type must be PackedArrayType or string");
}

auto ProducesPackedArrayRef(const RenderContext& ctx, const mir::Expr& expr)
    -> bool {
  if (!std::holds_alternative<mir::ElementSelectExpr>(expr.data) &&
      !std::holds_alternative<mir::RangeSelectExpr>(expr.data)) {
    return false;
  }
  const auto& result_ty = ctx.Unit().GetType(expr.type);
  return std::holds_alternative<mir::PackedArrayType>(result_ty.data);
}

}  // namespace

// Defined at file scope (matching the header's declaration with external
// linkage) so callsites in other translation units -- e.g.
// `RenderRuntimeCallExpr` for `LyraFRead` in `render_print.cpp` -- can route
// observable destinations through `Var<T>::Mutate(svc)`. `LhsRootPrimary` is
// visible at file scope via the anonymous-namespace's implicit using.
auto LhsRootIsObservableScalar(const RenderContext& ctx, const mir::Expr& expr)
    -> bool {
  const mir::Expr& root = LhsRootPrimary(ctx, expr);
  if (const auto* m = std::get_if<mir::MemberAccessExpr>(&root.data)) {
    const auto& scope = ctx.StructuralScopeAtHops(m->member.hops);
    return IsObservableCell(
        ctx.Unit().GetType(scope.GetStructuralVar(m->member.var).type));
  }
  if (const auto* d = std::get_if<mir::DerefExpr>(&root.data)) {
    const mir::Expr& ptr_expr = ctx.Expr(d->pointer);
    const auto& ptr_data = ctx.Unit().GetType(ptr_expr.type).data;
    if (const auto* ptr_t = std::get_if<mir::PointerType>(&ptr_data)) {
      return IsObservableCell(ctx.Unit().GetType(ptr_t->pointee));
    }
  }
  return false;
}

auto RenderExpr(const RenderContext& ctx, const mir::Expr& expr)
    -> diag::Result<std::string> {
  auto rendered_or = RenderExprNatural(ctx, expr);
  if (!rendered_or) return rendered_or;
  if (ProducesPackedArrayRef(ctx, expr)) {
    return "(" + *std::move(rendered_or) + ").Clone()";
  }
  return rendered_or;
}

// Read-side render of a borrowed-pointer dereference: the cell is reached with
// `(*ptr)` and observed via `.Get()` when the pointer points at an observable
// cell, mirroring the structural-var read split.
auto RenderDerefExpr(const RenderContext& ctx, const mir::DerefExpr& d)
    -> diag::Result<std::string> {
  const mir::Expr& ptr_expr = ctx.Expr(d.pointer);
  auto ptr_or = RenderExpr(ctx, ptr_expr);
  if (!ptr_or) return std::unexpected(std::move(ptr_or.error()));
  const auto& ptr_data = ctx.Unit().GetType(ptr_expr.type).data;
  if (const auto* ptr_t = std::get_if<mir::PointerType>(&ptr_data)) {
    if (IsObservableCell(ctx.Unit().GetType(ptr_t->pointee))) {
      return "(*" + *ptr_or + ").Get()";
    }
  }
  return "(*" + *ptr_or + ")";
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
          [&](const mir::ProceduralVarRef& l) -> diag::Result<std::string> {
            const std::string name = LookupProceduralVarName(ctx, l);
            return IsReferenceProceduralVar(ctx, l) ? name + ".Get()" : name;
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
            return RenderCallExpr(ctx, call, expr.type);
          },
          [&](const mir::RuntimeCallExpr& rc) -> diag::Result<std::string> {
            return RenderRuntimeCallExpr(ctx, rc);
          },
          [&](const mir::DerefExpr& d) -> diag::Result<std::string> {
            return RenderDerefExpr(ctx, d);
          },
          [&](const mir::MemberAccessExpr& m) -> diag::Result<std::string> {
            const auto& scope = ctx.StructuralScopeAtHops(m.member.hops);
            const auto& var = scope.GetStructuralVar(m.member.var);
            auto receiver_or = RenderExpr(ctx, ctx.Expr(m.receiver));
            if (!receiver_or) {
              return std::unexpected(std::move(receiver_or.error()));
            }
            std::string base = *receiver_or + "->" + var.name;
            // A read of an observable-storage member unwraps the value
            // through `Var<T>::Get`; a plain field is the value itself. The
            // wrap lives on the member's declared type, not on the
            // expression's apparent type.
            if (IsObservableCell(ctx.Unit().GetType(var.type))) {
              base += ".Get()";
            }
            return base;
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
          [&](const mir::ConstructExpr& c) -> diag::Result<std::string> {
            return RenderConstructExpr(ctx, expr, c);
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

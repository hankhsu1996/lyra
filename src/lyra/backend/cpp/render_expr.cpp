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

#include "lyra/backend/cpp/formatting.hpp"
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
  return ctx.StructuralScope().GetStructuralParam(r.param).name;
}

auto LookupProceduralVarName(
    const RenderContext& ctx, const mir::ProceduralVarRef& ref) -> std::string {
  const auto& var = ctx.ProceduralScopeAtHops(ref.hops).vars.at(ref.var.value);
  if (var.lifetime == mir::VariableLifetime::kStatic) {
    return std::format(
        "{}{}.{}", ctx.MemberPrefix(), ctx.StaticFrame(),
        StaticFrameMemberName(var.name, ref.var.value));
  }
  return var.name;
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
  return ctx.MemberPrefix() +
         ctx.StructuralScope().GetStructuralVar(ref.var).name;
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

// Peephole: a same-width / same-signedness integer-literal conversion that
// only flips the state axis (4-state <-> 2-state, no X/Z loss) folds to the
// literal in the destination shape -- no runtime ConvertFrom needed. Returns
// nullopt when the conversion is not eligible; the main dispatch then runs.
auto TryFoldLiteralIntegerConversion(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::ConversionExpr& cv) -> std::optional<std::string> {
  const auto& src_expr = ctx.Expr(cv.operand);
  const auto* lit = std::get_if<mir::IntegerLiteral>(&src_expr.data);
  if (lit == nullptr) return std::nullopt;
  const auto& src_ty = ctx.Unit().GetType(src_expr.type);
  const auto& dst_ty = ctx.Unit().GetType(expr.type);
  if (!src_ty.IsIntegralPacked() || !dst_ty.IsIntegralPacked() ||
      src_ty.IsEnum() || dst_ty.IsEnum()) {
    return std::nullopt;
  }
  const auto& src_pa = src_ty.AsIntegralPacked();
  const auto& dst_pa = dst_ty.AsIntegralPacked();
  if (src_pa.BitWidth() != dst_pa.BitWidth() ||
      src_pa.signedness != dst_pa.signedness) {
    return std::nullopt;
  }
  const bool literal_has_xz =
      !lit->value.state_words.empty() && lit->value.state_words[0] != 0U;
  const bool dst_two_state = dst_pa.atom == mir::BitAtom::kBit;
  if (literal_has_xz && dst_two_state) return std::nullopt;
  return RenderPackedArrayIntegerLiteral(dst_pa, lit->value);
}

auto RenderConversionExpr(
    const RenderContext& ctx, const mir::Expr& expr,
    const mir::ConversionExpr& cv) -> diag::Result<std::string> {
  if (auto folded = TryFoldLiteralIntegerConversion(ctx, expr, cv);
      folded.has_value()) {
    return *std::move(folded);
  }

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
                               ? ctx.ServicesRef()
                               : std::string{};
  const std::string raw_call = std::format(
      "({}).{}({})", *receiver_or, EventMethodMemberName(m.kind), args);
  if (mir::IsSuspending(m)) return "co_await " + raw_call;
  return raw_call;
}

// LRM 7.5.2 / 7.5.3 / 7.12.2 / 7.12.3: dispatch on the receiver
// (arguments[0]); none of the no-`with` methods in this PR take additional
// arguments. `Size()` returns std::size_t and gets wrapped in
// `PackedArray::Int` to land in SV `int` shape (mirrors EnumMethod::kNum).
// All other methods either return void (in-place mutators) or already return
// the receiver's element type (reductions); no wrap.
auto RenderArrayMethodCall(
    const RenderContext& ctx, const mir::CallExpr& call,
    const mir::ArrayMethodInfo& m) -> diag::Result<std::string> {
  if (call.arguments.empty()) {
    throw InternalError(
        "RenderArrayMethodCall: array method expects a receiver argument");
  }
  auto receiver_or = RenderExpr(ctx, ctx.Expr(call.arguments[0]));
  if (!receiver_or) {
    return std::unexpected(std::move(receiver_or.error()));
  }
  // LRM 7.12.2 / 7.12.3 with-clause: HIR -> MIR appends the closure as the
  // second positional argument. The runtime exposes a parallel `*By`
  // overload that takes the closure; both members share the no-with
  // base name in `ArrayMethodMemberName`.
  if (call.arguments.size() == 2) {
    auto closure_or = RenderExpr(ctx, ctx.Expr(call.arguments[1]));
    if (!closure_or) {
      return std::unexpected(std::move(closure_or.error()));
    }
    return std::format(
        "({}).{}By({})", *receiver_or, ArrayMethodMemberName(m.kind),
        *closure_or);
  }
  const std::string raw_call =
      std::format("({}).{}()", *receiver_or, ArrayMethodMemberName(m.kind));
  if (m.kind == mir::ArrayMethodKind::kSize) {
    return std::format("lyra::value::PackedArray::Int({})", raw_call);
  }
  return raw_call;
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

auto RenderCallExpr(
    const RenderContext& ctx, const mir::CallExpr& call,
    mir::TypeId result_type) -> diag::Result<std::string> {
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
            const auto& decl = scope.GetStructuralSubroutine(ref.subroutine);
            std::string args;
            for (std::size_t i = 0; i < call.arguments.size(); ++i) {
              const mir::Expr& actual = ctx.Expr(call.arguments[i]);
              const mir::ParamDirection dir = decl.params[i].direction;
              std::string rendered;
              if (dir == mir::ParamDirection::kRef ||
                  dir == mir::ParamDirection::kConstRef) {
                // A ref / const ref actual binds the variable's cell: render
                // its lvalue and wrap it in a `Ref<T>`. Constructor overload
                // picks the observable (`Var<T>`), plain (`T`), or
                // unpacked-element backing; a ref formal passed on forwards via
                // the copy ctor (LRM 13.5.2).
                auto lhs_or = RenderLhsExpr(ctx, actual, std::string_view{});
                if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
                auto type_or = RenderTypeAsCpp(
                    ctx.Unit(), ctx.StructuralScope(), decl.params[i].type);
                if (!type_or)
                  return std::unexpected(std::move(type_or.error()));
                rendered =
                    "lyra::runtime::Ref<" + *type_or + ">(" + *lhs_or + ")";
              } else {
                auto arg_or = RenderExpr(ctx, actual);
                if (!arg_or) return std::unexpected(std::move(arg_or.error()));
                rendered = *std::move(arg_or);
              }
              if (i != 0) args += ", ";
              args += rendered;
            }
            std::string call =
                std::format("{}{}({})", ctx.MemberPrefix(), decl.name, args);
            // A task is a coroutine enabled with `co_await`; the only legal
            // callers are process / task bodies, which are themselves
            // coroutines (LRM 13.4 b forbids a function enabling a task).
            if (decl.kind == mir::SubroutineKind::kTask) {
              return "co_await " + std::move(call);
            }
            return call;
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
                    [&](const mir::ArrayMethodInfo& m) {
                      return RenderArrayMethodCall(ctx, call, m);
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

// Indexed element access shared by rvalue `RenderElementSelectExpr` and the
// lvalue `RenderLhsExpr` ElementSelect arm. `PackedArray`, `UnpackedArray`,
// and `DynamicArray` expose a symmetric `ElementAt(const PackedArray&)` so
// the emit string is substrate-agnostic; declared-range translation and
// `ToInt64` canonicalize inside the runtime type.
auto RenderIndexedElementAccess(
    const mir::Type& base_ty, std::string_view base, std::string_view idx)
    -> std::string {
  if (!std::holds_alternative<mir::PackedArrayType>(base_ty.data) &&
      !std::holds_alternative<mir::UnpackedArrayType>(base_ty.data) &&
      !std::holds_alternative<mir::DynamicArrayType>(base_ty.data)) {
    throw InternalError(
        "RenderIndexedElementAccess: base type must be PackedArrayType, "
        "UnpackedArrayType, or DynamicArrayType");
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
  return RenderIndexedElementAccess(base_ty, *base_or, *idx_or);
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
          [&](const mir::StructuralVarRef& r) -> diag::Result<std::string> {
            auto name = RenderStructuralVarName(ctx, r);
            if (!name) return std::unexpected(std::move(name.error()));
            return *name + std::string{mutate_adapter};
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
            return RenderIndexedElementAccess(base_ty, *base, *idx);
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
          [&](const auto&) -> diag::Result<std::string> {
            throw InternalError(
                "RenderLhsExpr: expression form is not addressable; "
                "ValidateAssignableProcExpr should have rejected it");
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

// Whether the LHS root is an observable scalar -- a structural var, or a
// dereferenced borrowed-pointer slot whose pointee is an observable scalar
// type. A write to such a root routes through `Var::Set` / `Var::Mutate` so
// subscribers fire.
auto LhsRootIsObservableScalar(const RenderContext& ctx, const mir::Expr& expr)
    -> bool {
  const mir::Expr& root = LhsRootPrimary(ctx, expr);
  if (const auto* sv = std::get_if<mir::StructuralVarRef>(&root.data)) {
    return IsObservableScalarType(ctx.Unit().GetType(
        ctx.StructuralScope().GetStructuralVar(sv->var).type));
  }
  if (std::holds_alternative<mir::DerefExpr>(root.data)) {
    return IsObservableScalarType(ctx.Unit().GetType(root.type));
  }
  return false;
}

auto IsLhsBarePrimary(const mir::Expr& expr) -> bool {
  return std::holds_alternative<mir::StructuralVarRef>(expr.data) ||
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
           ctx.ServicesRef() + ", " + *value_or + ")";
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
          observable ? *root_or + ".Mutate(" + ctx.ServicesRef() + ")"
                     : *root_or;
      return "(" + RenderCompoundAssign(*a.compound_op, chain, *value_or) + ")";
    }
    if (observable) {
      return *root_or + ".Set(" + ctx.ServicesRef() + ", " + *value_or + ")";
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
      observable ? ".Mutate(" + ctx.ServicesRef() + ")" : std::string{};
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
    lhs =
        observable ? *root_or + ".Mutate(" + ctx.ServicesRef() + ")" : *root_or;
  } else {
    const std::string mutate_adapter =
        observable ? ".Mutate(" + ctx.ServicesRef() + ")" : std::string{};
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

  // Capture the receiver by value so the deferred body can reach scope members
  // (the Scope base's Services() accessor and SubmitObserved interface, plus
  // the emitted class's structural vars) when it later runs: `this` in a method
  // body, or the branch's `self` pointer when the closure is nested in a
  // fork-branch closure (LRM 9.3.2). The explicit by-value captures from the
  // MIR list go after.
  std::string captures_text{ctx.ReceiverObject()};
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
      RenderContext::ForRoot(ctx.Unit(), ctx.StructuralScope(), *closure.body)
          .WithReceiver(ctx.ReceiverObject());
  auto body_or = RenderProceduralScopeStatements(body_ctx, 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));

  return "[" + captures_text + "](" + params_text + ")" + return_clause +
         " {\n" + *body_or + "}";
}

auto RenderConcatExpr(
    const RenderContext& ctx, const mir::Expr& expr, const mir::ConcatExpr& c)
    -> diag::Result<std::string> {
  if (c.operands.empty()) {
    throw InternalError("RenderConcatExpr: hir lowering produced empty concat");
  }
  const auto& result_ty = ctx.Unit().GetType(expr.type);
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

// Read-side render of a borrowed-pointer dereference: the cell is reached with
// `(*ptr)` and observed via `.Get()` for an integral packed pointee, mirroring
// the structural-var read split.
auto RenderDerefExpr(
    const RenderContext& ctx, const mir::Expr& expr, const mir::DerefExpr& d)
    -> diag::Result<std::string> {
  auto ptr_or = RenderExpr(ctx, ctx.Expr(d.pointer));
  if (!ptr_or) return std::unexpected(std::move(ptr_or.error()));
  if (ctx.Unit().GetType(expr.type).IsIntegralPacked()) {
    return "(*" + *ptr_or + ").Get()";
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
          [&](const mir::StructuralVarRef& m) -> diag::Result<std::string> {
            return RenderStructuralVarReadExpr(ctx, expr, m);
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
            return RenderDerefExpr(ctx, expr, d);
          },
          [&](const mir::SelfScopeExpr&) -> diag::Result<std::string> {
            return std::string(ctx.ReceiverObject());
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

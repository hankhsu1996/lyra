#include "lyra/backend/cpp/render_expr.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/backend/cpp/render_stmt.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/string_literal.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/cast.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"

namespace lyra::backend::cpp {

namespace {

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
    const ScopeView& view, const mir::Expr& expr,
    const mir::IntegerLiteral& lit) -> std::string {
  const auto& ty = view.Unit().GetType(expr.type);
  if (!ty.IsIntegralPacked()) {
    throw InternalError(
        "RenderIntegerLiteralExpr: IntegerLiteral not typed as "
        "PackedArrayType or EnumType");
  }
  auto body = RenderPackedArrayIntegerLiteral(ty.AsIntegralPacked(), lit.value);
  if (ty.IsEnum()) {
    return std::format(
        "{}{{{}}}", RenderEnumClassName(view.Class(), expr.type), body);
  }
  return body;
}

auto RenderRealLiteralExpr(
    const ScopeView& view, const mir::Expr& expr, const mir::RealLiteral& r)
    -> std::string {
  // `std::format` with `{:.{}g}` and precision=17 round-trips a double;
  // precision=9 round-trips a float (IEEE 754 minimum representable-pair
  // widths). The trailing 'f' suffix keeps the shortreal body a `float` literal
  // so the wrapping `ShortReal{...}` constructs from a float, with no
  // double -> float narrowing.
  //
  // `g` strips trailing zeros and the decimal point for whole-number values,
  // which would produce literals like `0f` or `42e3f` that the C++ lexer
  // rejects (a digit sequence followed by `f` is not a valid float suffix).
  // Force a decimal point when the formatted body has neither `.` nor an
  // exponent, so `0` -> `0.0`, `42` -> `42.0`.
  const auto& ty = view.Unit().GetType(expr.type);
  const bool is_short = ty.Kind() == mir::TypeKind::kShortReal;
  std::string body = is_short ? std::format("{:.9g}", r.value)
                              : std::format("{:.17g}", r.value);
  if (body.find_first_of(".eE") == std::string::npos) {
    body += ".0";
  }
  if (is_short) {
    body += "f";
  }
  return std::format(
      "lyra::value::{}{{{}}}", is_short ? "ShortReal" : "Real", body);
}

auto RenderParamExpr(const ScopeView& view, const mir::ParamRef& r)
    -> std::string {
  if (r.hops.value != 0) {
    throw InternalError(
        "cross-scope param access is not yet implemented in "
        "cpp emit");
  }
  const std::string& name = view.Class().params.Get(r.param).name;
  return std::format("{}->{}", view.SelfSpelling(), name);
}

auto LookupLocalName(const ScopeView& view, const mir::LocalRef& ref)
    -> std::string {
  // The receiver is the body's `vars[0]`, named `self` in MIR (invariant 11).
  // How it spells in C++ depends on the enclosing method's form: a static
  // method's first parameter (`self`) or a virtual method's implicit receiver
  // (`this`).
  const std::string& name = view.BlockAtHops(ref.hops).vars.Get(ref.var).name;
  if (name == "self") {
    return std::string(view.SelfSpelling());
  }
  return name;
}

}  // namespace

namespace {

// The C++ operator token for the SV binary ops that render natively. The
// method-style ops (shifts, power, xnor, wildcard / case / implication /
// equivalence) are lifted to `CallExpr` at HIR-to-MIR and never reach this
// dispatch; reaching one is an MIR-invariant violation.
auto BinaryOpToken(mir::BinaryOp op) -> std::string_view {
  switch (op) {
    case mir::BinaryOp::kAdd:
      return "+";
    case mir::BinaryOp::kSub:
      return "-";
    case mir::BinaryOp::kMul:
      return "*";
    case mir::BinaryOp::kDiv:
      return "/";
    case mir::BinaryOp::kMod:
      return "%";
    case mir::BinaryOp::kBitwiseAnd:
      return "&";
    case mir::BinaryOp::kBitwiseOr:
      return "|";
    case mir::BinaryOp::kBitwiseXor:
      return "^";
    case mir::BinaryOp::kEquality:
      return "==";
    case mir::BinaryOp::kInequality:
      return "!=";
    case mir::BinaryOp::kLessThan:
      return "<";
    case mir::BinaryOp::kLessEqual:
      return "<=";
    case mir::BinaryOp::kGreaterThan:
      return ">";
    case mir::BinaryOp::kGreaterEqual:
      return ">=";
    case mir::BinaryOp::kLogicalAnd:
      return "&&";
    case mir::BinaryOp::kLogicalOr:
      return "||";
    case mir::BinaryOp::kPower:
    case mir::BinaryOp::kBitwiseXnor:
    case mir::BinaryOp::kShiftLeft:
    case mir::BinaryOp::kLogicalShiftRight:
    case mir::BinaryOp::kArithmeticShiftRight:
    case mir::BinaryOp::kLogicalImplication:
    case mir::BinaryOp::kLogicalEquivalence:
    case mir::BinaryOp::kWildcardEquality:
    case mir::BinaryOp::kWildcardInequality:
    case mir::BinaryOp::kCaseEquality:
    case mir::BinaryOp::kCaseInequality:
    case mir::BinaryOp::kCasezEquality:
    case mir::BinaryOp::kCasexEquality:
      throw InternalError(
          "BinaryOpToken: method-style operator reached backend render; "
          "HIR-to-MIR should have lifted it to a CallExpr");
  }
  throw InternalError("BinaryOpToken: unknown MIR BinaryOp");
}

auto UnaryOpToken(mir::UnaryOp op) -> std::string_view {
  switch (op) {
    case mir::UnaryOp::kMinus:
      return "-";
    case mir::UnaryOp::kBitwiseNot:
      return "~";
    case mir::UnaryOp::kLogicalNot:
      return "!";
    case mir::UnaryOp::kPlus:
      // kPlus has no C++ token: PackedArray / String have no `operator+()`
      // (LRM 11.4.3 unary plus is a no-op), so render emits the bare
      // operand instead -- handled separately in `RenderUnaryExpr`.
    case mir::UnaryOp::kReductionAnd:
    case mir::UnaryOp::kReductionOr:
    case mir::UnaryOp::kReductionXor:
    case mir::UnaryOp::kReductionNand:
    case mir::UnaryOp::kReductionNor:
    case mir::UnaryOp::kReductionXnor:
      throw InternalError(
          "UnaryOpToken: operator has no native C++ token; "
          "kPlus is identity (handled by RenderUnaryExpr) and reductions "
          "lift to CallExpr at HIR-to-MIR");
  }
  throw InternalError("UnaryOpToken: unknown MIR UnaryOp");
}

auto RenderUnaryExpr(const ScopeView& view, const mir::UnaryExpr& u)
    -> std::string {
  std::string operand = RenderExpr(view, view.Expr(u.operand));
  // LRM 11.4.3: unary plus is an identity; no C++ `operator+()` exists on
  // PackedArray / String / RealValue, so render the operand directly.
  if (u.op == mir::UnaryOp::kPlus) {
    return std::format("({})", operand);
  }
  return std::format("({}{})", UnaryOpToken(u.op), operand);
}

auto RenderBinaryExpr(const ScopeView& view, const mir::BinaryExpr& b)
    -> std::string {
  return std::format(
      "({} {} {})", RenderExpr(view, view.Expr(b.lhs)), BinaryOpToken(b.op),
      RenderExpr(view, view.Expr(b.rhs)));
}

// Brings an operand onto the host-bool plane so a native C++ logical
// operator (`&&` / `||` / `!`) can take it. The wrap is observable only as
// the operand of `kFromBool` -- the value-shape re-projection back to the
// SV 1-bit integral lives there.
auto RenderBoolCastExpr(const ScopeView& view, const mir::BoolCastExpr& b)
    -> std::string {
  return std::format("bool({})", RenderExpr(view, view.Expr(b.operand)));
}

auto RenderConditionalExpr(const ScopeView& view, const mir::ConditionalExpr& c)
    -> std::string {
  return std::format(
      "({} ? {} : {})", RenderConditionAsBool(view, view.Expr(c.condition)),
      RenderExpr(view, view.Expr(c.then_value)),
      RenderExpr(view, view.Expr(c.else_value)));
}

// `mir::CastExpr` is a type-level view change with no runtime semantic
// transform -- the C++ peer of `static_cast<T>(x)` and the LLVM peer of
// `bitcast`. The destination type comes from the enclosing `Expr::type`;
// render is a fixed function of the node kind, with no type-driven branching.
// Every other value-shape conversion (integral resize, real <-> integral,
// packed <-> string, real <-> real precision) is expressed upstream as a
// `CallExpr` against a `lyra::value` factory and renders through the call
// path.
auto RenderCastExpr(
    const ScopeView& view, const mir::Expr& expr, const mir::CastExpr& cast)
    -> std::string {
  return std::format(
      "static_cast<{}>({})",
      RenderTypeAsCpp(view.Unit(), view.Class(), expr.type),
      RenderExpr(view, view.Expr(cast.operand)));
}

// The bare C++ identifier this backend declares the builtin fn as. One
// of three orthogonal facts a render path composes (name, namespace,
// receiver expression / type qualifier) -- see `BuiltinFnCppNamespace`
// for the second.
auto BuiltinFnCppName(support::BuiltinFn id) -> std::string_view {
  switch (id) {
    case support::BuiltinFn::kServices:
      return "Services";
    case support::BuiltinFn::kGet:
      return "Get";
    case support::BuiltinFn::kSet:
      return "Set";
    case support::BuiltinFn::kMutate:
      return "Mutate";
    case support::BuiltinFn::kSubmitNba:
      return "SubmitNba";
    case support::BuiltinFn::kSubmitPostponed:
      return "SubmitPostponed";
    case support::BuiltinFn::kSubmitObserved:
      return "SubmitObserved";
    case support::BuiltinFn::kFiles:
      return "Files";
    case support::BuiltinFn::kCancellationFor:
      return "CancellationFor";
    case support::BuiltinFn::kIsCancelled:
      return "IsCancelled";
    case support::BuiltinFn::kFormat:
      return "Format";
    case support::BuiltinFn::kWrite:
      return "Write";
    case support::BuiltinFn::kWriteln:
      return "Writeln";
    case support::BuiltinFn::kDiagnostic:
      return "Diagnostic";
    case support::BuiltinFn::kEmitInfo:
      return "EmitInfo";
    case support::BuiltinFn::kEmitWarning:
      return "EmitWarning";
    case support::BuiltinFn::kEmitError:
      return "EmitError";
    case support::BuiltinFn::kEmitFatal:
      return "EmitFatal";
    case support::BuiltinFn::kTimeFormat:
      return "TimeFormat";
    case support::BuiltinFn::kSetTimeFormat:
      return "SetTimeFormat";
    case support::BuiltinFn::kResetTimeFormat:
      return "ResetTimeFormat";
    case support::BuiltinFn::kScan:
      return "Scan";
    case support::BuiltinFn::kPeekBuffered:
      return "PeekBuffered";
    case support::BuiltinFn::kAdvanceFd:
      return "AdvanceFd";
    case support::BuiltinFn::kTrigger:
      return "Trigger";
    case support::BuiltinFn::kAwait:
      return "Await";
    case support::BuiltinFn::kTriggered:
      return "Triggered";
    case support::BuiltinFn::kIsUnknown:
      return "IsUnknown";
    case support::BuiltinFn::kEnumFirst:
      return "First";
    case support::BuiltinFn::kEnumLast:
      return "Last";
    case support::BuiltinFn::kEnumNum:
      return "Num";
    case support::BuiltinFn::kEnumName:
      return "Name";
    case support::BuiltinFn::kEnumNext:
      return "Next";
    case support::BuiltinFn::kEnumPrev:
      return "Prev";
    case support::BuiltinFn::kLen:
      return "Len";
    case support::BuiltinFn::kGetc:
      return "Getc";
    case support::BuiltinFn::kPutc:
      return "Putc";
    case support::BuiltinFn::kToupper:
      return "Toupper";
    case support::BuiltinFn::kTolower:
      return "Tolower";
    case support::BuiltinFn::kCompare:
      return "Compare";
    case support::BuiltinFn::kIcompare:
      return "Icompare";
    case support::BuiltinFn::kSubstr:
      return "Substr";
    case support::BuiltinFn::kAtoi:
      return "Atoi";
    case support::BuiltinFn::kAtohex:
      return "Atohex";
    case support::BuiltinFn::kAtooct:
      return "Atooct";
    case support::BuiltinFn::kAtobin:
      return "Atobin";
    case support::BuiltinFn::kAtoreal:
      return "Atoreal";
    case support::BuiltinFn::kItoa:
      return "Itoa";
    case support::BuiltinFn::kHextoa:
      return "Hextoa";
    case support::BuiltinFn::kOcttoa:
      return "Octtoa";
    case support::BuiltinFn::kBintoa:
      return "Bintoa";
    case support::BuiltinFn::kRealtoa:
      return "Realtoa";
    case support::BuiltinFn::kElement:
      return "Element";
    case support::BuiltinFn::kElementRef:
      return "ElementRef";
    case support::BuiltinFn::kSlice:
      return "Slice";
    case support::BuiltinFn::kSliceRef:
      return "SliceRef";
    case support::BuiltinFn::kSize:
      return "Size";
    case support::BuiltinFn::kToOwned:
      return "ToOwned";
    case support::BuiltinFn::kDelete:
      return "Delete";
    case support::BuiltinFn::kReverse:
      return "Reverse";
    case support::BuiltinFn::kSort:
      return "Sort";
    case support::BuiltinFn::kRsort:
      return "Rsort";
    case support::BuiltinFn::kSum:
      return "Sum";
    case support::BuiltinFn::kProduct:
      return "Product";
    case support::BuiltinFn::kAnd:
      return "And";
    case support::BuiltinFn::kOr:
      return "Or";
    case support::BuiltinFn::kXor:
      return "Xor";
    case support::BuiltinFn::kFind:
      return "Find";
    case support::BuiltinFn::kFindIndex:
      return "FindIndex";
    case support::BuiltinFn::kFindFirst:
      return "FindFirst";
    case support::BuiltinFn::kFindFirstIndex:
      return "FindFirstIndex";
    case support::BuiltinFn::kFindLast:
      return "FindLast";
    case support::BuiltinFn::kFindLastIndex:
      return "FindLastIndex";
    case support::BuiltinFn::kMin:
      return "Min";
    case support::BuiltinFn::kMax:
      return "Max";
    case support::BuiltinFn::kUnique:
      return "Unique";
    case support::BuiltinFn::kUniqueIndex:
      return "UniqueIndex";
    case support::BuiltinFn::kMap:
      return "Map";
    case support::BuiltinFn::kInsert:
      return "Insert";
    case support::BuiltinFn::kPopFront:
      return "PopFront";
    case support::BuiltinFn::kPopBack:
      return "PopBack";
    case support::BuiltinFn::kPushFront:
      return "PushFront";
    case support::BuiltinFn::kPushBack:
      return "PushBack";
    case support::BuiltinFn::kExists:
      return "Exists";
    case support::BuiltinFn::kAssocFirst:
      return "First";
    case support::BuiltinFn::kAssocLast:
      return "Last";
    case support::BuiltinFn::kAssocNext:
      return "Next";
    case support::BuiltinFn::kAssocPrev:
      return "Prev";
    case support::BuiltinFn::kDelay:
      return "Delay";
    case support::BuiltinFn::kSimTime:
      return "SimTimeInUnit";
    case support::BuiltinFn::kSTime:
      return "STimeInUnit";
    case support::BuiltinFn::kRealTime:
      return "RealTimeInUnit";
    case support::BuiltinFn::kFinish:
      return "Finish";
    case support::BuiltinFn::kFatalFinish:
      return "FatalFinish";
    case support::BuiltinFn::kAsObservable:
      return "AsObservable";
    case support::BuiltinFn::kRegisterSignal:
      return "RegisterSignal";
    case support::BuiltinFn::kGetSignal:
      return "GetSignal";
    case support::BuiltinFn::kGetChild:
      return "GetChild";
    case support::BuiltinFn::kToInt64:
      return "ToInt64";
    case support::BuiltinFn::kRound:
      return "Round";
    case support::BuiltinFn::kFromInt:
      return "FromInt";
    case support::BuiltinFn::kConvertFrom:
      return "ConvertFrom";
    case support::BuiltinFn::kFromPackedArray:
      return "FromPackedArray";
    case support::BuiltinFn::kFromByteArray:
      return "FromByteArray";
    case support::BuiltinFn::kPow:
      return "Pow";
    case support::BuiltinFn::kShiftLeft:
      return "ShiftLeft";
    case support::BuiltinFn::kLogicalShiftRight:
      return "LogicalShiftRight";
    case support::BuiltinFn::kArithmeticShiftRight:
      return "ArithmeticShiftRight";
    case support::BuiltinFn::kBitwiseXnor:
      return "BitwiseXnor";
    case support::BuiltinFn::kLogicalImplication:
      return "LogicalImplication";
    case support::BuiltinFn::kLogicalEquivalence:
      return "LogicalEquivalence";
    case support::BuiltinFn::kWildcardEquals:
      return "WildcardEquals";
    case support::BuiltinFn::kCaseEqual:
      return "CaseEqual";
    case support::BuiltinFn::kCasezEquals:
      return "CasezEquals";
    case support::BuiltinFn::kCasexEquals:
      return "CasexEquals";
    case support::BuiltinFn::kReductionAnd:
      return "ReductionAnd";
    case support::BuiltinFn::kReductionOr:
      return "ReductionOr";
    case support::BuiltinFn::kReductionXor:
      return "ReductionXor";
    case support::BuiltinFn::kReductionNand:
      return "ReductionNand";
    case support::BuiltinFn::kReductionNor:
      return "ReductionNor";
    case support::BuiltinFn::kReductionXnor:
      return "ReductionXnor";
    case support::BuiltinFn::kFromBool:
      return "FromBool";
    case support::BuiltinFn::kFileOpen:
      return "Open";
    case support::BuiltinFn::kFileClose:
      return "Close";
    case support::BuiltinFn::kFileGetc:
      return "Getc";
    case support::BuiltinFn::kFileUngetc:
      return "Ungetc";
    case support::BuiltinFn::kFileGets:
      return "Gets";
    case support::BuiltinFn::kFileRead:
      return "Read";
    case support::BuiltinFn::kFileSeek:
      return "Seek";
    case support::BuiltinFn::kFileRewind:
      return "Rewind";
    case support::BuiltinFn::kFileTell:
      return "Tell";
    case support::BuiltinFn::kFileEof:
      return "Eof";
    case support::BuiltinFn::kFileError:
      return "Error";
    case support::BuiltinFn::kFileFlush:
      return "Flush";
  }
  throw InternalError("BuiltinFnCppName: unknown BuiltinFn");
}

// The C++ namespace path the builtin fn is declared in (e.g.
// `lyra::value` for `lyra::value::Scan`). A separate axis from the bare
// name in `BuiltinFnCppName` and from the calling shape in the MIR
// `Callee` variant. Every id may declare one; render paths choose
// whether to consume it -- a `FreeFnCallee` must (no receiver to supply
// the scope), a `BuiltinFnCallee` need not (the receiver expression's
// type already names the enclosing class), a `BuiltinStaticCallee` need
// not (the `type_qual` already names the SV type). Returns empty for
// ids whose render path never needs an explicit qualifier; extend this
// table when adding a free function or any caller that wants to render
// the fully-qualified path.
auto BuiltinFnCppNamespace(support::BuiltinFn id) -> std::string_view {
  switch (id) {
    case support::BuiltinFn::kScan:
    case support::BuiltinFn::kFormat:
      return "lyra::value";
    case support::BuiltinFn::kDelay:
    case support::BuiltinFn::kSimTime:
    case support::BuiltinFn::kSTime:
    case support::BuiltinFn::kRealTime:
    case support::BuiltinFn::kFinish:
    case support::BuiltinFn::kFatalFinish:
      return "lyra::runtime";
    default:
      return "";
  }
}

// `recv.name(args)` or `recv->name(args)` -- the receiver's MIR type
// (pointer vs value) selects the C++ separator. The pointer-vs-value fact is
// already on the receiver expression's type; the backend reads it and picks
// the separator mechanically.
auto RenderBuiltinFnCall(
    const ScopeView& view, const mir::CallExpr& call, support::BuiltinFn id)
    -> std::string {
  if (call.arguments.empty()) {
    throw InternalError(
        "RenderBuiltinFnCall: instance call expects a receiver argument");
  }
  const mir::Expr& receiver = view.Expr(call.arguments[0]);
  auto recv = RenderExpr(view, receiver);
  const std::string_view sep =
      view.Unit().GetType(receiver.type).Kind() == mir::TypeKind::kPointer
          ? "->"
          : ".";
  std::string args;
  for (std::size_t i = 1; i < call.arguments.size(); ++i) {
    if (i != 1) args += ", ";
    args += RenderExpr(view, view.Expr(call.arguments[i]));
  }
  return std::format("({}){}{}({})", recv, sep, BuiltinFnCppName(id), args);
}

auto RenderFreeFnCall(
    const ScopeView& view, const mir::CallExpr& call, support::BuiltinFn id)
    -> std::string {
  const std::string_view ns = BuiltinFnCppNamespace(id);
  if (ns.empty()) {
    throw InternalError(
        "RenderFreeFnCall: free-function id has no declared namespace");
  }
  std::string args;
  for (std::size_t i = 0; i < call.arguments.size(); ++i) {
    if (i != 0) args += ", ";
    args += RenderExpr(view, view.Expr(call.arguments[i]));
  }
  return std::format("{}::{}({})", ns, BuiltinFnCppName(id), args);
}

auto RenderBuiltinStaticCall(
    const ScopeView& view, const mir::CallExpr& call,
    const mir::BuiltinStaticCallee& callee) -> std::string {
  std::string args;
  for (std::size_t i = 0; i < call.arguments.size(); ++i) {
    if (i != 0) args += ", ";
    args += RenderExpr(view, view.Expr(call.arguments[i]));
  }
  // The qualifier names the SV type whose static method is being invoked.
  // Enum-typed qualifiers go through the alias-aware `RenderEnumClassName`
  // (the alias spelling matches the user's `typedef enum {...}` name); every
  // other SV type maps to its canonical `lyra::value::...` C++ name via
  // `RenderTypeAsCpp`. Splitting the two keeps the call-site choice
  // ("qualified by enum alias vs by runtime library type") visible.
  const auto& qual_ty = view.Unit().GetType(callee.type_qual);
  const std::string qualifier =
      qual_ty.IsEnum()
          ? RenderEnumClassName(view.Class(), callee.type_qual)
          : RenderTypeAsCpp(view.Unit(), view.Class(), callee.type_qual);
  return std::format(
      "{}::{}({})", qualifier, BuiltinFnCppName(callee.id), args);
}

// Constructs a value of the call's result data type: `<TypeName>(args)`, the
// type name from `RenderTypeAsCpp` (a constructor is a call whose callee is the
// type's constructor), the arguments rendered like any other call's. A
// `RefType` cell argument renders as a bare signal read, which is the cell
// itself -- the `Ref<T>` constructor binds it.
auto RenderConstructorCall(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> std::string {
  std::string type = RenderTypeAsCpp(view.Unit(), view.Class(), result_type);
  std::string args;
  for (std::size_t i = 0; i < call.arguments.size(); ++i) {
    if (i != 0) args += ", ";
    args += RenderExpr(view, view.Expr(call.arguments[i]));
  }
  return std::format("{}({})", type, args);
}

auto RenderCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::MethodRef& ref) -> std::string {
            if (ref.hops.value != 0) {
              throw InternalError(
                  "method call across classes is not yet "
                  "implemented in cpp emit");
            }
            const auto& cls = view.EnclosingClassAtHops(
                mir::EnclosingHops{.value = ref.hops.value});
            const auto& decl = cls.methods.Get(ref.method);
            // arguments[0] is the callee's `self` handle; a ref / const ref
            // actual is already a reference-construct (`Ref<T>(cell)`) in MIR,
            // so every argument renders uniformly.
            std::string args;
            for (std::size_t i = 0; i < call.arguments.size(); ++i) {
              if (i != 0) args += ", ";
              args += RenderExpr(view, view.Expr(call.arguments[i]));
            }
            return std::format("{}({})", decl.name, args);
          },
          [&](const mir::BuiltinFnCallee& b) -> std::string {
            return RenderBuiltinFnCall(view, call, b.id);
          },
          [&](const mir::BuiltinStaticCallee& b) -> std::string {
            return RenderBuiltinStaticCall(view, call, b);
          },
          [&](const mir::FreeFnCallee& f) -> std::string {
            return RenderFreeFnCall(view, call, f.id);
          },
          [&](const mir::ClosureRef& cr) -> std::string {
            std::string closure = RenderExpr(view, view.Expr(cr.closure));
            std::string args;
            for (std::size_t i = 0; i < call.arguments.size(); ++i) {
              if (i != 0) args += ", ";
              args += RenderExpr(view, view.Expr(call.arguments[i]));
            }
            return std::format("({})({})", closure, args);
          },
          [&](const mir::ConstructorCallee&) -> std::string {
            return RenderConstructorCall(view, call, result_type);
          },
      },
      call.callee);
}

}  // namespace

// LHS expression render: produces a write-target reference (a name, a
// dereference, or a chain of container-access `CallExpr`s whose runtime
// overloads return write-through references). The observable-cell
// `Mutate(svc)` adapter is already in MIR as a `DerefExpr` wrapping an
// `ObservableMethod{kMutate}` call -- this render emits nothing implicit
// on top of the explicit MIR shape.
auto RenderLhsExpr(const ScopeView& view, const mir::Expr& expr)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::MemberAccessExpr& m) -> std::string {
            const auto& cls = view.EnclosingClassAtHops(m.member.hops);
            const auto& var = cls.members.Get(m.member.var);
            return std::format(
                "{}->{}", RenderExpr(view, view.Expr(m.receiver)), var.name);
          },
          [&](const mir::LocalRef& l) -> std::string {
            return LookupLocalName(view, l);
          },
          // HIR-to-MIR lowers an LHS selector chain to a container-access
          // `CallExpr` (per `mir::IsContainerAccessCall`). The C++ surface
          // returns a write-through reference, so the natural value-side
          // rendering is itself the assignment
          // target; LHS context needs no extra fix-up.
          [&](const mir::CallExpr&) -> std::string {
            return RenderExpr(view, expr);
          },
          [&](const mir::DerefExpr& d) -> std::string {
            return std::format("(*{})", RenderExpr(view, view.Expr(d.pointer)));
          },
          [&](const auto&) -> std::string {
            throw InternalError(
                "RenderLhsExpr: expression form is not addressable; the "
                "assignment target lowering should have produced an "
                "addressable form");
          },
      },
      expr.data);
}

namespace {

auto IsLhsBarePrimary(const mir::Expr& expr) -> bool {
  return std::holds_alternative<mir::MemberAccessExpr>(expr.data) ||
         std::holds_alternative<mir::DerefExpr>(expr.data) ||
         std::holds_alternative<mir::LocalRef>(expr.data);
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
      return std::format("{} += {}", chain, rhs);
    case mir::BinaryOp::kSub:
      return std::format("{} -= {}", chain, rhs);
    case mir::BinaryOp::kMul:
      return std::format("{} *= {}", chain, rhs);
    case mir::BinaryOp::kDiv:
      return std::format("{} /= {}", chain, rhs);
    case mir::BinaryOp::kMod:
      return std::format("{} %= {}", chain, rhs);
    case mir::BinaryOp::kBitwiseAnd:
      return std::format("{} &= {}", chain, rhs);
    case mir::BinaryOp::kBitwiseOr:
      return std::format("{} |= {}", chain, rhs);
    case mir::BinaryOp::kBitwiseXor:
      return std::format("{} ^= {}", chain, rhs);
    case mir::BinaryOp::kShiftLeft:
      return std::format("{}.ShiftLeftAssign({})", chain, rhs);
    case mir::BinaryOp::kLogicalShiftRight:
      return std::format("{}.LogicalShiftRightAssign({})", chain, rhs);
    case mir::BinaryOp::kArithmeticShiftRight:
      return std::format("{}.ArithmeticShiftRightAssign({})", chain, rhs);
    default:
      throw InternalError(
          "RenderCompoundAssign: BinaryOp is not a legal SV compound "
          "assignment operator (LRM 11.4 only allows arithmetic, bitwise, "
          "and shift compounds)");
  }
}

auto RenderAssignExpr(const ScopeView& view, const mir::AssignExpr& a)
    -> std::string {
  std::string value = RenderExpr(view, view.Expr(a.value));

  const mir::Expr& lhs_expr = view.Expr(a.target);

  // Mechanical render: an observable cell's write surfaces in MIR as an
  // explicit `CallExpr` against the cell type's `Set` method, and a partial
  // mutation surfaces as `DerefExpr` over a `Mutate` call at the chain root.
  // Both shapes arrive from HIR-to-MIR already lowered, so this path emits a
  // plain C++ assignment over whatever the LHS render produces.
  if (IsLhsBarePrimary(lhs_expr)) {
    std::string root = RenderLhsExpr(view, lhs_expr);
    if (a.compound_op.has_value()) {
      return std::format(
          "({})", RenderCompoundAssign(*a.compound_op, root, value));
    }
    return std::format("({} = {})", root, value);
  }
  std::string chain = RenderLhsExpr(view, lhs_expr);
  if (a.compound_op.has_value()) {
    return std::format(
        "({})", RenderCompoundAssign(*a.compound_op, chain, value));
  }
  return std::format("{} = {}", chain, value);
}

auto RenderIncDecExpr(const ScopeView& view, const mir::IncDecExpr& inc)
    -> std::string {
  const mir::Expr& target_expr = view.Expr(inc.target);
  std::string lhs = RenderLhsExpr(view, target_expr);

  switch (inc.op) {
    case mir::IncDecOp::kPreInc:
      return std::format("(++{})", lhs);
    case mir::IncDecOp::kPostInc:
      return std::format("({}++)", lhs);
    case mir::IncDecOp::kPreDec:
      return std::format("(--{})", lhs);
    case mir::IncDecOp::kPostDec:
      return std::format("({}--)", lhs);
  }
  throw InternalError("RenderIncDecExpr: unknown IncDecOp");
}

// Renders a binding's parameter declaration -- its type then its name. A
// `RefType` binding renders as `Ref<T> name`, a value binding as `T name`;
// the wrapper comes from the type alone (RenderTypeAsCpp), never hand-written.
auto RenderBindingParamDecl(const ScopeView& view, const mir::LocalDecl& bind)
    -> std::string {
  return std::format(
      "{} {}", RenderTypeAsCpp(view.Unit(), view.Class(), bind.type),
      bind.name);
}

// A closure renders from its captures, parameters, result type, and body alone.
// A synchronous closure is a capture-clause lambda. A coroutine closure (result
// type `Coroutine`) is a stateless lambda whose captures pass as frame-copied
// parameters and are supplied by an immediate call -- a capturing coroutine
// lambda would dangle once the spawned branch outlives the referencing site.
auto RenderClosureExpr(
    const ScopeView& view, const mir::ClosureExpr& closure,
    mir::TypeId result_type) -> std::string {
  if (closure.body == nullptr) {
    throw InternalError("RenderClosureExpr: closure has no body");
  }

  const std::string return_clause = std::format(
      " -> {}", RenderTypeAsCpp(view.Unit(), view.Class(), result_type));

  const ScopeView body_view =
      ScopeView::ForRoot(view.Unit(), view.Class(), *closure.body);
  const std::string body =
      std::format(" {{\n{}}}", RenderBlockStatements(body_view, 1));

  if (std::holds_alternative<mir::CoroutineType>(
          view.Unit().GetType(result_type).data)) {
    if (!closure.params.empty()) {
      throw InternalError(
          "RenderClosureExpr: coroutine closure has parameters");
    }
    std::string params;
    std::string args;
    for (std::size_t i = 0; i < closure.captures.size(); ++i) {
      const auto& bind = closure.body->vars.Get(closure.captures[i].binding);
      if (i != 0) {
        params += ", ";
        args += ", ";
      }
      params += RenderBindingParamDecl(view, bind);
      args += RenderExpr(view, view.Expr(closure.captures[i].value));
    }
    return std::format("[]({}){}{}({})", params, return_clause, body, args);
  }

  // The capture clause never contains `[this]`, `[=]`, or `[&]` -- every entry
  // is a named by-value binding `name = <value>`; an alias capture's value is a
  // reference-construct, so it binds a `Ref<T>` without a hidden C++ reference.
  std::string captures_text;
  for (std::size_t i = 0; i < closure.captures.size(); ++i) {
    const std::string& bind_name =
        closure.body->vars.Get(closure.captures[i].binding).name;
    if (i != 0) captures_text += ", ";
    captures_text += std::format(
        "{} = {}", bind_name,
        RenderExpr(view, view.Expr(closure.captures[i].value)));
  }

  std::string params_text;
  for (std::size_t i = 0; i < closure.params.size(); ++i) {
    const auto& bind = closure.body->vars.Get(closure.params[i].binding);
    if (i != 0) params_text += ", ";
    params_text += RenderBindingParamDecl(view, bind);
  }

  return std::format(
      "[{}]({}){}{}", captures_text, params_text, return_clause, body);
}

// LRM 10.10 unpacked array concatenation into a queue. Each operand is spliced
// (an unpacked container of the element type) or appended as a single element
// (anything else); the empty `{}` is the empty queue. The result is unseeded --
// the assignment destination keeps its own element shape (LRM 10.6.1) -- so no
// element default is threaded.
auto RenderQueueConcat(
    const ScopeView& view, const mir::Type& result_ty, const mir::ConcatExpr& c)
    -> std::string {
  const mir::TypeId elem_type_id =
      std::get<mir::QueueType>(result_ty.data).element_type;
  std::string out = std::format(
      "lyra::value::MakeQueueConcat<{}>(",
      RenderTypeAsCpp(view.Unit(), view.Class(), elem_type_id));
  for (std::size_t i = 0; i < c.operands.size(); ++i) {
    const auto& op_expr = view.Expr(c.operands[i]);
    const auto& op_ty = view.Unit().GetType(op_expr.type);
    const bool spread =
        std::holds_alternative<mir::QueueType>(op_ty.data) ||
        std::holds_alternative<mir::DynamicArrayType>(op_ty.data) ||
        std::holds_alternative<mir::UnpackedArrayType>(op_ty.data);
    std::string rendered = RenderExpr(view, op_expr);
    if (i != 0) out += ", ";
    out += spread ? std::format("lyra::value::QSpread({})", rendered)
                  : std::format("lyra::value::QElem({})", rendered);
  }
  out += ")";
  return out;
}

auto RenderConcatExpr(
    const ScopeView& view, const mir::Expr& expr, const mir::ConcatExpr& c)
    -> std::string {
  const auto& result_ty = view.Unit().GetType(expr.type);
  if (std::holds_alternative<mir::QueueType>(result_ty.data)) {
    return RenderQueueConcat(view, result_ty, c);
  }
  if (c.operands.empty()) {
    throw InternalError("RenderConcatExpr: hir lowering produced empty concat");
  }
  const auto join = [&](std::string_view open, std::string_view sep,
                        std::string_view close) -> std::string {
    std::string out{open};
    for (std::size_t i = 0; i < c.operands.size(); ++i) {
      if (i != 0) out += sep;
      out += RenderExpr(view, view.Expr(c.operands[i]));
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
// and as a construction-call argument.
auto RenderArrayLiteralExpr(
    const ScopeView& view, const mir::Expr& expr,
    const mir::ArrayLiteralExpr& a) -> std::string {
  const auto& container_ty = view.Unit().GetType(expr.type);
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
  std::string out = std::format(
      "std::array<{}, {}>{{",
      RenderTypeAsCpp(view.Unit(), view.Class(), elem_type_id),
      a.elements.size());
  for (std::size_t i = 0; i < a.elements.size(); ++i) {
    if (i != 0) out += ", ";
    out += RenderExpr(view, view.Expr(a.elements[i]));
  }
  out += "}";
  return out;
}

// Render the full `std::tuple<...>{...}` rather than a bare brace list so the
// tuple's conditionally-explicit converting constructor is never in doubt,
// including when the tuple is an element of an outer array literal.
auto RenderTupleExpr(
    const ScopeView& view, const mir::Expr& expr, const mir::TupleExpr& t)
    -> std::string {
  std::string out = std::format(
      "{}{{", RenderTypeAsCpp(view.Unit(), view.Class(), expr.type));
  for (std::size_t i = 0; i < t.components.size(); ++i) {
    if (i != 0) out += ", ";
    out += RenderExpr(view, view.Expr(t.components[i]));
  }
  out += "}";
  return out;
}

auto RenderReplicationExpr(
    const ScopeView& view, const mir::Expr& expr, const mir::ReplicationExpr& r)
    -> std::string {
  std::string count = RenderExpr(view, view.Expr(r.count));
  std::string concat = RenderExpr(view, view.Expr(r.concat));
  const auto& count_ty = view.Unit().GetType(view.Expr(r.count).type);
  std::string count_text = count_ty.IsIntegralPacked()
                               ? std::format("({}).ToInt64()", count)
                               : count;
  const auto& result_ty = view.Unit().GetType(expr.type);
  if (result_ty.IsIntegralPacked()) {
    return std::format(
        "lyra::value::PackedArray::Replicate({}, "
        "static_cast<std::uint64_t>({}))",
        concat, count_text);
  }
  if (result_ty.Kind() == mir::TypeKind::kString) {
    return std::format(
        "lyra::value::ReplicateString({}, {})", concat, count_text);
  }
  throw InternalError(
      "RenderReplicationExpr: result type must be PackedArrayType or string");
}

// Read-side render of a borrowed-pointer dereference: the cell is reached
// with `(*ptr)`. The `.Get()` unwrap, if applicable, is emitted by the
// explicit `ObservableMethod{kGet}` call that HIR-to-MIR wraps around an
// observable read.
auto RenderDerefExpr(const ScopeView& view, const mir::DerefExpr& d)
    -> std::string {
  return std::format("(*{})", RenderExpr(view, view.Expr(d.pointer)));
}

// `&place` emitted as the C++ address-of operator. Backend-side
// canonicalization: `&(*p)` collapses to `p` directly, avoiding a no-op
// round-trip through the address-of/dereference pair.
auto RenderAddressOfExpr(const ScopeView& view, const mir::AddressOfExpr& a)
    -> std::string {
  const mir::Expr& operand_expr = view.Expr(a.operand);
  if (const auto* deref = std::get_if<mir::DerefExpr>(&operand_expr.data)) {
    return RenderExpr(view, view.Expr(deref->pointer));
  }
  return std::format("&{}", RenderLhsExpr(view, operand_expr));
}

}  // namespace

auto RenderExpr(const ScopeView& view, const mir::Expr& expr) -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::IntegerLiteral& lit) -> std::string {
            return RenderIntegerLiteralExpr(view, expr, lit);
          },
          [&](const mir::StringLiteral& s) -> std::string {
            return RenderCStringLiteral(s.value);
          },
          [](const mir::TimeLiteral&) -> std::string {
            throw InternalError(
                "TimeLiteral is not yet implemented in cpp emit");
          },
          [&](const mir::RealLiteral& r) -> std::string {
            return RenderRealLiteralExpr(view, expr, r);
          },
          [](const mir::NullLiteral&) -> std::string {
            return std::string{"nullptr"};
          },
          [](const mir::HostIntLiteral& h) -> std::string {
            return std::format("{}LL", h.value);
          },
          [&](const mir::ParamRef& r) -> std::string {
            return RenderParamExpr(view, r);
          },
          [&](const mir::LocalRef& l) -> std::string {
            return LookupLocalName(view, l);
          },
          [&](const mir::UnaryExpr& u) -> std::string {
            return RenderUnaryExpr(view, u);
          },
          [&](const mir::BinaryExpr& b) -> std::string {
            return RenderBinaryExpr(view, b);
          },
          [&](const mir::BoolCastExpr& b) -> std::string {
            return RenderBoolCastExpr(view, b);
          },
          [&](const mir::ConditionalExpr& c) -> std::string {
            return RenderConditionalExpr(view, c);
          },
          [&](const mir::AssignExpr& a) -> std::string {
            return RenderAssignExpr(view, a);
          },
          [&](const mir::IncDecExpr& inc) -> std::string {
            return RenderIncDecExpr(view, inc);
          },
          [&](const mir::CastExpr& cast) -> std::string {
            return RenderCastExpr(view, expr, cast);
          },
          [&](const mir::CallExpr& call) -> std::string {
            return RenderCallExpr(view, call, expr.type);
          },
          [&](const mir::DerefExpr& d) -> std::string {
            return RenderDerefExpr(view, d);
          },
          [&](const mir::AddressOfExpr& a) -> std::string {
            return RenderAddressOfExpr(view, a);
          },
          [&](const mir::MemberAccessExpr& m) -> std::string {
            const auto& cls = view.EnclosingClassAtHops(m.member.hops);
            const auto& var = cls.members.Get(m.member.var);
            return std::format(
                "{}->{}", RenderExpr(view, view.Expr(m.receiver)), var.name);
          },
          [&](const mir::ClosureExpr& cl) -> std::string {
            return RenderClosureExpr(view, cl, expr.type);
          },
          [&](const mir::ConcatExpr& c) -> std::string {
            return RenderConcatExpr(view, expr, c);
          },
          [&](const mir::ReplicationExpr& r) -> std::string {
            return RenderReplicationExpr(view, expr, r);
          },
          [&](const mir::ArrayLiteralExpr& a) -> std::string {
            return RenderArrayLiteralExpr(view, expr, a);
          },
          [&](const mir::TupleExpr& t) -> std::string {
            return RenderTupleExpr(view, expr, t);
          },
          [&](const mir::AwaitExpr& a) -> std::string {
            return std::format(
                "co_await {}", RenderExpr(view, view.Expr(a.awaitable)));
          },
          [&](const mir::TupleGetExpr& g) -> std::string {
            return std::format(
                "std::get<{}>({})", g.index,
                RenderExpr(view, view.Expr(g.tuple)));
          },
      },
      expr.data);
}

auto RenderConditionAsBool(const ScopeView& view, const mir::Expr& expr)
    -> std::string {
  // PackedArray's `explicit operator bool` fires in any boolean context (if /
  // while / for / ternary cond / `&&` / `||` / `!`), so no wrapping needed.
  return RenderExpr(view, expr);
}

}  // namespace lyra::backend::cpp

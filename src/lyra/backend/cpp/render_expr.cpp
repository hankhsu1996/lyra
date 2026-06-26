#include "lyra/backend/cpp/render_expr.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/backend/cpp/render_call.hpp"
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
  const auto& ty = view.Unit().types.Get(expr.type);
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
  const auto& ty = view.Unit().types.Get(expr.type);
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
  return std::format("self->{}", name);
}

auto LookupLocalName(const ScopeView& view, const mir::LocalRef& ref)
    -> std::string {
  // Every callable body is a static function over the explicit receiver `self`
  // (its `vars[0]`), so a local reference renders as its own name -- `self`
  // included, which is just the first parameter's name.
  return view.BlockAtHops(ref.hops).vars.Get(ref.var).name;
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

// The BuiltinFn name and namespace tables, plus the per-callee-variant call
// renderers and the `RenderCallExpr` dispatch, live in `render_call.cpp` so
// this file stays focused on the per-Expr-shape dispatch.

}  // namespace

// LHS expression render: produces a write-target reference (a name, a
// dereference, or a chain of container-access `CallExpr`s whose runtime
// overloads return write-through references). The observable-cell
// `Mutate(svc)` adapter is already in MIR as a `DerefExpr` wrapping an
// `ObservableMethod{kMutate}` call -- this render emits nothing implicit
// on top of the explicit MIR shape.
// The C++ field name a member access reaches. The member belongs to the
// receiver's class, named by the receiver's object type -- so the class is
// resolved from the receiver, not from the ambient enclosing-class chain.
auto MemberFieldName(const ScopeView& view, const mir::MemberAccessExpr& m)
    -> const std::string& {
  const mir::TypeId recv_type = view.Expr(m.receiver).type;
  const auto& ptr =
      std::get<mir::PointerType>(view.Unit().types.Get(recv_type).data);
  return view.ClassByObjectType(ptr.pointee).members.Get(m.member.var).name;
}

auto RenderLhsExpr(const ScopeView& view, const mir::Expr& expr)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::MemberAccessExpr& m) -> std::string {
            return std::format(
                "{}->{}", RenderExpr(view, view.Expr(m.receiver)),
                MemberFieldName(view, m));
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
          // An unpacked-struct member write: a tuple component reached through
          // an addressable base (the Mutate snapshot). The deducing-this `Get`
          // yields a mutable reference on the mutable base, so the projection
          // is itself the assignment target.
          [&](const mir::TupleGetExpr& g) -> std::string {
            return std::format(
                "({}).template Get<{}>()",
                RenderLhsExpr(view, view.Expr(g.tuple)), g.index);
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

// A closure renders from its code (parameters, result type, body) and its bound
// environment alone. A synchronous closure is a capture-clause lambda whose
// captures are the bound parameters and whose lambda parameters are the unbound
// (per-invocation) ones. A coroutine closure (result type `Coroutine`) is a
// stateless lambda whose bound parameters pass as frame-copied lambda
// parameters supplied by an immediate call -- a capturing coroutine lambda
// would dangle once the spawned branch outlives the referencing site.
auto RenderClosureExpr(
    const ScopeView& view, const mir::ClosureExpr& closure,
    mir::TypeId result_type) -> std::string {
  if (closure.code == nullptr) {
    throw InternalError("RenderClosureExpr: closure has no code");
  }
  const mir::CallableCode& code = *closure.code;

  const std::string return_clause = std::format(
      " -> {}", RenderTypeAsCpp(view.Unit(), view.Class(), result_type));

  const ScopeView body_view =
      ScopeView::ForRoot(view.Unit(), view.Class(), code.body);
  const std::string body =
      std::format(" {{\n{}}}", RenderBlockStatements(body_view, 1));

  const auto is_bound = [&](mir::LocalId param) {
    return std::ranges::any_of(
        closure.environment,
        [&](const mir::EnvBinding& b) { return b.param == param; });
  };

  if (std::holds_alternative<mir::CoroutineType>(
          view.Unit().types.Get(result_type).data)) {
    for (const mir::LocalId param : code.params) {
      if (!is_bound(param)) {
        throw InternalError(
            "RenderClosureExpr: coroutine closure has an unbound parameter");
      }
    }
    std::string params;
    std::string args;
    for (std::size_t i = 0; i < closure.environment.size(); ++i) {
      const auto& bind = code.body.vars.Get(closure.environment[i].param);
      if (i != 0) {
        params += ", ";
        args += ", ";
      }
      params += RenderBindingParamDecl(view, bind);
      args += RenderExpr(view, view.Expr(closure.environment[i].value));
    }
    return std::format("[]({}){}{}({})", params, return_clause, body, args);
  }

  // The capture clause never contains `[this]`, `[=]`, or `[&]` -- every entry
  // is a named by-value binding `name = <value>`; an alias binding's value is a
  // reference-construct, so it binds a `Ref<T>` without a hidden C++ reference.
  std::string captures_text;
  for (std::size_t i = 0; i < closure.environment.size(); ++i) {
    const std::string& bind_name =
        code.body.vars.Get(closure.environment[i].param).name;
    if (i != 0) captures_text += ", ";
    captures_text += std::format(
        "{} = {}", bind_name,
        RenderExpr(view, view.Expr(closure.environment[i].value)));
  }

  std::string params_text;
  bool first_param = true;
  for (const mir::LocalId param : code.params) {
    if (is_bound(param)) continue;
    if (!first_param) params_text += ", ";
    params_text += RenderBindingParamDecl(view, code.body.vars.Get(param));
    first_param = false;
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
    const auto& op_ty = view.Unit().types.Get(op_expr.type);
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
  const auto& result_ty = view.Unit().types.Get(expr.type);
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
  const auto& container_ty = view.Unit().types.Get(expr.type);
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
  const auto& count_ty = view.Unit().types.Get(view.Expr(r.count).type);
  std::string count_text = count_ty.IsIntegralPacked()
                               ? std::format("({}).ToInt64()", count)
                               : count;
  const auto& result_ty = view.Unit().types.Get(expr.type);
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

// Reinterprets a borrowed pointer as a different pointer type stated by the
// expression's `type`. Renders as `static_cast<DestType>(operand)`; the
// destination spelling comes from the type table, not from any local inference.
auto RenderPointerCastExpr(
    const ScopeView& view, const mir::PointerCastExpr& cast, mir::TypeId dest)
    -> std::string {
  return "static_cast<" + RenderTypeAsCpp(view.Unit(), view.Class(), dest) +
         ">(" + RenderExpr(view, view.Expr(cast.operand)) + ")";
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
          [&](const mir::PointerCastExpr& c) -> std::string {
            return RenderPointerCastExpr(view, c, expr.type);
          },
          [&](const mir::MemberAccessExpr& m) -> std::string {
            return std::format(
                "{}->{}", RenderExpr(view, view.Expr(m.receiver)),
                MemberFieldName(view, m));
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
                "({}).template Get<{}>()", RenderExpr(view, view.Expr(g.tuple)),
                g.index);
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

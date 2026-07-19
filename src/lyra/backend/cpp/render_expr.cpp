#include "lyra/backend/cpp/render_expr.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_call.hpp"
#include "lyra/backend/cpp/render_stmt.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/string_literal.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/binary_op.hpp"
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
  // A literal materializes as a construction of its own value: a narrow 2-state
  // value via Int / FromInt, a wide or X/Z-bearing value via FromWords. The
  // shape -- a 1-D vector or a multi-dimensional stack -- rides in the
  // PackedType, so the same value path serves any rank and always carries the
  // literal's actual bits.
  const bool literal_has_xz = !c.state_words.empty() && c.state_words[0] != 0U;
  const bool needs_word_planes = c.width > 64U || literal_has_xz;

  if (!needs_word_planes) {
    const auto value = IntegralConstantToInt64(c);
    // A 1-D 32-bit signed value has the named shorthands Int (2-state) /
    // Integer (4-state); both read far better than the explicit FromInt for the
    // ubiquitous `int` / `integer` literal. A multi-dimensional 32-bit type
    // takes the general path so its dimension stack survives.
    if (pa.dims.size() == 1U && pa.BitWidth() == 32U &&
        pa.signedness == mir::Signedness::kSigned) {
      return std::format(
          "lyra::value::PackedArray::{}({})",
          pa.atom == mir::BitAtom::kBit ? "Int" : "Integer", value);
    }
    return std::format(
        "lyra::value::PackedArray::FromInt({}LL, {})", value,
        RenderPackedType(pa));
  }

  const bool is_four_state = pa.atom != mir::BitAtom::kBit;
  if (literal_has_xz && !is_four_state) {
    throw InternalError(
        "RenderPackedArrayIntegerLiteral: 2-state PackedArray cannot hold "
        "X/Z literal");
  }
  // Word planes ride as `std::array` spans (a literal that does not fit an
  // int64 carrier is the rare wide / X-bearing case); the empty unknown plane
  // of a 2-state value is an empty span.
  const std::size_t n = (pa.BitWidth() + 63U) / 64U;
  const std::string value_init = std::format(
      "std::array<std::uint64_t, {}>{}", n, RenderWordList(c.value_words, n));
  const std::string unknown_init =
      is_four_state ? std::format(
                          "std::array<std::uint64_t, {}>{}", n,
                          RenderWordList(c.state_words, n))
                    : std::string{"std::span<const std::uint64_t>{}"};
  return std::format(
      "lyra::value::PackedArray::FromWords({}, {}, {})", value_init,
      unknown_init, RenderPackedType(pa));
}

auto RenderIntegerLiteralExpr(
    const ScopeView& view, const mir::Expr& expr,
    const mir::IntegerLiteral& lit) -> std::string {
  const auto& ty = view.Unit().types.Get(expr.type);
  if (std::holds_alternative<mir::MachineIntType>(ty.data)) {
    // A machine integer is a raw target scalar, so its literal is the bare
    // numeric value -- no `PackedArray` wrapper.
    return std::to_string(IntegralConstantToInt64(lit.value));
  }
  if (!ty.IsIntegralPacked()) {
    throw InternalError(
        "RenderIntegerLiteralExpr: IntegerLiteral not typed as "
        "PackedArrayType, EnumType, or MachineIntType");
  }
  auto body = RenderPackedArrayIntegerLiteral(ty.AsIntegralPacked(), lit.value);
  if (ty.IsEnum()) {
    return std::format(
        "{}{{{}}}", RenderEnumClassName(view.Unit(), expr.type), body);
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

auto LookupLocalName(const ScopeView& view, const mir::LocalRef& ref)
    -> std::string {
  // Every local -- including `self` (`locals[0]`), which the method emit
  // seeds from `this` -- renders as its declared name.
  return view.Local(ref).name;
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

// Emits the host-bool reduction the node states, so a condition and a native
// C++ logical operand read a value as a boolean the same way, without leaving
// the boolean decision to a contextual conversion at the use site.
auto RenderBoolCastExpr(const ScopeView& view, const mir::BoolCastExpr& b)
    -> std::string {
  return std::format("bool({})", RenderExpr(view, view.Expr(b.operand)));
}

auto RenderConditionalExpr(const ScopeView& view, const mir::ConditionalExpr& c)
    -> std::string {
  return std::format(
      "({} ? {} : {})", RenderExpr(view, view.Expr(c.condition)),
      RenderExpr(view, view.Expr(c.then_value)),
      RenderExpr(view, view.Expr(c.else_value)));
}

// Converts a machine integer to the machine integer named by the enclosing
// `Expr::type` -- a truncation or an extension, which `static_cast` performs.
// This is a machine conversion, not a simulation-value one: every SV value
// reshape (integral resize, real <-> integral, packed <-> string) is a
// `CallExpr` against a `lyra::value` factory and renders through the call path.
auto RenderIntCastExpr(
    const ScopeView& view, const mir::Expr& expr, const mir::IntCastExpr& cast)
    -> std::string {
  return std::format(
      "static_cast<{}>({})", RenderTypeAsCpp(view.Unit(), expr.type),
      RenderExpr(view, view.Expr(cast.operand)));
}

// The BuiltinFn name and namespace tables, plus the per-callee-variant call
// renderers and the `RenderCallExpr` dispatch, live in `render_call.cpp` so
// this file stays focused on the per-Expr-shape dispatch.

}  // namespace

// How a field access reaches its field: the C++ field name, plus whether it is
// reached through the receiver (`recv->name`, an object member) or named
// directly in scope (bare `name`, a closure capture that is an in-scope lambda
// binding). This is the one place that maps a field-bearing nominal receiver
// and a field id to a rendered field, so a new receiver kind is added here, not
// at every access site.
struct FieldAccess {
  std::string name;
  bool through_receiver;
};

// The C++ name of a closure capture, distinct from the field's source name. A
// capture is realized as a lambda capture and shares the lambda's scope with
// the closure's per-invocation parameters and body locals, so its name must not
// collide with a parameter -- a nested clause may capture an enclosing iterator
// whose source name matches this closure's own iterator parameter -- nor with
// another capture of the same source name. The field id disambiguates: it is
// unique within the closure and is a shape no source-level name carries. This
// stays in the backend so the MIR field name remains the plain source name.
auto ClosureCaptureCppName(const mir::ClosureDecl& decl, mir::FieldId field)
    -> std::string {
  return std::format("{}_c{}", decl.fields.Get(field).name, field.value);
}

// The field name for a reference-storage receiver -- a class instance or a
// promoted scope. Both are reference storage reached through the receiver, so a
// field access is one shape (`recv->field`); they differ only in which field
// list the pointee type names.
auto ReferenceStorageFieldName(
    const ScopeView& view, mir::TypeId pointee, mir::FieldId field)
    -> std::string_view {
  const auto& data = view.Unit().types.Get(pointee).data;
  if (const auto* s = std::get_if<mir::StructType>(&data)) {
    return view.Unit().GetStruct(s->struct_id).fields.Get(field).name;
  }
  return view.ClassByObjectType(pointee).fields.Get(field).name;
}

auto ResolveFieldAccess(const ScopeView& view, const mir::FieldAccessExpr& m)
    -> FieldAccess {
  // The receiver reaches its field-bearing value through a borrowed pointer (a
  // class `self`, a closure receiver), a shared handle (a promoted scope), or a
  // managed reference (a class handle). The field target is owner-qualified
  // for a class receiver (owner names the declaring class arena) and a bare
  // field id otherwise (struct or closure aggregate, whose arena is uniquely
  // determined by the receiver's type).
  //
  // A closure captures its fields into a lambda whose captures are in scope,
  // so a read over the closure receiver is the bare capture name, not a
  // receiver dereference. Every other receiver is `recv->field`.
  return std::visit(
      Overloaded{
          [&](const mir::FieldTarget& t) -> FieldAccess {
            const auto& cls = view.Unit().GetClass(t.owner);
            return FieldAccess{
                .name = cls.fields.Get(t.slot).name, .through_receiver = true};
          },
          [&](const mir::FieldId& id) -> FieldAccess {
            const mir::TypeId recv_type = view.Expr(m.receiver).type;
            const auto& recv_data = view.Unit().types.Get(recv_type).data;
            mir::TypeId pointee{};
            if (const auto* ptr = std::get_if<mir::PointerType>(&recv_data)) {
              pointee = ptr->pointee;
            } else {
              throw InternalError(
                  "ResolveFieldAccess: bare-field-id access expects a pointer "
                  "receiver (struct or closure aggregate)");
            }
            const auto& pointee_data = view.Unit().types.Get(pointee).data;
            if (const auto* c = std::get_if<mir::ClosureType>(&pointee_data)) {
              return FieldAccess{
                  .name = ClosureCaptureCppName(
                      view.Unit().GetClosure(c->closure_id), id),
                  .through_receiver = false};
            }
            if (const auto* s = std::get_if<mir::StructType>(&pointee_data)) {
              return FieldAccess{
                  .name =
                      view.Unit().GetStruct(s->struct_id).fields.Get(id).name,
                  .through_receiver = true};
            }
            throw InternalError(
                "ResolveFieldAccess: bare-field-id access on a receiver "
                "that is neither a struct nor a closure");
          }},
      m.field);
}

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
          [&](const mir::FieldAccessExpr& m) -> std::string {
            const FieldAccess field = ResolveFieldAccess(view, m);
            if (!field.through_receiver) {
              return field.name;
            }
            return std::format(
                "{}->{}", RenderExpr(view, view.Expr(m.receiver)), field.name);
          },
          [&](const mir::LocalRef& l) -> std::string {
            return LookupLocalName(view, l);
          },
          [&](const mir::StaticConstantRef& r) -> std::string {
            const mir::Class& cls = view.Class();
            return std::format(
                "{}::{}", ToCppName(cls.name),
                cls.static_constants.Get(r.constant).name);
          },
          [&](const mir::StaticPropertyRef& r) -> std::string {
            const mir::Class& owner_cls = view.Unit().GetClass(r.owner);
            return std::format(
                "{}::{}", ToCppName(owner_cls.name),
                owner_cls.static_properties.Get(r.prop).name);
          },
          [&](const mir::ExternalUnitVariableRef& r) -> std::string {
            return std::format(
                "{}::{}", ToCppName(r.unit_name), r.variable_name);
          },
          // HIR-to-MIR lowers an LHS selector chain to write-form nodes -- a
          // container-access `CallExpr` with a write callee (per
          // `mir::IsContainerAccessCall`), a `UnionGetRefExpr`, a
          // `TupleGetExpr` on a mutable base -- whose value-side render is
          // already a writable place. So the lvalue rendering of a container
          // access is just its value-side render, with no extra fix-up here.
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
          // A union member write: a reference to the active member reached
          // through the addressable union (the Mutate snapshot). The reference
          // makes the member active, so the projection is itself the assignment
          // target and composes for a nested `u.f.g`.
          [&](const mir::UnionGetRefExpr& g) -> std::string {
            return std::format(
                "({}).template GetRef<{}>()",
                RenderLhsExpr(view, view.Expr(g.union_value)), g.index);
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
  return std::holds_alternative<mir::FieldAccessExpr>(expr.data) ||
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
      "{} {}", RenderTypeAsCpp(view.Unit(), bind.type), bind.name);
}

// A struct value renders as C++ aggregate initialization, one designated
// initializer per field in the construction's listed evaluation order:
// `StructName{.x = a, .y = b}`.
auto RenderStructConstructExpr(
    const ScopeView& view, const mir::StructConstructExpr& construct)
    -> std::string {
  const mir::StructDecl& decl = view.Unit().GetStruct(construct.struct_id);
  std::string inits;
  bool first = true;
  for (const mir::FieldInit& init : construct.field_inits) {
    if (!first) inits += ", ";
    inits += std::format(
        ".{} = {}", decl.fields.Get(init.target).name,
        RenderExpr(view, view.Expr(init.value)));
    first = false;
  }
  return std::format("{}{{{}}}", decl.name, inits);
}

// A closure renders as a lambda whose captured fields are the closure's fields,
// in field order. A captured read in the body resolves to the bare field name
// (an in-scope lambda binding), so the capture clause and the body agree by
// construction. The capture list is derived solely from the closure's fields
// and field order and this construction's field initializers -- never
// re-inferred from the body. A synchronous closure captures each field by value
// (`[name = init]`) and renders the closure's per-invocation `params` as lambda
// parameters. A coroutine closure (result
// type `Coroutine`) is a stateless lambda whose captured fields pass as
// frame-copied parameters supplied by an immediate call -- a capturing
// coroutine lambda would dangle once the spawned branch outlives the
// referencing site. The clause never contains `[this]`, `[=]`, or `[&]`: each
// entry is a by-value field, and an alias field is a `Ref<T>`, not a hidden C++
// reference.
auto RenderClosureExpr(const ScopeView& view, const mir::ClosureExpr& construct)
    -> std::string {
  const mir::ClosureDecl& decl = view.Unit().GetClosure(construct.closure);
  const mir::CallableCode& code = decl.invoke;

  const std::string return_clause =
      std::format(" -> {}", RenderTypeAsCpp(view.Unit(), code.result_type));

  const ScopeView body_view = view.WithClosure(code);
  const std::string body =
      std::format(" {{\n{}}}", RenderBlockStatements(body_view, 1));

  if (std::holds_alternative<mir::CoroutineType>(
          view.Unit().types.Get(code.result_type).data)) {
    if (!code.params.empty()) {
      throw InternalError(
          "RenderClosureExpr: coroutine closure has per-invocation parameters");
    }
    std::string params_text;
    std::string args_text;
    bool first = true;
    for (const mir::FieldId field_id : decl.field_order) {
      if (!first) {
        params_text += ", ";
        args_text += ", ";
      }
      const mir::FieldDecl& field = decl.fields.Get(field_id);
      params_text += std::format(
          "{} {}", RenderTypeAsCpp(view.Unit(), field.type),
          ClosureCaptureCppName(decl, field_id));
      args_text += RenderExpr(
          view, view.Expr(construct.field_inits[field_id.value].value));
      first = false;
    }
    return std::format(
        "[]({}){}{}({})", params_text, return_clause, body, args_text);
  }

  std::string captures_text;
  bool first_capture = true;
  for (const mir::FieldId field_id : decl.field_order) {
    if (!first_capture) captures_text += ", ";
    captures_text += std::format(
        "{} = {}", ClosureCaptureCppName(decl, field_id),
        RenderExpr(
            view, view.Expr(construct.field_inits[field_id.value].value)));
    first_capture = false;
  }

  std::string params_text;
  bool first_param = true;
  for (const mir::LocalId param : code.params) {
    if (!first_param) params_text += ", ";
    params_text += RenderBindingParamDecl(view, code.locals.Get(param));
    first_param = false;
  }

  return std::format(
      "[{}]({}){}{}", captures_text, params_text, return_clause, body);
}

auto RenderConcatExpr(
    const ScopeView& view, const mir::Expr& expr, const mir::ConcatExpr& c)
    -> std::string {
  const auto& result_ty = view.Unit().types.Get(expr.type);
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
      "std::array<{}, {}>{{", RenderTypeAsCpp(view.Unit(), elem_type_id),
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
  std::string out =
      std::format("{}{{", RenderTypeAsCpp(view.Unit(), expr.type));
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
// canonicalization: `&(*p)` collapses to `p` directly when `p` is a borrowed
// pointer, avoiding a no-op round-trip; dereferencing a managed handle yields
// the object, whose address is a distinct borrowed pointer, so that case does
// not collapse.
auto RenderAddressOfExpr(const ScopeView& view, const mir::AddressOfExpr& a)
    -> std::string {
  const mir::Expr& operand_expr = view.Expr(a.operand);
  if (const auto* deref = std::get_if<mir::DerefExpr>(&operand_expr.data)) {
    const mir::Expr& inner = view.Expr(deref->pointer);
    if (std::holds_alternative<mir::PointerType>(
            view.Unit().types.Get(inner.type).data)) {
      return RenderExpr(view, inner);
    }
  }
  return std::format("&{}", RenderLhsExpr(view, operand_expr));
}

// Reinterprets a borrowed pointer as a different pointer type stated by the
// expression's `type`. Renders as `static_cast<DestType>(operand)`; the
// destination spelling comes from the type table, not from any local inference.
auto RenderPointerCastExpr(
    const ScopeView& view, const mir::PointerCastExpr& cast, mir::TypeId dest)
    -> std::string {
  return "static_cast<" + RenderTypeAsCpp(view.Unit(), dest) + ">(" +
         RenderExpr(view, view.Expr(cast.operand)) + ")";
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
          [](const mir::MachineIntLiteral& h) -> std::string {
            return std::format("{}LL", h.value);
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
          [&](const mir::IntCastExpr& cast) -> std::string {
            return RenderIntCastExpr(view, expr, cast);
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
          [&](const mir::MoveExpr& m) -> std::string {
            return std::format(
                "std::move({})", RenderExpr(view, view.Expr(m.operand)));
          },
          [&](const mir::PointerCastExpr& c) -> std::string {
            return RenderPointerCastExpr(view, c, expr.type);
          },
          [&](const mir::FieldAccessExpr& m) -> std::string {
            const FieldAccess field = ResolveFieldAccess(view, m);
            if (!field.through_receiver) {
              return field.name;
            }
            return std::format(
                "{}->{}", RenderExpr(view, view.Expr(m.receiver)), field.name);
          },
          [&](const mir::StructConstructExpr& sc) -> std::string {
            return RenderStructConstructExpr(view, sc);
          },
          [&](const mir::FunctionRef& fr) -> std::string {
            const mir::Class& cls = view.Class();
            return std::format(
                "&{}::{}", ToCppName(cls.name),
                cls.abi_adapters.Get(fr.adapter).name);
          },
          [&](const mir::StaticConstantRef& r) -> std::string {
            const mir::Class& cls = view.Class();
            return std::format(
                "{}::{}", ToCppName(cls.name),
                cls.static_constants.Get(r.constant).name);
          },
          [&](const mir::StaticPropertyRef& r) -> std::string {
            const mir::Class& owner_cls = view.Unit().GetClass(r.owner);
            return std::format(
                "{}::{}", ToCppName(owner_cls.name),
                owner_cls.static_properties.Get(r.prop).name);
          },
          [&](const mir::ExternalUnitVariableRef& r) -> std::string {
            return std::format(
                "{}::{}", ToCppName(r.unit_name), r.variable_name);
          },
          [&](const mir::ClosureExpr& cl) -> std::string {
            return RenderClosureExpr(view, cl);
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
          [&](const mir::UnionExpr& u) -> std::string {
            return std::format(
                "{}::Make<{}>({})", RenderTypeAsCpp(view.Unit(), expr.type),
                u.index, RenderExpr(view, view.Expr(u.value)));
          },
          [&](const mir::UnionGetExpr& g) -> std::string {
            return std::format(
                "({}).template Get<{}>()",
                RenderExpr(view, view.Expr(g.union_value)), g.index);
          },
          // The write node reaches rvalue render only as the receiver of a
          // container-access write (`u.h[i] = v`), where it is still the active
          // member reference; it renders the same as in lvalue position.
          [&](const mir::UnionGetRefExpr& g) -> std::string {
            return std::format(
                "({}).template GetRef<{}>()",
                RenderExpr(view, view.Expr(g.union_value)), g.index);
          },
      },
      expr.data);
}

}  // namespace lyra::backend::cpp

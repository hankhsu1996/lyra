#include "lyra/backend/cpp/render_expr.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

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
#include "lyra/mir/packed_type_descriptor.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderDerefExpr(const ScopeView& view, const mir::DerefExpr& d)
    -> std::string;

auto LookupLocalName(const ScopeView& view, const mir::LocalRef& ref)
    -> std::string {
  // Every local -- including `self` (`locals[0]`), which the method emit
  // seeds from `this` -- renders as its declared name.
  return view.Local(ref).name;
}

// The C++ token for an operator this target applies to two values. A shift has
// none -- C++ decides between the arithmetic and the logical form from the
// operand's signedness where SV names it in the operator -- so a shift is
// reached through the library instead.
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
    case mir::BinaryOp::kShiftLeft:
    case mir::BinaryOp::kLogicalShiftRight:
    case mir::BinaryOp::kArithmeticShiftRight:
      throw InternalError(
          "BinaryOpToken: a shift is performed by a library entry and reaches "
          "no expression; only a compound assignment names one");
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
  }
  throw InternalError("UnaryOpToken: unknown MIR UnaryOp");
}

auto RenderUnaryExpr(const ScopeView& view, const mir::UnaryExpr& u)
    -> std::string {
  return std::format(
      "({}{})", UnaryOpToken(u.op), RenderExpr(view, view.Expr(u.operand)));
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

auto ResolveFieldAccess(const ScopeView& view, const mir::FieldAccessExpr& m)
    -> FieldAccess {
  // The receiver reaches its field-bearing value through a borrowed pointer (a
  // class `self`, a closure receiver), a shared handle (a promoted scope), or a
  // managed reference (a class handle). The field target is owner-qualified
  // for a class receiver (owner names the declaring class arena) and a bare
  // field id otherwise (a struct, a closure, or another unit's object, whose
  // arena is uniquely determined by the receiver's type).
  //
  // A closure captures its fields into a lambda whose captures are in scope,
  // so a read over the closure receiver is the bare capture name, not a
  // receiver dereference. Every other receiver is `recv->field`.
  return std::visit(
      Overloaded{
          [&](const mir::FieldTarget& t) -> FieldAccess {
            // Qualified by the declaring class: a derived class may redeclare
            // a name its base already used, both storages exist at once, and
            // which of them an access reaches is fixed where the access is
            // written rather than by the receiver's type (LRM 8.14).
            const auto& cls = view.Unit().GetClass(t.owner);
            return FieldAccess{
                .name = std::format(
                    "{}::{}", ToCppName(cls.name), cls.fields.Get(t.slot).name),
                .through_receiver = true};
          },
          [&](const mir::ExternalFieldTarget& t) -> FieldAccess {
            // Cross-unit class field: the declaring unit's header pulls the
            // field name into scope through the include; the receiver reaches
            // it by its source name, which the target-language compiler
            // resolves against the receiver's static type.
            return FieldAccess{.name = t.field_name, .through_receiver = true};
          },
          // A product's component is a position rather than a name in an arena,
          // so its access composes before this dispatch is reached.
          [](const mir::ComponentTarget&) -> FieldAccess {
            throw InternalError(
                "ResolveFieldAccess: a product's component has no arena name");
          },
          [&](const mir::FieldId& id) -> FieldAccess {
            const mir::TypeId recv_type = view.Expr(m.receiver).type;
            const auto& recv_data = view.Unit().types.Get(recv_type);
            mir::TypeId pointee{};
            if (const auto* ptr = recv_data.As<mir::PointerType>()) {
              pointee = ptr->pointee;
            } else {
              throw InternalError(
                  "ResolveFieldAccess: bare-field-id access expects a pointer "
                  "receiver (a struct, a closure, or another unit's object)");
            }
            const auto& pointee_data = view.Unit().types.Get(pointee);
            if (const auto* c = pointee_data.As<mir::ClosureType>()) {
              return FieldAccess{
                  .name = ClosureCaptureCppName(
                      view.Unit().GetClosure(c->closure_id), id),
                  .through_receiver = false};
            }
            if (const auto* s = pointee_data.As<mir::StructType>()) {
              return FieldAccess{
                  .name =
                      view.Unit().GetStruct(s->struct_id).fields.Get(id).name,
                  .through_receiver = true};
            }
            if (const auto* e =
                    pointee_data.As<mir::ExternalUnitObjectType>()) {
              return FieldAccess{
                  .name = view.Unit()
                              .external_unit_objects.Get(e->object)
                              .fields.Get(id)
                              .name,
                  .through_receiver = true};
            }
            throw InternalError(
                "ResolveFieldAccess: bare-field-id access on a receiver that "
                "is not a member-bearing aggregate or object");
          }},
      m.field);
}

// Which of a field access's two readings an occurrence is. The node is one --
// `u.f` is one syntax in the target language too -- and where it stands settles
// the rest, which is what value category means.
enum class FieldPosition : std::uint8_t { kValue, kTarget };

// A product's component, reached by position. Every component is live at once,
// so the component itself answers both readings and only the receiver's own
// form tells them apart.
auto RenderComponentAccess(
    const ScopeView& view, mir::ExprId product, base::ComponentIndex index,
    FieldPosition position) -> std::string {
  const mir::Expr& receiver = view.Expr(product);
  return std::format(
      "({}).template Get<{}>()",
      position == FieldPosition::kTarget ? RenderLhsExpr(view, receiver)
                                         : RenderExpr(view, receiver),
      index.value);
}

// A member of an active-member value, reached by its declaration-order
// position. A read takes the member's value and a write takes a reference to
// it; what either means for a member that is not the live one belongs to the
// value's own type, so the same pair of forms serves a union and a tagged one.
auto RenderActiveMemberAccess(
    const ScopeView& view, mir::ExprId union_value, base::ComponentIndex index,
    FieldPosition position) -> std::string {
  const mir::Expr& receiver = view.Expr(union_value);
  return std::format(
      "({}).template {}<{}>()",
      position == FieldPosition::kTarget ? RenderLhsExpr(view, receiver)
                                         : RenderExpr(view, receiver),
      position == FieldPosition::kTarget ? "GetRef" : "Get", index.value);
}

auto RenderFieldAccessExpr(
    const ScopeView& view, const mir::FieldAccessExpr& m,
    FieldPosition position) -> std::string {
  if (const auto* c = std::get_if<mir::ComponentTarget>(&m.field)) {
    return RenderComponentAccess(view, m.receiver, c->index, position);
  }
  const FieldAccess field = ResolveFieldAccess(view, m);
  if (!field.through_receiver) {
    return field.name;
  }
  return std::format(
      "{}->{}", RenderExpr(view, view.Expr(m.receiver)), field.name);
}

// The C++ text a reference names. Every alternative comes out as a name, or a
// scope and a name joined; what differs is which table the strings are read out
// of, which is the whole of what separates one referent from another. A
// function is named by its address, since C++ spells a bare function name as a
// call.
auto RenderReferenceExpr(
    const ScopeView& view, const mir::ReferenceExpr& reference) -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::LocalRef& l) -> std::string {
            return LookupLocalName(view, l);
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
          [&](const mir::PackedTypeRef& r) -> std::string {
            return mir::PackedTypeDescriptionName(r.integral);
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
          [&](const mir::ExternalStaticPropertyRef& r) -> std::string {
            return std::format(
                "{}::{}::{}", ToCppName(r.unit_name), ToCppName(r.class_name),
                r.property_name);
          }},
      reference.target);
}

// LHS expression render: produces a write-target reference (a name, a
// dereference, or a chain of container-access `CallExpr`s whose runtime
// overloads return write-through references). A dereference of a capability
// wrapper is where the wrapper's own write protocol enters, supplied by the
// place-access dispatch on the wrapper's type.
//
// Only an access into a value aggregate spells differently in target position;
// every other addressable form is the expression as it reads. The forms are
// listed rather than defaulted, so what a target may be is stated here and
// nothing else reaches a write.
auto RenderLhsExpr(const ScopeView& view, const mir::Expr& expr)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::FieldAccessExpr& m) -> std::string {
            return RenderFieldAccessExpr(view, m, FieldPosition::kTarget);
          },
          [&](const mir::UnionMemberExpr& m) -> std::string {
            return RenderActiveMemberAccess(
                view, m.union_value, m.index, FieldPosition::kTarget);
          },
          [&](const mir::ReferenceExpr&) -> std::string {
            return RenderExpr(view, expr);
          },
          [&](const mir::CallExpr&) -> std::string {
            return RenderExpr(view, expr);
          },
          [&](const mir::DerefExpr&) -> std::string {
            return RenderExpr(view, expr);
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

// The library method that applies a shift to the value it is called on. A
// shift is performed by the library rather than applied by the target, and a
// compound assignment needs the applying form so its destination is reached
// once (LRM 11.4.1).
auto ShiftAssignMethod(mir::BinaryOp op) -> std::string_view {
  switch (op) {
    case mir::BinaryOp::kShiftLeft:
      return "ShiftLeftAssign";
    case mir::BinaryOp::kLogicalShiftRight:
      return "LogicalShiftRightAssign";
    case mir::BinaryOp::kArithmeticShiftRight:
      return "ArithmeticShiftRightAssign";
    case mir::BinaryOp::kAdd:
    case mir::BinaryOp::kSub:
    case mir::BinaryOp::kMul:
    case mir::BinaryOp::kDiv:
    case mir::BinaryOp::kMod:
    case mir::BinaryOp::kBitwiseAnd:
    case mir::BinaryOp::kBitwiseOr:
    case mir::BinaryOp::kBitwiseXor:
    case mir::BinaryOp::kEquality:
    case mir::BinaryOp::kInequality:
    case mir::BinaryOp::kGreaterEqual:
    case mir::BinaryOp::kGreaterThan:
    case mir::BinaryOp::kLessEqual:
    case mir::BinaryOp::kLessThan:
    case mir::BinaryOp::kLogicalAnd:
    case mir::BinaryOp::kLogicalOr:
      break;
  }
  throw InternalError(
      "ShiftAssignMethod: the operator is applied by the target and needs no "
      "method");
}

// C++ spells a compound assignment by suffixing the operator it applies, so
// the two forms differ only in whether the target applies the operator at all.
auto RenderCompoundAssign(
    mir::BinaryOp op, const std::string& chain, const std::string& rhs)
    -> std::string {
  if (mir::BinaryOpAsBuiltinFn(op).has_value()) {
    return std::format("{}.{}({})", chain, ShiftAssignMethod(op), rhs);
  }
  return std::format("{} {}= {}", chain, BinaryOpToken(op), rhs);
}

auto RenderAssignExpr(const ScopeView& view, const mir::AssignExpr& a)
    -> std::string {
  std::string value = RenderExpr(view, view.Expr(a.value));

  const mir::Expr& lhs_expr = view.Expr(a.target);

  // Mechanical render: the target names the storage the store reaches, whether
  // that is a plain place or the storage a capability wrapper stands for, so
  // this path emits a plain C++ assignment over whatever the LHS render
  // produces. An assignment is an expression, so it parenthesizes to keep its
  // value usable wherever it appears.
  const std::string target = RenderLhsExpr(view, lhs_expr);
  if (a.compound_op.has_value()) {
    return std::format(
        "({})", RenderCompoundAssign(*a.compound_op, target, value));
  }
  return std::format("({} = {})", target, value);
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

// The value a construction supplies for one field. A field init names its
// target, because the entries are in the source's evaluation order and that is
// not the order the fields were declared in -- so the entry for a field is the
// one that says so, never the entry sitting at the field's own position.
auto FieldInitValue(
    const std::vector<mir::FieldInit>& inits, mir::FieldId field)
    -> mir::ExprId {
  for (const mir::FieldInit& init : inits) {
    if (init.target == field) return init.value;
  }
  throw InternalError(
      "FieldInitValue: the construction supplies no value for a field it "
      "declares -- please report this as a bug");
}

// C++ has no block expression, and what stands for one is a lambda invoked
// where it is written. Capturing by reference is right here for the reason it
// is wrong for a callable value: this one runs before the statement it sits in
// finishes, so nothing it borrowed can have gone.
//
// The `return` is part of that spelling rather than a statement of the block:
// it leaves the lambda, which is how the lambda produces the block's value.
// What makes the spelling sound is that MIR admits no return among the steps,
// so the only `return` inside is this one and it can only mean the block.
auto RenderBlockExpr(const ScopeView& view, const mir::BlockExpr& block)
    -> std::string {
  const ScopeView body_view =
      view.WithBlock(view.Block().child_scopes.Get(block.scope));
  return std::format(
      "[&] {{\n{}{}return {};\n}}()", RenderBlockStatements(body_view, 1),
      Indent(1), RenderExpr(body_view, body_view.Expr(block.value)));
}

// A closure renders as a lambda whose captured fields are the closure's fields,
// in field order. A captured read in the body resolves to the bare field name
// (an in-scope lambda binding), so the capture clause and the body agree by
// construction. The capture list is derived solely from the closure's fields
// and field order and this construction's field initializers -- never
// re-inferred from the body. A synchronous closure captures each field by value
// (`[name = init]`) and renders the closure's per-invocation `params` as lambda
// parameters. A coroutine closure (result type `Coroutine`) is a stateless
// lambda whose captured fields pass as frame-copied parameters supplied by an
// immediate call -- a capturing coroutine lambda would dangle once the spawned
// branch outlives the referencing site. The clause never contains `[this]`,
// `[=]`, or `[&]`: each entry is a by-value field, and an alias field is a
// `Ref<T>`, not a hidden C++ reference.
auto RenderClosureExpr(const ScopeView& view, const mir::ClosureExpr& construct)
    -> std::string {
  const mir::ClosureDecl& decl = view.Unit().GetClosure(construct.closure);
  const mir::CallableCode& code = decl.invoke;

  const std::string return_clause =
      std::format(" -> {}", RenderTypeAsCpp(view.Unit(), code.result_type));

  const ScopeView body_view = view.WithClosure(code);
  const std::string body =
      std::format(" {{\n{}}}", RenderBlockStatements(body_view, 1));

  if (view.Unit().types.Get(code.result_type).Is<mir::CoroutineType>()) {
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
          view, view.Expr(FieldInitValue(construct.field_inits, field_id)));
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
            view, view.Expr(FieldInitValue(construct.field_inits, field_id))));
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

// A brace initializer over the parts, naming the type it builds. Naming it
// rather than leaving a bare brace list is what makes the same string correct
// standalone and in the position of an argument or an outer literal's part,
// where a bare list would be resolved against the surrounding type instead.
auto RenderPartsAsBraceInit(
    const ScopeView& view, mir::TypeId type, std::span<const mir::ExprId> parts)
    -> std::string {
  return std::format(
      "{}{{{}}}", RenderTypeAsCpp(view.Unit(), type),
      JoinCommaSeparated(RenderEachExpr(view, parts)));
}

// A dereference: the storage the operand's pointer stands for.
auto RenderDerefExpr(const ScopeView& view, const mir::DerefExpr& d)
    -> std::string {
  return std::format("(*{})", RenderExpr(view, view.Expr(d.pointer)));
}

// `&place` emitted as the C++ address-of operator.
auto RenderAddressOfExpr(const ScopeView& view, const mir::AddressOfExpr& a)
    -> std::string {
  return std::format("&{}", RenderLhsExpr(view, view.Expr(a.operand)));
}

// Re-types a reference as the reference type the expression's `type` states.
// Renders as `static_cast<DestType>(operand)`; the destination spelling comes
// from the type table, not from any local inference.
auto RenderPointerCastExpr(
    const ScopeView& view, const mir::PointerCastExpr& cast, mir::TypeId dest)
    -> std::string {
  return "static_cast<" + RenderTypeAsCpp(view.Unit(), dest) + ">(" +
         RenderExpr(view, view.Expr(cast.operand)) + ")";
}

}  // namespace

auto RenderEachExpr(
    const ScopeView& view, std::span<const mir::ExprId> operands)
    -> std::vector<std::string> {
  std::vector<std::string> rendered;
  rendered.reserve(operands.size());
  for (const mir::ExprId operand : operands) {
    rendered.push_back(RenderExpr(view, view.Expr(operand)));
  }
  return rendered;
}

auto RenderExpr(const ScopeView& view, const mir::Expr& expr) -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::StringLiteral& s) -> std::string {
            return RenderCStringLiteral(s.value);
          },
          [](const mir::NullLiteral&) -> std::string {
            return std::string{"nullptr"};
          },
          [](const mir::MachineBoolLiteral& b) -> std::string {
            return std::string{b.value ? "true" : "false"};
          },
          [&](const mir::MachineFloatLiteral& f) -> std::string {
            // A machine float is spelled at the precision its own type is
            // read back at: 9 significant digits round-trip a `float` and 17 a
            // `double`, the IEEE 754 minimum representable-pair widths, and a
            // single-precision literal carries the suffix that keeps it one.
            // `g` drops a trailing decimal point, which the C++ lexer rejects
            // before a suffix, so a whole number gets one back.
            const auto& machine =
                view.Unit().types.Get(expr.type).Get<mir::MachineFloatType>();
            const bool single = machine.bit_width == 32;
            std::string body = std::format("{:.{}g}", f.value, single ? 9 : 17);
            if (body.find_first_of(".eE") == std::string::npos) {
              body += ".0";
            }
            body += single ? "f" : "";
            return body;
          },
          [&](const mir::MachineIntLiteral& h) -> std::string {
            // A machine integer is spelled as its own type reads it. An
            // unsigned one is a bit pattern, so it is written in hex and with
            // an unsigned suffix; a signed spelling would go negative and
            // narrow where the value lands in unsigned storage.
            const auto& machine =
                view.Unit().types.Get(expr.type).Get<mir::MachineIntType>();
            if (machine.signedness == mir::Signedness::kUnsigned) {
              return std::format(
                  "0x{:x}ULL", static_cast<std::uint64_t>(h.value));
            }
            return std::format("{}LL", h.value);
          },
          [&](const mir::ReferenceExpr& r) -> std::string {
            return RenderReferenceExpr(view, r);
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
          [&](const mir::BlockExpr& b) -> std::string {
            return RenderBlockExpr(view, b);
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
          [&](const mir::FunctionCastExpr& c) -> std::string {
            return std::format(
                "reinterpret_cast<{}>({})",
                RenderTypeAsCpp(view.Unit(), expr.type),
                RenderExpr(view, view.Expr(c.operand)));
          },
          [&](const mir::MachineArrayDataExpr& d) -> std::string {
            return std::format(
                "({}).data()", RenderExpr(view, view.Expr(d.array)));
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
            return RenderFieldAccessExpr(view, m, FieldPosition::kValue);
          },
          [&](const mir::ClosureExpr& cl) -> std::string {
            return RenderClosureExpr(view, cl);
          },
          // The value is unchanged and so is its C++ type: an enumeration and
          // its base share one runtime class, so ascribing the other type to a
          // value spells nothing here.
          [&](const mir::ValueCastExpr& v) -> std::string {
            return RenderExpr(view, view.Expr(v.operand));
          },
          [&](const mir::CompositeExpr& c) -> std::string {
            return RenderPartsAsBraceInit(view, expr.type, c.parts);
          },
          [&](const mir::VectorExpr& v) -> std::string {
            return RenderPartsAsBraceInit(view, expr.type, v.elements);
          },
          [&](const mir::AwaitExpr& a) -> std::string {
            return std::format(
                "co_await {}", RenderExpr(view, view.Expr(a.awaitable)));
          },
          [&](const mir::VectorGetExpr& g) -> std::string {
            return std::format(
                "({})[{}]", RenderExpr(view, view.Expr(g.vector)),
                RenderExpr(view, view.Expr(g.index)));
          },
          [&](const mir::UnionExpr& u) -> std::string {
            return std::format(
                "{}::Make<{}>({})", RenderTypeAsCpp(view.Unit(), expr.type),
                u.index.value, RenderExpr(view, view.Expr(u.value)));
          },
          [&](const mir::TaggedIsExpr& g) -> std::string {
            return std::format(
                "({}).template IsTagged<{}>()",
                RenderExpr(view, view.Expr(g.union_value)), g.tag_index.value);
          },
          [&](const mir::UnionMemberExpr& m) -> std::string {
            return RenderActiveMemberAccess(
                view, m.union_value, m.index, FieldPosition::kValue);
          },
      },
      expr.data);
}

}  // namespace lyra::backend::cpp

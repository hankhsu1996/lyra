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
#include "lyra/backend/cpp/naming.hpp"
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

auto LookupLocalName(const ScopeView& view, const mir::LocalRef& ref)
    -> std::string {
  // Every local -- including `self` (`locals[0]`), which the method emit
  // seeds from `this` -- renders as its declared name spelled for this target.
  return ToCppName(view.Local(ref).name);
}

// The C++ token for an operator this target applies to two values.
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

auto RenderConditionalExpr(const ScopeView& view, const mir::ConditionalExpr& c)
    -> std::string {
  return std::format(
      "({} ? {} : {})", RenderExpr(view, view.Expr(c.condition)),
      RenderExpr(view, view.Expr(c.then_value)),
      RenderExpr(view, view.Expr(c.else_value)));
}

// The operand read as the type the expression has, written as the C++ cast
// notation from the one to the other. That notation is the C++ spelling for
// whichever conversion a pair of types calls for, so the pair decides the
// conversion here as it does in the node, and this names no type of its own.
//
// The enclosing parentheses are load-bearing: cast notation is not a primary
// expression, so a `->` or a `[` written after it would take the cast's own
// operand instead, and the conversion would silently apply to the wrong thing.
//
// A reference to an object is the exception, and the pair is what says so:
// every static view of one is the same target type, so cast notation would
// copy it unchanged where what the pair calls for is the same object under
// another view. The conversion is named for that instead, and the two classes
// it takes are the ones the pair already carries.
auto RenderCastExpr(
    const ScopeView& view, const mir::Expr& expr, const mir::CastExpr& cast)
    -> std::string {
  const mir::Expr& operand = view.Expr(cast.operand);
  const auto* from =
      view.Unit().types.Get(operand.type).As<mir::ManagedRefType>();
  const auto* to = view.Unit().types.Get(expr.type).As<mir::ManagedRefType>();
  if (from != nullptr && to != nullptr) {
    return std::format(
        "{}<{}, {}>({})", ObjectViewConversionCppName(),
        RenderTypeAsCpp(view.Unit(), from->pointee),
        RenderTypeAsCpp(view.Unit(), to->pointee), RenderExpr(view, operand));
  }
  return std::format(
      "(({})({}))", RenderTypeAsCpp(view.Unit(), expr.type),
      RenderExpr(view, operand));
}

// The C++ name of a closure capture, which is not the field's source name. A
// capture is realized as a lambda capture and shares the lambda's scope with
// the closure's per-invocation parameters and body locals, so its name must not
// collide with a parameter -- a nested clause may capture an enclosing iterator
// whose source name matches this closure's own iterator parameter -- nor with
// another capture of the same source name. Its position in the closure is
// unique within it and reaches no source name, so that is what it is minted
// from. This stays in the backend so the MIR field name remains the plain
// source name.
auto ClosureCaptureCppName(mir::FieldId field) -> std::string {
  return MintedCppName("capture", field.value);
}

auto RenderFieldAccessExpr(const ScopeView& view, const mir::FieldAccessExpr& m)
    -> std::string {
  const auto through_receiver = [&](std::string_view name) {
    const mir::Expr& receiver = view.Expr(m.receiver);
    return std::format(
        "{}.{}",
        RenderPlaceAccessAsCpp(
            view.Unit(), receiver.type, RenderExpr(view, receiver)),
        name);
  };
  return std::visit(
      Overloaded{
          [&](const mir::ClassFieldTarget& t) -> std::string {
            // Qualified by the declaring class: a derived class may redeclare
            // a name its base already used, both storages exist at once, and
            // which of them an access reaches is fixed where the access is
            // written rather than by the receiver's type (LRM 8.14).
            const auto& cls = view.Unit().GetClass(t.owner);
            return through_receiver(
                std::format(
                    "{}::{}", ToCppName(cls.name),
                    ToCppName(cls.fields.Get(t.slot).name)));
          },
          [&](const mir::StructFieldTarget& t) -> std::string {
            return through_receiver(ToCppName(
                view.Unit().GetStruct(t.owner).fields.Get(t.slot).name));
          },
          [&](const mir::ClosureFieldTarget& t) -> std::string {
            // A closure is emitted as a lambda whose captures are bindings of
            // the enclosing scope, so naming the capture is the whole access
            // and the receiver never appears.
            return ClosureCaptureCppName(t.slot);
          },
          [&](const mir::ExternalUnitObjectFieldTarget& t) -> std::string {
            return through_receiver(
                ToCppName(view.Unit()
                              .external_unit_objects.Get(t.owner)
                              .fields.Get(t.slot)
                              .name));
          },
          [&](const mir::CrossUnitClassFieldTarget& t) -> std::string {
            // The declaring unit's header pulls the property name into scope
            // through the include, so the receiver reaches it by its source
            // name and the target-language compiler resolves it against the
            // receiver's static type. The slot is what the access states, so
            // the name is read out of what that class promised rather than
            // restated at the access.
            const mir::ExternalClass* declaring = mir::FindExternalClass(
                view.Unit().external_classes, t.unit_name, t.class_name);
            if (declaring == nullptr ||
                t.slot.value >= declaring->fields.size()) {
              throw InternalError(
                  "RenderFieldAccessExpr: a property access names a slot no "
                  "consumed promise describes");
            }
            return through_receiver(
                ToCppName(declaring->fields.Get(t.slot).name));
          },
          [](const mir::ResolvedFieldTarget&) -> std::string {
            throw InternalError(
                "RenderFieldAccessExpr: a storage position that arrived as a "
                "value reached a backend that states it does not render one");
          }},
      m.field);
}

// The C++ text a reference names. Every alternative comes out as a name, or a
// scope and a name joined; what differs is which table the strings are read out
// of, which is the whole of what separates one referent from another. A
// function is named by its address, since C++ spells a bare function name as a
// call -- and that address, alone among these, is not a primary expression, so
// it carries the parentheses that let it stand wherever the others do.
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
                "(&{}::{})", ToCppName(cls.name),
                CppAbiAdapterName(fr.adapter));
          },
          [&](const mir::StaticConstantRef& r) -> std::string {
            const mir::Class& cls = view.Class();
            return std::format(
                "{}::{}", ToCppName(cls.name),
                CppStaticConstantName(r.constant));
          },
          [&](const mir::PackedTypeRef& r) -> std::string {
            return mir::PackedTypeDescriptionName(r.integral);
          },
          [&](const mir::StaticPropertyRef& r) -> std::string {
            const mir::Class& owner_cls = view.Unit().GetClass(r.owner);
            return std::format(
                "{}::{}", ToCppName(owner_cls.name),
                ToCppName(owner_cls.static_properties.Get(r.prop).name));
          },
          [&](const mir::ExternalUnitVariableRef& r) -> std::string {
            return std::format(
                "{}::{}", ToCppName(r.unit_name), ToCppName(r.variable_name));
          },
          [&](const mir::ExternalStaticPropertyRef& r) -> std::string {
            return std::format(
                "{}::{}::{}", ToCppName(r.unit_name), ToCppName(r.class_name),
                ToCppName(r.property_name));
          }},
      reference.target);
}

auto RenderAssignExpr(const ScopeView& view, const mir::AssignExpr& a)
    -> std::string {
  const std::string value = RenderExpr(view, view.Expr(a.value));

  // Mechanical render: the target names the storage the store reaches, whether
  // that is a plain place or a part of the value one holds, and either renders
  // as a C++ lvalue -- so this path emits a plain assignment over it. C++
  // spells an applied operator by suffixing its token, and evaluates the left
  // operand once, which is what LRM 11.4.1 asks. An assignment is an
  // expression, so it parenthesizes to keep its value usable wherever it
  // appears.
  const std::string target = RenderExpr(view, view.Expr(a.target));
  if (a.compound_op.has_value()) {
    return std::format(
        "({} {}= {})", target, BinaryOpToken(*a.compound_op), value);
  }
  return std::format("({} = {})", target, value);
}

auto RenderIncDecExpr(const ScopeView& view, const mir::IncDecExpr& inc)
    -> std::string {
  std::string lhs = RenderExpr(view, view.Expr(inc.target));

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
      "{} {}", RenderTypeAsCpp(view.Unit(), bind.type), ToCppName(bind.name));
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
          ClosureCaptureCppName(field_id));
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
        "{} = {}", ClosureCaptureCppName(field_id),
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

// A dereference: the storage the operand stands for.
auto RenderDerefExpr(const ScopeView& view, const mir::DerefExpr& d)
    -> std::string {
  const mir::Expr& pointer = view.Expr(d.pointer);
  return RenderPlaceAccessAsCpp(
      view.Unit(), pointer.type, RenderExpr(view, pointer));
}

// `&place` emitted as the C++ address-of operator.
auto RenderAddressOfExpr(const ScopeView& view, const mir::AddressOfExpr& a)
    -> std::string {
  return std::format("(&{})", RenderExpr(view, view.Expr(a.operand)));
}

// How a machine float literal is written so the target reads back the value it
// was given: the digit count is the IEEE 754 minimum that round-trips the
// width, and a single-precision literal carries the suffix that keeps it
// single.
struct MachineFloatSpelling {
  int digits;
  std::string_view suffix;
};

auto FloatSpellingOf(mir::MachineFloatWidth width) -> MachineFloatSpelling {
  switch (width) {
    case mir::MachineFloatWidth::k32:
      return {.digits = 9, .suffix = "f"};
    case mir::MachineFloatWidth::k64:
      return {.digits = 17, .suffix = ""};
  }
  throw InternalError("FloatSpellingOf: unknown MachineFloatWidth");
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
            // `g` drops a trailing decimal point, which the C++ lexer rejects
            // before a suffix, so a whole number gets one back.
            const auto& machine =
                view.Unit().types.Get(expr.type).Get<mir::MachineFloatType>();
            const MachineFloatSpelling spelling =
                FloatSpellingOf(machine.width);
            std::string body = std::format("{:.{}g}", f.value, spelling.digits);
            if (body.find_first_of(".eE") == std::string::npos) {
              body += ".0";
            }
            body += spelling.suffix;
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
          [&](const mir::CastExpr& c) -> std::string {
            return RenderCastExpr(view, expr, c);
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
          [&](const mir::CallExpr& call) -> std::string {
            return RenderCallExpr(view, call, expr.type);
          },
          [&](const mir::DerefExpr& d) -> std::string {
            return RenderDerefExpr(view, d);
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
          [&](const mir::FieldAccessExpr& m) -> std::string {
            return RenderFieldAccessExpr(view, m);
          },
          [&](const mir::ClosureExpr& cl) -> std::string {
            return RenderClosureExpr(view, cl);
          },
          [&](const mir::CompositeExpr& c) -> std::string {
            return RenderPartsAsBraceInit(view, expr.type, c.parts);
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
      },
      expr.data);
}

}  // namespace lyra::backend::cpp

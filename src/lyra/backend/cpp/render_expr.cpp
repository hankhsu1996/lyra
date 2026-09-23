#include "lyra/backend/cpp/render_expr.hpp"

#include <array>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_call.hpp"
#include "lyra/backend/cpp/render_stmt.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/string_literal.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"

namespace lyra::backend::cpp {

namespace {

auto LookupLocalName(const ScopeView& view, const mir::LocalRef& ref)
    -> MintedName {
  // Every local -- including the receiver (`locals[0]`), which the method emit
  // seeds from `this` -- renders under whichever of the two ranges names it.
  return CppLocalName(view.Code().named_locals, ref.var);
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

void RenderUnaryExpr(
    const ScopeView& view, const mir::UnaryExpr& u, TargetText& out) {
  Write(view, out, "(", UnaryOpToken(u.op), u.operand, ")");
}

void RenderBinaryExpr(
    const ScopeView& view, const mir::BinaryExpr& b, TargetText& out) {
  Write(view, out, "(", b.lhs, " ", BinaryOpToken(b.op), " ", b.rhs, ")");
}

void RenderConditionalExpr(
    const ScopeView& view, const mir::ConditionalExpr& c, TargetText& out) {
  Write(
      view, out, "(", c.condition, " ? ", c.then_value, " : ", c.else_value,
      ")");
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
void RenderCastExpr(
    const ScopeView& view, const mir::Expr& expr, const mir::CastExpr& cast,
    TargetText& out) {
  const mir::Expr& operand = view.Expr(cast.operand);
  const auto* from =
      view.Unit().types.Get(operand.type).As<mir::ManagedRefType>();
  const auto* to = view.Unit().types.Get(expr.type).As<mir::ManagedRefType>();
  if (from != nullptr && to != nullptr) {
    Write(
        view, out, ObjectViewConversionCppName(), "<", from->pointee, ", ",
        to->pointee, ">(", cast.operand, ")");
    return;
  }
  Write(view, out, "((", expr.type, ")(", cast.operand, "))");
}

void RenderFieldAccessExpr(
    const ScopeView& view, const mir::FieldAccessExpr& m, TargetText& out) {
  const auto write_receiver = [&]() {
    const mir::Expr& receiver = view.Expr(m.receiver);
    WriteStorageOf(out, view.Unit(), receiver.type, [&]() {
      RenderExpr(view, receiver, out);
    });
    out += ".";
  };
  std::visit(
      Overloaded{
          [&](const mir::ClassFieldTarget& t) {
            // Qualified by the declaring class: a derived class may redeclare
            // a name its base already used, both storages exist at once, and
            // which of them an access reaches is fixed where the access is
            // written rather than by the receiver's type (LRM 8.14).
            const auto& cls = view.Unit().GetClass(t.owner);
            write_receiver();
            Write(
                out, CppClassName(cls, t.owner),
                "::", CppFieldName(cls.named_fields, t.slot));
          },
          [&](const mir::StructFieldTarget& t) {
            write_receiver();
            Write(out, CppStructFieldName(t.slot));
          },
          [&](const mir::ClosureFieldTarget& t) {
            // A closure is emitted as a lambda whose captures are bindings of
            // the enclosing scope, so naming the capture is the whole access
            // and the receiver never appears.
            Write(out, CppClosureCaptureName(t.slot));
          },
          [&](const mir::CrossUnitClassFieldTarget& t) {
            // The declaring unit's header pulls the property's declaration
            // into scope through the include, so the receiver reaches it by
            // the identifier that unit emitted it under and the
            // target-language compiler resolves it against the receiver's
            // static type. The slot is what the access states; the identifier
            // is composed from it and what that class promised, by the same
            // composition the declaring side used.
            const mir::ExternalClass* declaring = mir::FindExternalClass(
                view.Unit().external_classes, t.unit_name, t.class_name);
            if (declaring == nullptr ||
                t.slot.value >= declaring->fields.size()) {
              throw InternalError(
                  "RenderFieldAccessExpr: a property access names a slot no "
                  "consumed promise describes");
            }
            write_receiver();
            Write(
                out,
                CppFieldNameOf(t.slot, declaring->fields.Get(t.slot).name));
          }},
      m.field);
}

// The C++ text a reference names. Every alternative comes out as a name, or a
// scope and a name joined; what differs is which table the name is read out of,
// which is the whole of what separates one referent from another. A
// function is named by its address, since C++ spells a bare function name as a
// call -- and that address, alone among these, is not a primary expression, so
// it carries the parentheses that let it stand wherever the others do.
void RenderReferenceExpr(
    const ScopeView& view, const mir::ReferenceExpr& reference,
    TargetText& out) {
  std::visit(
      Overloaded{
          [&](const mir::LocalRef& l) { Write(out, LookupLocalName(view, l)); },
          [&](const mir::FunctionRef& fr) {
            Write(
                out, "(&",
                CppClassName(view.Unit().GetClass(fr.owner), fr.owner),
                "::", CppAbiAdapterName(fr.adapter), ")");
          },
          [&](const mir::StaticConstantRef& r) {
            Write(
                out, CppClassName(view.Class(), view.ClassId()),
                "::", CppStaticConstantName(r.constant));
          },
          [&](const mir::ObjectRecordRef& r) {
            Write(
                out, CppClassRef(view.Unit(), r.of),
                "::", CppObjectRecordName());
          },
          [&](const mir::TypeDescriptorRef& r) {
            Write(out, CppTypeDescriptorName(r.descriptor));
          },
          [&](const mir::IntegralConstantRef& r) {
            Write(out, CppIntegralConstantName(r.constant));
          },
          [&](const mir::StaticPropertyRef& r) {
            const mir::Class& owner_cls = view.Unit().GetClass(r.owner);
            Write(
                out, CppClassName(owner_cls, r.owner), "::",
                CppStaticPropertyName(
                    owner_cls.named_static_properties, r.prop));
          },
          [&](const mir::StaticVariableRef& r) {
            // Named through the declaring unit's namespace even from a body of
            // that unit: a class body is inside its own class's scope first,
            // where a member of the same identifier would answer instead.
            Write(
                out, CppUnitScope(view.Unit().name), "::",
                CppStaticVariableName(
                    view.Unit().named_static_variables, r.variable));
          },
          [&](const mir::ExternalUnitVariableRef& r) {
            Write(
                out, CppUnitScope(r.unit_name),
                "::", ToCppName(r.variable_name));
          },
          [&](const mir::ExternalStaticPropertyRef& r) {
            Write(
                out, CppUnitScope(r.unit_name), "::", ToCppName(r.class_name),
                "::", ToCppName(r.property_name));
          }},
      reference.target);
}

// Mechanical render: the target names the storage the store reaches, whether
// that is a plain place or a part of the value one holds, and either renders
// as a C++ lvalue -- so this path emits a plain assignment over it. C++ spells
// an applied operator by suffixing its token, and evaluates the left operand
// once, which is what LRM 11.4.1 asks. An assignment is an expression, so it
// parenthesizes to keep its value usable wherever it appears.
void RenderAssignExpr(
    const ScopeView& view, const mir::AssignExpr& a, TargetText& out) {
  if (a.compound_op.has_value()) {
    Write(
        view, out, "(", a.target, " ", BinaryOpToken(*a.compound_op), "= ",
        a.value, ")");
    return;
  }
  Write(view, out, "(", a.target, " = ", a.value, ")");
}

void RenderIncDecExpr(
    const ScopeView& view, const mir::IncDecExpr& inc, TargetText& out) {
  switch (inc.op) {
    case mir::IncDecOp::kPreInc:
      Write(view, out, "(++", inc.target, ")");
      return;
    case mir::IncDecOp::kPostInc:
      Write(view, out, "(", inc.target, "++)");
      return;
    case mir::IncDecOp::kPreDec:
      Write(view, out, "(--", inc.target, ")");
      return;
    case mir::IncDecOp::kPostDec:
      Write(view, out, "(", inc.target, "--)");
      return;
  }
  throw InternalError("RenderIncDecExpr: unknown IncDecOp");
}

// Renders a binding's parameter declaration -- its type then its name. A
// `RefType` binding renders as `Ref<T> name`, a value binding as `T name`;
// the wrapper comes from the type alone, never hand-written.
void RenderBindingParamDecl(
    const ScopeView& view, const mir::CallableCode& code, mir::LocalId param,
    TargetText& out) {
  Write(
      view, out, code.locals.Get(param).type, " ",
      CppLocalName(code.named_locals, param));
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
void RenderBlockExpr(
    const ScopeView& view, const mir::BlockExpr& block, TargetText& out) {
  const ScopeView body_view =
      view.WithBlock(view.Block().child_scopes.Get(block.scope));
  out += "[&] {\n";
  {
    const TargetText::BodyDepth body(out, 1);
    RenderBlockStatements(body_view, out);
    out.OpenLine();
    out += "return ";
    RenderExpr(body_view, body_view.Expr(block.value), out);
    out += ";\n";
  }
  out += "}()";
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
void RenderClosureExpr(
    const ScopeView& view, const mir::ClosureExpr& construct, TargetText& out) {
  const mir::ClosureDecl& decl = view.Unit().GetClosure(construct.closure);
  const mir::CallableCode& code = decl.invoke;
  const ScopeView body_view = view.WithClosure(code);

  const auto write_body = [&]() {
    Write(view, out, " -> ", code.result_type, " {\n");
    {
      const TargetText::BodyDepth body(out, 1);
      RenderBlockStatements(body_view, out);
    }
    out += "}";
  };

  if (view.Unit().types.Get(code.result_type).Is<mir::CoroutineType>()) {
    if (!code.params.empty()) {
      throw InternalError(
          "RenderClosureExpr: coroutine closure has per-invocation parameters");
    }
    out += "[](";
    bool first_param = true;
    for (const mir::FieldId field_id : decl.field_order) {
      if (!first_param) out += ", ";
      const mir::FieldDecl& field = decl.fields.Get(field_id);
      Write(view, out, field.type, " ", CppClosureCaptureName(field_id));
      first_param = false;
    }
    out += ")";
    write_body();
    out += "(";
    bool first_arg = true;
    for (const mir::FieldId field_id : decl.field_order) {
      if (!first_arg) out += ", ";
      RenderExpr(
          view, view.Expr(FieldInitValue(construct.field_inits, field_id)),
          out);
      first_arg = false;
    }
    out += ")";
    return;
  }

  out += "[";
  bool first_capture = true;
  for (const mir::FieldId field_id : decl.field_order) {
    if (!first_capture) out += ", ";
    Write(out, CppClosureCaptureName(field_id), " = ");
    RenderExpr(
        view, view.Expr(FieldInitValue(construct.field_inits, field_id)), out);
    first_capture = false;
  }
  out += "](";
  bool first_param = true;
  for (const mir::LocalId param : code.params) {
    if (!first_param) out += ", ";
    RenderBindingParamDecl(view, code, param, out);
    first_param = false;
  }
  out += ")";
  write_body();
}

// A brace initializer over the parts, naming the type it builds. Naming it
// rather than leaving a bare brace list is what makes the same text correct
// standalone and in the position of an argument or an outer literal's part,
// where a bare list would be resolved against the surrounding type instead.
void RenderPartsAsBraceInit(
    const ScopeView& view, mir::TypeId type, std::span<const mir::ExprId> parts,
    TargetText& out) {
  Write(view, out, type, "{");
  WriteCommaSeparated(view, out, parts);
  out += "}";
}

// A dereference: the storage the operand stands for.
void RenderDerefExpr(
    const ScopeView& view, const mir::DerefExpr& d, TargetText& out) {
  const mir::Expr& pointer = view.Expr(d.pointer);
  WriteStorageOf(out, view.Unit(), pointer.type, [&]() {
    RenderExpr(view, pointer, out);
  });
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

void RenderMachineFloatLiteral(
    const mir::MachineFloatLiteral& f, mir::MachineFloatWidth width,
    TargetText& out) {
  const MachineFloatSpelling spelling = FloatSpellingOf(width);
  std::array<char, 64> room{};
  const std::to_chars_result converted = std::to_chars(
      room.data(), room.data() + room.size(), f.value,
      std::chars_format::general, spelling.digits);
  if (converted.ec != std::errc{}) {
    throw InternalError(
        "RenderMachineFloatLiteral: a machine float did not fit the room kept "
        "for writing one -- please report this as a bug");
  }
  const std::string_view body{
      room.data(), static_cast<std::size_t>(converted.ptr - room.data())};
  out += body;
  // The general form drops a trailing decimal point, which the C++ lexer
  // rejects before a suffix, so a whole number gets one back.
  if (body.find_first_of(".eE") == std::string_view::npos) {
    out += ".0";
  }
  out += spelling.suffix;
}

}  // namespace

void WriteType(const ScopeView& view, TargetText& out, mir::TypeId type) {
  Write(out, CppType(view.Unit(), type));
}

void WriteCommaSeparated(
    const ScopeView& view, TargetText& out,
    std::span<const mir::ExprId> operands) {
  bool first = true;
  for (const mir::ExprId operand : operands) {
    if (!first) out += ", ";
    RenderExpr(view, view.Expr(operand), out);
    first = false;
  }
}

void RenderExpr(const ScopeView& view, const mir::Expr& expr, TargetText& out) {
  std::visit(
      Overloaded{
          [&](const mir::StringLiteral& s) {
            WriteCStringLiteral(s.value, out);
          },
          [&](const mir::NullLiteral&) {
            // A handle naming no object and a chandle carrying no pointer are
            // values of their own types (LRM 8.4, 6.14), so each is spelled as
            // one and receives an operation the way any other value of that
            // type does. Every other type a null literal takes names an
            // address, which is spelled as itself.
            const mir::Type& type = view.Unit().types.Get(expr.type);
            if (type.Is<mir::ManagedRefType>() || type.Is<mir::ChandleType>()) {
              Write(view, out, expr.type, "{}");
              return;
            }
            out += "nullptr";
          },
          [&](const mir::MachineBoolLiteral& b) {
            out += b.value ? "true" : "false";
          },
          [&](const mir::MachineFloatLiteral& f) {
            const auto& machine =
                view.Unit().types.Get(expr.type).Get<mir::MachineFloatType>();
            RenderMachineFloatLiteral(f, machine.width, out);
          },
          [&](const mir::MachineIntLiteral& h) {
            // A machine integer is spelled as its own type reads it. An
            // unsigned one is a bit pattern, so it is written in hex and with
            // an unsigned suffix; a signed spelling would go negative and
            // narrow where the value lands in unsigned storage.
            const auto& machine =
                view.Unit().types.Get(expr.type).Get<mir::MachineIntType>();
            if (machine.signedness == mir::Signedness::kUnsigned) {
              out += "0x";
              WriteNumber(out, static_cast<std::uint64_t>(h.value), 16);
              out += "ULL";
              return;
            }
            WriteNumber(out, static_cast<std::int64_t>(h.value), 10);
            out += "LL";
          },
          [&](const mir::ReferenceExpr& r) {
            RenderReferenceExpr(view, r, out);
          },
          [&](const mir::UnaryExpr& u) { RenderUnaryExpr(view, u, out); },
          [&](const mir::BinaryExpr& b) { RenderBinaryExpr(view, b, out); },
          [&](const mir::CastExpr& c) { RenderCastExpr(view, expr, c, out); },
          [&](const mir::ConditionalExpr& c) {
            RenderConditionalExpr(view, c, out);
          },
          [&](const mir::BlockExpr& b) { RenderBlockExpr(view, b, out); },
          [&](const mir::AssignExpr& a) { RenderAssignExpr(view, a, out); },
          [&](const mir::IncDecExpr& inc) { RenderIncDecExpr(view, inc, out); },
          [&](const mir::CallExpr& call) {
            RenderCallExpr(view, call, expr.type, out);
          },
          [&](const mir::DerefExpr& d) { RenderDerefExpr(view, d, out); },
          [&](const mir::MachineArrayDataExpr& d) {
            Write(view, out, "(", d.array, ").data()");
          },
          [&](const mir::AddressOfExpr& a) {
            Write(view, out, "(&", a.operand, ")");
          },
          [&](const mir::MoveExpr& m) {
            Write(view, out, "std::move(", m.operand, ")");
          },
          [&](const mir::FieldAccessExpr& m) {
            RenderFieldAccessExpr(view, m, out);
          },
          [&](const mir::ClosureExpr& cl) { RenderClosureExpr(view, cl, out); },
          [&](const mir::CompositeExpr& c) {
            RenderPartsAsBraceInit(view, expr.type, c.parts, out);
          },
          [&](const mir::AwaitExpr& a) {
            // What is awaited says which of the two this is, and the spelling
            // follows: an execution is awaitable in this target already, while
            // a call that arranged a wait answers whether it parked, and the
            // target needs that answer as something it can await.
            const mir::Expr& awaited = view.Expr(a.awaitable);
            if (view.Unit().types.Get(awaited.type).Is<mir::CoroutineType>()) {
              Write(view, out, "co_await ", a.awaitable);
              return;
            }
            Write(
                view, out, "co_await ", SuspensionCppType(), "{", a.awaitable,
                "}");
          },
          [&](const mir::VectorGetExpr& g) {
            Write(view, out, "(", g.vector, ")[", g.index, "]");
          },
      },
      expr.data);
}

}  // namespace lyra::backend::cpp

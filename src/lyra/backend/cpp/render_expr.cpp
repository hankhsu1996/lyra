#include "lyra/backend/cpp/render_expr.hpp"

#include <array>
#include <charconv>
#include <cmath>
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
#include "lyra/backend/cpp/render_decl.hpp"
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
  // Every local is named the same way, the receiver `self` included.
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

auto BinaryPrecedence(mir::BinaryOp op) -> Precedence {
  switch (op) {
    case mir::BinaryOp::kMul:
    case mir::BinaryOp::kDiv:
    case mir::BinaryOp::kMod:
      return Precedence::kMultiplicative;
    case mir::BinaryOp::kAdd:
    case mir::BinaryOp::kSub:
      return Precedence::kAdditive;
    case mir::BinaryOp::kLessThan:
    case mir::BinaryOp::kLessEqual:
    case mir::BinaryOp::kGreaterThan:
    case mir::BinaryOp::kGreaterEqual:
      return Precedence::kRelational;
    case mir::BinaryOp::kEquality:
    case mir::BinaryOp::kInequality:
      return Precedence::kEquality;
    case mir::BinaryOp::kBitwiseAnd:
      return Precedence::kBitwiseAnd;
    case mir::BinaryOp::kBitwiseXor:
      return Precedence::kBitwiseXor;
    case mir::BinaryOp::kBitwiseOr:
      return Precedence::kBitwiseOr;
    case mir::BinaryOp::kLogicalAnd:
      return Precedence::kLogicalAnd;
    case mir::BinaryOp::kLogicalOr:
      return Precedence::kLogicalOr;
  }
  throw InternalError("BinaryPrecedence: unknown MIR BinaryOp");
}

// Parentheses around one form: the opening one is written when this is
// constructed, before the form, and the closing one when it is destroyed,
// after. Both are written only when the position needs more than the form's own
// precedence.
class Enclosure {
 public:
  Enclosure(TargetText& out, Precedence held, Precedence needed)
      : out_(out), enclosed_(NeedsParentheses(held, needed)) {
    if (enclosed_) out_ += "(";
  }
  ~Enclosure() {
    if (enclosed_) out_ += ")";
  }
  Enclosure(const Enclosure&) = delete;
  Enclosure(Enclosure&&) = delete;
  auto operator=(const Enclosure&) -> Enclosure& = delete;
  auto operator=(Enclosure&&) -> Enclosure& = delete;

 private:
  TargetText& out_;
  bool enclosed_;
};

// The operand of a prefix operator has to be postfix, not just prefix: `-(-1)`
// written as `--1` would be a decrement.
void RenderUnaryExpr(
    const ScopeView& view, const mir::UnaryExpr& u, Precedence at_least,
    TargetText& out) {
  const Enclosure enclosure(out, Precedence::kPrefix, at_least);
  Write(
      view, out, UnaryOpToken(u.op),
      Operand{.expr = u.operand, .at_least = Precedence::kPostfix});
}

void RenderBinaryExpr(
    const ScopeView& view, const mir::BinaryExpr& b, Precedence at_least,
    TargetText& out) {
  const Precedence level = BinaryPrecedence(b.op);
  const Enclosure enclosure(out, level, at_least);
  Write(
      view, out, Operand{.expr = b.lhs, .at_least = level}, " ",
      BinaryOpToken(b.op), " ",
      Operand{.expr = b.rhs, .at_least = TighterThan(level)});
}

// `c ? a : b`. The else arm can be another conditional without parentheses,
// `c1 ? a : c2 ? b : d`, so a long chain stays flat; only the condition needs a
// tighter form.
void RenderConditionalExpr(
    const ScopeView& view, const mir::ConditionalExpr& c, Precedence at_least,
    TargetText& out) {
  const Enclosure enclosure(out, Precedence::kAssignment, at_least);
  Write(
      view, out,
      Operand{.expr = c.condition, .at_least = Precedence::kLogicalOr}, " ? ",
      c.then_value, " : ",
      Operand{.expr = c.else_value, .at_least = Precedence::kAssignment});
}

// A conversion, `(T)x`. C++'s cast notation picks the conversion from the pair
// of types, the same way the MIR node does. It is a prefix form: in `(T)x->m`
// the `->` applies to `x`, so a position like that gets `((T)x)->m`.
//
// An object reference is the exception. Every object reference is one C++
// type, so `(T)r` would only copy it; the conversion to the same object seen as
// another class is `ViewAs<From, To>(r)`.
void RenderCastExpr(
    const ScopeView& view, const mir::Expr& expr, const mir::CastExpr& cast,
    Precedence at_least, TargetText& out) {
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
  const Enclosure enclosure(out, Precedence::kPrefix, at_least);
  Write(
      view, out, "(", expr.type, ")",
      Operand{.expr = cast.operand, .at_least = Precedence::kPostfix});
}

void RenderFieldAccessExpr(
    const ScopeView& view, const mir::FieldAccessExpr& m, TargetText& out) {
  const auto write_receiver = [&]() {
    const mir::Expr& receiver = view.Expr(m.receiver);
    WriteStorageOf(out, view.Unit(), receiver.type, [&](Precedence at_least) {
      Write(view, out, Operand{.expr = m.receiver, .at_least = at_least});
    });
    out += ".";
  };
  std::visit(
      Overloaded{
          [&](const mir::ClassFieldTarget& t) {
            // Qualified by the declaring class, `obj.Base::x`: a derived class
            // may declare its own `x` beside the base's, and which one the
            // access means was settled by the source, not by the receiver's
            // type (LRM 8.14).
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
            // Inside a lambda a capture is a plain name, so the access is that
            // name and the receiver is not written.
            Write(out, CppClosureCaptureName(t.slot));
          },
          [&](const mir::CrossUnitClassFieldTarget& t) {
            // A field of another unit's class, declared in that unit's
            // header. Its name is built from the slot and the field name that
            // unit published, the same way that unit built it.
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

// A reference is a name, or a scope and a name, `C::x`; the kinds differ only
// in which table the name comes from. A function is its address, `&C::f`,
// which is the only one here that is a prefix form and may need parentheses.
void RenderReferenceExpr(
    const ScopeView& view, const mir::ReferenceExpr& reference,
    Precedence at_least, TargetText& out) {
  std::visit(
      Overloaded{
          [&](const mir::LocalRef& l) { Write(out, LookupLocalName(view, l)); },
          [&](const mir::FunctionRef& fr) {
            const Enclosure enclosure(out, Precedence::kPrefix, at_least);
            Write(
                out, "&",
                CppClassName(view.Unit().GetClass(fr.owner), fr.owner),
                "::", CppAbiAdapterName(fr.adapter));
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
            // Qualified by the unit's namespace even inside that unit, because
            // inside a class a member of the same name would be found first.
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

// `target = value` or `target op= value`. The target always renders as a C++
// lvalue, and `op=` evaluates it once, as LRM 11.4.1 requires. An assignment
// has the loosest precedence, so it is parenthesized wherever its value is used
// as an operand.
void RenderAssignExpr(
    const ScopeView& view, const mir::AssignExpr& a, Precedence at_least,
    TargetText& out) {
  const Enclosure enclosure(out, Precedence::kAssignment, at_least);
  const Operand target{.expr = a.target, .at_least = Precedence::kPrefix};
  if (a.compound_op.has_value()) {
    Write(view, out, target, " ", BinaryOpToken(*a.compound_op), "= ", a.value);
    return;
  }
  Write(view, out, target, " = ", a.value);
}

void RenderIncDecExpr(
    const ScopeView& view, const mir::IncDecExpr& inc, Precedence at_least,
    TargetText& out) {
  const Operand target{.expr = inc.target, .at_least = Precedence::kPostfix};
  switch (inc.op) {
    case mir::IncDecOp::kPreInc: {
      const Enclosure enclosure(out, Precedence::kPrefix, at_least);
      Write(view, out, "++", target);
      return;
    }
    case mir::IncDecOp::kPostInc:
      Write(view, out, target, "++");
      return;
    case mir::IncDecOp::kPreDec: {
      const Enclosure enclosure(out, Precedence::kPrefix, at_least);
      Write(view, out, "--", target);
      return;
    }
    case mir::IncDecOp::kPostDec:
      Write(view, out, target, "--");
      return;
  }
  throw InternalError("RenderIncDecExpr: unknown IncDecOp");
}

// The value a closure construction gives one field. The values are listed in
// evaluation order, not field order, so each is found by the field it names.
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

// C++ has no block expression, so one is a lambda called where it is written:
// `[&] { steps; return value; }()`. Capturing by reference is safe because it
// runs immediately. MIR allows no `return` among the steps, so the only
// `return` inside is the one that yields the value.
void RenderBlockExpr(
    const ScopeView& view, const mir::BlockExpr& block, TargetText& out) {
  const ScopeView body_view =
      view.WithBlock(view.Block().child_scopes.Get(block.scope));
  out += "[&] ";
  WriteBody(out, [&] {
    RenderBlockStatements(body_view, out);
    out.OpenLine();
    Write(body_view, out, "return ", block.value, ";\n");
  });
  out += "()";
}

// A closure is a lambda capturing its fields by value, in field order:
// `[sv_capture_0 = init](params) -> R { body }`. The body names a capture by
// the same name, so the two agree.
//
// A coroutine closure is `[](T sv_capture_0) -> R { body }(init)` instead: its
// fields are parameters, copied into the coroutine frame, because a capturing
// coroutine lambda would dangle once the process it starts outlives it.
//
// Nothing is captured as `this`, `=` or `&`; a field that aliases storage has a
// `Ref<T>` type.
void RenderClosureExpr(
    const ScopeView& view, const mir::ClosureExpr& construct, TargetText& out) {
  const mir::ClosureDecl& decl = view.Unit().GetClosure(construct.closure);
  const mir::CallableCode& code = decl.invoke;
  const ScopeView body_view = view.WithClosure(code);

  const auto write_body = [&]() {
    Write(view, out, " -> ", code.result_type, " ");
    WriteBody(out, [&] { RenderBlockStatements(body_view, out); });
  };

  const auto init_of = [&](mir::FieldId field) {
    return FieldInitValue(construct.field_inits, field);
  };

  if (view.Unit().types.Get(code.result_type).Is<mir::CoroutineType>()) {
    if (!code.params.empty()) {
      throw InternalError(
          "RenderClosureExpr: coroutine closure has per-invocation parameters");
    }
    out += "[](";
    WriteSeparated(out, decl.field_order, ", ", [&](mir::FieldId field) {
      Write(
          view, out, decl.fields.Get(field).type, " ",
          CppClosureCaptureName(field));
    });
    out += ")";
    write_body();
    out += "(";
    WriteSeparated(out, decl.field_order, ", ", [&](mir::FieldId field) {
      Write(view, out, init_of(field));
    });
    out += ")";
    return;
  }

  out += "[";
  WriteSeparated(out, decl.field_order, ", ", [&](mir::FieldId field) {
    Write(view, out, CppClosureCaptureName(field), " = ", init_of(field));
  });
  out += "](";
  WriteParameters(view.Unit(), code, code.params, out);
  out += ")";
  write_body();
}

// `T{parts}`. The type is always written: a bare `{parts}` as an argument or
// inside another literal would be read as whatever type that position expects.
void RenderPartsAsBraceInit(
    const ScopeView& view, mir::TypeId type, std::span<const mir::ExprId> parts,
    TargetText& out) {
  Write(view, out, type, "{");
  WriteCommaSeparated(view, out, parts);
  out += "}";
}

void RenderDerefExpr(
    const ScopeView& view, const mir::DerefExpr& d, TargetText& out) {
  const mir::Expr& pointer = view.Expr(d.pointer);
  WriteStorageOf(out, view.Unit(), pointer.type, [&](Precedence at_least) {
    Write(view, out, Operand{.expr = d.pointer, .at_least = at_least});
  });
}

// How a float literal is written so it reads back exactly: the fewest digits
// that round-trip the width (9 for 32-bit, 17 for 64-bit), and an `f` suffix
// to keep a 32-bit one single precision.
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

// A value with its sign bit set is written with a leading minus, which makes it
// a prefix form rather than a literal.
void RenderMachineFloatLiteral(
    const mir::MachineFloatLiteral& f, mir::MachineFloatWidth width,
    Precedence at_least, TargetText& out) {
  const Enclosure enclosure(
      out, std::signbit(f.value) ? Precedence::kPrefix : Precedence::kPostfix,
      at_least);
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
  WriteSeparated(out, operands, ", ", [&](mir::ExprId operand) {
    Write(view, out, operand);
  });
}

void RenderExpr(
    const ScopeView& view, const mir::Expr& expr, Precedence at_least,
    TargetText& out) {
  std::visit(
      Overloaded{
          [&](const mir::StringLiteral& s) {
            WriteCStringLiteral(s.value, out);
          },
          [&](const mir::NullLiteral&) {
            // A null object handle or chandle is a value of its own type (LRM
            // 8.4, 6.14), `T{}`; any other null is a pointer, `nullptr`.
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
            RenderMachineFloatLiteral(f, machine.width, at_least, out);
          },
          [&](const mir::MachineIntLiteral& h) {
            // An unsigned integer is written in hex, `0xffULL`, since a signed
            // spelling of a large one would be negative. A signed one is
            // decimal, `-5LL`, and a negative one is a prefix form.
            const auto& machine =
                view.Unit().types.Get(expr.type).Get<mir::MachineIntType>();
            if (machine.signedness == mir::Signedness::kUnsigned) {
              out += "0x";
              WriteNumber(out, static_cast<std::uint64_t>(h.value), 16);
              out += "ULL";
              return;
            }
            const auto value = static_cast<std::int64_t>(h.value);
            const Enclosure enclosure(
                out, value < 0 ? Precedence::kPrefix : Precedence::kPostfix,
                at_least);
            WriteNumber(out, value, 10);
            out += "LL";
          },
          [&](const mir::ReferenceExpr& r) {
            RenderReferenceExpr(view, r, at_least, out);
          },
          [&](const mir::UnaryExpr& u) {
            RenderUnaryExpr(view, u, at_least, out);
          },
          [&](const mir::BinaryExpr& b) {
            RenderBinaryExpr(view, b, at_least, out);
          },
          [&](const mir::CastExpr& c) {
            RenderCastExpr(view, expr, c, at_least, out);
          },
          [&](const mir::ConditionalExpr& c) {
            RenderConditionalExpr(view, c, at_least, out);
          },
          [&](const mir::BlockExpr& b) { RenderBlockExpr(view, b, out); },
          [&](const mir::AssignExpr& a) {
            RenderAssignExpr(view, a, at_least, out);
          },
          [&](const mir::IncDecExpr& inc) {
            RenderIncDecExpr(view, inc, at_least, out);
          },
          [&](const mir::CallExpr& call) {
            RenderCallExpr(view, call, expr.type, out);
          },
          [&](const mir::DerefExpr& d) { RenderDerefExpr(view, d, out); },
          [&](const mir::MachineArrayDataExpr& d) {
            Write(
                view, out,
                Operand{.expr = d.array, .at_least = Precedence::kPostfix},
                ".data()");
          },
          [&](const mir::AddressOfExpr& a) {
            const Enclosure enclosure(out, Precedence::kPrefix, at_least);
            Write(
                view, out, "&",
                Operand{.expr = a.operand, .at_least = Precedence::kPostfix});
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
            // Awaiting a coroutine is `co_await c`. A call that may park
            // returns whether it parked, so it is wrapped to be awaitable:
            // `co_await Suspension{call}`.
            const Enclosure enclosure(out, Precedence::kPrefix, at_least);
            const mir::Expr& awaited = view.Expr(a.awaitable);
            if (view.Unit().types.Get(awaited.type).Is<mir::CoroutineType>()) {
              Write(
                  view, out, "co_await ",
                  Operand{
                      .expr = a.awaitable, .at_least = Precedence::kPostfix});
              return;
            }
            Write(
                view, out, "co_await ", SuspensionCppType(), "{", a.awaitable,
                "}");
          },
          [&](const mir::VectorGetExpr& g) {
            Write(
                view, out,
                Operand{.expr = g.vector, .at_least = Precedence::kPostfix},
                "[", g.index, "]");
          },
      },
      expr.data);
}

}  // namespace lyra::backend::cpp

#include "lyra/backend/cpp/render_constant.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <ranges>
#include <span>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/string_literal.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

namespace {

// How a text names a constant of the unit. Where only this file defines what
// the text writes, by the name the code file defines it under, which the
// report records. Where every artifact stating the text defines it alike and
// keeps one copy, in place, as a static of its own: C++ asks that every name in
// such a definition denote one entity in every copy ([basic.def.odr]/16), which
// a static of an inline definition is ([dcl.inline]), while a constant of one
// file's own would be another entity in each. Inside such a spelling, by the
// static it defines before the one naming it, named by where that static
// stands among `statics`, so the spelling is the same in every copy whatever
// position the unit holds each constant at.
struct NamedByFile {};
struct SpelledInPlace {};
struct NamedByEnclosingStatics {
  std::span<const mir::ConstantId> statics;
};
using ConstantNaming =
    std::variant<NamedByFile, SpelledInPlace, NamedByEnclosingStatics>;

auto NamingFor(support::Linkage linkage) -> ConstantNaming {
  switch (linkage) {
    case support::Linkage::kInternal:
    case support::Linkage::kExternal:
      return NamedByFile{};
    case support::Linkage::kShared:
      return SpelledInPlace{};
  }
  throw InternalError("backend::cpp: unknown linkage");
}

void WriteReference(
    const mir::CompilationUnit& unit, const ConstantNaming& naming,
    UnitRenderReport& report, mir::ConstantId constant, TargetText& out);

void WriteValue(
    const mir::CompilationUnit& unit, const ConstantNaming& naming,
    UnitRenderReport& report, mir::TypeId type, const mir::ConstantValue& value,
    TargetText& out);

// The address `value` holds, at its own type: `&` before what it names where
// that is data, and the name alone where naming it already answers an
// address.
void WritePointer(
    const mir::CompilationUnit& unit, const ConstantNaming& naming,
    UnitRenderReport& report, const mir::PointerValue& value, TargetText& out) {
  if (!mir::PointsAtData(value.to)) {
    WriteNamedEntity(unit, value.to, out);
    return;
  }
  out += "&";
  if (const auto* constant = std::get_if<mir::ConstantRef>(&value.to)) {
    WriteReference(unit, naming, report, constant->constant, out);
    return;
  }
  WriteNamedEntity(unit, value.to, out);
}

void AppendReached(
    const mir::CompilationUnit& unit, mir::ConstantId constant,
    std::vector<mir::ConstantId>& reached);

// Every constant `value` names, and every one those name, each before what
// names it, in the order the value names them.
void AppendNamedBy(
    const mir::CompilationUnit& unit, const mir::ConstantValue& value,
    std::vector<mir::ConstantId>& reached) {
  std::visit(
      Overloaded{
          [&](const mir::PointerValue& v) {
            if (const auto* c = std::get_if<mir::ConstantRef>(&v.to)) {
              AppendReached(unit, c->constant, reached);
            }
          },
          [&](const mir::SpanValue& v) {
            AppendReached(unit, v.array, reached);
          },
          [&](const mir::RecordValue& v) {
            for (const mir::ConstantValue& part : v.parts) {
              AppendNamedBy(unit, part, reached);
            }
          },
          [&](const mir::ArrayValue& v) {
            for (const mir::ConstantValue& part : v.elements) {
              AppendNamedBy(unit, part, reached);
            }
          },
          [](const mir::IntegralValue&) {}, [](const mir::MachineIntValue&) {},
          [](const mir::NullValue&) {}, [](const mir::CStringValue&) {}},
      value.value);
}

// `constant`, after every constant it reaches: the statics its in-place
// spelling defines, in an order that follows the values alone, so every copy
// of the spelling defines them alike.
void AppendReached(
    const mir::CompilationUnit& unit, mir::ConstantId constant,
    std::vector<mir::ConstantId>& reached) {
  if (std::ranges::find(reached, constant) != reached.end()) return;
  AppendNamedBy(unit, unit.constants.Get(constant).value, reached);
  reached.push_back(constant);
}

// The name of the static an in-place spelling defines for `constant`.
auto StaticName(
    std::span<const mir::ConstantId> statics, mir::ConstantId constant)
    -> MintedName {
  const auto found = std::ranges::find(statics, constant);
  if (found == statics.end()) {
    throw InternalError(
        "backend::cpp: an in-place constant names one it does not define -- "
        "please report this as a bug");
  }
  return CppHeldConstantName(
      static_cast<std::uint32_t>(found - statics.begin()));
}

// In place, a constant is a static of its own, which a call of a lambda
// defines with every constant it reaches before it:
// `[]() -> const T& { static const T sv_held_0 = {...}; return sv_held_0; }()`.
// A static whose initializer is a constant expression is initialized before
// anything runs ([basic.start.static]/2), so reaching one then runs nothing.
void WriteReference(
    const mir::CompilationUnit& unit, const ConstantNaming& naming,
    UnitRenderReport& report, mir::ConstantId constant, TargetText& out) {
  std::visit(
      Overloaded{
          [&](const NamedByFile&) {
            report.constants_named.insert(constant);
            Write(out, CppConstantName(constant));
          },
          [&](const NamedByEnclosingStatics& enclosing) {
            Write(out, StaticName(enclosing.statics, constant));
          },
          [&](const SpelledInPlace&) {
            const mir::ConstantDecl& named = unit.constants.Get(constant);
            std::vector<mir::ConstantId> statics;
            AppendReached(unit, constant, statics);
            const ConstantNaming inside = NamedByEnclosingStatics{statics};
            Write(out, "[]() -> const ", CppType(unit, named.type), "& { ");
            for (const mir::ConstantId held : statics) {
              const mir::ConstantDecl& defined = unit.constants.Get(held);
              Write(
                  out, "static const ", CppType(unit, defined.type), " ",
                  StaticName(statics, held), " = ");
              WriteValue(
                  unit, inside, report, defined.type, defined.value, out);
              out += "; ";
            }
            Write(out, "return ", StaticName(statics, constant), "; }()");
          }},
      naming);
}

void WriteValue(
    const mir::CompilationUnit& unit, const ConstantNaming& naming,
    UnitRenderReport& report, mir::TypeId type, const mir::ConstantValue& value,
    TargetText& out) {
  const mir::Type& held = unit.types.Get(type);
  std::visit(
      Overloaded{
          [&](const mir::IntegralValue& v) {
            Write(
                out, CppIntegralLiteral{
                         .type = CppType(unit, type),
                         .value_words = v.planes.value_words,
                         .state_words = v.planes.state_words});
          },
          // A boolean as the word for its truth, and a number as itself.
          [&](const mir::MachineIntValue& v) {
            if (held.Is<mir::MachineBoolType>()) {
              out += v.value != 0 ? "true" : "false";
              return;
            }
            Write(
                out,
                CppMachineIntLiteral{
                    .value = v.value,
                    .signedness = held.Get<mir::MachineIntType>().signedness});
          },
          [&](const mir::NullValue&) {
            WriteNull(out, unit, NullAsNullAddress{});
          },
          [&](const mir::CStringValue& v) { WriteCStringLiteral(v.text, out); },
          // A pointer held at another type than its own is read as that
          // type, as a cast reads one.
          [&](const mir::PointerValue& v) {
            if (mir::OwnPointerType(unit.types, v) == type) {
              WritePointer(unit, naming, report, v, out);
              return;
            }
            Write(out, "(", CppType(unit, type), ")(");
            WritePointer(unit, naming, report, v, out);
            out += ")";
          },
          [&](const mir::RecordValue& v) {
            const std::span<const mir::TypeId> members =
                *unit.LibraryRecordMembers(held.Get<mir::LibraryType>().decl);
            Write(out, CppType(unit, type), "{");
            WriteSeparated(
                out, std::views::zip(members, v.parts), ", ",
                [&](const auto& member_and_part) {
                  const auto& [member, part] = member_and_part;
                  WriteValue(unit, naming, report, member, part, out);
                });
            out += "}";
          },
          [&](const mir::ArrayValue& v) {
            const mir::TypeId element =
                held.Get<mir::MachineArrayType>().element;
            Write(out, CppType(unit, type), "{");
            WriteSeparated(
                out, v.elements, ", ", [&](const mir::ConstantValue& part) {
                  WriteValue(unit, naming, report, element, part, out);
                });
            out += "}";
          },
          [&](const mir::SpanValue& v) {
            Write(out, CppConstructorName{.of = CppType(unit, type)}, "(&");
            WriteReference(unit, naming, report, v.array, out);
            out += ")";
          }},
      value.value);
}

}  // namespace

void WriteConstantReference(
    const mir::CompilationUnit& unit, support::Linkage linkage,
    UnitRenderReport& report, mir::ConstantId constant, TargetText& out) {
  WriteReference(unit, NamingFor(linkage), report, constant, out);
}

void WriteConstantValue(
    const mir::CompilationUnit& unit, support::Linkage linkage,
    UnitRenderReport& report, mir::TypeId type, const mir::ConstantValue& value,
    TargetText& out) {
  WriteValue(unit, NamingFor(linkage), report, type, value, out);
}

}  // namespace lyra::backend::cpp

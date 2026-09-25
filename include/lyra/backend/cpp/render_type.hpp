#pragma once

#include <string_view>
#include <variant>

#include "lyra/backend/cpp/precedence.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

// Runtime library types for things MIR states as structure rather than as a
// type, so no MIR type maps to them. They are spelled here because this file is
// where every runtime library type is spelled.

// The object that runs a `finally` cleanup from its destructor. C++ has no
// `finally`, so the cleanup is an object declared ahead of the body, and it
// runs however the body is left, a `return` or `break` included.
[[nodiscard]] auto BodyCleanupExtentCppType() -> std::string_view;

// What a call that may park the process is wrapped in to be awaited:
// `co_await Suspension{call(...)}`. The call returns whether it parked, and C++
// can only suspend by awaiting something.
[[nodiscard]] auto SuspensionCppType() -> std::string_view;

// The base of every class that extends nothing (LRM 8.13). Objects are held by
// shared ownership, and an object that hands out a handle to itself (LRM 8.11)
// has to know its owner, which this base records.
[[nodiscard]] auto ManagedObjectRootCppType() -> std::string_view;

// A MIR type, written into the output as its C++ type. An enum is written as
// its base type, a packed array; there is no C++ enum. It holds a view of the
// unit, which outlives the writing.
class CppType {
 public:
  CppType(const mir::CompilationUnit& unit, mir::TypeId type)
      : unit_(&unit), type_(type) {
  }
  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }
  [[nodiscard]] auto Type() const -> mir::TypeId {
    return type_;
  }

 private:
  const mir::CompilationUnit* unit_;
  mir::TypeId type_;
};

void WriteOne(TargetText& out, const CppType& spelling);

// What a construction call names before its argument list. Usually the type
// itself, `T(args)`; for an owning pointer the function that allocates and
// constructs, `std::make_unique<T>(args)`; for a sequence the library function
// that takes the elements, since the container type takes no element list.
struct CppConstructorName {
  CppType of;
};

void WriteOne(TargetText& out, const CppConstructorName& constructor);

// A class a reference names, as C++: its own name for a class of this unit,
// `::Unit::Name` for another unit's, and the runtime's name for a runtime
// class.
class CppClassRef {
 public:
  CppClassRef(const mir::CompilationUnit& unit, const mir::ClassRef& ref)
      : unit_(&unit), ref_(&ref) {
  }
  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }
  [[nodiscard]] auto Ref() const -> const mir::ClassRef& {
    return *ref_;
  }

 private:
  const mir::CompilationUnit* unit_;
  const mir::ClassRef* ref_;
};

void WriteOne(TargetText& out, const CppClassRef& ref);

// How to reach the storage behind a place of this type as an lvalue. A pointer,
// and a `ref` to a cell (LRM 23.3.3.2), are dereferenced: `(*p)`. A reference
// to an object is opened as the class the code assumes, `r.Deref<T>()`, which
// is also where a null reference is caught (LRM 8.4). Any other type stands for
// no storage and is refused.
//
// This is the only place that spells the runtime's access protocol; the rest
// of the render writes punctuation around its answer.
struct OpenedByDereference {};

struct OpenedThroughView {
  mir::TypeId pointee;
};

using PlaceAccess = std::variant<OpenedByDereference, OpenedThroughView>;

[[nodiscard]] auto PlaceAccessAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id) -> PlaceAccess;

// The storage behind a place, written as a postfix form so the caller can put a
// member or a call right after it. `write_place` writes the place itself and is
// told the precedence its position needs: after the `*` of a dereference, or
// before a member call.
template <typename WritePlace>
void WriteStorageOf(
    TargetText& out, const mir::CompilationUnit& unit, mir::TypeId place_type,
    WritePlace write_place) {
  std::visit(
      Overloaded{
          [&](OpenedByDereference) {
            out += "(*";
            write_place(Precedence::kPrefix);
            out += ")";
          },
          [&](const OpenedThroughView& view) {
            write_place(Precedence::kPostfix);
            Write(out, ".Deref<", CppType(unit, view.pointee), ">()");
          }},
      PlaceAccessAsCpp(unit, place_type));
}

// How a value of one type is written as another. C++'s cast notation picks the
// conversion from the pair of types, the same way the MIR node does, so naming
// the destination is enough: `(T)x`. An object reference is the pair it cannot
// pick from, because every object reference is one C++ type whatever class it
// is seen as and `(T)r` would only copy it; the same object seen as another
// class is `ViewAs<From, To>(r)`, over the classes the two reference types
// name.
//
// This is the only place that spells a conversion; a cast writes punctuation
// around its answer.
struct ConvertedByCastNotation {
  mir::TypeId to;
};

struct ConvertedThroughView {
  mir::TypeId from;
  mir::TypeId to;
};

using Conversion = std::variant<ConvertedByCastNotation, ConvertedThroughView>;

[[nodiscard]] auto ConversionAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId from, mir::TypeId to)
    -> Conversion;

// The conversion of a value, in a position needing `at_least`. `write_value`
// writes the value being converted and is told the precedence its position in
// the conversion needs.
template <typename WriteValue>
void WriteConversionOf(
    TargetText& out, const mir::CompilationUnit& unit, mir::TypeId from,
    mir::TypeId to, Precedence at_least, WriteValue write_value) {
  std::visit(
      Overloaded{
          // A prefix form: in `(T)x->m` the `->` applies to `x`, so a position
          // like that gets `((T)x)->m`.
          [&](const ConvertedByCastNotation& c) {
            const Enclosure enclosure(out, Precedence::kPrefix, at_least);
            Write(out, "(", CppType(unit, c.to), ")");
            write_value(Precedence::kPostfix);
          },
          [&](const ConvertedThroughView& v) {
            Write(
                out, "lyra::runtime::ViewAs<", CppType(unit, v.from), ", ",
                CppType(unit, v.to), ">(");
            write_value(Precedence::kAssignment);
            out += ")";
          }},
      ConversionAsCpp(unit, from, to));
}

}  // namespace lyra::backend::cpp

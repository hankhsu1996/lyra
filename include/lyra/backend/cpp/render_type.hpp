#pragma once

#include <span>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/backend/cpp/precedence.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
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

// The library's product of a product type's components, `Tuple<Ts...>`: what a
// tuple is spelled as, and what the type a unit defines for a struct is built
// on.
struct CppTupleComponents {
  const mir::CompilationUnit* unit;
  std::span<const mir::TypeId> of;
};

void WriteOne(TargetText& out, const CppTupleComponents& components);

// A class a reference names, as C++: its own name for a class of this unit,
// `::Unit::Name` for another unit's, and the runtime's name for a runtime
// class.
class CppClassRef {
 public:
  CppClassRef(const mir::CompilationUnit& unit, mir::ClassRef ref)
      : unit_(&unit), ref_(std::move(ref)) {
  }
  CppClassRef(
      const mir::CompilationUnit& unit, const mir::DeclaredClassRef& ref)
      : CppClassRef(unit, mir::AsClassRef(ref)) {
  }
  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }
  [[nodiscard]] auto Ref() const -> const mir::ClassRef& {
    return ref_;
  }

 private:
  const mir::CompilationUnit* unit_;
  mir::ClassRef ref_;
};

void WriteOne(TargetText& out, const CppClassRef& ref);

// How C++ dereferences a value of this type. A pointer takes the indirection
// operator built in; a `ref` to a cell (LRM 23.3.3.2) and a write in progress
// take it through the `operator*` and `operator->` their library types declare.
// An object reference declares neither, being one type whatever class it
// refers to, so it is opened through its view as the class the code assumes,
// which is also where a null reference is caught (LRM 8.4). Any other type
// reaches nothing to dereference and is refused.
struct DerefByOperator {};

struct DerefThroughView {
  mir::TypeId pointee;
};

using DerefSpelling = std::variant<DerefByOperator, DerefThroughView>;

[[nodiscard]] auto DerefSpellingAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id) -> DerefSpelling;

// C++'s indirection over a value of `pointer_type`, `(*p)` ([expr.unary.op]),
// or `r.Deref<T>()` where the dereference goes through a view. `write_pointer`
// writes the pointer and is told the precedence its position needs.
template <typename WritePointer>
void WriteDeref(
    TargetText& out, const mir::CompilationUnit& unit, mir::TypeId pointer_type,
    WritePointer write_pointer) {
  std::visit(
      Overloaded{
          [&](DerefByOperator) {
            out += "(*";
            write_pointer(Precedence::kPrefix);
            out += ")";
          },
          [&](const DerefThroughView& view) {
            write_pointer(Precedence::kPostfix);
            Write(out, ".Deref<", CppType(unit, view.pointee), ">()");
          }},
      DerefSpellingAsCpp(unit, pointer_type));
}

// A member access through what `pointer_type` designates, up to the member's
// name: `p->`, which C++ defines as `(*p).` ([expr.ref]), or `r.Deref<T>().`
// where the dereference goes through a view. `write_pointer` writes the
// pointer and is told the precedence its position needs.
template <typename WritePointer>
void WriteArrowAccess(
    TargetText& out, const mir::CompilationUnit& unit, mir::TypeId pointer_type,
    WritePointer write_pointer) {
  write_pointer(Precedence::kPostfix);
  std::visit(
      Overloaded{
          [&](DerefByOperator) { out += "->"; },
          [&](const DerefThroughView& view) {
            Write(out, ".Deref<", CppType(unit, view.pointee), ">().");
          }},
      DerefSpellingAsCpp(unit, pointer_type));
}

// How a value of one type is written as another. C++'s cast notation picks the
// conversion from the pair of types, the same way the MIR node does, so naming
// the destination is enough: `(T)x`. An object reference is the pair it cannot
// pick from, because every object reference is one C++ type whatever class it
// is seen as and `(T)r` would only copy it; the same object seen as another
// class is `ViewAs<From, To>(r)`, over the classes the two reference types
// name. A pair this target realizes neither way is refused as unsupported
// rather than written as a cast the host compiler would reject.
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
    -> diag::Result<Conversion>;

// The conversion of a value, in a position needing `at_least`. `write_value`
// writes the value being converted and is told the precedence its position in
// the conversion needs.
template <typename WriteValue>
void WriteConversion(
    TargetText& out, const mir::CompilationUnit& unit,
    const Conversion& conversion, Precedence at_least, WriteValue write_value) {
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
      conversion);
}

// How a value of a type that names nothing is written (LRM 8.4, 6.14). An
// object reference and a chandle are values of their own types, `T{}`; a
// pointer and a code address are the null address. Any other type has no such
// value on this target and is refused as unsupported.
struct NullAsEmptyValue {
  mir::TypeId type;
};

struct NullAsNullAddress {};

using NullSpelling = std::variant<NullAsEmptyValue, NullAsNullAddress>;

[[nodiscard]] auto NullSpellingAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type)
    -> diag::Result<NullSpelling>;

void WriteNull(
    TargetText& out, const mir::CompilationUnit& unit,
    const NullSpelling& spelling);

}  // namespace lyra::backend::cpp

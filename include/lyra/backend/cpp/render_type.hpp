#pragma once

#include <span>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/backend/cpp/precedence.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/component_index.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enum_table_id.hpp"
#include "lyra/mir/integral_constant_id.hpp"
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

// What reads a value out of storage another member of an untagged union may
// have written, `lyra::value::ReadOverlaid(place)`: the value the bits there
// stand for at the place's type (LRM 7.3.2).
[[nodiscard]] auto OverlaidReadCppFunction() -> std::string_view;

// A MIR type, written into the output as its C++ type. An enum is written as
// its base type; there is no C++ enum. It holds a view of the unit, which
// outlives the writing.
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

// What follows a type to say whether it may be written through, ` const` for
// read-only and nothing otherwise. It is written after the type, so it
// qualifies that type whatever the type is, a pointer included.
struct CppConstQualifier {
  mir::Mutability of;
};

void WriteOne(TargetText& out, const CppConstQualifier& qualifier);

// A constant of an integral type, written as the value its words are: the
// value plane's words and then the unknown plane's, least significant first.
class CppIntegralLiteral {
 public:
  CppIntegralLiteral(
      const mir::CompilationUnit& unit, mir::IntegralConstantId constant)
      : unit_(&unit), constant_(constant) {
  }
  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }
  [[nodiscard]] auto Constant() const -> mir::IntegralConstantId {
    return constant_;
  }

 private:
  const mir::CompilationUnit* unit_;
  mir::IntegralConstantId constant_;
};

void WriteOne(TargetText& out, const CppIntegralLiteral& literal);

// An enumeration's member table, written as the constant its members are:
// every member's planes in declared order, then the members' names in the same
// order.
class CppEnumTableLiteral {
 public:
  CppEnumTableLiteral(const mir::CompilationUnit& unit, mir::EnumTableId table)
      : unit_(&unit), table_(table) {
  }
  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }
  [[nodiscard]] auto Table() const -> mir::EnumTableId {
    return table_;
  }

 private:
  const mir::CompilationUnit* unit_;
  mir::EnumTableId table_;
};

void WriteOne(TargetText& out, const CppEnumTableLiteral& literal);

// What a construction call names before its argument list. Usually the type
// itself, `T(args)`; for a sequence the library function that takes the
// elements, since the container type takes no element list.
struct CppConstructorName {
  CppType of;
};

void WriteOne(TargetText& out, const CppConstructorName& constructor);

// What says which member a union is built holding, ahead of that member's
// value: `std::in_place_index<I>`, the tag a union type takes a constructor by,
// as `std::variant` does. The constructor places the member at the start of
// the union's storage with every other byte zero.
struct CppUnionMemberTag {
  base::ComponentIndex member;
};

void WriteOne(TargetText& out, const CppUnionMemberTag& tag);

// What takes ownership of the storage a `new` allocated for a value, applied
// to the pointer `new` yields: `std::unique_ptr<T>(new T(init))`.
struct CppNewOwner {
  CppType of;
};

void WriteOne(TargetText& out, const CppNewOwner& owner);

// The library function a `new` of an object is, which creates it whole, makes
// `of` the owner holding it, and then enters its constructor:
// `lyra::runtime::CreateObject<C>(&C::sv_construct, args)` for a class handle,
// `lyra::runtime::CreateOwned<C>(...)` for a uniquely owning pointer.
struct CppObjectCreation {
  CppType of;
};

void WriteOne(TargetText& out, const CppObjectCreation& creation);

// The library's product of a product type's components, `Tuple<Ts...>`: what a
// tuple is spelled as.
struct CppTupleComponents {
  const mir::CompilationUnit* unit;
  std::span<const mir::TypeId> of;
};

void WriteOne(TargetText& out, const CppTupleComponents& components);

// The library type a struct or a union the unit declares is built on, holding
// its members: the product of a struct's, `Tuple<Ts...>`, or the one storage a
// union's overlay, `Union<Ts...>`, with the tag a tagged one adds,
// `TaggedUnion<Ts...>`.
struct CppRecordBase {
  const mir::CompilationUnit* unit;
  const mir::RecordDecl* of;
};

void WriteOne(TargetText& out, const CppRecordBase& base);

// The part of every closure the runtime library keeps, which a closure's type
// extends: the definition it is called and ended through.
[[nodiscard]] auto CppClosureBase() -> std::string_view;

// What builds that definition, a constant each closure's type states, from the
// closure's type, `DefinitionOf<C>()`: the library reads the signature of the
// closure's body off the type, and the constant's type follows from it.
[[nodiscard]] auto CppClosureDefinitionOf() -> std::string_view;

// The signature a closure of no known type is called at, `R(Ts...)`, as a
// borrow or a box of one names it.
class CppCallSignature {
 public:
  CppCallSignature(const mir::CompilationUnit& unit, mir::TypeId erased)
      : unit_(&unit), erased_(erased) {
  }
  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }
  [[nodiscard]] auto Erased() const -> mir::TypeId {
    return erased_;
  }

 private:
  const mir::CompilationUnit* unit_;
  mir::TypeId erased_;
};

void WriteOne(TargetText& out, const CppCallSignature& signature);

// A name written into a file needs the file declaring it read first, and how
// the name is used says whether a declaration is enough or the definition is
// needed. A class's name says so as it is written, as a struct's does, so a
// file's includes are what its own text asked for.

// A class a reference names, as C++: its own name for a class this unit
// defines, `::Unit::Name` for one another unit defines, and the runtime's name
// for a runtime class. An object is reached through a pointer, so the name
// needs the class declared and no more.
class CppClassRef {
 public:
  CppClassRef(const mir::CompilationUnit& unit, mir::ClassRef ref)
      : unit_(&unit), ref_(std::move(ref)) {
  }
  CppClassRef(const mir::CompilationUnit& unit, mir::ClassId id)
      : CppClassRef(unit, mir::ClassRef{id}) {
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

// A class named as the base of another (LRM 8.13, 8.26). C++ lays the derived
// class out over its base, so the name needs the base defined.
struct CppBaseClass {
  CppClassRef of;
};

void WriteOne(TargetText& out, const CppBaseClass& base);

// How C++ dereferences a value of this type. A pointer takes the indirection
// operator built in; a `ref` to a cell (LRM 23.3.3.2) and a write in progress
// take it through the `operator*` and `operator->` their library types declare.
// An object reference declares neither, being one type whatever class it
// refers to, so it is opened through its view as the class the code assumes,
// which is also where a null reference is caught (LRM 8.4). Any other type
// reaches nothing to dereference and is refused. A reference is already its
// referent wherever C++ names it ([expr.type]), so opening one writes nothing.
struct DerefByOperator {};

struct DerefThroughView {
  mir::TypeId pointee;
};

struct DerefImplicit {};

using DerefSpelling =
    std::variant<DerefByOperator, DerefThroughView, DerefImplicit>;

[[nodiscard]] auto DerefSpellingAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id) -> DerefSpelling;

// C++'s indirection over a value of `pointer_type`, `(*p)` ([expr.unary.op]),
// `r.Deref<T>()` where the dereference goes through a view, or `r` itself for a
// reference. `write_pointer`
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
          },
          [&](DerefImplicit) { write_pointer(Precedence::kPostfix); }},
      DerefSpellingAsCpp(unit, pointer_type));
}

// A member access through what `pointer_type` designates, up to the member's
// name: `p->`, which C++ defines as `(*p).` ([expr.ref]), `r.Deref<T>().`
// where the dereference goes through a view, or `r.` for a reference.
// `write_pointer` writes the pointer and is told the precedence its position
// needs.
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
          },
          [&](DerefImplicit) { out += "."; }},
      DerefSpellingAsCpp(unit, pointer_type));
}

// How C++ hands a member function's body the object it runs against, as the
// body's receiver of this type holds it. C++ hands it as `this`, a pointer
// that is read-only exactly where the function is qualified `const`
// ([class.this]), so a receiver pointing at its object is bound from `this`
// and one referring to it from `*this`, the function read-only where the
// receiver is. A body taking its object over is handed a box, which `this`
// cannot be, so it is a static function taking the box as its first
// parameter. Any other type holds no object and is refused.
struct ReceiverFromThis {
  mir::Mutability object;
  std::string_view bound_from;
};

struct ReceiverAsParameter {};

using ReceiverSpelling = std::variant<ReceiverFromThis, ReceiverAsParameter>;

[[nodiscard]] auto ReceiverSpellingAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId receiver_type)
    -> ReceiverSpelling;

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

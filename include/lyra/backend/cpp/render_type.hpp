#pragma once

#include <string_view>
#include <variant>

#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

// Three things MIR states as structure rather than as a value, and that this
// target has to realize with a library type. No MIR type names any of them, so
// none is reached through the type mapping below -- but each is a library
// type's spelling, and this is where a library type is spelled.

// A body paired with a cleanup that runs on every way out of it: an object
// declared ahead of the body whose destruction runs the cleanup. C++ states an
// extent's exit through a destructor and offers no construct of its own.
[[nodiscard]] auto BodyCleanupExtentCppType() -> std::string_view;

// Giving up control where a call says it parked this execution. C++ gives up
// control by awaiting something, so the answer the call gave has to become
// something awaitable; a target whose suspension is an edge in its own graph
// needs nothing here, because the edge is the same nothing.
[[nodiscard]] auto SuspensionCppType() -> std::string_view;

// What an SV class extending nothing (LRM 8.13) is emitted over, so an object
// can answer with a handle to itself (LRM 8.11): realizing that handle as a
// shared owner means the object records which owner refers to it, and this is
// where that record lives.
[[nodiscard]] auto ManagedObjectRootCppType() -> std::string_view;

// The same object under another static view. Every view of a reference is one
// target type, so the target's own cast notation copies it unchanged where what
// a conversion calls for is a new view over the same object; naming that is the
// only way the pair of classes reaches the emitted text.
[[nodiscard]] auto ObjectViewConversionCppName() -> std::string_view;

// A MIR type, as the C++ type expression that holds it. An enum is a nominal
// type over a base integral, so its value is spelled as that base -- a packed
// array -- with no distinct emitted enum type.
//
// Like a name, a type is spelled in one place and written wherever it goes, so
// what the mapping answers with is what decides the spelling. It holds a view
// of the unit being emitted, which outlives the writing.
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

// The name a call brings a value of this type into existence through, which
// the argument list is then applied to. It is the type's own answer and not the
// call's: a value type is built by naming itself, a wrapper that owns what it
// points at by the entry that allocates and constructs together, and a
// sequence by the library entry that takes its elements, since the type it is
// kept in takes no element list of its own.
struct CppConstructorName {
  CppType of;
};

void WriteOne(TargetText& out, const CppConstructorName& constructor);

// The C++ type expression naming a class a reference reaches. A class of this
// unit is named through the unit's class registry; one of another unit by its
// qualified name.
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

// How the storage behind a place of this type is named as an lvalue. A pointer
// names it with the target's own dereference. A reference to a lent cell opens
// that cell the same way (LRM 23.3.3.2). A reference to a managed object states
// which object it names rather than being it, so the object is reached first --
// which is where the class the reading point assumes is written down, and where
// reaching through a reference that names no object is caught (LRM 8.4). A type
// this backend states no access for is refused rather than answered, so a place
// it was never asked about cannot be given a plausible one.
//
// This is the one entry that names a runtime library's access protocol; an
// entry that emits a value writes punctuation around its answer and never
// spells the protocol itself.
struct OpenedByDereference {};

struct OpenedThroughView {
  mir::TypeId pointee;
};

using PlaceAccess = std::variant<OpenedByDereference, OpenedThroughView>;

[[nodiscard]] auto PlaceAccessAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id) -> PlaceAccess;

// The storage behind a place, with the place itself written where the access
// puts it.
template <typename WritePlace>
void WriteStorageOf(
    TargetText& out, const mir::CompilationUnit& unit, mir::TypeId place_type,
    WritePlace write_place) {
  std::visit(
      Overloaded{
          [&](OpenedByDereference) {
            out += "(*";
            write_place();
            out += ")";
          },
          [&](const OpenedThroughView& view) {
            write_place();
            Write(out, ".Deref<", CppType(unit, view.pointee), ">()");
          }},
      PlaceAccessAsCpp(unit, place_type));
}

}  // namespace lyra::backend::cpp

#pragma once

#include <cstdint>
#include <optional>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/declared_class.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

class CompilationUnit;

// How long an object lives: for the whole run, as an instance of the design
// hierarchy does, owned by the tree built at time zero, or while a handle
// refers to it, as an object the program makes does (LRM 8.29). A borrow of
// the first is what Rust writes `&'static T`, a borrow that is valid at every
// use; a borrow of the second is valid only while something keeps the object.
//
// MIR's borrowed pointer states no lifetime of its own, so this is read off
// what the pointer reaches -- the root a class's lineage reaches, or which of
// the library's classes it is -- rather than off the pointer.
enum class ObjectLifetime : std::uint8_t { kTheRun, kWhileReferenced };

// The lifetime of an object of a class whose lineage passes through `of`.
// `base_of` answers the class a declared class extends, for a class another
// unit declares as for one of this unit: an instance of another module is a
// class of that unit. An interface class extends none, and only a value the
// program makes is ever one (LRM 8.26).
template <typename BaseOf>
[[nodiscard]] auto LifetimeOfLineage(const ClassRef& of, const BaseOf& base_of)
    -> ObjectLifetime {
  const auto through = [&](const DeclaredClassRef& declared) {
    const std::optional<ClassRef> base = base_of(declared);
    return base.has_value() ? LifetimeOfLineage(*base, base_of)
                            : ObjectLifetime::kWhileReferenced;
  };
  return std::visit(
      Overloaded{
          [&](const IntraUnitClassRef& own) { return through(own); },
          [&](const CrossUnitClassRef& other) { return through(other); },
          [](const ObjectTreeRootRef&) { return ObjectLifetime::kTheRun; },
          [](const ManagedObjectRootRef&) {
            return ObjectLifetime::kWhileReferenced;
          }},
      of);
}

// The lifetime of the object a borrowed pointer to `pointee` reaches: an
// object of a class the program declares, or one of the library's own --
// a scope lasts the run, and an object or a process the program makes does
// not. Absent where `pointee` is no object, whose storage lives as long as
// whatever holds it.
[[nodiscard]] auto ObjectLifetimeOf(const CompilationUnit& unit, TypeId pointee)
    -> std::optional<ObjectLifetime>;

}  // namespace lyra::mir

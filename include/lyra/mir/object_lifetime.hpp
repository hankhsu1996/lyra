#pragma once

#include <optional>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::mir {

class CompilationUnit;

// The lifetime of an object of a class whose lineage passes through `of`.
// `base_of` answers the class a class extends, whichever unit defines it: an
// instance of another module is a class of that unit. An interface
// extends none, and only a value the program makes is ever one (LRM 8.26).
template <typename BaseOf>
[[nodiscard]] auto LifetimeOfLineage(const ClassRef& of, const BaseOf& base_of)
    -> support::ObjectLifetime {
  using support::ObjectLifetime;
  return std::visit(
      Overloaded{
          [&](const ClassId& cls) {
            const std::optional<ClassRef> base = base_of(cls);
            return base.has_value() ? LifetimeOfLineage(*base, base_of)
                                    : ObjectLifetime::kWhileReferenced;
          },
          [](const ObjectTreeRootRef&) { return ObjectLifetime::kTheRun; },
          [](const ClassObjectRootRef&) {
            return ObjectLifetime::kWhileReferenced;
          },
          [](const ObjectRootRef&) {
            return ObjectLifetime::kWhileReferenced;
          }},
      of);
}

// The lifetime of the object `pointee`, which a pointer or a reference reaches:
// an object of a class the program declares, or one of a class of the
// runtime's, whose declaration states it. Neither states a lifetime of its own,
// so this is read off what it reaches. Absent where `pointee` is no object,
// whose storage lives as long as whatever holds it.
[[nodiscard]] auto ObjectLifetimeOf(const CompilationUnit& unit, TypeId pointee)
    -> std::optional<support::ObjectLifetime>;

}  // namespace lyra::mir

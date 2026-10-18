#include "lyra/mir/object_lifetime.hpp"

#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/declared_class.hpp"
#include "lyra/mir/external_class.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/runtime_class.hpp"

namespace lyra::mir {

namespace {

auto LifetimeOfLibraryClass(support::RuntimeClass which) -> ObjectLifetime {
  switch (which) {
    case support::RuntimeClass::kScope:
      return ObjectLifetime::kTheRun;
    case support::RuntimeClass::kObject:
    case support::RuntimeClass::kProcess:
      return ObjectLifetime::kWhileReferenced;
  }
  throw InternalError("mir::ObjectLifetimeOf: unknown RuntimeClass");
}

}  // namespace

auto ObjectLifetimeOf(const CompilationUnit& unit, TypeId pointee)
    -> std::optional<ObjectLifetime> {
  const Type& reached = unit.types.Get(pointee);
  if (const auto* object = reached.As<ObjectType>()) {
    return LifetimeOfLineage(
        AsClassRef(object->of), [&](const DeclaredClassRef& declared) {
          return std::visit(
              Overloaded{
                  [&](const IntraUnitClassRef& own) {
                    return unit.GetClass(own.class_id).base;
                  },
                  [&](const CrossUnitClassRef& other) {
                    return BaseOfExternalClass(unit.external_classes, other);
                  }},
              declared);
        });
  }
  if (const auto* library = reached.As<RuntimeClassType>()) {
    return LifetimeOfLibraryClass(library->which);
  }
  return std::nullopt;
}

}  // namespace lyra::mir

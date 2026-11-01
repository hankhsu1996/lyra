#include "lyra/mir/object_lifetime.hpp"

#include <optional>

#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::mir {

auto ObjectLifetimeOf(const CompilationUnit& unit, TypeId pointee)
    -> std::optional<support::ObjectLifetime> {
  const Type& reached = unit.types.Get(pointee);
  if (const auto* object = reached.As<ObjectType>()) {
    return LifetimeOfLineage(ClassRef{object->of}, [&](ClassId cls) {
      return unit.GetClass(cls).base;
    });
  }
  return reached.RuntimeClassOf().transform([](support::LibraryDecl decl) {
    return support::DeclarationOf(decl).runtime_class->lifetime;
  });
}

}  // namespace lyra::mir

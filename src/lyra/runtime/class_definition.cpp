#include "lyra/runtime/class_definition.hpp"

#include <memory>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/object_ref.hpp"

namespace lyra::runtime {

namespace {

// The definition, checked before anything is read from it, because a reference
// to a class with no definition is a linkage failure rather than a value.
auto RequireDefinition(const ObjectDefinition* definition)
    -> const ObjectDefinition* {
  if (definition == nullptr) {
    throw InternalError("class definition: the value has no definition");
  }
  return definition;
}

}  // namespace

auto AdoptObject(void* object) -> value::ObjectRef {
  return RefToObject(std::shared_ptr<GcObject>(static_cast<GcObject*>(object)));
}

auto RequireScopeClass(const ObjectDefinition* definition)
    -> const ObjectDefinition* {
  if (RequireDefinition(definition)->scope == nullptr) {
    throw InternalError(
        "class definition: an instance of the design hierarchy is built of a "
        "class that states nothing of its instances -- please report this as a "
        "bug");
  }
  return definition;
}

auto ViewOf(const value::ObjectRef& ref) -> void* {
  void* view = ref.View<void>();
  if (view == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
  return view;
}

}  // namespace lyra::runtime

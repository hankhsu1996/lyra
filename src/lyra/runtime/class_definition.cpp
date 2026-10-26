#include "lyra/runtime/class_definition.hpp"

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/object_change.hpp"
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

// The row of `iface` among those the class `object_class` defines lists, or
// none where it does not implement it. A class implements a handful at most,
// so a search is what finds one, as Swift's runtime searches a type's
// conformances.
auto RowOf(const ObjectDefinition* object_class, const ObjectDefinition* iface)
    -> const InterfaceRow* {
  for (const InterfaceRow& row : object_class->interfaces.Entries()) {
    if (row.of == iface) {
      return &row;
    }
  }
  return nullptr;
}

}  // namespace

auto IsOfClass(
    const ObjectDefinition* object_class, const ObjectDefinition* cls) -> bool {
  for (const ObjectDefinition* at = RequireDefinition(object_class);
       at != nullptr; at = at->base) {
    if (at == cls) {
      return true;
    }
  }
  return RowOf(object_class, cls) != nullptr;
}

auto DynamicCast(const value::ObjectRef& ref, const ObjectDefinition* cls)
    -> value::ObjectRef {
  const GcObject* object = ObjectRootOf(ref);
  if (object == nullptr || !IsOfClass(object->definition, cls)) {
    return value::ObjectRef{};
  }
  return ref;
}

auto InterfaceTableOf(
    const ObjectDefinition* object_class, const ObjectDefinition* iface)
    -> const void* {
  const InterfaceRow* row = RowOf(RequireDefinition(object_class), iface);
  if (row == nullptr) {
    throw InternalError(
        "class definition: a method of an interface class is called on an "
        "object whose class does not implement it -- please report this as a "
        "bug");
  }
  return row->table;
}

void EndObject(GcObject* object) noexcept {
  object->definition->end(object);
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

auto ObjectOf(const value::ObjectRef& ref) -> void* {
  return &ref.Deref<GcObject>();
}

}  // namespace lyra::runtime

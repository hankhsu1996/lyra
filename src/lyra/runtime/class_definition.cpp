#include "lyra/runtime/class_definition.hpp"

#include <algorithm>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"

namespace lyra::runtime {

namespace {

// The definition, checked before anything is read from it, because a reference
// to a class with no definition is a linkage failure rather than a value.
auto RequireDefinition(const abi::ObjectDefinition* definition)
    -> const abi::ObjectDefinition* {
  if (definition == nullptr) {
    throw InternalError("class definition: the value has no definition");
  }
  return definition;
}

// The row of `iface` among those the class `object_class` defines lists, or
// none where it does not implement it. A class implements a handful at most,
// so a search is what finds one, as Swift's runtime searches a type's
// conformances.
auto RowOf(
    const abi::ObjectDefinition* object_class,
    const abi::ObjectDefinition* iface) -> const abi::InterfaceRow* {
  for (const abi::InterfaceRow& row : object_class->interfaces.Entries()) {
    if (row.of == iface) {
      return &row;
    }
  }
  return nullptr;
}

// One interface class at the instance whose copy of it a walk stands on.
struct InterfaceCopy {
  const abi::ObjectDefinition* of;
  Scope* instance;

  auto operator==(const InterfaceCopy&) const -> bool = default;
};

// Whether a declaration whose definition is `declaring`, belonging to the
// instance `from`, names `iface` as `instance`'s copy: by a name of its own, or
// through what one of the interface classes it names extends, each reached
// from the instance the name before it landed on. Interface classes may extend
// one another along many paths to one copy (LRM 8.26.6.3), so each copy is
// gone on from once, kept in `seen`.
auto Reaches(
    const abi::ObjectDefinition* declaring, Scope* from,
    const abi::ObjectDefinition* iface, const Scope* instance,
    std::vector<InterfaceCopy>& seen) -> bool {
  for (const abi::InterfaceReach& named :
       declaring->interface_reaches.Entries()) {
    const InterfaceCopy copy{.of = named.of, .instance = named.reach(from)};
    if (copy.of == iface && copy.instance == instance) return true;
    if (std::ranges::contains(seen, copy)) continue;
    seen.push_back(copy);
    if (Reaches(copy.of, copy.instance, iface, instance, seen)) return true;
  }
  return false;
}

}  // namespace

auto IsOfClass(
    const abi::ObjectDefinition* object_class, const abi::ObjectDefinition* cls)
    -> bool {
  for (const abi::ObjectDefinition* at = RequireDefinition(object_class);
       at != nullptr; at = at->base) {
    if (at == cls) {
      return true;
    }
  }
  return RowOf(object_class, cls) != nullptr;
}

auto DynamicCast(const value::ObjectRef& ref, const abi::ObjectDefinition* cls)
    -> value::ObjectRef {
  const GcObject* object = ObjectRootOf(ref);
  if (object == nullptr || !IsOfClass(object->definition, cls)) {
    return value::ObjectRef{};
  }
  return ref;
}

auto IsOfInterfaceOf(
    const value::ObjectRef& ref, const abi::ObjectDefinition* iface,
    const Scope* instance) -> bool {
  GcObject* object = ObjectRootOf(ref);
  if (object == nullptr) return false;
  std::vector<InterfaceCopy> seen;
  for (const abi::ObjectDefinition* at = RequireDefinition(object->definition);
       at != nullptr; at = at->base) {
    if (at->declaring_instance != nullptr &&
        Reaches(at, at->declaring_instance(object), iface, instance, seen)) {
      return true;
    }
  }
  return false;
}

auto InterfaceTableOf(
    const abi::ObjectDefinition* object_class,
    const abi::ObjectDefinition* iface) -> const void* {
  const abi::InterfaceRow* row = RowOf(RequireDefinition(object_class), iface);
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

auto RequireScopeClass(const abi::ObjectDefinition* definition)
    -> const abi::ObjectDefinition* {
  if (RequireDefinition(definition)->scope == nullptr) {
    throw InternalError(
        "class definition: an instance of the design hierarchy is built of a "
        "class that states nothing of its instances -- please report this as a "
        "bug");
  }
  return definition;
}

}  // namespace lyra::runtime

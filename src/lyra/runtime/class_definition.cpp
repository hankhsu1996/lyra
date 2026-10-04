#include "lyra/runtime/class_definition.hpp"

#include <format>
#include <memory>
#include <span>
#include <string>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/scope_info.hpp"

namespace lyra::runtime {

namespace {

// Where `name` lands, starting at the class the access names and walking what
// each class extends. The first class in that walk that declares the name is
// the one the access reaches, which is the rule a subclass redeclaring a base's
// name answers to (LRM 8.14): the access names a class, and that class's own
// declaration is what it means.
template <typename Entry>
auto Find(const ObjectDefinition* cls, auto table, std::string_view name)
    -> const Entry* {
  for (const ObjectDefinition* at = cls; at != nullptr; at = at->base) {
    for (const Entry& entry : (at->*table).Entries()) {
      if (entry.name == name) {
        return &entry;
      }
    }
  }
  return nullptr;
}

auto NoSuchName(std::string_view what, std::string_view name) -> std::string {
  return std::format(
      "a name reaching past a compilation unit's signature asks for {} '{}' on "
      "a class that declares none (LRM 23.6)",
      what, name);
}

}  // namespace

auto AdoptObject(void* object) -> value::ObjectRef {
  return RefToObject(std::shared_ptr<GcObject>(static_cast<GcObject*>(object)));
}

auto RequireDefinition(const ObjectDefinition* definition)
    -> const ObjectDefinition* {
  if (definition == nullptr) {
    throw InternalError("class definition: the value has no definition");
  }
  return definition;
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

auto FindProperty(const ObjectDefinition* cls, std::string_view name)
    -> const PropertyCoordinate* {
  const auto* found = Find<ResolvedProperty>(
      RequireDefinition(cls), &ObjectDefinition::property_names, name);
  if (found == nullptr) {
    throw SimulationError(NoSuchName("a property", name));
  }
  return &found->at;
}

auto FindBehaviorBody(const ObjectDefinition* cls, std::string_view name)
    -> ErasedEntry {
  const auto* found = Find<DeclaredBody>(
      RequireDefinition(cls), &ObjectDefinition::body_names, name);
  if (found == nullptr) {
    throw SimulationError(NoSuchName("a behavior", name));
  }
  return found->body;
}

auto ViewOf(const value::ObjectRef& ref) -> void* {
  void* view = ref.View<void>();
  if (view == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
  return view;
}

auto PropertyAt(GcObject* object, const PropertyCoordinate* at) -> void* {
  if (object == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
  const std::span<const PropertySlotEntry> slots =
      RequireDefinition(at->declared_by)->property_slots.Entries();
  if (at->slot >= slots.size()) {
    throw InternalError(
        "class definition: the class an access names holds no property at the "
        "position the access carries");
  }
  return slots[at->slot](object);
}

}  // namespace lyra::runtime

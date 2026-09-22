#include "lyra/runtime/managed_object.hpp"

#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/scope_program.hpp"

namespace lyra::runtime {

namespace {

// The definition, checked before anything is read from it, because a reference
// to a class with no definition is a linkage failure rather than a value.
auto Checked(const ObjectDefinition* definition) -> const ObjectDefinition* {
  if (definition == nullptr) {
    throw InternalError("ManagedObject: the object has no definition");
  }
  return definition;
}

auto Text(AbiStringRef name) -> std::string_view {
  return {name.data, name.size};
}

// The names one class declares itself, each paired with where it lands on that
// class. What its lineage declares is not copied in: a class states what it
// adds, and a name it does not declare is found by asking what it extends.
template <typename Entry>
void Declare(
    std::span<const DeclaredName> own, const ObjectDefinition& declarer,
    std::vector<Entry>& into) {
  into.clear();
  into.reserve(own.size());
  for (const DeclaredName& declared : own) {
    into.push_back(Entry{declared.name, {&declarer, declared.position}});
  }
}

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
      if (Text(entry.name) == name) {
        return &entry;
      }
    }
  }
  return nullptr;
}

// The body the class `of` answers `introduced_by`'s `ordinal`-th behavior with
// (LRM 8.20, 8.22), read off the table its lineage left flat.
auto MethodOf(
    const ObjectDefinition* of, const ObjectDefinition* introduced_by,
    std::uint32_t ordinal) -> ErasedMethodEntry {
  const std::uint32_t position =
      Checked(introduced_by)->first_behavior + ordinal;
  const std::span<const ErasedMethodEntry> entries = of->methods.Entries();
  if (position >= entries.size()) {
    throw InternalError(
        "ManagedObject: the object's class holds no dispatch position this "
        "call names");
  }
  // A position nothing in the lineage supplied a body for belongs to a class
  // LRM 8.21 forbids constructing, so an object holding one is a class that was
  // built when it should not have been rather than a call that went wrong.
  if (entries[position] == nullptr) {
    throw InternalError(
        "ManagedObject: the object's class supplies no body for the dispatch "
        "position this call names");
  }
  return entries[position];
}

// The two answers a class whose storage the runtime owns gives. They are the
// same for every such class, because what they read is the object's own storage
// and its own dispatch table rather than anything one class settled.
auto RuntimeOwnedPropertyAt(
    const GcObject* object, const ObjectDefinition* declared_by,
    std::uint32_t slot) -> void* {
  return static_cast<ManagedObject*>(object->IdentityAddress())
      ->MemberAddress(declared_by, slot);
}

auto RuntimeOwnedBehaviorAt(
    const GcObject* object, const ObjectDefinition* introduced_by,
    std::uint32_t ordinal) -> ErasedMethodEntry {
  return MethodOf(Checked(object->Class()), introduced_by, ordinal);
}

// The object a coordinate is applied to, checked once for the two ways it can
// fail to answer. Naming no object is the design's own failure (LRM 8.4);
// naming one whose class answers nothing is a class that was brought up
// incompletely, which is a bug in whoever generated it.
auto Answering(const GcObject* object) -> const GcObject* {
  if (object == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
  const ObjectDefinition* of = Checked(object->Class());
  if (of->property_at == nullptr || of->behavior_at == nullptr) {
    throw InternalError(
        "an object's class was brought up without the answers a name resolved "
        "while the design elaborated reaches it by");
  }
  return object;
}

auto NoSuchName(std::string_view what, std::string_view name) -> std::string {
  return std::format(
      "a name reaching past a compilation unit's signature asks for {} '{}' on "
      "a class that declares none (LRM 23.6)",
      what, name);
}

}  // namespace

void RealizeClass(
    const ClassContribution& adds, RealizedClass& realization,
    ObjectDefinition& definition) {
  const ObjectDefinition empty;
  const ObjectDefinition& base = adds.base != nullptr ? *adds.base : empty;

  realization.members.assign(
      base.members.Descriptors().begin(), base.members.Descriptors().end());
  realization.members.insert(
      realization.members.end(), adds.members.begin(), adds.members.end());

  realization.methods.assign(
      base.methods.Entries().begin(), base.methods.Entries().end());
  realization.methods.insert(
      realization.methods.end(), adds.introductions.begin(),
      adds.introductions.end());

  definition.base = adds.base;
  definition.first_member = base.members.size;
  definition.first_behavior = base.methods.size;
  // Realizing a class here is what gives its objects runtime-owned storage, so
  // the answers that read that storage are this step's to install.
  definition.property_at = &RuntimeOwnedPropertyAt;
  definition.behavior_at = &RuntimeOwnedBehaviorAt;

  Declare(adds.property_names, definition, realization.property_names);
  Declare(adds.behavior_names, definition, realization.behavior_names);
  realization.body_names.assign(adds.body_names.begin(), adds.body_names.end());

  for (const DispatchTakeover& taken : adds.takeovers) {
    const std::uint32_t position =
        Checked(taken.introduced_by)->first_behavior + taken.ordinal;
    if (position >= realization.methods.size()) {
      throw InternalError(
          "RealizeClass: a class takes over a behavior its lineage does not "
          "carry");
    }
    realization.methods[position] = taken.body;
  }

  definition.members = MemberStorageSchema{
      .data = realization.members.data(),
      .size = static_cast<std::uint32_t>(realization.members.size())};
  definition.methods = MethodDispatchTable{
      realization.methods.data(),
      static_cast<std::uint32_t>(realization.methods.size())};
  definition.property_names = ResolvedPropertyTable{
      realization.property_names.data(),
      static_cast<std::uint32_t>(realization.property_names.size())};
  definition.behavior_names = ResolvedBehaviorTable{
      realization.behavior_names.data(),
      static_cast<std::uint32_t>(realization.behavior_names.size())};
  definition.body_names = DeclaredBodyTable{
      realization.body_names.data(),
      static_cast<std::uint32_t>(realization.body_names.size())};
}

auto FindProperty(const ObjectDefinition* cls, std::string_view name)
    -> const PropertyCoordinate* {
  const auto* found = Find<ResolvedProperty>(
      Checked(cls), &ObjectDefinition::property_names, name);
  if (found == nullptr) {
    throw SimulationError(NoSuchName("a property", name));
  }
  return &found->at;
}

auto FindBehavior(const ObjectDefinition* cls, std::string_view name)
    -> const BehaviorCoordinate* {
  const auto* found = Find<ResolvedBehavior>(
      Checked(cls), &ObjectDefinition::behavior_names, name);
  if (found == nullptr) {
    throw SimulationError(NoSuchName("a behavior", name));
  }
  return &found->at;
}

auto FindBehaviorBody(const ObjectDefinition* cls, std::string_view name)
    -> ErasedMethodEntry {
  const auto* found =
      Find<DeclaredBody>(Checked(cls), &ObjectDefinition::body_names, name);
  if (found == nullptr) {
    throw SimulationError(NoSuchName("a behavior", name));
  }
  return found->body;
}

auto LineagePropertyAt(
    const GcObject* object, const ObjectDefinition* declared_by,
    std::uint32_t slot) -> void* {
  void* self = object->IdentityAddress();
  for (const ObjectDefinition* at = Checked(object->Class()); at != nullptr;
       at = at->base) {
    if (at == Checked(declared_by)) {
      const std::span<const PropertySlotEntry> entries =
          at->property_slots.Entries();
      if (slot >= entries.size()) {
        throw InternalError(
            "the class an access names holds no property at the position the "
            "access carries");
      }
      return entries[slot](self);
    }
    if (at->to_base == nullptr) {
      break;
    }
    self = at->to_base(self);
  }
  throw InternalError(
      "the class an access names is not one the object's own class extends");
}

auto LineageBehaviorAt(
    const GcObject* object, const ObjectDefinition* introduced_by,
    std::uint32_t ordinal) -> ErasedMethodEntry {
  // A class takes a behavior over from whatever introduced it (LRM 8.20), and
  // the object's own class is where the answer starts, so the first class in
  // the walk that answers this position is the one whose body runs (LRM 8.22).
  for (const ObjectDefinition* at = Checked(object->Class()); at != nullptr;
       at = at->base) {
    for (const DispatchTakeover& taken : at->takeovers.Entries()) {
      if (taken.introduced_by == Checked(introduced_by) &&
          taken.ordinal == ordinal) {
        return taken.body;
      }
    }
    if (at == introduced_by) {
      const std::span<const ErasedMethodEntry> entries =
          at->introductions.Entries();
      // A position nothing in the lineage supplied a body for belongs to a
      // class LRM 8.21 forbids constructing, so an object holding one is a
      // class that was built when it should not have been.
      if (ordinal >= entries.size() || entries[ordinal] == nullptr) {
        throw InternalError(
            "the class that introduced the behavior this call names supplies "
            "no body for it");
      }
      return entries[ordinal];
    }
  }
  throw InternalError(
      "the class that introduced the behavior this call names is not one the "
      "object's own class extends");
}

ManagedObject::ManagedObject(const ObjectDefinition* definition)
    : members_(Checked(definition)->members) {
  AdoptClass(definition);
}

auto ManagedObject::MemberAddress(
    const ObjectDefinition* declared_by, std::uint32_t slot) -> void* {
  return members_.Address(Checked(declared_by)->first_member + slot);
}

auto ObjectOf(const value::ManagedRef& handle) -> void* {
  return Answering(static_cast<const GcObject*>(handle.Share().get()))
      ->IdentityAddress();
}

auto PropertyAt(const GcObject* object, const PropertyCoordinate* at) -> void* {
  return Answering(object)->Class()->property_at(
      object, at->declared_by, at->slot);
}

auto BehaviorAt(const GcObject* object, const BehaviorCoordinate* at)
    -> ErasedMethodEntry {
  return Answering(object)->Class()->behavior_at(
      object, at->introduced_by, at->ordinal);
}

auto ObjectIsOfClass(
    const value::ManagedRef& handle, const ObjectDefinition* wanted)
    -> std::int64_t {
  const auto* object = static_cast<const GcObject*>(handle.Share().get());
  // A class states what it extends and nothing about the rest of its lineage,
  // so the walk is the whole answer. A handle referring to no object starts it
  // at nothing and it ends having found nothing, which is the same answer.
  for (const ObjectDefinition* at = object == nullptr ? nullptr
                                                      : object->Class();
       at != nullptr; at = at->base) {
    if (at == Checked(wanted)) {
      return 1;
    }
  }
  return 0;
}

}  // namespace lyra::runtime

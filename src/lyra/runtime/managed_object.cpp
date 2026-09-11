#include "lyra/runtime/managed_object.hpp"

#include <algorithm>
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

// The names a class answers, from those its lineage already answered and those
// it declares itself. A name the class redeclares replaces the inherited entry,
// because which declaration an access reaches is fixed by the class the access
// names (LRM 8.14) and that class's own is what it names.
template <typename Entry>
void Extend(
    std::span<const Entry> inherited, std::span<const DeclaredName> own,
    const ObjectDefinition& declarer, std::vector<Entry>& into) {
  into.assign(inherited.begin(), inherited.end());
  for (const DeclaredName& declared : own) {
    const Entry entry{
        .name = declared.name, .at = {&declarer, declared.position}};
    const auto shadowed = std::ranges::find_if(into, [&](const Entry& at) {
      return Text(at.name) == Text(declared.name);
    });
    if (shadowed == into.end()) {
      into.push_back(entry);
    } else {
      *shadowed = entry;
    }
  }
}

template <typename Entry>
auto Find(std::span<const Entry> entries, std::string_view name)
    -> const Entry* {
  for (const Entry& entry : entries) {
    if (Text(entry.name) == name) {
      return &entry;
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

  definition.first_member = base.members.size;
  definition.first_behavior = base.methods.size;

  Extend(
      base.property_names.Entries(), adds.property_names, definition,
      realization.property_names);
  Extend(
      base.behavior_names.Entries(), adds.behavior_names, definition,
      realization.behavior_names);

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
      .data = realization.methods.data(),
      .size = static_cast<std::uint32_t>(realization.methods.size())};
  definition.property_names = ResolvedPropertyTable{
      .data = realization.property_names.data(),
      .size = static_cast<std::uint32_t>(realization.property_names.size())};
  definition.behavior_names = ResolvedBehaviorTable{
      .data = realization.behavior_names.data(),
      .size = static_cast<std::uint32_t>(realization.behavior_names.size())};
}

auto FindProperty(const ObjectDefinition* cls, std::string_view name)
    -> const PropertyCoordinate* {
  const ResolvedProperty* found =
      Find(Checked(cls)->property_names.Entries(), name);
  if (found == nullptr) {
    throw SimulationError(NoSuchName("a property", name));
  }
  return &found->at;
}

auto FindBehavior(const ObjectDefinition* cls, std::string_view name)
    -> const BehaviorCoordinate* {
  const ResolvedBehavior* found =
      Find(Checked(cls)->behavior_names.Entries(), name);
  if (found == nullptr) {
    throw SimulationError(NoSuchName("a behavior", name));
  }
  return &found->at;
}

ManagedObject::ManagedObject(const ObjectDefinition* definition)
    : definition_(Checked(definition)), members_(definition_->members) {
}

auto ManagedObject::MemberAddress(
    const ObjectDefinition* declared_by, std::uint32_t slot) -> void* {
  return members_.Address(Checked(declared_by)->first_member + slot);
}

auto ManagedObject::Method(
    const ObjectDefinition* introduced_by, std::uint32_t ordinal) const
    -> ErasedMethodEntry {
  const std::uint32_t position =
      Checked(introduced_by)->first_behavior + ordinal;
  const std::span<const ErasedMethodEntry> entries =
      definition_->methods.Entries();
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

}  // namespace lyra::runtime

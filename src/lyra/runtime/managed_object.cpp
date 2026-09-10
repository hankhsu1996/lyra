#include "lyra/runtime/managed_object.hpp"

#include <cstdint>
#include <span>

#include "lyra/base/internal_error.hpp"
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

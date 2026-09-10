#include "lyra/runtime/managed_object.hpp"

#include <cstdint>
#include <span>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/scope_program.hpp"

namespace lyra::runtime {

namespace {

// The definition, checked before anything is built from it, because an object
// with no definition is a linkage failure rather than a value.
auto Checked(const ObjectDefinition* definition) -> const ObjectDefinition* {
  if (definition == nullptr) {
    throw InternalError("ManagedObject: the object has no definition");
  }
  return definition;
}

}  // namespace

ManagedObject::ManagedObject(const ObjectDefinition* definition)
    : definition_(Checked(definition)), members_(definition_->members) {
}

auto ManagedObject::MemberAddress(std::uint32_t index) -> void* {
  return members_.Address(index);
}

auto ManagedObject::Method(std::uint32_t position) const -> ErasedMethodEntry {
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

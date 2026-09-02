#include "lyra/runtime/managed_object.hpp"

#include <cstdint>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/scope_program.hpp"

namespace lyra::runtime {

namespace {

// The property schema, checked before any storage is built from it, because an
// object with no definition is a linkage failure rather than a value.
auto MemberSchemaOf(const ObjectDefinition* definition) -> MemberStorageSchema {
  if (definition == nullptr) {
    throw InternalError("ManagedObject: the object has no definition");
  }
  return definition->members;
}

}  // namespace

ManagedObject::ManagedObject(const ObjectDefinition* definition)
    : definition_(definition), members_(MemberSchemaOf(definition)) {
}

auto ManagedObject::MemberAddress(std::uint32_t index) -> void* {
  return members_.Address(index);
}

void ManagedObject::Construct() {
  if (definition_->construct == nullptr) {
    throw InternalError("ManagedObject: the class has no constructor body");
  }
  definition_->construct(this);
}

}  // namespace lyra::runtime

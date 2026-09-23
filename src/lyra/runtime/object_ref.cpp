#include "lyra/runtime/object_ref.hpp"

namespace lyra::runtime {

GcObject::GcObject() = default;
GcObject::~GcObject() = default;
GcObject::GcObject(const GcObject&) = default;
auto GcObject::operator=(const GcObject&) -> GcObject& = default;

void GcObject::AdoptIdentity(void* address) {
  identity_ = address;
}

auto GcObject::IdentityAddress() const -> void* {
  return identity_;
}

void GcObject::AdoptClass(const ObjectDefinition* of) {
  class_ = of;
}

auto GcObject::Class() const -> const ObjectDefinition* {
  return class_;
}

}  // namespace lyra::runtime

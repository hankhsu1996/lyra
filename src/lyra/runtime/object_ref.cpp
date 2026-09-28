#include "lyra/runtime/object_ref.hpp"

#include <memory>

#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"

namespace lyra::runtime {

GcObject::GcObject() = default;
GcObject::~GcObject() = default;

GcObject::GcObject(const GcObject& other)
    : std::enable_shared_from_this<GcObject>(other),
      identity_(other.identity_),
      class_(other.class_) {
}

auto GcObject::operator=(const GcObject& other) -> GcObject& {
  if (this == &other) {
    return *this;
  }
  std::enable_shared_from_this<GcObject>::operator=(other);
  identity_ = other.identity_;
  class_ = other.class_;
  return *this;
}

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

auto GcObject::EventSource() -> Observable& {
  if (event_source_ == nullptr) {
    event_source_ = std::make_unique<Observable>();
  }
  return *event_source_;
}

auto GcObject::Watched() const -> bool {
  return event_source_ != nullptr && event_source_->HasWaiter();
}

void GcObject::PublishChange() {
  if (Watched()) {
    current_runtime().WakeWaitersOf(
        *event_source_, MakeWholeValueProjectionTest());
  }
}

}  // namespace lyra::runtime

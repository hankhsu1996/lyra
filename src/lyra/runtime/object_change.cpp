#include "lyra/runtime/object_change.hpp"

#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/value/managed_ref.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

auto ObjectRootOf(const value::ManagedRef& handle) -> GcObject* {
  return static_cast<GcObject*>(handle.Share().get());
}

auto EventSourceOf(GcObject* object) -> Observable* {
  if (object == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
  return &object->EventSource();
}

ObjectWrite::ObjectWrite(GcObject* object, const void* place)
    : object_(object), place_(place) {
  if (object_ == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
}

ObjectWrite::~ObjectWrite() {
  object_->PublishChange();
}

auto ObjectWrite::Place() const -> const void* {
  return place_;
}

}  // namespace lyra::runtime

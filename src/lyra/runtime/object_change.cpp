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

ErasedObjectWrite::ErasedObjectWrite(GcObject* object) : object_(object) {
  if (object_ == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
}

ErasedObjectWrite::~ErasedObjectWrite() {
  object_->PublishChange();
}

auto ErasedObjectWrite::Object() const -> GcObject* {
  return object_;
}

}  // namespace lyra::runtime

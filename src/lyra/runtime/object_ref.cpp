#include "lyra/runtime/object_ref.hpp"

#include <memory>

#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"

namespace lyra::runtime {

GcObject::GcObject() = default;
GcObject::~GcObject() = default;

// A copy is a different object, so it starts with no event source of its own:
// what waits on the one it was copied from waits on that one.
GcObject::GcObject(const GcObject& other)
    : std::enable_shared_from_this<GcObject>(other) {
}

auto GcObject::operator=(const GcObject& other) -> GcObject& {
  if (this == &other) {
    return *this;
  }
  std::enable_shared_from_this<GcObject>::operator=(other);
  return *this;
}

auto GcObject::EventSource() -> Observable& {
  if (event_source_ == nullptr) {
    event_source_ = std::make_unique<Observable>();
  }
  return *event_source_;
}

// A write to any object also reaches the waits watching every object, so the
// object is watched while one of them is, whether or not it has a source of
// its own.
auto GcObject::Watched() const -> bool {
  return (event_source_ != nullptr && event_source_->HasWaiter()) ||
         current_runtime().EveryObject().HasWaiter();
}

void GcObject::PublishChange() {
  RuntimeEffects& runtime = current_runtime();
  if (event_source_ != nullptr && event_source_->HasWaiter()) {
    runtime.WakeWaitersOf(*event_source_, MakeWholeValueProjectionTest());
  }
  if (Observable& every = runtime.EveryObject(); every.HasWaiter()) {
    runtime.WakeWaitersOf(every, MakeWholeValueProjectionTest());
  }
}

}  // namespace lyra::runtime

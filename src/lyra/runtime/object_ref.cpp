#include "lyra/runtime/object_ref.hpp"

#include <memory>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

namespace {

// Ends an object once its last reference goes, where it was handed over to
// the code that created it. An instance of the design hierarchy never is: its
// parent owns it alone and ends it, so its own end does nothing.
struct EndObject {
  bool handed_over = false;

  void operator()(GcObject* object) const noexcept {
    if (handed_over) {
      delete object;
    }
  }
};

}  // namespace

struct GcObject::Watchers {
  Observable properties;
  IntrusiveList<WaitMembership> expiry;
};

GcObject::GcObject() : creators_share_(this, EndObject{}) {
}

// What reached this object through a weak reference is handed over to be told
// once the object is gone; what watched its properties is let go of, nothing
// having changed for it.
GcObject::~GcObject() {
  if (watchers_ != nullptr) {
    HandOverExpiry(watchers_->expiry);
  }
}

// A copy is a different object, so it starts with no event source of its own
// -- what waits on the one it was copied from waits on that one -- and with
// its own creator's reference.
GcObject::GcObject(const GcObject& other)
    : std::enable_shared_from_this<GcObject>(other),
      creators_share_(this, EndObject{}) {
}

auto GcObject::TakeCreatorsShare() -> std::shared_ptr<GcObject> {
  if (creators_share_ == nullptr) {
    throw InternalError(
        "GcObject: an object is handed to the code that built it twice -- "
        "please report this as a bug");
  }
  std::get_deleter<EndObject>(creators_share_)->handed_over = true;
  return std::move(creators_share_);
}

// Assigning changes what an object holds, not which object it is.
auto GcObject::operator=(const GcObject& other) -> GcObject& {
  if (this == &other) {
    return *this;
  }
  std::enable_shared_from_this<GcObject>::operator=(other);
  return *this;
}

auto GcObject::WatchersOf() -> Watchers& {
  if (watchers_ == nullptr) {
    watchers_ = std::make_unique<Watchers>();
  }
  return *watchers_;
}

auto GcObject::EventSource() -> Observable& {
  return WatchersOf().properties;
}

auto GcObject::Expiry() -> IntrusiveList<WaitMembership>& {
  return WatchersOf().expiry;
}

auto GcObject::Watched() const -> bool {
  return watchers_ != nullptr && watchers_->properties.HasMembers();
}

void GcObject::PublishChange() {
  if (Watched()) {
    current_runtime().WakeParkedOn(
        watchers_->properties, PartPath{}, Change::Whole());
  }
}

}  // namespace lyra::runtime

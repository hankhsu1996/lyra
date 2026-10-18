#include "lyra/runtime/object_ref.hpp"

#include <memory>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/runtime_effects.hpp"

namespace lyra::runtime {

namespace {

// Ends an object once its last reference goes, where it was handed over to
// the code that asked for it. One whose constructor left by an exception was
// never handed over: unwinding the construction has already ended it and given
// its storage back, which a reference taken while it was being built cannot
// change, so its end does nothing.
struct EndObject {
  bool handed_over = false;

  void operator()(GcObject* object) const noexcept {
    if (handed_over) {
      delete object;
    }
  }
};

}  // namespace

GcObject::GcObject() : creators_share_(this, EndObject{}) {
}

GcObject::~GcObject() = default;

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
  return (event_source_ != nullptr && event_source_->HasMembers()) ||
         current_runtime().EveryObject().HasMembers();
}

void GcObject::PublishChange() {
  RuntimeEffects& runtime = current_runtime();
  if (event_source_ != nullptr && event_source_->HasMembers()) {
    runtime.WakeParkedOn(*event_source_, PartPath{}, Change::Whole());
  }
  if (Observable& every = runtime.EveryObject(); every.HasMembers()) {
    runtime.WakeParkedOn(every, PartPath{}, Change::Whole());
  }
}

}  // namespace lyra::runtime

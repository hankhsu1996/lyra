#include "lyra/runtime/synchronization_object.hpp"

#include <cstdint>
#include <memory>

#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

const abi::ObjectDefinition SynchronizationObject::kDefinition =
    LibraryObjectDefinition<SynchronizationObject>();

SynchronizationObject::SynchronizationObject() : GcObject(kDefinition) {
}
SynchronizationObject::~SynchronizationObject() = default;

void SynchronizationObject::Trigger(RuntimeEffects& runtime) {
  last_triggered_at_ = runtime.Now();
  ++occurrences_;
  if (Watched()) {
    runtime.WakeParkedOn(EventSource(), PartPath{}, Change::Whole());
  }
}

auto SynchronizationObject::Triggered(RuntimeEffects& runtime) const
    -> value::Bit {
  return value::Bit::FromBool(
      last_triggered_at_.has_value() && *last_triggered_at_ == runtime.Now());
}

auto SynchronizationObject::Occurrences() const -> std::int64_t {
  return occurrences_;
}

namespace {

void StateReached(const value::ObjectRef& event, ReadReport* report) {
  if (report != nullptr && event) {
    report->AddObject(event);
  }
}

}  // namespace

auto EventNew() -> value::ObjectRef {
  // The object holds the reference its creator owns until the handle takes it
  // over, as every object a `new` builds does.
  auto object = std::make_unique<SynchronizationObject>();
  return AdoptObject(object.release());
}

void EventTrigger(const value::ObjectRef& event, RuntimeEffects& runtime) {
  if (SynchronizationObject* object = event.Get<SynchronizationObject>()) {
    object->Trigger(runtime);
  }
}

auto EventTriggered(
    const value::ObjectRef& event, RuntimeEffects& runtime, ReadReport* report)
    -> value::Bit {
  StateReached(event, report);
  const SynchronizationObject* object = event.Get<SynchronizationObject>();
  return object == nullptr ? value::Bit::FromBool(false)
                           : object->Triggered(runtime);
}

auto EventOccurrences(const value::ObjectRef& event, ReadReport* report)
    -> std::int64_t {
  StateReached(event, report);
  const SynchronizationObject* object = event.Get<SynchronizationObject>();
  return object == nullptr ? 0 : object->Occurrences();
}

auto EventReportsTo(const value::ObjectRef& event) -> WatchedPlace {
  SynchronizationObject* const object = event.Get<SynchronizationObject>();
  return WatchedPlace{object == nullptr ? nullptr : &object->EventSource()};
}

}  // namespace lyra::runtime

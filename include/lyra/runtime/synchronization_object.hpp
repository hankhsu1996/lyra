#pragma once

#include <cstdint>
#include <optional>

#include "lyra/base/time.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

class Observable;
class ReadReport;
class RuntimeEffects;

// What an event variable is a handle to (LRM 6.17, 15.5): the object a trigger
// occurs on and a wait is put on. A variable holds a reference to one, so
// assigning one event to another makes both name it, and it lives for as long
// as anything refers to it -- a variable, or a wait put on it (LRM 15.5.5.2).
//
// A trigger carries no value, so nothing about it can be shown not to have
// reached a wait: every wait put on the object is asked. The "triggered in the
// current time step" state (LRM 15.5.3) is the time of the last trigger
// compared with the current time, so nothing clears it.
//
// Generated code reaches it only through the functions below, so every member
// is defined in the library and none is compiled into a unit.
class SynchronizationObject final : public GcObject {
 public:
  SynchronizationObject();
  SynchronizationObject(const SynchronizationObject&) = delete;
  auto operator=(const SynchronizationObject&)
      -> SynchronizationObject& = delete;
  SynchronizationObject(SynchronizationObject&&) = delete;
  auto operator=(SynchronizationObject&&) -> SynchronizationObject& = delete;
  ~SynchronizationObject();

  // The definition every object of this class is created with.
  static const ObjectDefinition kDefinition;

  // LRM 15.5.1: records the time it occurred and asks every wait put on this
  // object.
  void Trigger(RuntimeEffects& runtime);

  // LRM 15.5.3.
  [[nodiscard]] auto Triggered(RuntimeEffects& runtime) const -> value::Bit;

  // How many times this has been triggered, which a wait measures from where
  // it began: it is woken by any trigger, and one of the triggers since then is
  // what makes reaching it an event.
  [[nodiscard]] auto Occurrences() const -> std::int64_t;

 private:
  std::optional<SimTime> last_triggered_at_;
  std::int64_t occurrences_ = 0;
};

// The operations an event variable's value is handed to. `event` is a reference
// to a synchronization object or to none.

// A new synchronization object, which a declaration of an event with no
// initializer names (LRM 6.17, Table 6-7).
[[nodiscard]] auto EventNew() -> value::ObjectRef;

// LRM 15.5.1. Triggering an event that names no object has no effect (LRM
// 15.5.5.2).
void EventTrigger(const value::ObjectRef& event, RuntimeEffects& runtime);

// LRM 15.5.3, stating the object reached in `report` where the evaluation
// asking makes one, so a wait on the answer is put on that object. What an
// event naming no object answers is left undefined by LRM 15.5.5.2; it answers
// 0 and states nothing, since nothing can trigger it.
[[nodiscard]] auto EventTriggered(
    const value::ObjectRef& event, RuntimeEffects& runtime, ReadReport* report)
    -> value::Bit;

// The triggers so far of the object `event` names, which a wait on the event
// compares across a candidacy, stating the object in `report` the same way.
// None for an event naming no object.
[[nodiscard]] auto EventOccurrences(
    const value::ObjectRef& event, ReadReport* report) -> std::int64_t;

// What a wait on `event` is put on: the object's own event source, or none for
// an event that names no object, where the wait is put on nothing and so waits
// for as long as the run lasts -- one of the outcomes LRM 15.5.5.2 leaves open.
[[nodiscard]] auto EventReportsTo(const value::ObjectRef& event) -> Observable*;

}  // namespace lyra::runtime

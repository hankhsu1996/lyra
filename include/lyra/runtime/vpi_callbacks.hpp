#pragma once

// The callbacks foreign code registers with `vpi_register_cb` (LRM 36.9.2,
// 38.36): the run keeps what each was registered with, attaches it to the
// source its reason names, and calls it there until it is removed (LRM 38.39)
// or its moment is spent. A callback is an object of the run (LRM 37.80),
// named by its identity among those the run has registered, which no handle
// to it is: releasing a handle leaves the callback, and two registrations of
// the same data are two callbacks.
//
// What a reason waits for is answered by what already owns the occurrence: a
// change of a variable by the place a wait on it enrols on (LRM 9.4.2), a
// disable by the target it ends (LRM 9.6.2), a point of a time slot by a timer
// in that slot's region (LRM 4.10, Table 4-1), and a simulator action by the
// run. So nothing here watches storage, and a run that registers no callback
// pays nothing to have its storage watched.

#include <cstdint>
#include <deque>
#include <map>
#include <memory>
#include <optional>
#include <span>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/timer.hpp"
#include "lyra/runtime/vpi_object.hpp"
#include "lyra/runtime/vpi_status.hpp"
#include "lyra/runtime/vpi_value.hpp"
#include "lyra/runtime/vpi_variable.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/runtime/weak_reference.hpp"
#include "vpi_user.h"

namespace lyra::runtime {

class CancellationTarget;
class Runtime;
class VpiCallback;
class VpiCallbacks;

// The simulator actions a callback is called for (LRM 38.36.3): the start and
// the end of the simulation, each once, and each run-time error of the design
// and each error of a VPI routine.
enum class SimulatorAction : std::uint8_t {
  kSimulationStarted,
  kSimulationEnded,
  kRunTimeError,
  kRoutineError,
};

// What a callback on a change of an object is enrolled on that object's place
// as, from when it is made until it is withdrawn: told of each write that may
// have changed what it watches as the write lands, and asking the run's
// callbacks to call it at the occurrence where it did.
class CallbackWatch {
 public:
  CallbackWatch(
      VpiCallbacks& callbacks, CallbackObject callback, const Trigger& watched);
  CallbackWatch(const CallbackWatch&) = delete;
  auto operator=(const CallbackWatch&) -> CallbackWatch& = delete;
  CallbackWatch(CallbackWatch&&) = delete;
  auto operator=(CallbackWatch&&) -> CallbackWatch& = delete;
  ~CallbackWatch() = default;

  void Reached();
  void Withdraw();

 private:
  VpiCallbacks* callbacks_;
  CallbackObject callback_;
  WaitMembership membership_;
};

// The size an array or a string changed to (LRM 37.17 detail 14).
struct NewSize {
  std::int64_t size = 0;
};

// What a callback is handed of the occurrence it is called for, taken as the
// occurrence happened: the object's value, the size it changed to, the error
// `vpi_chk_error` reports during the call, or nothing.
using Occurrence =
    std::variant<std::monostate, HeldValue, NewSize, VpiErrorReport>;

// Which change of an object a callback is called for (LRM 38.36.1): a change
// of the object's value, handed the value it holds after; a change of a class
// variable, which has no value a callback is handed and changes where it names
// another object or none; and a change of the object's size, handed the size
// it changed to. What a write tells says which callbacks it may affect, and a
// callback decides whether its object changed by comparing what it reads now
// with what it last read -- a value, an object, a size -- so no write's account
// of what it did is taken for a change.
struct ValueChanges {
  HeldValue last;
};
struct HandleChanges {
  WeakObjectRef last;
};
struct SizeChanges {
  std::int64_t last = 0;
};
using ChangeCalledFor = std::variant<ValueChanges, HandleChanges, SizeChanges>;

// How a callback is attached to the source its reason names: enrolled on the
// place of the object it watches the changes of, waiting for a disable of a
// target, a timer in a region of a time slot it names or of the one time next
// moves to, a simulator action, or a signal the process is sent. Each holds
// what it is linked into that source by, at an address that does not move. A
// timer is shared with whoever fires it, which keeps it until it has fired.
struct OnChange {
  VpiObject object;
  ChangeCalledFor called_for;
  std::unique_ptr<CallbackWatch> watch;
};
struct OnDisable {
  VpiObject object;
  // None for a function, which no disable ends.
  CancellationTarget* target = nullptr;
};
struct InSlot {
  SimTime at;
  std::shared_ptr<Timer> timer;
};
struct AtNextSlot {
  std::shared_ptr<Timer> timer;
};
struct AtAction {
  SimulatorAction action;
};
struct OnSignal {
  int signal = 0;
};
using Attachment =
    std::variant<OnChange, OnDisable, InSlot, AtNextSlot, AtAction, OnSignal>;

// One callback: what foreign code registered it with, kept as copies of its
// own since what it was handed belongs to the caller (LRM 38.36), and how it
// is attached. The time and value structures are what `vpi_get_cb_info`
// hands back (LRM 38.8). It is shared by the registry, while it stands, and by
// each call of it running, so one removed during its own call lives until that
// call returns.
class VpiCallback {
 public:
  VpiCallback(
      CallbackObject id, const s_cb_data& given, std::int8_t time_unit,
      vpiHandle object, Attachment attachment);
  VpiCallback(const VpiCallback&) = delete;
  auto operator=(const VpiCallback&) -> VpiCallback& = delete;
  VpiCallback(VpiCallback&&) = delete;
  auto operator=(VpiCallback&&) -> VpiCallback& = delete;
  ~VpiCallback();

  [[nodiscard]] auto Id() const -> CallbackObject {
    return id_;
  }
  [[nodiscard]] auto Attached() -> Attachment& {
    return attachment_;
  }
  // Whether it is called once and then ends: one at a point of a time slot, or
  // for a simulator action that happens once.
  [[nodiscard]] auto OneShot() const -> bool;
  // The object of LRM 37.80's diagram it is placed on -- an expression, a
  // statement or a time queue -- none where it is placed on none of them.
  [[nodiscard]] auto PlacedOn() const -> std::optional<VpiObject>;

  // `vpi_get_cb_info` (LRM 38.8).
  void Info(s_cb_data& into);

  // The data a call is handed, in structures the run owns for as long as the
  // call lasts (LRM 38.36).
  struct CallData {
    s_cb_data data{};
    s_vpi_time time{};
    s_vpi_value value{};
    VpiValueStore store;
  };
  // Fills `into` for a call for `occurrence`; where that is an error, makes it
  // what `vpi_chk_error` reports in the call (LRM 38.36.3).
  void Hand(
      RuntimeEffects& run, const Occurrence& occurrence, CallData& into) const;

 private:
  friend class VpiCallbacks;

  CallbackObject id_;
  PLI_INT32 reason_;
  PLI_INT32 (*routine_)(p_cb_data);
  // The unit a scaled time is handed in: that of the object it was registered
  // with, the simulation's where it was registered with none (LRM 38.36.2).
  std::int8_t time_unit_;
  // The run's own handle to the object it was registered with, none where it
  // was registered with none, released with it, since a call is handed it
  // (LRM 38.36).
  vpiHandle object_;
  std::optional<s_vpi_time> time_;
  std::optional<s_vpi_value> value_;
  PLI_INT32 index_;
  PLI_BYTE8* user_data_;
  Attachment attachment_;
};

// Every callback of the run, and what is due of them.
class VpiCallbacks {
 public:
  explicit VpiCallbacks(Runtime& run);
  VpiCallbacks(const VpiCallbacks&) = delete;
  auto operator=(const VpiCallbacks&) -> VpiCallbacks& = delete;
  VpiCallbacks(VpiCallbacks&&) = delete;
  auto operator=(VpiCallbacks&&) -> VpiCallbacks& = delete;
  ~VpiCallbacks();

  // `vpi_register_cb` (LRM 38.36): the callback registered, or why none is.
  // `object` is what `given.obj` refers to, none where it is null.
  auto Register(const s_cb_data& given, std::optional<VpiObject> object)
      -> VpiAnswer<CallbackObject>;

  // The callback `id` names, none where it has been removed or has ended.
  [[nodiscard]] auto Find(CallbackObject id) -> VpiCallback*;

  // Ends `callback`, removed by `vpi_remove_cb` (LRM 38.39) or with its moment
  // spent: it is taken out of the registry and of its source at once, and not
  // called again.
  void Remove(VpiCallback& callback);

  // The callbacks placed on `object` (LRM 37.80), and those placed on none of
  // the objects of that diagram.
  [[nodiscard]] auto RelatedTo(const VpiObject& object)
      -> std::vector<VpiObject>;
  [[nodiscard]] auto RelatedToNothing() -> std::vector<VpiObject>;

  // The callback `id` was reached by a write that may have changed what it
  // watches.
  void Reached(CallbackObject id);
  // Calls what occurrences made due, at the occurrence (LRM 38.36.1).
  void RunDue();
  // Whether any callback waits for a disable, which is what makes a disable
  // gather the targets it ends.
  [[nodiscard]] auto WatchesDisables() const -> bool {
    return !disable_listeners_.empty();
  }
  // A `disable` ended `ended` (LRM 9.6.2).
  void Disabled(std::span<CancellationTarget* const> ended);
  // `action` happened (LRM 38.36.3), the error each call reads through
  // `vpi_chk_error` where it is an error.
  void ActionOccurred(SimulatorAction action, const Occurrence& occurrence);
  // The run reported a run-time error of the design, `message` at `at`.
  void ErrorReported(base::SourceLocation at, const base::DiagMessage& message);
  // The process was sent `signal`, which a callback traps (LRM 38.36.3).
  void Signalled(int signal);

 private:
  struct DueCall {
    CallbackObject id;
    Occurrence occurrence;
  };

  // What `given` is attached by for the callback `id`, linked into its source,
  // or why it is refused.
  auto Attach(
      CallbackObject id, const s_cb_data& given,
      const std::optional<VpiObject>& object, std::int8_t time_unit)
      -> VpiAnswer<Attachment>;
  auto AttachToChange(
      CallbackObject id, const s_cb_data& given,
      const std::optional<VpiObject>& object) -> VpiAnswer<Attachment>;
  auto AttachToDisable(
      CallbackObject id, const std::optional<VpiObject>& object)
      -> VpiAnswer<Attachment>;
  auto AttachToSlot(
      CallbackObject id, const s_cb_data& given, std::int8_t time_unit)
      -> VpiAnswer<Attachment>;
  auto AttachToAction(CallbackObject id, PLI_INT32 reason)
      -> VpiAnswer<Attachment>;
  auto AttachToSignal(CallbackObject id, PLI_INT32 signal)
      -> VpiAnswer<Attachment>;
  // Calls each of `ids` still registered, in turn, for `occurrence`.
  void CallEach(
      std::span<const CallbackObject> ids, const Occurrence& occurrence);
  // Calls the callback `id`, sharing it for as long as the call runs, where it
  // is still registered.
  void Call(CallbackObject id, const Occurrence& occurrence);
  // Makes the callback `id` due for `occurrence`, and asks for what is due to
  // be called at the occurrence.
  void Due(CallbackObject id, Occurrence occurrence);
  void CallDueAtTheOccurrence();

  Runtime* run_;
  std::uint64_t next_id_ = 0;
  // Ordered by registration, which is the order the callbacks of one
  // occurrence are called in.
  std::map<CallbackObject, std::shared_ptr<VpiCallback>> live_;
  // What occurrences made due and has not been called yet, in the order
  // reached, and whether that is being called now or will be at the
  // occurrence.
  std::deque<DueCall> due_;
  bool calling_due_ = false;
  bool due_queued_ = false;
  // The simulator actions whose callbacks are being called, which a callback
  // of one does not reach again from inside, however deep.
  std::vector<SimulatorAction> calling_actions_;
  // How many calls of start-of-time callbacks are running, inside which one
  // may be placed for the slot under way (LRM 38.36.2).
  int start_of_time_calls_ = 0;
  // The callbacks each such source calls, in the order they were registered,
  // which is the order they are called in: a disable target's (LRM 9.6.2), a
  // simulator action's, a signal's (LRM 38.36.3). A source no callback waits
  // for has no entry, so one occurrence finds its callbacks without walking
  // the rest.
  std::map<const CancellationTarget*, std::vector<CallbackObject>>
      disable_listeners_;
  std::map<SimulatorAction, std::vector<CallbackObject>> action_listeners_;
  std::map<int, std::vector<CallbackObject>> signal_listeners_;
};

// The callbacks `from` relates to (LRM 37.80): those placed on an expression or
// a statement or waiting in a time queue, and with no object those placed on
// none of them.
[[nodiscard]] auto CallbacksRelatedTo(RuntimeEffects& run, const Referent& from)
    -> VpiAnswer<std::vector<VpiObject>>;

}  // namespace lyra::runtime

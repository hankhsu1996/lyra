#include "lyra/runtime/vpi_callbacks.hpp"

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <expected>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/delay.hpp"
#include "lyra/runtime/dpi_context.hpp"
#include "lyra/runtime/reflection.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/signal_watcher.hpp"
#include "lyra/runtime/timer.hpp"
#include "lyra/runtime/vpi_object.hpp"
#include "lyra/runtime/vpi_status.hpp"
#include "lyra/runtime/vpi_time.hpp"
#include "lyra/runtime/vpi_value.hpp"
#include "lyra/runtime/vpi_variable.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/support/region.hpp"
#include "lyra/value/real.hpp"
#include "sv_vpi_user.h"
#include "vpi_user.h"

namespace lyra::runtime {

namespace {

// Every version of the standard from 1800-2012 on names an object's type as
// the latest does, and a callback asks of an object only what every version
// names alike.
constexpr VpiMode kLatest = VpiMode::k1800v2012;

auto Refused(std::string why) -> std::unexpected<std::string> {
  return std::unexpected(std::move(why));
}

// The region of a time slot a time callback runs in (LRM Table 4-1), for each
// reason that names one. A read-write synchronization may run in Pre-NBA or
// Post-NBA, the tool choosing; it runs "after execution of events for a
// specified time" (LRM 38.36.2), which Post-NBA is, so it runs there.
auto RegionOf(PLI_INT32 reason) -> std::optional<support::Region> {
  switch (reason) {
    case cbAfterDelay:
    case cbAtStartOfSimTime:
    case cbNextSimTime:
      return support::Region::kPreActive;
    case cbNBASynch:
      return support::Region::kPreNba;
    case cbReadWriteSynch:
      return support::Region::kPostNba;
    case cbAtEndOfSimTime:
      return support::Region::kPrePostponed;
    case cbReadOnlySynch:
      return support::Region::kPostponed;
    default:
      return std::nullopt;
  }
}

// What a reason of a simulator feature names that Lyra does not have (LRM
// 38.36.3 lets a tool lack each), none for any other reason.
auto AbsentFeature(PLI_INT32 reason) -> std::optional<std::string> {
  switch (reason) {
    case cbStartOfSave:
    case cbEndOfSave:
    case cbStartOfRestart:
    case cbEndOfRestart:
      return "saving and restarting a simulation";
    case cbStartOfReset:
    case cbEndOfReset:
      return "$reset";
    case cbEnterInteractive:
    case cbExitInteractive:
    case cbInteractiveScopeChange:
      return "an interactive mode";
    // The front end refuses a system task or function the standard does not
    // define before any foreign code runs, so none is met while one does.
    case cbUnresolvedSystf:
      return "meeting an unresolved system task or function while running";
    default:
      return std::nullopt;
  }
}

// What a reason needs that VPI does not hand out yet, none for any other.
auto ObjectNotYetReached(PLI_INT32 reason) -> std::optional<std::string> {
  switch (reason) {
    case cbStmt:
    case cbForce:
    case cbRelease:
    case cbAssign:
    case cbDeassign:
      return "a callback on a statement";
    case cbStartOfFrame:
    case cbEndOfFrame:
      return "a callback on a frame";
    case cbStartOfThread:
    case cbEndOfThread:
    case cbEnterThread:
      return "a callback on a thread";
    case cbCreateObj:
    case cbReclaimObj:
    case cbEndOfObject:
      return "a callback on a class object or a transient object";
    case cbTchkViolation:
      return "a callback on a timing check, which no run carries out yet,";
    default:
      return std::nullopt;
  }
}

// The kind of array a variable of `type` is, none for one that is no array.
auto ArrayKind(const VpiObject& object, PLI_INT32 type)
    -> std::optional<PLI_INT64> {
  if (type != vpiArrayVar) return std::nullopt;
  const VpiAnswer<PLI_INT64> kind =
      VariablePartIntegerProperty(vpiArrayType, object, kLatest);
  if (!kind.has_value()) return std::nullopt;
  return *kind;
}

// Whether a variable of `type` is one the run resizes: a string, a dynamic or
// associative array, or a queue (LRM 37.17 detail 14).
auto IsResizable(const VpiObject& object, PLI_INT32 type) -> bool {
  if (type == vpiStringVar) return true;
  const std::optional<PLI_INT64> kind = ArrayKind(object, type);
  return kind.has_value() && (*kind == vpiDynamicArray ||
                              *kind == vpiQueueArray || *kind == vpiAssocArray);
}

// The index of an element of an array, which a callback on one is handed
// (LRM 38.36.1); 0 for any other object, and for an entry of an associative
// array whose key is no number.
auto IndexOf(const VpiObject& object) -> PLI_INT32 {
  const auto* variable = std::get_if<VariableObject>(&object);
  if (variable == nullptr || variable->selection.Depth() == 0) return 0;
  const VpiAnswer<PLI_INT64> member =
      VariablePartIntegerProperty(vpiArrayMember, object, kLatest);
  if (!member.has_value() || *member == 0) return 0;
  return static_cast<PLI_INT32>(
      IndexOfStep(variable->selection.Steps().back()).value_or(0));
}

// What `read` answers of `object` now -- the value it holds, or the object a
// class variable names -- read as the design reads it; none where the design
// cannot read it now -- a member of a tagged union its tag does not name --
// which is then no change a callback is told of.
template <typename T>
auto ReadNow(VpiAnswer<T> (*read)(const VpiObject&), const VpiObject& object)
    -> std::optional<T> {
  try {
    VpiAnswer<T> now = read(object);
    if (!now.has_value()) return std::nullopt;
    return *std::move(now);
  } catch (const SimulationError&) {
    return std::nullopt;
  }
}

// Whether `a` and `b` are one value bit for bit, which is what makes a write
// no change of it (LRM 9.4.2).
auto SameValue(const HeldValue& a, const HeldValue& b) -> bool {
  if (a.index() != b.index()) return false;
  return std::visit(
      Overloaded{
          [&](const value::BitVec& bits) {
            return bits.IsBitIdentical(std::get<value::BitVec>(b));
          },
          [&](double real) {
            return std::bit_cast<std::uint64_t>(real) ==
                   std::bit_cast<std::uint64_t>(std::get<double>(b));
          },
          [&](const value::String& text) {
            return text.IsBitIdentical(std::get<value::String>(b));
          }},
      a);
}

// The unit a callback's object reads time in, the simulation's where it was
// registered with none (LRM 38.36.2).
auto UnitOf(RuntimeEffects& run, const std::optional<VpiObject>& object)
    -> std::int8_t {
  const Referent of =
      object.has_value() ? Referent{*object} : Referent{Simulation{}};
  const VpiAnswer<std::int8_t> unit = TimeUnitOf(run, of);
  return unit.has_value() ? *unit : run.GlobalPrecisionPower();
}

// Whether `object` is one of LRM 37.80's diagram a callback can be placed on
// and found by: an expression, which a variable is, a statement, which a named
// block is, or a time queue.
auto IsOnTheDiagram(const VpiObject& object) -> bool {
  return std::visit(
      Overloaded{
          [](const VariableObject&) { return true; },
          [&](const LexicalScope&) {
            const PLI_INT32 type = TypeOf(object, kLatest);
            return type == vpiNamedBegin || type == vpiNamedFork;
          },
          [](const TimeQueueObject&) { return true; },
          [](Scope*) { return false; },
          [](const NamespaceObject&) { return false; },
          [](const ScopeArray&) { return false; },
          [](const TypespecObject&) { return false; },
          [](const TypespecMemberObject&) { return false; },
          [](const RangeObject&) { return false; },
          [](const ConstantObject&) { return false; },
          [](const EnumConstantObject&) { return false; },
          [](const CallbackObject&) { return false; }},
      object);
}

// The extent of a callback's foreign call (LRM 38.36), left however the call
// ends: it runs on no thread, as no procedure of the design, inside an
// application routine call of its own, where no DPI scope is current and no
// export may be called (LRM Annex H.9).
class CallingACallback {
 public:
  explicit CallingACallback(Runtime& run) : run_(&run) {
    run.EnterNoThread(NoThreadContext{});
    Calls().EnterCallback();
  }
  CallingACallback(const CallingACallback&) = delete;
  auto operator=(const CallingACallback&) -> CallingACallback& = delete;
  CallingACallback(CallingACallback&&) = delete;
  auto operator=(CallingACallback&&) -> CallingACallback& = delete;
  ~CallingACallback() {
    Calls().Leave();
    run_->LeaveNoThread();
  }

 private:
  [[nodiscard]] auto Calls() -> ApplicationRoutineCalls& {
    RuntimeEffects& effects = *run_;
    return effects.Running().application_calls;
  }

  Runtime* run_;
};

}  // namespace

CallbackWatch::CallbackWatch(
    VpiCallbacks& callbacks, CallbackObject callback, const Trigger& watched)
    : callbacks_(&callbacks), callback_(callback) {
  EnrolOnPlace(membership_, this, watched);
}

void CallbackWatch::Reached() {
  callbacks_->Reached(callback_);
}

void CallbackWatch::Withdraw() {
  membership_.Unlink();
}

VpiCallback::VpiCallback(
    CallbackObject id, const s_cb_data& given, std::int8_t time_unit,
    vpiHandle object, Attachment attachment)
    : id_(id),
      reason_(given.reason),
      routine_(given.cb_rtn),
      time_unit_(time_unit),
      object_(object),
      time_(
          given.time == nullptr ? std::nullopt
                                : std::optional<s_vpi_time>(*given.time)),
      value_(
          given.value == nullptr
              ? std::nullopt
              : std::optional<s_vpi_value>(
                    s_vpi_value{.format = given.value->format, .value = {}})),
      index_(given.index),
      user_data_(given.user_data),
      attachment_(std::move(attachment)) {
}

VpiCallback::~VpiCallback() {
  if (object_ == nullptr) return;
  if (AmbientRunContext* context = AmbientRunContext::TryCurrent()) {
    context->ObjectHandles().Release(object_);
  }
}

auto VpiCallback::OneShot() const -> bool {
  return std::visit(
      Overloaded{
          [](const OnChange&) { return false; },
          [](const OnDisable&) { return false; },
          [](const InSlot&) { return true; },
          [](const AtNextSlot&) { return true; },
          [](const OnSignal&) { return false; },
          [](const AtAction& at) {
            switch (at.action) {
              case SimulatorAction::kSimulationStarted:
              case SimulatorAction::kSimulationEnded:
                return true;
              case SimulatorAction::kRunTimeError:
              case SimulatorAction::kRoutineError:
                return false;
            }
            throw InternalError(
                "VpiCallback::OneShot: unknown simulator action");
          }},
      attachment_);
}

auto VpiCallback::PlacedOn() const -> std::optional<VpiObject> {
  return std::visit(
      Overloaded{
          [](const OnChange& change) -> std::optional<VpiObject> {
            return change.object;
          },
          // A task or a function is none of the diagram's objects.
          [](const OnDisable& disable) -> std::optional<VpiObject> {
            if (!IsOnTheDiagram(disable.object)) return std::nullopt;
            return disable.object;
          },
          [](const InSlot& slot) -> std::optional<VpiObject> {
            return TimeQueueObject{.at = slot.at};
          },
          // One due as time next moves is in no time queue yet.
          [](const AtNextSlot&) -> std::optional<VpiObject> {
            return std::nullopt;
          },
          [](const AtAction&) -> std::optional<VpiObject> {
            return std::nullopt;
          },
          [](const OnSignal&) -> std::optional<VpiObject> {
            return std::nullopt;
          }},
      attachment_);
}

void VpiCallback::Info(s_cb_data& into) {
  into.reason = reason_;
  into.cb_rtn = routine_;
  into.obj = object_;
  into.time = time_.has_value() ? &*time_ : nullptr;
  into.value = value_.has_value() ? &*value_ : nullptr;
  into.index = index_;
  into.user_data = user_data_;
}

void VpiCallback::Hand(
    RuntimeEffects& run, const Occurrence& occurrence, CallData& into) const {
  into.data.reason = reason_;
  into.data.cb_rtn = routine_;
  into.data.obj = object_;
  // A callback on an element of an array is handed the element's index (LRM
  // 38.36.1).
  const auto* change = std::get_if<OnChange>(&attachment_);
  into.data.index = change == nullptr ? index_ : IndexOf(change->object);
  into.data.user_data = user_data_;
  // A time or a value the registration asked to be left out is no structure
  // (LRM 38.36.1.1).
  into.data.time = nullptr;
  if (time_.has_value() && time_->type != vpiSuppressTime) {
    into.time.type = time_->type;
    // A time in simulation time is in the global time precision, and a scaled
    // one in the time unit of the object the callback was registered with
    // (LRM 38.13, 38.36.2).
    const std::int8_t unit =
        time_->type == vpiSimTime ? run.GlobalPrecisionPower() : time_unit_;
    if (FillTime(run, unit, into.time)) into.data.time = &into.time;
  }
  into.data.value = nullptr;
  const bool value_wanted = change != nullptr && value_.has_value() &&
                            value_->format != vpiSuppressVal;
  if (value_wanted) into.value.format = value_->format;
  std::visit(
      Overloaded{
          [](std::monostate) {},
          [&](const HeldValue& held) {
            if (!value_wanted) return;
            const VpiAnswer<ValueShape> shape = ValueShapeOf(change->object);
            if (!shape.has_value()) return;
            if (FillValue(*shape, held, into.value, into.store)) {
              into.data.value = &into.value;
            }
          },
          [&](const NewSize& changed) {
            if (!value_wanted) return;
            value::BitVec bits = value::BitVec::Zeros(32);
            value::FromInt(bits.MutableView(), changed.size);
            const ValueShape shape{
                .held = IntegralShape{.width = 32, .is_signed = true},
                .natural_format = vpiIntVal};
            if (FillValue(
                    shape, HeldValue{std::move(bits)}, into.value,
                    into.store)) {
              into.data.value = &into.value;
            }
          },
          // The error a call is for is what `vpi_chk_error` reports in it (LRM
          // 38.36.3), whatever an earlier call cleared.
          [](const VpiErrorReport& error) { SetVpiError(error); }},
      occurrence);
}

VpiCallbacks::VpiCallbacks(Runtime& run) : run_(&run) {
}

// What a callback that stands to the end holds of the process is given back:
// a signal it trapped is handled as it was before it, and a handle it kept is
// released.
VpiCallbacks::~VpiCallbacks() {
  while (!live_.empty()) Remove(*live_.begin()->second);
}

auto VpiCallbacks::Register(
    const s_cb_data& given, std::optional<VpiObject> object)
    -> VpiAnswer<CallbackObject> {
  if (given.cb_rtn == nullptr) return Refused("no routine was given");
  const CallbackObject id{.id = ++next_id_};
  const std::int8_t time_unit = UnitOf(*run_, object);
  VpiAnswer<Attachment> attachment = Attach(id, given, object, time_unit);
  if (!attachment.has_value()) return std::unexpected(attachment.error());
  vpiHandle kept = nullptr;
  if (object.has_value()) {
    kept = AmbientRunContext::Current().ObjectHandles().Issue(*object);
  }
  live_.emplace(
      id, std::make_shared<VpiCallback>(
              id, given, time_unit, kept, *std::move(attachment)));
  return id;
}

namespace {

// Takes `id` off the list `listeners` keeps for `source`, and the source off
// the map where no callback is left on its list.
template <class Source>
void DropFrom(
    std::map<Source, std::vector<CallbackObject>>& listeners,
    const Source& source, CallbackObject id) {
  const auto found = listeners.find(source);
  if (found == listeners.end()) return;
  std::erase(found->second, id);
  if (found->second.empty()) listeners.erase(found);
}

}  // namespace

auto VpiCallbacks::Attach(
    CallbackObject id, const s_cb_data& given,
    const std::optional<VpiObject>& object, std::int8_t time_unit)
    -> VpiAnswer<Attachment> {
  const PLI_INT32 reason = given.reason;
  if (std::optional<std::string> absent = AbsentFeature(reason)) {
    return Refused("this simulator has no " + *absent);
  }
  if (std::optional<std::string> unreached = ObjectNotYetReached(reason)) {
    return VpiNotYet(*unreached);
  }
  switch (reason) {
    case cbValueChange:
    case cbSizeChange:
      return AttachToChange(id, given, object);
    case cbDisable:
      return AttachToDisable(id, object);
    case cbAfterDelay:
    case cbAtStartOfSimTime:
    case cbNextSimTime:
    case cbNBASynch:
    case cbReadWriteSynch:
    case cbAtEndOfSimTime:
    case cbReadOnlySynch:
      return AttachToSlot(id, given, time_unit);
    case cbEndOfCompile:
    case cbStartOfSimulation:
    case cbEndOfSimulation:
    case cbError:
    case cbPLIError:
      return AttachToAction(id, reason);
    case cbSignal:
      return AttachToSignal(id, given.index);
    default:
      return Refused("the reason is none LRM 38.36 defines");
  }
}

auto VpiCallbacks::AttachToChange(
    CallbackObject id, const s_cb_data& given,
    const std::optional<VpiObject>& object) -> VpiAnswer<Attachment> {
  // LRM 38.36.1.
  if (!object.has_value()) {
    return Refused("a change is called back of the object given");
  }
  const auto* variable = std::get_if<VariableObject>(&*object);
  if (variable == nullptr) {
    return VpiNotYet("a change of an object that is no variable");
  }
  const PLI_INT32 type = TypeOfActual(*object, kLatest);
  std::optional<ChangeCalledFor> called_for;
  if (given.reason == cbSizeChange) {
    if (!IsResizable(*object, type)) {
      return Refused(
          "a size changes only of a string, a dynamic or associative array "
          "or a queue");
    }
    const VpiAnswer<PLI_INT64> size =
        VariablePartIntegerProperty(vpiSize, *object, kLatest);
    if (!size.has_value()) return std::unexpected(size.error());
    called_for = SizeChanges{.last = *size};
  } else if (IsResizable(*object, type)) {
    // LRM 38.36.1.
    return Refused(
        "a value change is not called back for a string, a dynamic or "
        "associative array or a queue");
  } else if (type == vpiClassVar) {
    const std::optional<value::ObjectRef> named =
        ReadNow(&ObjectNamedByVariablePart, *object);
    if (!named.has_value()) {
      return Refused("the object's value cannot be read now");
    }
    called_for = HandleChanges{.last = Downgrade(*named)};
  } else {
    const VpiAnswer<ValueShape> shape = ValueShapeOf(*object);
    if (!shape.has_value()) return std::unexpected(shape.error());
    if (given.value != nullptr && given.value->format == vpiStrengthVal) {
      return Refused("a variable has no strength to change");
    }
    std::optional<HeldValue> held = ReadNow(&ReadVariablePart, *object);
    if (!held.has_value()) {
      return Refused("the object's value cannot be read now");
    }
    if (given.value != nullptr && given.value->format != vpiSuppressVal) {
      s_vpi_value tried{.format = given.value->format, .value = {}};
      VpiValueStore scratch;
      if (!FillValue(*shape, *held, tried, scratch)) {
        return Refused("the value format does not apply to the object");
      }
    }
    called_for = ValueChanges{.last = *std::move(held)};
  }
  if (given.time != nullptr && given.time->type != vpiSimTime &&
      given.time->type != vpiScaledRealTime &&
      given.time->type != vpiSuppressTime) {
    return Refused("the time structure names no time type");
  }
  return Attachment{OnChange{
      .object = *object,
      .called_for = *std::move(called_for),
      .watch = std::make_unique<CallbackWatch>(
          *this, id, WatchOfVariablePart(*variable))}};
}

auto VpiCallbacks::AttachToDisable(
    CallbackObject id, const std::optional<VpiObject>& object)
    -> VpiAnswer<Attachment> {
  const LexicalScope* scope =
      object.has_value() ? std::get_if<LexicalScope>(&*object) : nullptr;
  const PLI_INT32 type =
      scope == nullptr ? vpiUndefined : TypeOf(*object, kLatest);
  // LRM 38.36.1.
  if (type != vpiNamedBegin && type != vpiNamedFork && type != vpiTask &&
      type != vpiFunction) {
    return Refused(
        "a disable is called back for a named block, a named fork, a task or "
        "a function");
  }
  // No `disable` names a function, and a function never waits, so no disable
  // from elsewhere can find one running; the one way left, a function that
  // disables what called it, is undefined (LRM 9.6.2). Its callback is kept
  // and waits on no target.
  if (type == vpiFunction) {
    return Attachment{OnDisable{.object = *object, .target = nullptr}};
  }
  CancellationTarget* target =
      ScopeDeclarationsOf(scope->holder)
          .disable_target(
              InstanceOf(scope->holder), scope->declaration->position);
  if (target == nullptr) {
    return VpiNotYet("a disable of a block no instance holds");
  }
  disable_listeners_[target].push_back(id);
  return Attachment{OnDisable{.object = *object, .target = target}};
}

auto VpiCallbacks::AttachToSlot(
    CallbackObject id, const s_cb_data& given, std::int8_t time_unit)
    -> VpiAnswer<Attachment> {
  const PLI_INT32 reason = given.reason;
  // LRM 38.36.2.
  if (given.time == nullptr || (given.time->type != vpiSimTime &&
                                given.time->type != vpiScaledRealTime)) {
    return Refused(
        "a time callback is handed a time in simulation time or scaled real "
        "time");
  }
  const auto timer = std::make_shared<Timer>();
  timer->Bind([this, id] { Call(id, std::monostate{}); });
  // A callback for a moment the run will not reach is refused: a later slot
  // once none will run, the run having been asked to end or being over, or a
  // region of the slot under way that has passed.
  const SimTime now = run_->Now();
  if (reason == cbNextSimTime) {
    if (!run_->CanSchedule(now + 1, support::Region::kPreActive)) {
      return Refused("no later time slot runs");
    }
    run_->ScheduleAtNextSlot(*timer);
    return Attachment{AtNextSlot{.timer = timer}};
  }
  // Every reason's time is a delay from now: the clause speaks of a delay of
  // zero for a start-of-time callback and a read-write one alike (LRM
  // 38.36.2).
  SimTime delay = 0;
  if (given.time->type == vpiSimTime) {
    delay = (static_cast<SimTime>(given.time->high) << 32U) | given.time->low;
  } else {
    if (!(given.time->real >= 0.0)) return Refused("the delay is negative");
    delay = DelayTicksReal(
        value::Real{given.time->real}, time_unit, run_->GlobalPrecisionPower());
  }
  const SimTime at = now + delay;
  support::Region region = *RegionOf(reason);
  // Once the slot is under way a start-of-time callback for it is placed only
  // from inside one (LRM 38.36.2), which runs where it would.
  if (delay == 0 && reason == cbAtStartOfSimTime &&
      !run_->CanSchedule(at, support::Region::kPreponed) &&
      start_of_time_calls_ == 0) {
    return Refused("the time slot under way has started");
  }
  if (!run_->CanSchedule(at, region)) {
    if (delay != 0) return Refused("no later time slot runs");
    // An after-delay callback of no delay is no error (LRM 38.36.2), so it
    // runs in the earliest region the slot still runs, its Active region.
    if (reason != cbAfterDelay ||
        !run_->CanSchedule(at, support::Region::kActive)) {
      return Refused(
          "the region of this time slot the callback runs in has passed");
    }
    region = support::Region::kActive;
  }
  run_->Schedule(at, region, *timer);
  return Attachment{InSlot{.at = at, .timer = timer}};
}

auto VpiCallbacks::AttachToAction(CallbackObject id, PLI_INT32 reason)
    -> VpiAnswer<Attachment> {
  SimulatorAction action{};
  switch (reason) {
    // Nothing of the run reaches foreign code before its design is built.
    case cbEndOfCompile:
      return Refused("the end of compilation has passed");
    case cbStartOfSimulation:
      if (run_->SimulationStarted()) {
        return Refused("the simulation has already started");
      }
      action = SimulatorAction::kSimulationStarted;
      break;
    case cbEndOfSimulation:
      action = SimulatorAction::kSimulationEnded;
      break;
    case cbError:
      action = SimulatorAction::kRunTimeError;
      break;
    case cbPLIError:
      action = SimulatorAction::kRoutineError;
      break;
    default:
      return Refused("the reason names no simulator action");
  }
  action_listeners_[action].push_back(id);
  return Attachment{AtAction{.action = action}};
}

auto VpiCallbacks::AttachToSignal(CallbackObject id, PLI_INT32 signal)
    -> VpiAnswer<Attachment> {
  SignalWatcher* signals = run_->Signals();
  if (signals == nullptr) {
    return Refused("no signal reaches this run");
  }
  if (!signals->Watch(signal)) {
    return Refused(
        "the signal is none the system lets a program catch and carry on "
        "past");
  }
  signal_listeners_[signal].push_back(id);
  return Attachment{OnSignal{.signal = signal}};
}

auto VpiCallbacks::Find(CallbackObject id) -> VpiCallback* {
  const auto found = live_.find(id);
  return found == live_.end() ? nullptr : found->second.get();
}

void VpiCallbacks::Remove(VpiCallback& callback) {
  const auto found = live_.find(callback.Id());
  if (found == live_.end()) return;
  const std::shared_ptr<VpiCallback> ending = std::move(found->second);
  live_.erase(found);
  // Taken out of its source now, so it neither keeps a time slot nor is told
  // of a change; a timer firing is already off its queue. It goes once the
  // last call of it sharing it returns, here where none is running.
  const CallbackObject id = ending->Id();
  std::visit(
      Overloaded{
          [](OnChange& change) { change.watch->Withdraw(); },
          [&](const OnDisable& disable) {
            DropFrom<const CancellationTarget*>(
                disable_listeners_, disable.target, id);
          },
          [](InSlot& slot) { slot.timer->Place().Unlink(); },
          [](AtNextSlot& next) { next.timer->Place().Unlink(); },
          [&](const AtAction& on) {
            DropFrom(action_listeners_, on.action, id);
          },
          [&](const OnSignal& on) {
            DropFrom(signal_listeners_, on.signal, id);
            if (SignalWatcher* signals = run_->Signals()) {
              signals->Unwatch(on.signal);
            }
          }},
      ending->Attached());
}

auto VpiCallbacks::RelatedTo(const VpiObject& object)
    -> std::vector<VpiObject> {
  std::vector<VpiObject> placed;
  for (const auto& [id, callback] : live_) {
    const std::optional<VpiObject> on = callback->PlacedOn();
    if (on.has_value() && SameVariablePart(*on, object)) {
      placed.emplace_back(id);
    }
  }
  return placed;
}

auto VpiCallbacks::RelatedToNothing() -> std::vector<VpiObject> {
  std::vector<VpiObject> placed;
  for (const auto& [id, callback] : live_) {
    if (!callback->PlacedOn().has_value()) placed.emplace_back(id);
  }
  return placed;
}

void VpiCallbacks::Reached(CallbackObject id) {
  VpiCallback* callback = Find(id);
  if (callback == nullptr) return;
  auto* on = std::get_if<OnChange>(&callback->Attached());
  if (on == nullptr) return;
  // A write reaching the object may have changed it; whether it did is what
  // the callback reads now against what it last read (LRM 38.36.1).
  std::visit(
      Overloaded{
          [&](ValueChanges& value) {
            std::optional<HeldValue> now =
                ReadNow(&ReadVariablePart, on->object);
            if (!now.has_value() || SameValue(*now, value.last)) return;
            value.last = *now;
            Due(id, *std::move(now));
          },
          [&](HandleChanges& handle) {
            const std::optional<value::ObjectRef> now =
                ReadNow(&ObjectNamedByVariablePart, on->object);
            if (!now.has_value() || WeakRefersTo(handle.last, *now)) return;
            handle.last = Downgrade(*now);
            Due(id, std::monostate{});
          },
          [&](SizeChanges& size) {
            const VpiAnswer<PLI_INT64> now =
                VariablePartIntegerProperty(vpiSize, on->object, kLatest);
            if (!now.has_value() || *now == size.last) return;
            size.last = *now;
            Due(id, NewSize{.size = *now});
          }},
      on->called_for);
}

void VpiCallbacks::Due(CallbackObject id, Occurrence occurrence) {
  due_.push_back(DueCall{.id = id, .occurrence = std::move(occurrence)});
  CallDueAtTheOccurrence();
}

void VpiCallbacks::CallDueAtTheOccurrence() {
  if (calling_due_ || due_queued_) return;
  due_queued_ = true;
  run_->RunCallbacksAtTheOccurrence(*this);
}

void VpiCallbacks::RunDue() {
  due_queued_ = false;
  calling_due_ = true;
  // What one occurrence made due is ordered once it has all been reached: a
  // size change ahead of any value change made due with it (LRM 37.17 detail
  // 14). What a call made due comes after what was due before it, so only
  // what was made due since the last call is ordered again.
  std::size_t ordered = 0;
  while (!due_.empty()) {
    std::stable_partition(
        due_.begin() + static_cast<std::ptrdiff_t>(ordered), due_.end(),
        [](const DueCall& due) {
          return std::holds_alternative<NewSize>(due.occurrence);
        });
    const DueCall next = std::move(due_.front());
    due_.pop_front();
    ordered = due_.size();
    Call(next.id, next.occurrence);
  }
  calling_due_ = false;
}

void VpiCallbacks::Call(CallbackObject id, const Occurrence& occurrence) {
  const auto found = live_.find(id);
  if (found == live_.end()) return;
  const std::shared_ptr<VpiCallback> callback = found->second;
  const bool start_of_time = callback->reason_ == cbAtStartOfSimTime;
  if (start_of_time) ++start_of_time_calls_;
  VpiCallback::CallData data;
  callback->Hand(*run_, occurrence, data);
  // What the design wrote is handed over before foreign code runs (LRM 35),
  // and what `vpi_control` asked for is done as the call returns (LRM 38.4).
  BeforeForeignCodeRuns(*run_);
  {
    const CallingACallback calling(*run_);
    callback->routine_(&data.data);
    CallbackReturned(*run_);
  }
  if (start_of_time) --start_of_time_calls_;
  if (callback->OneShot()) Remove(*callback);
}

void VpiCallbacks::CallEach(
    std::span<const CallbackObject> ids, const Occurrence& occurrence) {
  for (const CallbackObject id : ids) Call(id, occurrence);
}

void VpiCallbacks::Disabled(std::span<CancellationTarget* const> ended) {
  // Everything the disable made due is due before any of it is called, as
  // what one write made due is, so what a call of it makes due comes after
  // the rest; a call may remove one still due, which is then not called.
  for (const CancellationTarget* target : ended) {
    const auto listening = disable_listeners_.find(target);
    if (listening == disable_listeners_.end()) continue;
    for (const CallbackObject id : listening->second) {
      due_.push_back(DueCall{.id = id, .occurrence = std::monostate{}});
    }
  }
  CallDueAtTheOccurrence();
}

void VpiCallbacks::ActionOccurred(
    SimulatorAction action, const Occurrence& occurrence) {
  // A callback for an action that happens again from inside a call for that
  // action, however deep, is not called again: an error in a routine a
  // callback for routine errors calls would otherwise call it without end,
  // directly or through a callback for run-time errors.
  if (std::ranges::contains(calling_actions_, action)) return;
  const auto listening = action_listeners_.find(action);
  if (listening == action_listeners_.end()) return;
  // A copy, since a call may remove a callback from the list.
  const std::vector<CallbackObject> for_action = listening->second;
  calling_actions_.push_back(action);
  CallEach(for_action, occurrence);
  calling_actions_.pop_back();
}

void VpiCallbacks::Signalled(int signal) {
  const auto listening = signal_listeners_.find(signal);
  if (listening == signal_listeners_.end()) return;
  // A copy, since a call may remove a callback from the list.
  const std::vector<CallbackObject> trapping = listening->second;
  CallEach(trapping, std::monostate{});
}

void VpiCallbacks::ErrorReported(
    base::SourceLocation at, const base::DiagMessage& message) {
  ActionOccurred(
      SimulatorAction::kRunTimeError,
      VpiErrorReport{
          .message = std::string(message.Text()),
          .routine = std::string(),
          .level = vpiError,
          .file = at.IsValid() ? std::string(at.File()) : std::string(),
          .line = at.IsValid() ? static_cast<PLI_INT32>(at.Line()) : 0});
}

auto CallbacksRelatedTo(RuntimeEffects& run, const Referent& from)
    -> VpiAnswer<std::vector<VpiObject>> {
  VpiCallbacks* callbacks = AsRuntime(run).CallbacksIfMade();
  return std::visit(
      Overloaded{
          [&](const Simulation&) -> VpiAnswer<std::vector<VpiObject>> {
            if (callbacks == nullptr) return std::vector<VpiObject>{};
            return callbacks->RelatedToNothing();
          },
          [&](const VpiObject& object) -> VpiAnswer<std::vector<VpiObject>> {
            if (!IsOnTheDiagram(object)) {
              return Refused(
                  "callbacks are related to an expression, a statement or a "
                  "time queue");
            }
            if (callbacks == nullptr) return std::vector<VpiObject>{};
            return callbacks->RelatedTo(object);
          },
          [](const VpiIteration*) -> VpiAnswer<std::vector<VpiObject>> {
            return Refused("an iterator is related to no callbacks");
          }},
      from);
}

}  // namespace lyra::runtime

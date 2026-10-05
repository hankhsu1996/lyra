#include "lyra/runtime/var.hpp"

#include <iterator>
#include <optional>
#include <span>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

namespace {

// Waiting for one of an event control's leaves to report an occurrence. None of
// what a leaf reports is a level: what happens while the procedure is not
// waiting here is missed, so waiting again waits for the next occurrence and
// compares against what it finds now rather than against what it left, which is
// what LRM 9.7 asks of an event expression when a stopped process is started
// again.
class EventControlWait : public Wait {
 public:
  explicit EventControlWait(std::span<const Trigger> triggers)
      : triggers_(triggers.begin(), triggers.end()) {
  }

  explicit EventControlWait(std::span<const Trigger* const> triggers) {
    triggers_.reserve(triggers.size());
    for (const Trigger* trigger : triggers) {
      triggers_.push_back(*trigger);
    }
  }

  // NOLINTNEXTLINE(readability-named-parameter)
  auto Begin(RuntimeEffects&, CoroutineHandle leaf) -> WaitOutcome override {
    SubscribeToLeaves(leaf, triggers_);
    return WaitOutcome::kBlocked;
  }

  auto Again(RuntimeEffects& services, CoroutineHandle leaf)
      -> WaitOutcome override {
    // The triggers were armed against the values as they stood when the wait
    // was first made, and those are not the values a change is now measured
    // from: whatever moved while the process was stopped is not an occurrence
    // this wait may report.
    for (const Trigger& trigger : triggers_) {
      if (ArmedObservation* observation = trigger.observation.Get()) {
        observation->Arm();
      }
    }
    return Begin(services, leaf);
  }

  // The construct behind this wait is an event control, a `wait` condition, or
  // an always_comb / always_latch sensitivity list -- each of which
  // LRM 12.4.2.1 names as a violation report flush point when it resumes the
  // process.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return true;
  }

 private:
  std::vector<Trigger> triggers_;
};

// Waiting for a condition the body itself tests (LRM 9.4.3). It watches the
// same leaves, because a change to what the condition reads is the only thing
// that can make it true -- but what it waits for is a state rather than an
// occurrence, and the only thing able to read that state is the body. So
// starting again after the process was stopped answers satisfied and lets the
// body's own loop decide, which is exactly the evaluation LRM 9.7 calls for:
// "resensitize the process ... to wait for the wait condition to become true.
// If the wait condition is now true ... the process is scheduled into the
// Active or Reactive region to continue its execution in the current time
// step."
class LevelConditionWait : public EventControlWait {
 public:
  using EventControlWait::EventControlWait;

  // NOLINTNEXTLINE(readability-named-parameter)
  auto Again(RuntimeEffects&, CoroutineHandle) -> WaitOutcome override {
    return WaitOutcome::kSatisfied;
  }
};

// Waiting for an event control its process decides: every place the last
// evaluation reached wakes it, and the process evaluates again to learn whether
// that was an event and what the expression reaches now (LRM 4.5, 9.4.2).
// Starting again after the process was stopped measures from what the
// expression is worth then, not from what it was worth before (LRM 9.7); that
// is an evaluation, so it is left to the process, which arms the observations
// on its next one.
class RecollectingEventWait : public EventControlWait {
 public:
  RecollectingEventWait(
      std::span<const Trigger> triggers,
      std::span<const Observation* const> observations)
      : EventControlWait(triggers) {
    observations_.reserve(observations.size());
    for (const Observation* observation : observations) {
      observations_.push_back(*observation);
    }
  }

  // NOLINTNEXTLINE(readability-named-parameter)
  auto Again(RuntimeEffects&, CoroutineHandle) -> WaitOutcome override {
    for (const Observation& observation : observations_) {
      if (ArmedObservation* held = observation.Get()) {
        held->Disarm();
      }
    }
    return WaitOutcome::kSatisfied;
  }

 private:
  std::vector<Observation> observations_;
};

}  // namespace

auto WriteBits(
    value::PackedArrayRef& bits, const value::PackedArray& value, bool watched)
    -> std::optional<Change> {
  const std::optional<value::BitPositions> reached =
      watched ? bits.Reached() : std::nullopt;
  if (!reached.has_value()) {
    bits = value;
    return std::nullopt;
  }
  KeptPart<value::PackedArray> kept(bits.Root(), *reached);
  bits = value;
  return kept.ChangeTo(bits.Root());
}

KeptPart<value::PackedArray>::KeptPart(const value::PackedArray& part)
    : KeptPart(part, {.lsb = 0, .width = part.BitWidth()}) {
}

KeptPart<value::PackedArray>::KeptPart(
    const value::PackedArray& storage, value::BitPositions reached)
    : reached_(Change::Reaching(storage, reached)) {
}

KeptPart<value::PackedArray>::~KeptPart() = default;

auto KeptPart<value::PackedArray>::ChangeTo(const value::PackedArray& part)
    -> std::optional<Change> {
  reached_.SetAfter(part);
  if (reached_.Unmoved()) {
    return std::nullopt;
  }
  return reached_;
}

RareWriteState::~RareWriteState() = default;

VariableCell::VariableCell() = default;
VariableCell::~VariableCell() = default;

void ErasedReference::Report(const Change& change) const {
  std::visit(
      Overloaded{
          [](std::monostate) {},
          [&](VariableCell* variable) {
            current_runtime().WakeWaitersOf(*variable, change);
          },
          [](GcObject* object) { object->PublishChange(); }},
      holder);
}

auto ErasedReference::ReportsTo() const -> Observable* {
  return std::visit(
      Overloaded{
          [](std::monostate) -> Observable* { return nullptr; },
          [](VariableCell* variable) -> Observable* { return variable; },
          [](GcObject* object) -> Observable* {
            return &object->EventSource();
          }},
      holder);
}

void ErasedReference::AdmitStep() const {
  if (!Admits()) {
    throw SimulationError(
        "lending part of a variable a procedural continuous assignment holds "
        "is not yet supported");
  }
}

auto ErasedReference::Part(void* part, value::Formation formed) const
    -> ErasedReference {
  switch (formed) {
    case value::Formation::kExisting:
      return {.holder = holder, .storage = part};
    case value::Formation::kMade:
      if (Watched()) {
        Report(Change::Whole());
      }
      return {.holder = holder, .storage = part};
    case value::Formation::kNowhere:
      return {.holder = std::monostate{}, .storage = part};
  }
  throw InternalError("ErasedReference::Part: unknown formation");
}

void SubscribeToLeaves(
    CoroutineHandle frame, std::span<const Trigger> triggers) {
  for (const Trigger& trigger : triggers) {
    // Storage that belongs to nothing is never told of a write, so a wait on
    // it has nothing to register on and waits on its other leaves.
    if (trigger.observable == nullptr) {
      continue;
    }
    trigger.observable->Subscribe(frame, trigger.observation, trigger.reads);
  }
}

auto WaitAny(RuntimeEffects& services, std::span<const Trigger> triggers)
    -> bool {
  return services.CurrentProcess().ParkOn<EventControlWait>(services, triggers);
}

auto WaitAny(RuntimeEffects& services, std::span<const Trigger* const> triggers)
    -> bool {
  return services.CurrentProcess().ParkOn<EventControlWait>(services, triggers);
}

namespace {

// Every leaf the reports collected, one wait's worth, leaving each report empty
// for the evaluation after.
auto CollectedLeaves(std::span<ReadReport* const> reports)
    -> std::vector<Trigger> {
  std::vector<Trigger> leaves;
  for (ReadReport* report : reports) {
    std::vector<Trigger> reported = report->TakeTriggers();
    leaves.insert(
        leaves.end(), std::make_move_iterator(reported.begin()),
        std::make_move_iterator(reported.end()));
  }
  return leaves;
}

}  // namespace

auto WaitRecollecting(
    RuntimeEffects& services, std::span<ReadReport* const> reports,
    std::span<const Observation* const> observations) -> bool {
  const std::vector<Trigger> leaves = CollectedLeaves(reports);
  return services.CurrentProcess().ParkOn<RecollectingEventWait>(
      services, std::span<const Trigger>{leaves}, observations);
}

auto WaitAny(RuntimeEffects& services, const ReadReport* report) -> bool {
  return services.CurrentProcess().ParkOn<EventControlWait>(
      services, report->ImplicitList());
}

auto WaitUntil(RuntimeEffects& services, std::span<ReadReport* const> reports)
    -> bool {
  const std::vector<Trigger> leaves = CollectedLeaves(reports);
  return services.CurrentProcess().ParkOn<LevelConditionWait>(
      services, std::span<const Trigger>{leaves});
}

}  // namespace lyra::runtime

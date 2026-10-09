#include "lyra/runtime/value_change_wait.hpp"

#include <iterator>
#include <span>
#include <vector>

#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

namespace {

// A wait on storage that stays the same for as long as the body holding it
// runs: an event control a change decides (LRM 9.4.2), or an implicit list
// (LRM 9.2.2.2.1, 9.4.2.2). It enrols on each place once, where it is built,
// and a stop says only that an activation is parked here now.
class StandingAwaiter final : public Awaiter {
 public:
  explicit StandingAwaiter(std::span<const Trigger> triggers) {
    for (const Trigger& trigger : triggers) {
      Watch(trigger);
    }
  }

  explicit StandingAwaiter(std::span<const Trigger* const> triggers) {
    for (const Trigger* trigger : triggers) {
      Watch(*trigger);
    }
  }

  // What a change is measured from is what the expression was worth where the
  // wait begins (LRM 9.4.2). Each occurrence that reaches the wait while one is
  // parked moves that baseline as it is asked, so it is already current unless
  // something reached the wait while none was.
  auto Begin() -> Resumption override {
    if (reached_while_unparked_) {
      Arm();
    }
    return OnAnOccurrence{};
  }

  // Whatever moved while the process was stopped is not an occurrence this wait
  // may report (LRM 9.7), so each observation measures from what its
  // expression is worth now.
  auto Again() -> Resumption override {
    Arm();
    return OnAnOccurrence{};
  }

  // An event control and an implicit list are each a violation report flush
  // point when they resume their process (LRM 12.4.2.1).
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return true;
  }

 private:
  void Watch(const Trigger& trigger) {
    EnrolOnLeaf(trigger);
    observations_.push_back(trigger.observation);
  }

  void ReachedWhileUnparked() override {
    reached_while_unparked_ = true;
  }

  // Each observation measures from what its expression is worth now.
  void Arm() {
    for (const Observation& observation : observations_) {
      observation.Arm();
    }
    reached_while_unparked_ = false;
  }

  std::vector<Observation> observations_;
  // Nothing has measured a baseline before the first stop, which is the same
  // as something having moved since the last one.
  bool reached_while_unparked_ = true;
};

// Waiting for one of the places an evaluation the process made reached to
// report an occurrence. What it watches is known only once the process has
// evaluated, so it is built at the stop, on what that evaluation reached; each
// construct built on it says for itself what starting a stopped process again
// asks (LRM 9.7).
class ReachedPlacesAwaiter : public Awaiter {
 public:
  explicit ReachedPlacesAwaiter(std::span<const Trigger> triggers) {
    for (const Trigger& trigger : triggers) {
      EnrolOnLeaf(trigger);
    }
  }

  auto Begin() -> Resumption override {
    return OnAnOccurrence{};
  }

  // The construct behind this wait is an event control its process decides or
  // a `wait` condition, each of which LRM 12.4.2.1 names as a violation report
  // flush point when it resumes the process.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return true;
  }
};

// Waiting for a condition the body itself tests (LRM 9.4.3). It watches the
// same leaves, because a change to what the condition reads is the only thing
// that can make it true -- but what it waits for is a state rather than an
// occurrence, and the only thing able to read that state is the body. So
// starting again after the process was stopped answers that it carries on and
// lets the body's own loop decide, which is exactly the evaluation LRM 9.7
// calls for: "resensitize the process ... to wait for the wait condition to
// become true. If the wait condition is now true ... the process is scheduled
// into the Active or Reactive region to continue its execution in the current
// time step."
class LevelConditionAwaiter final : public ReachedPlacesAwaiter {
 public:
  using ReachedPlacesAwaiter::ReachedPlacesAwaiter;

  auto Again() -> Resumption override {
    return WithoutStopping{};
  }
};

// Waiting for an event control its process decides: every place the last
// evaluation reached wakes it, and the process evaluates again to learn whether
// that was an event and what the expression reaches now (LRM 4.5, 9.4.2).
// Starting again after the process was stopped measures from what the
// expression is worth then, not from what it was worth before (LRM 9.7); that
// is an evaluation, so it is left to the process, which arms the observations
// on its next one.
class RecollectingEventAwaiter final : public ReachedPlacesAwaiter {
 public:
  RecollectingEventAwaiter(
      std::span<const Trigger> triggers,
      std::span<const Observation* const> observations)
      : ReachedPlacesAwaiter(triggers) {
    observations_.reserve(observations.size());
    for (const Observation* observation : observations) {
      observations_.push_back(*observation);
    }
  }

  auto Again() -> Resumption override {
    for (const Observation& observation : observations_) {
      observation.Disarm();
    }
    return WithoutStopping{};
  }

 private:
  std::vector<Observation> observations_;
};

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

auto WaitOn(std::span<const Trigger> triggers) -> Wait {
  return MakeWait<StandingAwaiter>(triggers);
}

auto WaitOn(std::span<const Trigger* const> triggers) -> Wait {
  return MakeWait<StandingAwaiter>(triggers);
}

auto WaitOnImplicitList(const ReadReport* report) -> Wait {
  return WaitOn(report->ImplicitList());
}

auto WaitRecollecting(
    std::span<ReadReport* const> reports,
    std::span<const Observation* const> observations) -> Wait {
  const std::vector<Trigger> leaves = CollectedLeaves(reports);
  return MakeWait<RecollectingEventAwaiter>(
      std::span<const Trigger>{leaves}, observations);
}

auto WaitUntil(std::span<ReadReport* const> reports) -> Wait {
  const std::vector<Trigger> leaves = CollectedLeaves(reports);
  return MakeWait<LevelConditionAwaiter>(std::span<const Trigger>{leaves});
}

}  // namespace lyra::runtime

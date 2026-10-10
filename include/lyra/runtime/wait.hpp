#pragma once

#include <deque>
#include <memory>
#include <utility>
#include <variant>

#include "lyra/base/time.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

struct Activation;
class Awaiter;
struct ErasedReference;
class RuntimeEffects;

// What an execution stopping at a wait carries on at, as the awaiter answers it
// and the scheduler arranges it. Each construct knows which of these its wait
// is; only the scheduler knows the time it is now and how to queue an
// activation, so the awaiter states one and the scheduler does the rest.

// What it waits for has already happened, so it carries on without stopping.
struct WithoutStopping {};

// When an occurrence reaches one of the awaiter's memberships and is an event
// for it: a value change, a trigger, a join, a termination.
struct OnAnOccurrence {};

// Later in the time step it stopped in, in `region`: a `#0` delay resumes in
// the Inactive region (LRM 4.4.2.3), a nonblocking assignment's carrier in the
// NBA region (LRM 4.4.2.4).
struct LaterInThisTimeStep {
  Region region;
};

// At the time step `when`, in the Active region (LRM 4.4.2.2); a time already
// reached is now.
struct AtTime {
  SimTime when;
};

using Resumption =
    std::variant<WithoutStopping, OnAnOccurrence, LaterInThisTimeStep, AtTime>;

// One place an awaiter watches: an observable, a named event, a join, a `wait
// fork` condition, a process's termination. It stands for as long as the
// awaiter does, so what the awaiter watches is enrolled on once however often
// the execution stops there.
struct WaitMembership : IntrusiveListNode<WaitMembership> {
  // Null on the sentinel a list embeds to close its ring.
  Awaiter* awaiter = nullptr;

  // The member of a scope the place was reached through, where it was reached
  // through a reference bound into one (LRM 23.3.3). A force on that member
  // makes it name other storage (LRM 10.6.2), and what was enrolled through it
  // is then enrolled there.
  const ErasedReference* through = nullptr;

  // Which bits of the place the wait reads. It bounds what a change there could
  // do to the wait, so a change confined outside it needs no further question
  // asked; a width of zero is "the whole of it", which bounds nothing.
  value::BitPositions reads;

  // What decides whether reaching this membership is an event for the wait
  // (LRM 9.4.2): reaching it is a candidacy, and this decides. It holds nothing
  // where being reached is the whole of it -- an implicit sensitivity, an
  // unqualified event, a join. It is held rather than pointed at, so it lives
  // as long as anything watching for that wait does.
  Observation observation;
};

// How one kind of timing control waits: what it watches, and what it answers
// when the execution stops at it and when a stopped execution is started again
// (LRM 9.7). It is the awaiter of the execution's `co_await`, in the C++ sense:
// the object the suspension asks how to suspend and how to be woken.
//
// It owns its memberships, which stand for its whole life, and names the
// activation parked on it only while one is. Reaching a membership asks it
// whether that activation, if any, is woken -- so a change while the execution
// runs, or while it is stopped from outside, wakes nothing, and nothing has to
// be withdrawn for that to hold.
//
// The scheduler reaches every kind through this surface without asking which
// construct it came from.
class Awaiter {
 public:
  Awaiter() = default;
  Awaiter(const Awaiter&) = delete;
  auto operator=(const Awaiter&) -> Awaiter& = delete;
  // Non-movable: each place it enrols on points at its memberships.
  Awaiter(Awaiter&&) = delete;
  auto operator=(Awaiter&&) -> Awaiter& = delete;
  // Defined in this class's own source file, because a class whose virtual
  // functions are all written in a header is emitted into every translation
  // unit that builds one.
  virtual ~Awaiter();

  // What the execution stopping here carries on at.
  virtual auto Begin() -> Resumption = 0;

  // The same, after the execution was stopped from outside and started again
  // (LRM 9.7). The standard gives each construct its own rule there, and for
  // most of them it is the same answer asked afresh: a delay still resumes at
  // the deadline it holds, and a join is asked whether its branches are done.
  virtual auto Again() -> Resumption {
    return Begin();
  }

  // Whether a process resuming from this wait reaches a violation report flush
  // point (LRM 12.4.2.1), discarding the reports it still has pending. The LRM
  // names two: resuming from an event control or a wait statement, and an
  // always_comb / always_latch resumed by a transition on what it reads. Time
  // passing is not one of them, so each construct answers for its own
  // suspension rather than the resume path guessing from the queue it came off.
  [[nodiscard]] virtual auto IsReportFlushPoint() const -> bool = 0;

  // The activation an occurrence reaching `membership` wakes: the one parked
  // here, where one is and the occurrence is an event for it. Waking it is
  // what ends its being parked. Nothing parked means nothing to ask, so a
  // change while the activation runs -- its own writes among them -- or while
  // it is stopped is no occurrence for it.
  [[nodiscard]] auto WokenBy(const WaitMembership& membership) -> Activation* {
    if (parked_ == nullptr) {
      ReachedWhileUnparked();
      return nullptr;
    }
    if (!membership.observation.Fires() || !Holds()) {
      return nullptr;
    }
    return parked_;
  }

 protected:
  // Enrols on the place `trigger` watches, for this awaiter's whole life.
  void EnrolOnLeaf(const Trigger& trigger);

  // Enrols on `target`, which fires unconditionally, for this awaiter's whole
  // life.
  void EnrolOn(IntrusiveList<WaitMembership>& target);

 private:
  // An activation is parked here by naming this awaiter, and the two are kept
  // in step in one place, the activation's own parking and withdrawing.
  friend struct Activation;

  // Makes one membership, whole, and links it on `target`: which bits of the
  // place it reads, a width of zero being all of it, and what decides whether
  // reaching it is an event, which holds nothing where being reached is.
  void Enrol(
      IntrusiveList<WaitMembership>& target, const ErasedReference* through,
      value::BitPositions reads, Observation observation);

  // Whether what this waits for holds now, asked once an occurrence that is an
  // event for it reached it while an activation is parked. A wait for a state
  // rather than for an occurrence -- branches having terminated, a process
  // having no live child -- is reached by each change on the way there, and
  // only the last of them is the one it waits for.
  [[nodiscard]] virtual auto Holds() const -> bool {
    return true;
  }

  // An occurrence reached a membership while nothing was parked here. Most
  // waits ask afresh at every stop and have nothing to keep of it.
  virtual void ReachedWhileUnparked() {
  }

  std::deque<WaitMembership> memberships_;
  Activation* parked_ = nullptr;
};

// A wait for a state rather than for an occurrence: branches having
// terminated, a process having no live child, another process having
// terminated. Each such state, once reached, stays reached, so the execution
// carries on without stopping where it already holds, waits for the change
// that makes it hold otherwise, and asks the same afresh when it is started
// again after being stopped (LRM 9.7).
class StateAwaiter : public Awaiter {
 public:
  auto Begin() -> Resumption final;

 private:
  [[nodiscard]] auto Holds() const -> bool override = 0;
};

// What a body holds in its frame for one timing control: the awaiter its
// construct needs, at an address that does not move however the frame moves
// the holder. It is built before the execution first stops at it and ends
// with the scope that holds it, and ending it ends every membership the
// awaiter made, on every way out of that scope.
class Wait {
 public:
  explicit Wait(std::unique_ptr<Awaiter> awaiter);

  Wait(const Wait&) = delete;
  auto operator=(const Wait&) -> Wait& = delete;
  Wait(Wait&&) noexcept;
  auto operator=(Wait&&) noexcept -> Wait&;
  ~Wait();

  [[nodiscard]] auto Awaited() -> Awaiter& {
    return *awaiter_;
  }

 private:
  std::unique_ptr<Awaiter> awaiter_;
};

// The wait whose awaiter is a `Kind` built from `arguments`.
template <class Kind, class... Arguments>
[[nodiscard]] auto MakeWait(Arguments&&... arguments) -> Wait {
  return Wait{std::make_unique<Kind>(std::forward<Arguments>(arguments)...)};
}

// Stops the frame carrying the running process's thread at `wait`, answering
// whether the caller must give up control. This is the one way a body stops
// to wait.
auto ParkAt(RuntimeEffects& services, Wait* wait) -> bool;

// The dual of parking: `activation` is runnable now, so it is parked on no
// awaiter and sits in no queue until its body stops again, and nothing it was
// waiting on can wake it a second time. A wait the LRM counts as a violation
// report flush point clears its process's report queue on the way out (LRM
// 12.4.2.1); the activation cannot run between here and its resume, so
// discarding at either point is the same discard.
void ConsumeWait(Activation* activation);

}  // namespace lyra::runtime

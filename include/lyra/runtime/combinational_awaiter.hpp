#pragma once

#include <span>

#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

class CombinationalAwaiter;
class RuntimeEffects;

// A wait's place in the run's list of the waits the event's execution under
// way has to ask once it ends: those its writes reached while a process was
// parked on them, and those that began watching in the middle of it.
struct ReachedWait : IntrusiveListNode<ReachedWait> {
  // Null on the sentinel a list embeds to close its ring.
  CombinationalAwaiter* awaiter = nullptr;
};

// The wait of an `always_comb` or `always_latch` on its implicit list (LRM
// 9.2.2.2.1), which models combinational logic and is triggered when what it
// reads changes (LRM 9.2.2.2, 9.2.2.3). A process writing a default and then a
// value changes what it writes twice in one run, and LRM 4.7 leaves open
// whether a procedure reading it sees the default: in a schedule interleaving
// the two, neither is waiting while the other writes. Lyra runs a process to
// its stop as the execution of one event, and judges what reaches this wait by
// what each event's execution -- a process up to its stop, one deferred update,
// one timer firing -- leaves (LRM 4.5): each part a write landed on is
// compared, once the execution ends, with what it held when the execution
// began, and only a part left changed wakes the process. Two procedures each
// reading the other's defaulted output then settle, as they do in the
// interleaved schedule. A wait begun during an event's execution has nothing
// from the execution's start to be measured by, so a write reaching it after
// that wakes it. A change while the process runs, or while it is stopped, is
// no event for it (LRM 9.4.2).
class CombinationalAwaiter final : public Awaiter {
 public:
  explicit CombinationalAwaiter(std::span<const Trigger> triggers);

  // What reached it before the process stopped here came before the wait
  // began, so it is no event for it.
  auto Begin() -> Resumption override;

  // A process stopped from outside after a write reached it, while it was
  // parked here, was runnable under the per-write reading, so started again it
  // runs; otherwise it waits afresh (LRM 9.7), which may be in the middle of
  // another process's execution.
  auto Again() -> Resumption override;

  // Resuming from it is a violation report flush point (LRM 12.4.2.1).
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return true;
  }

  // A write of the event's execution under way reached `membership`. `judged`
  // says whether what the place kept holds the part written as it was before
  // the execution, which decides it once the execution ends; where it does
  // not, being reached decides it.
  void Reached(
      RuntimeEffects& effects, const WaitMembership& membership, bool judged);

  // What the event's execution under way left changed of a part this wait
  // reads, as measured from before the execution, which wakes the process
  // where a write of the execution reached it while it was parked here.
  void LeftChanged(RuntimeEffects& effects);

  // A membership of this wait has been moved to another place while an event
  // is executed, so it watches that place from the middle of the execution on.
  void Relocated(RuntimeEffects& effects);

  // The event's execution the run asked this wait about has ended, and nothing
  // it left has woken the process: what it wrote that the place kept was left
  // as it was. The run has taken it off its list.
  void EventExecuted(RuntimeEffects& effects);

 private:
  // Begins watching in the middle of the event's execution under way, so
  // being reached decides it until the execution ends, and has the run ask it
  // then.
  void BeginWatching(RuntimeEffects& effects);
  // Puts it on the run's list of waits to ask once the execution ends.
  void AskOnceTheEventIsExecuted(RuntimeEffects& effects);

  // Wakes the process parked here, and forgets what the execution told it.
  void Wake(RuntimeEffects& effects);
  void Forget();

  // Its place in the run's list, linked exactly while the event's execution
  // under way has something to ask it once it ends.
  ReachedWait reached_;
  // A membership a write of the execution reached with a process parked here,
  // which waking the process is asked through.
  const WaitMembership* reached_through_ = nullptr;
  // Whether it began watching during the event's execution under way, so that
  // what the execution kept from its start says nothing of it.
  bool begun_during_the_execution_ = false;
  // Whether a write reached it whose place keeps nothing to judge by, or since
  // it began watching during the execution, which being reached then decides.
  bool unjudged_ = false;
  // Whether a process stopped from outside after such a write is owed its run.
  bool owed_ = false;
};

}  // namespace lyra::runtime

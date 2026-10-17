#pragma once

#include <memory>

#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

class RuntimeEffects;
class RuntimeProcess;

// A process's place among the executions its owner keeps, holding it there: a
// process is released by unlinking the place and dropping the hold.
struct OwnedPlace : IntrusiveListNode<OwnedPlace> {
  std::shared_ptr<RuntimeProcess> held;
};

// The executions one owner started and still keeps: a process's children (LRM
// 9.5), a program's initial procedures (LRM 24.3), or, for the runtime, every
// other execution no process started. One that has ended is kept while
// anything it started is still live, because ending a process reaches the
// descendants of a child that already terminated (LRM 9.6.3, 9.7). Past that
// nothing here keeps it, and only a handle the design holds can (LRM 9.7).
class OwnedProcesses {
 public:
  // `parent` is the process these were started by, null where no process
  // started them.
  explicit OwnedProcesses(RuntimeProcess* parent) : parent_(parent) {
  }
  OwnedProcesses(const OwnedProcesses&) = delete;
  auto operator=(const OwnedProcesses&) -> OwnedProcesses& = delete;
  // Non-movable: each kept process points back at the set that keeps it.
  OwnedProcesses(OwnedProcesses&&) = delete;
  auto operator=(OwnedProcesses&&) -> OwnedProcesses& = delete;
  // Releases every member, which destroys the ones nothing else holds and,
  // through their own sets, what they started.
  ~OwnedProcesses();

  [[nodiscard]] auto Parent() const -> RuntimeProcess* {
    return parent_;
  }

  void Adopt(std::shared_ptr<RuntimeProcess> process);

  // `process`, one of these, has terminated. Whatever waits for all of them to
  // have ended is asked, since this may have been the last (LRM 9.6.1, 24.3).
  void Ended(RuntimeProcess& process, RuntimeEffects& effects);

  [[nodiscard]] auto AllEnded() const -> bool {
    return live_.Empty();
  }

  [[nodiscard]] auto Empty() const -> bool {
    return live_.Empty() && ended_.Empty();
  }

  // What a `wait fork` of the owner enrols on (LRM 9.6.1), and what a program
  // observes its initial procedures by (LRM 24.3).
  [[nodiscard]] auto AllEndedCondition() -> IntrusiveList<WaitMembership>& {
    return all_ended_;
  }

  // Ends every execution kept here and everything they started (LRM 9.6.3,
  // 9.7, 24.3), except the chain down to `running`, whose frame is executing:
  // that is asked to end, and settles once it has left. Answers whether
  // `running` was under one of them.
  auto EndAll(RuntimeEffects& effects, RuntimeProcess* running) -> bool;

  // Visits each kept process, the ended ones first, so one `fn` ends is not
  // visited again. `fn` may end or release the one it is handed, but no other.
  template <class Fn>
  void ForEach(Fn fn) {
    ended_.ForEach([&fn](OwnedPlace& place) { fn(*place.held); });
    live_.ForEach([&fn](OwnedPlace& place) { fn(*place.held); });
  }

 private:
  RuntimeProcess* parent_;
  IntrusiveList<OwnedPlace> live_;
  IntrusiveList<OwnedPlace> ended_;
  IntrusiveList<WaitMembership> all_ended_;
};

}  // namespace lyra::runtime

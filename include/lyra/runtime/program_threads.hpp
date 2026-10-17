#pragma once

#include <memory>

#include "lyra/runtime/owned_processes.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

class RuntimeEffects;
class RuntimeProcess;

// The threads of one program instance (LRM 24.3): its initial procedures, each
// a root it keeps, and through them every thread they start. When the last root
// has ended, the program ends whatever they started that is still running and
// tells the runtime, which ends the run once that holds of every program; a
// `$exit` from any of its threads ends all of them at once (LRM 24.7).
class ProgramThreads {
 public:
  ProgramThreads();
  ProgramThreads(const ProgramThreads&) = delete;
  auto operator=(const ProgramThreads&) -> ProgramThreads& = delete;
  // Non-movable: each root points back at the set this holds.
  ProgramThreads(ProgramThreads&&) = delete;
  auto operator=(ProgramThreads&&) -> ProgramThreads& = delete;
  ~ProgramThreads() = default;

  // Keeps `root`, an initial procedure of this program.
  void AdoptRoot(std::shared_ptr<RuntimeProcess> root);

  [[nodiscard]] auto AllRootsEnded() const -> bool {
    return roots_.AllEnded();
  }

  // A root has ended, as the observer of their ending is told; the last one
  // ends the program.
  void RootEnded(RuntimeEffects& effects);

  // `$exit` from one of this program's threads, which is the running one.
  // Leaves that thread, so it does not return.
  [[noreturn]] void Exit(RuntimeEffects& effects);

 private:
  // Ends every thread of the program, the running one excepted where it is
  // one; answers whether it was, in which case it has been asked to end and
  // has yet to leave.
  auto EndEveryThread(RuntimeEffects& effects) -> bool;

  OwnedProcesses roots_{nullptr};
  // Enrolled on the roots' ending, which tells it as each one ends.
  WaitMembership roots_ending_;
  // Set once the program has ended its threads, after which a root ending is
  // one of those endings and ends nothing more.
  bool ended_ = false;
};

}  // namespace lyra::runtime

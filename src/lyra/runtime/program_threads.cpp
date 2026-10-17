#include "lyra/runtime/program_threads.hpp"

#include <memory>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

ProgramThreads::ProgramThreads() {
  roots_ending_.observer = this;
  roots_.AllEndedCondition().PushBack(roots_ending_);
}

void ProgramThreads::AdoptRoot(std::shared_ptr<RuntimeProcess> root) {
  roots_.Adopt(std::move(root));
}

void ProgramThreads::RootEnded(RuntimeEffects& effects) {
  if (!roots_.AllEnded()) {
    return;
  }
  // A root ending as the program ends its threads is one of the endings that
  // call is already making.
  if (!ended_) {
    EndEveryThread(effects);
  }
  AsRuntime(effects).ProgramEnded();
}

void ProgramThreads::Exit(RuntimeEffects& effects) {
  if (!EndEveryThread(effects)) {
    throw InternalError(
        "ProgramThreads::Exit: the thread calling $exit is not among its "
        "program's -- please report this as a bug");
  }
  RaiseUnclaimableEffect();
}

auto ProgramThreads::EndEveryThread(RuntimeEffects& effects) -> bool {
  ended_ = true;
  return roots_.EndAll(effects, effects.TryCurrentProcess());
}

}  // namespace lyra::runtime

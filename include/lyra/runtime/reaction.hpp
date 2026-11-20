#pragma once

#include <array>
#include <cstddef>
#include <expected>
#include <memory>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/abi/records.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// A static process whose body runs to its end each time it is triggered, as
// the run holds it (LRM 4.9.1, Annex P): no frame and no thread of its own, but
// the object its parts run on, the wait its setup builds, what it runs in, and
// the generator it was seeded with as it was created (LRM 18.14.1). The engine
// runs it as it runs any activation; its first run calls the setup and each
// run after it a pass, which is a plain call returning before the run does.
// While one runs, the reaction is what runs.
//
// What the language may ask of it as a process -- `process::self()`, the
// parent of a process it forks, the pass its deferred reports belong to,
// process control (LRM 9.7, 9.5, 12.4.2.1) -- is answered by its process, which
// is made the first time it is asked for. Making it creates nothing the
// language can see: it only gives an existing process its object, which from
// then on holds its generator, and which process control acts on through it.
class Reaction final : public Activation, public ProcessExecution {
 public:
  Reaction(
      const abi::ReactionDefinition& definition, void* object,
      const ExecutionContext& context, DrawRng generator);
  Reaction(const Reaction&) = delete;
  auto operator=(const Reaction&) -> Reaction& = delete;
  Reaction(Reaction&&) = delete;
  auto operator=(Reaction&&) -> Reaction& = delete;
  ~Reaction() override;

  [[nodiscard]] auto Context() const noexcept
      -> const ExecutionContext& override;
  [[nodiscard]] auto ProcessIfMade() -> RuntimeProcess* override;

  // Asked only while a part of it runs, which holds what is in force for that
  // run.
  [[nodiscard]] auto State() -> RunningState& override;
  [[nodiscard]] auto Leaf() -> Activation* override;
  // Its process, made now where it has none, and kept by the run.
  [[nodiscard]] auto Process() -> RuntimeProcess* override;
  void SetCombinationalUpdates(support::CombinationalUpdates updates) override {
    context_.combinational_updates = updates;
  }

  // Its process, made now and handed to whoever is to keep it: an initial
  // procedure of a program is one of the program's roots (LRM 24.3).
  [[nodiscard]] auto MakeProcess() -> std::shared_ptr<RuntimeProcess>;

  // Nothing of it runs or is resumed again, and the wait it held ends.
  void ReleaseExecution() override;
  // An imported task's call is the foreign call that may stop, and only a body
  // that can stop makes one.
  [[nodiscard]] auto HasLiveForeignCall() const -> bool override;
  // Nothing of a pass outlives it, so only a run holds any.
  [[nodiscard]] auto HeldState() const -> const RunningState* override;
  [[nodiscard]] auto Activity() const -> ProcessExecutionState override;

 private:
  // Its process, as far as the language has asked for it: not yet, the
  // reaction holding the generator it was seeded with; made, the process
  // holding it from then on; or ended with the reaction, after which nothing
  // asks.
  struct NotAskedFor {
    DrawRng seeded;
  };
  struct Made {
    RuntimeProcess* process;
  };
  struct Ended {};

  // One run of it as it goes on: what is in force while its parts run, and its
  // process where the run made it, held to the run's end as the process it
  // began with is, since the run's own ending may drop every other hold on it.
  struct CurrentRun {
    explicit CurrentRun(DrawRng& generator)
        : state(generator, nullptr, nullptr) {
    }

    RunningState state;
    std::shared_ptr<RuntimeProcess> made_process;
  };

  // Calls the part due now, and the passes it calls for, to the stop the last
  // one parks at, as what runs.
  auto CarryOn(RuntimeEffects& effects) -> bool override;

  // The generator its randomization calls draw from, which its process's
  // `srandom()` seeds once made (LRM 18.13.3).
  [[nodiscard]] auto Generator() -> DrawRng&;

  // Calls the setup where it has not run, then passes until one parks,
  // answering whether the body returned -- it runs once, or returned before
  // its loop -- or what left it.
  auto CallParts() -> std::expected<bool, Unwound>;

  // The reaction ran its last statement: its process, where made, settles that
  // way (LRM 9.7), and nothing resumes it again.
  void End(RuntimeEffects& effects, bool killed);

  // Left from inside a part, as a body is left (LRM 9.6.2, 9.7): it ends there,
  // and an error that left it is reported once it has, under its procedure.
  void Land(RuntimeEffects& effects, const Unwound& unwound);

  const abi::ReactionDefinition* definition_;
  void* object_;
  Wait wait_;
  ExecutionContext context_;
  std::variant<NotAskedFor, Made, Ended> process_;
  // None between runs, since a part never stops.
  CurrentRun* current_run_ = nullptr;
  bool set_up_ = false;
};

// Every reaction of the run, each in storage that never moves, made a page at
// a time so that a design of millions of them makes no allocation for each.
// They live for the run, as static processes do.
class ReactionTable {
 public:
  template <typename... Args>
  auto Add(Args&&... args) -> Reaction& {
    if (pages_.empty() || used_ == kPerPage) {
      pages_.push_back(std::make_unique<Page>());
      used_ = 0;
    }
    return pages_.back()->at(used_++).emplace(std::forward<Args>(args)...);
  }

 private:
  static constexpr std::size_t kPerPage = 256;
  using Page = std::array<std::optional<Reaction>, kPerPage>;

  std::vector<std::unique_ptr<Page>> pages_;
  // How many reactions the last page holds.
  std::size_t used_ = 0;
};

}  // namespace lyra::runtime

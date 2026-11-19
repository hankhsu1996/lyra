#include "lyra/runtime/reaction.hpp"

#include <expected>
#include <memory>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/support/start_kind.hpp"

namespace lyra::runtime {

Reaction::Reaction(
    const abi::ReactionDefinition& definition, void* object,
    const ExecutionContext& context, DrawRng generator)
    : definition_(&definition),
      object_(object),
      context_(context),
      process_(NotAskedFor{.seeded = generator}) {
}

Reaction::~Reaction() = default;

auto Reaction::Run(RuntimeEffects& effects) -> bool {
  if (state_ != nullptr) {
    throw InternalError(
        "Reaction::Run: a reaction runs again while it runs -- please report "
        "this as a bug");
  }
  RunningState state(Generator(), nullptr, nullptr);
  state_ = &state;
  bool left = false;
  {
    const RunnerGuard running(effects, *this);
    const std::expected<bool, Unwound> returned = CallParts();
    if (!returned.has_value()) {
      Land(effects, returned.error());
      left = true;
    } else if (*returned) {
      End(effects, false);
    }
  }
  state_ = nullptr;
  return left;
}

auto Reaction::Context() const noexcept -> const ExecutionContext& {
  return context_;
}

auto Reaction::ProcessIfMade() -> RuntimeProcess* {
  const Made* made = std::get_if<Made>(&process_);
  return made != nullptr ? made->process : nullptr;
}

auto Reaction::State() -> RunningState& {
  if (state_ == nullptr) {
    throw InternalError(
        "Reaction::State: a reaction holds what is in force only while a part "
        "of it runs -- please report this as a bug");
  }
  return *state_;
}

auto Reaction::Leaf() -> Activation* {
  return this;
}

auto Reaction::Generator() -> DrawRng& {
  return std::visit(
      Overloaded{
          [](NotAskedFor& asked) -> DrawRng& { return asked.seeded; },
          [](const Made& made) -> DrawRng& {
            return made.process->Generator();
          },
          [](const Ended&) -> DrawRng& {
            throw InternalError(
                "Reaction::Generator: a reaction that ended draws nothing -- "
                "please report this as a bug");
          }},
      process_);
}

auto Reaction::MakeProcess() -> std::shared_ptr<RuntimeProcess> {
  auto* asked = std::get_if<NotAskedFor>(&process_);
  if (asked == nullptr) {
    throw InternalError(
        "Reaction::MakeProcess: a reaction's process is made once, and before "
        "it ends -- please report this as a bug");
  }
  std::shared_ptr<RuntimeProcess> made =
      RuntimeProcess::Create(*this, asked->seeded);
  process_ = Made{.process = made.get()};
  // From here on what it draws comes from the generator its process holds,
  // including the rest of a run that is going on now.
  if (state_ != nullptr) {
    state_->generator = &made->Generator();
  }
  return made;
}

auto Reaction::Process() -> RuntimeProcess* {
  if (std::holds_alternative<NotAskedFor>(process_)) {
    AsRuntime(current_runtime()).AdoptProcess(MakeProcess());
  }
  RuntimeProcess* const made = ProcessIfMade();
  if (made == nullptr) {
    throw InternalError(
        "Reaction::Process: a reaction that ended is asked for its process -- "
        "please report this as a bug");
  }
  return made;
}

auto Reaction::CallParts() -> std::expected<bool, Unwound> {
  bool must_stop = false;
  if (!set_up_) {
    set_up_ = true;
    const std::expected<bool, Unwound> answer =
        CatchUnwind(definition_->setup, object_, &wait_);
    if (!answer.has_value()) {
      return std::unexpected(answer.error());
    }
    switch (definition_->Form()) {
      case support::ReactionForm::kRunsOnce:
        return true;
      case support::ReactionForm::kRunThenStop:
        // A setup answering that no pass follows is a body that returned
        // before its loop.
        if (!*answer) {
          return true;
        }
        break;
      case support::ReactionForm::kStopThenRun:
        must_stop = *answer;
        break;
    }
  }
  while (!must_stop) {
    const std::expected<bool, Unwound> answer =
        CatchUnwind(definition_->pass, object_, &wait_);
    if (!answer.has_value()) {
      return std::unexpected(answer.error());
    }
    must_stop = *answer;
  }
  return false;
}

void Reaction::End(RuntimeEffects& effects, bool killed) {
  RuntimeProcess* const made = ProcessIfMade();
  if (made == nullptr) {
    ReleaseExecution();
    return;
  }
  made->SettleTerminated(
      killed ? ProcessTerminationCause::kKilled
             : ProcessTerminationCause::kCompleted,
      effects);
  RuntimeProcess::ReleaseTerminatedLineage(*made);
}

void Reaction::Land(RuntimeEffects& effects, const Unwound& unwound) {
  End(effects, true);
  if (!unwound.control_effect) {
    ReportRaisedError(effects, unwound.raised);
  }
}

void Reaction::ReleaseExecution() {
  Withdraw();
  wait_ = Wait{};
  process_ = Ended{};
}

auto Reaction::HasLiveForeignCall() const -> bool {
  return false;
}

auto Reaction::HeldState() const -> const RunningState* {
  return state_;
}

auto Reaction::Activity() const -> ProcessExecutionState {
  if (!set_up_) {
    return ProcessExecutionState::kCreated;
  }
  return state_ != nullptr ? ProcessExecutionState::kRunning
                           : ProcessExecutionState::kWaiting;
}

}  // namespace lyra::runtime

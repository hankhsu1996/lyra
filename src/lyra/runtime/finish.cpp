#include "lyra/runtime/finish.hpp"

#include <cstdint>
#include <string_view>

#include "lyra/base/source_location.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

namespace {

// The two tasks differ only in the name the engine is told, so what each of
// them does is written once. The departure names no region, so nothing between
// here and the execution's landing claims it; the execution is also asked to
// stop, which is what carries the departure on past a foreign frame it does
// not cross (LRM 35.9).
[[noreturn]] void EndRunFrom(
    RuntimeEffects& runtime, std::string_view task, std::int64_t finish_number,
    base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  runtime.EndRun(task, finish_number);
  if (RuntimeProcess* process = runtime.TryCurrentProcess();
      process != nullptr) {
    process->RequestTermination(ProcessTerminationCause::kKilled);
  }
  RaiseUnclaimableEffect();
}

}  // namespace

void Finish(
    RuntimeEffects& runtime, std::int64_t finish_number,
    base::SourceLocation at) {
  EndRunFrom(runtime, "$finish", finish_number, at);
}

void Stop(
    RuntimeEffects& runtime, std::int64_t finish_number,
    base::SourceLocation at) {
  EndRunFrom(runtime, "$stop", finish_number, at);
}

}  // namespace lyra::runtime

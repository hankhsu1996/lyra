#include "lyra/runtime/finish.hpp"

#include <cstdint>
#include <string_view>

#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

// The two tasks differ only in the name the engine is told, so what each of
// them does is written once. The departure names no region, so nothing between
// here and the execution's landing claims it; the execution is also asked to
// stop, which is what carries the departure on past a foreign frame it does
// not cross (LRM 35.9).
[[noreturn]] void EndRunFrom(
    RuntimeEffects& runtime, std::string_view task,
    const lyra::value::String& origin, std::int64_t finish_number) {
  runtime.EndRun(task, origin, finish_number);
  if (RuntimeProcess* process = runtime.TryCurrentProcess();
      process != nullptr) {
    process->RequestTermination(ProcessTerminationCause::kKilled);
  }
  RaiseUnclaimableEffect();
}

}  // namespace

void Finish(
    RuntimeEffects& runtime, const lyra::value::String& origin,
    std::int64_t finish_number) {
  EndRunFrom(runtime, "$finish", origin, finish_number);
}

void Stop(
    RuntimeEffects& runtime, const lyra::value::String& origin,
    std::int64_t finish_number) {
  EndRunFrom(runtime, "$stop", origin, finish_number);
}

}  // namespace lyra::runtime

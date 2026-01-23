#include "lyra/runtime/simulation.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <print>

#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace {
uint64_t g_final_time = 0;
}  // namespace

extern "C" void LyraRunSimulation(LyraProcessFunc process, void* state) {
  // State layout: SuspendRecord at offset 0, followed by slots
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);

  // Initialize suspend record to finished (default)
  suspend->tag = lyra::runtime::SuspendTag::kFinished;

  lyra::runtime::Engine engine([&](lyra::runtime::Engine& eng,
                                   lyra::runtime::ProcessHandle handle,
                                   lyra::runtime::ResumePoint resume) {
    // Reset tag to finished before calling process
    // (process will overwrite if suspending)
    suspend->tag = lyra::runtime::SuspendTag::kFinished;

    // Call process function with resume block
    process(state, resume.block_index);

    // Check suspend record and schedule accordingly
    switch (suspend->tag) {
      case lyra::runtime::SuspendTag::kFinished:
        // Process completed, nothing to do
        break;

      case lyra::runtime::SuspendTag::kDelay:
        eng.Delay(
            handle,
            lyra::runtime::ResumePoint{
                .block_index = suspend->resume_block, .instruction_index = 0},
            suspend->delay_ticks);
        break;

      case lyra::runtime::SuspendTag::kWait:
      case lyra::runtime::SuspendTag::kRepeat:
        // TODO(hankhsu): implement wait and repeat
        eng.Delay(
            handle,
            lyra::runtime::ResumePoint{
                .block_index = suspend->resume_block, .instruction_index = 0},
            1);
        break;
    }
  });

  // Schedule initial process at block 0
  engine.ScheduleInitial(
      lyra::runtime::ProcessHandle{.process_id = 0, .instance_id = 0});

  // Run simulation until completion
  auto final_time = engine.Run();
  g_final_time = std::max(g_final_time, final_time);
}

extern "C" void LyraInitRuntime() {
  g_final_time = 0;
}

extern "C" void LyraReportTime() {
  std::print("__LYRA_TIME__={}\n", g_final_time);
  std::fflush(stdout);
}

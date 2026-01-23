#include "lyra/runtime/simulation.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <print>
#include <span>

#include "lyra/common/edge_kind.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace {

// Process state header layout (matches LLVM-generated struct)
struct StateHeader {
  lyra::runtime::SuspendRecord suspend;
  void* design_ptr = nullptr;
  void* engine_ptr = nullptr;
};

auto FinalTime() -> uint64_t& {
  static uint64_t value = 0;
  return value;
}

void HandleSuspendRecord(
    lyra::runtime::Engine& eng, lyra::runtime::ProcessHandle handle,
    lyra::runtime::SuspendRecord* suspend) {
  auto resume = lyra::runtime::ResumePoint{
      .block_index = suspend->resume_block, .instruction_index = 0};

  switch (suspend->tag) {
    case lyra::runtime::SuspendTag::kFinished:
      break;

    case lyra::runtime::SuspendTag::kDelay:
      eng.Delay(handle, resume, suspend->delay_ticks);
      break;

    case lyra::runtime::SuspendTag::kWait: {
      auto triggers = std::span(suspend->triggers).first(suspend->num_triggers);
      for (const auto& trigger : triggers) {
        eng.Subscribe(
            handle, resume, trigger.signal_id,
            static_cast<lyra::runtime::EdgeKind>(trigger.edge));
      }
      break;
    }

    case lyra::runtime::SuspendTag::kRepeat:
      eng.ScheduleNextDelta(
          handle, lyra::runtime::ResumePoint{.block_index = 0});
      break;
  }
}

}  // namespace

extern "C" void LyraRunSimulation(LyraProcessFunc process, void* state) {
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  suspend->tag = lyra::runtime::SuspendTag::kFinished;

  lyra::runtime::Engine engine([&](lyra::runtime::Engine& eng,
                                   lyra::runtime::ProcessHandle handle,
                                   lyra::runtime::ResumePoint resume) {
    suspend->tag = lyra::runtime::SuspendTag::kFinished;
    process(state, resume.block_index);
    HandleSuspendRecord(eng, handle, suspend);
  });

  engine.ScheduleInitial(
      lyra::runtime::ProcessHandle{.process_id = 0, .instance_id = 0});

  auto final_time = engine.Run();
  FinalTime() = std::max(FinalTime(), final_time);
}

extern "C" void LyraRunSimulationMulti(
    LyraProcessFunc* processes, void** states_raw, uint32_t num_processes) {
  auto states = std::span(states_raw, num_processes);
  auto procs = std::span(processes, num_processes);

  lyra::runtime::Engine engine([&](lyra::runtime::Engine& eng,
                                   lyra::runtime::ProcessHandle handle,
                                   lyra::runtime::ResumePoint resume) {
    uint32_t proc_idx = handle.process_id;
    void* state = states[proc_idx];
    auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
    suspend->tag = lyra::runtime::SuspendTag::kFinished;

    procs[proc_idx](state, resume.block_index);
    HandleSuspendRecord(eng, handle, suspend);
  });

  // Store engine pointer in each process state header
  for (auto* state : states) {
    auto* header = static_cast<StateHeader*>(state);
    header->engine_ptr = &engine;
  }

  for (uint32_t i = 0; i < num_processes; ++i) {
    engine.ScheduleInitial(
        lyra::runtime::ProcessHandle{.process_id = i, .instance_id = 0});
  }

  auto final_time = engine.Run();
  FinalTime() = std::max(FinalTime(), final_time);
}

extern "C" void LyraDesignStoreAndNotify(
    void* engine_ptr, void* slot_ptr, const void* new_value_ptr,
    uint32_t byte_size, uint32_t signal_id) {
  auto* slot_bytes = static_cast<uint8_t*>(slot_ptr);
  const auto* new_bytes = static_cast<const uint8_t*>(new_value_ptr);

  bool old_lsb = (*slot_bytes & 1) != 0;
  bool value_changed = std::memcmp(slot_ptr, new_value_ptr, byte_size) != 0;
  std::memcpy(slot_ptr, new_value_ptr, byte_size);
  bool new_lsb = (*new_bytes & 1) != 0;

  if (value_changed && engine_ptr != nullptr) {
    auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
    engine->NotifyChange(signal_id, old_lsb, new_lsb, true);
  }
}

extern "C" void LyraDesignStoreStringAndNotify(
    void* engine_ptr, void* slot_ptr, void* new_str, uint32_t signal_id) {
  auto** str_slot = static_cast<void**>(slot_ptr);
  void* old_str = *str_slot;
  bool value_changed = (old_str != new_str);

  *str_slot = new_str;

  if (value_changed && engine_ptr != nullptr) {
    bool old_lsb = (old_str != nullptr);
    bool new_lsb = (new_str != nullptr);
    auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
    engine->NotifyChange(signal_id, old_lsb, new_lsb, true);
  }
}

extern "C" void LyraInitRuntime() {
  FinalTime() = 0;
}

extern "C" void LyraReportTime() {
  std::print("__LYRA_TIME__={}\n", FinalTime());
  std::fflush(stdout);
}

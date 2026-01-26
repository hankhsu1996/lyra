#include "lyra/runtime/simulation.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <print>
#include <span>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/string.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace {

std::filesystem::path g_fs_base_dir;

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
            static_cast<lyra::common::EdgeKind>(trigger.edge));
      }
      break;
    }

    case lyra::runtime::SuspendTag::kRepeat:
      eng.ScheduleNextDelta(
          handle, lyra::runtime::ResumePoint{.block_index = 0});
      break;
  }
}

void SuspendReset(lyra::runtime::SuspendRecord* suspend) {
  suspend->tag = lyra::runtime::SuspendTag::kFinished;
}

}  // namespace

extern "C" void LyraSuspendDelay(
    void* state, uint64_t ticks, uint32_t resume_block) {
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  suspend->tag = lyra::runtime::SuspendTag::kDelay;
  suspend->delay_ticks = ticks;
  suspend->resume_block = resume_block;
}

extern "C" void LyraSuspendWait(
    void* state, uint32_t resume_block, const void* triggers,
    uint32_t num_triggers) {
  if (num_triggers > lyra::runtime::kMaxInlineTriggers) {
    std::abort();
  }
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  suspend->tag = lyra::runtime::SuspendTag::kWait;
  suspend->resume_block = resume_block;
  suspend->num_triggers = num_triggers;
  std::memcpy(
      suspend->triggers.data(), triggers,
      num_triggers * sizeof(lyra::runtime::WaitTriggerRecord));
}

extern "C" void LyraSuspendRepeat(void* state) {
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  suspend->tag = lyra::runtime::SuspendTag::kRepeat;
  suspend->resume_block = 0;
}

extern "C" void LyraRunProcessSync(LyraProcessFunc process, void* state) {
  // Entry block is always block 0 (ABI contract with process generation)
  constexpr uint32_t kEntryBlock = 0;

  // Reset suspend record before execution
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  suspend->tag = lyra::runtime::SuspendTag::kFinished;

  process(state, kEntryBlock);

  // Init processes must not suspend - they run to completion
  if (suspend->tag != lyra::runtime::SuspendTag::kFinished) {
    std::println(
        stderr, "error: init process suspended (tag={}), aborting",
        static_cast<int>(suspend->tag));
    std::abort();
  }
}

extern "C" void LyraRunSimulation(
    LyraProcessFunc* processes, void** states_raw, uint32_t num_processes) {
  auto states = std::span(states_raw, num_processes);
  auto procs = std::span(processes, num_processes);

  lyra::runtime::Engine engine([&](lyra::runtime::Engine& eng,
                                   lyra::runtime::ProcessHandle handle,
                                   lyra::runtime::ResumePoint resume) {
    uint32_t proc_idx = handle.process_id;
    void* state = states[proc_idx];
    auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
    SuspendReset(suspend);

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

extern "C" void LyraStorePacked(
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

extern "C" void LyraStoreString(
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

extern "C" void LyraScheduleNba(
    void* engine_ptr, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* mask_ptr, uint32_t byte_size,
    uint32_t notify_slot_id) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->ScheduleNba(
      write_ptr, notify_base_ptr, value_ptr, mask_ptr, byte_size,
      notify_slot_id);
}

extern "C" void LyraTerminate(
    void* engine_ptr, uint32_t kind, int32_t level, LyraStringHandle message) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  uint64_t time = engine->CurrentTime();

  // Negative level treated as silent
  if (level >= 1) {
    // Out-of-range kind defaults to kFinish behavior
    switch (kind) {
      case 0:  // kFinish
      default:
        std::print("$finish called at time {}\n", time);
        break;
      case 1:  // kFatal - "fatal: <msg>\n", NOT "called at time"
        std::print("fatal: ");
        if (message != nullptr) {
          // No width/align for fatal message
          lyra::runtime::LyraFormatSpec spec{
              .kind = static_cast<int32_t>(lyra::FormatKind::kString),
              .width = -1,
              .precision = -1,
              .flags = 0,
              .reserved = {}};
          LyraPrintString(message, &spec);
        }
        std::print("\n");
        break;
      case 2:  // kStop
        // MVP: same as finish. Future: may pause for interactive debugger.
        std::print("$stop called at time {}\n", time);
        break;
      case 3:  // kExit
        // MVP: same as finish. Future: may have program-block semantics.
        std::print("$exit called at time {}\n", time);
        break;
    }
    std::fflush(stdout);
  }

  // MVP: all kinds terminate simulation. Future: $stop may not terminate.
  // Message handle is borrowed (read-only), no release needed since:
  // 1. All kinds currently call Finish() which terminates immediately
  // 2. If $stop becomes non-terminating later, must add LyraStringRelease here
  engine->Finish();
}

extern "C" auto LyraGetTime(void* engine_ptr) -> uint64_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  return engine->CurrentTime();
}

extern "C" void LyraSetTimeFormat(
    void* engine_ptr, int8_t units, int32_t precision, const char* suffix,
    int32_t min_width) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->SetTimeFormat(
      units, precision, suffix != nullptr ? suffix : "", min_width);
}

extern "C" void LyraInitRuntime(const char* fs_base_dir) {
  std::filesystem::path base(fs_base_dir);
  if (!base.is_absolute()) {
    throw lyra::common::InternalError(
        "LyraInitRuntime", "fs_base_dir must be absolute");
  }
  g_fs_base_dir = base.lexically_normal();
  FinalTime() = 0;
}

namespace lyra::runtime {

auto GetFsBaseDir() -> const std::filesystem::path& {
  return g_fs_base_dir;
}

}  // namespace lyra::runtime

extern "C" void LyraReportTime() {
  std::print("__LYRA_TIME__={}\n", FinalTime());
  std::fflush(stdout);
}

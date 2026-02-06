#include "lyra/runtime/simulation.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/format_spec_abi.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/slot_meta_abi.hpp"
#include "lyra/runtime/string.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace {

auto FsBaseDir() -> std::filesystem::path& {
  static std::filesystem::path value;
  return value;
}

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
      // Debug invariant: triggers_ptr must be valid if num_triggers > 0
      if (suspend->num_triggers > 0 && suspend->triggers_ptr == nullptr) {
        throw lyra::common::InternalError(
            "HandleSuspendRecord",
            "triggers_ptr null with non-zero num_triggers");
      }
      auto triggers = std::span(suspend->triggers_ptr, suspend->num_triggers);
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

// Release heap-allocated triggers if any (call before overwriting
// SuspendRecord) Resets num_triggers and triggers_ptr to make non-wait states
// trivially safe.
void ReleaseTriggerOverflow(lyra::runtime::SuspendRecord* suspend) {
  if (suspend->triggers_ptr != nullptr &&
      suspend->triggers_ptr != suspend->inline_triggers.data()) {
    delete[] suspend->triggers_ptr;  // NOLINT(cppcoreguidelines-owning-memory)
  }
  suspend->num_triggers = 0;
  suspend->triggers_ptr = nullptr;
}

void SuspendReset(lyra::runtime::SuspendRecord* suspend) {
  ReleaseTriggerOverflow(suspend);
  suspend->tag = lyra::runtime::SuspendTag::kFinished;
}

}  // namespace

// Runtime allocation for large trigger lists (called from LLVM-generated code)
extern "C" auto LyraAllocTriggers(uint32_t count)
    -> lyra::runtime::WaitTriggerRecord* {
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  return new lyra::runtime::WaitTriggerRecord[count];
}

extern "C" void LyraFreeTriggers(lyra::runtime::WaitTriggerRecord* ptr) {
  delete[] ptr;  // NOLINT(cppcoreguidelines-owning-memory)
}

extern "C" void LyraSuspendDelay(
    void* state, uint64_t ticks, uint32_t resume_block) {
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  ReleaseTriggerOverflow(suspend);  // Clean up any previous wait
  suspend->tag = lyra::runtime::SuspendTag::kDelay;
  suspend->delay_ticks = ticks;
  suspend->resume_block = resume_block;
}

extern "C" void LyraSuspendWait(
    void* state, uint32_t resume_block, const void* triggers,
    uint32_t num_triggers) {
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  ReleaseTriggerOverflow(suspend);  // Clean up any previous wait

  suspend->tag = lyra::runtime::SuspendTag::kWait;
  suspend->resume_block = resume_block;
  suspend->num_triggers = num_triggers;

  // Always set triggers_ptr for invariant consistency (even if num_triggers ==
  // 0)
  if (num_triggers <= lyra::runtime::kInlineTriggerCapacity) {
    suspend->triggers_ptr = suspend->inline_triggers.data();
  } else {
    // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
    suspend->triggers_ptr = new lyra::runtime::WaitTriggerRecord[num_triggers];
  }

  if (num_triggers > 0) {
    std::memcpy(
        suspend->triggers_ptr, triggers,
        num_triggers * sizeof(lyra::runtime::WaitTriggerRecord));
  }
}

extern "C" void LyraSuspendRepeat(void* state) {
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  ReleaseTriggerOverflow(suspend);  // Clean up any previous wait
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
    fmt::println(
        stderr, "error: init process suspended (tag={}), aborting",
        static_cast<int>(suspend->tag));
    std::abort();
  }
}

namespace {

// Create the process runner callback used by both LyraRunSimulation and
// LyraRunSimulationWithPlusargs.
auto MakeProcessRunner(
    std::span<LyraProcessFunc> procs, std::span<void*> states) {
  return [procs, states](
             lyra::runtime::Engine& eng, lyra::runtime::ProcessHandle handle,
             lyra::runtime::ResumePoint resume) {
    uint32_t proc_idx = handle.process_id;
    void* state = states[proc_idx];
    auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
    SuspendReset(suspend);

    procs[proc_idx](state, resume.block_index);
    HandleSuspendRecord(eng, handle, suspend);
  };
}

void SetupAndRunSimulation(
    lyra::runtime::Engine& engine, std::span<void*> states,
    uint32_t num_processes) {
  // Store engine pointer in each process state header
  for (auto* state : states) {
    auto* header = static_cast<StateHeader*>(state);
    header->engine_ptr = &engine;
  }

  // Set design state base for flush-based trace snapshots.
  // Codegen invariant: all process states share the same DesignState pointer.
  if (!states.empty()) {
    const auto* first_header = static_cast<const StateHeader*>(states[0]);
    void* design_base = first_header->design_ptr;

    for (size_t i = 1; i < states.size(); ++i) {
      const auto* header = static_cast<const StateHeader*>(states[i]);
      if (header->design_ptr != design_base) {
        throw lyra::common::InternalError(
            "SetupAndRunSimulation",
            std::format(
                "design_ptr mismatch: states[0]={} vs states[{}]={}",
                design_base, i, header->design_ptr));
      }
    }

    engine.SetDesignStateBase(design_base);
  }

  for (uint32_t i = 0; i < num_processes; ++i) {
    engine.ScheduleInitial(
        lyra::runtime::ProcessHandle{.process_id = i, .instance_id = 0});
  }

  auto final_time = engine.Run();
  FinalTime() = std::max(FinalTime(), final_time);
}

}  // namespace

extern "C" void LyraRunSimulation(
    LyraProcessFunc* processes, void** states_raw, uint32_t num_processes,
    const char** plusargs_raw, uint32_t num_plusargs,
    const char** instance_paths_raw, uint32_t num_instance_paths,
    const uint32_t* slot_meta_words, uint32_t num_slot_metas,
    uint32_t slot_meta_version, bool dump_slot_meta, bool enable_trace) {
  auto states = std::span(states_raw, num_processes);
  auto procs = std::span(processes, num_processes);

  // Convert plusargs to vector<string> for Engine
  std::vector<std::string> plusargs_vec;
  if (plusargs_raw != nullptr && num_plusargs > 0) {
    auto plusargs_span = std::span(plusargs_raw, num_plusargs);
    plusargs_vec.reserve(num_plusargs);
    for (const char* arg : plusargs_span) {
      plusargs_vec.emplace_back(arg != nullptr ? arg : "");
    }
  }

  // Convert instance paths to vector<string> for Engine (%m support)
  std::vector<std::string> instance_paths_vec;
  if (instance_paths_raw != nullptr && num_instance_paths > 0) {
    auto paths_span = std::span(instance_paths_raw, num_instance_paths);
    instance_paths_vec.reserve(num_instance_paths);
    for (const char* path : paths_span) {
      instance_paths_vec.emplace_back(path != nullptr ? path : "");
    }
  }

  lyra::runtime::Engine engine(
      MakeProcessRunner(procs, states), std::span(plusargs_vec),
      std::move(instance_paths_vec));
  if (slot_meta_words != nullptr && num_slot_metas > 0) {
    if (slot_meta_version != lyra::runtime::slot_meta_abi::kVersion) {
      throw lyra::common::InternalError(
          "LyraRunSimulation",
          std::format(
              "slot_meta version mismatch: got {}, expected {}",
              slot_meta_version, lyra::runtime::slot_meta_abi::kVersion));
    }
    engine.InitSlotMeta(
        lyra::runtime::SlotMetaRegistry(slot_meta_words, num_slot_metas));
  }
  if (dump_slot_meta) {
    engine.GetSlotMetaRegistry().DumpSummary();
  }
  if (enable_trace) {
    engine.GetTraceManager().SetEnabled(true);
  }
  SetupAndRunSimulation(engine, states, num_processes);
  if (enable_trace) {
    engine.GetTraceManager().PrintSummary();
  }
}

extern "C" auto LyraPlusargsTest(void* engine_ptr, LyraStringHandle query)
    -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  std::string_view query_str = LyraStringAsView(query);
  return engine->TestPlusargs(query_str);
}

extern "C" auto LyraPlusargsValueInt(
    void* engine_ptr, LyraStringHandle format, int32_t* output) -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  std::string_view format_str = LyraStringAsView(format);
  return engine->ValuePlusargsInt(format_str, output);
}

extern "C" auto LyraPlusargsValueString(
    void* engine_ptr, LyraStringHandle format, LyraStringHandle* output)
    -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  std::string_view format_str = LyraStringAsView(format);
  std::string result_str;
  int32_t found = engine->ValuePlusargsString(format_str, &result_str);
  if (found != 0 && output != nullptr) {
    // Create a new string handle for the result
    *output = LyraStringFromLiteral(
        result_str.data(), static_cast<int64_t>(result_str.size()));
  }
  return found;
}

extern "C" void LyraStorePacked(
    void* engine_ptr, void* slot_ptr, const void* new_value_ptr,
    uint32_t byte_size, uint32_t signal_id) {
  bool value_changed = std::memcmp(slot_ptr, new_value_ptr, byte_size) != 0;
  std::memcpy(slot_ptr, new_value_ptr, byte_size);

  if (value_changed && engine_ptr != nullptr) {
    auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
    engine->MarkSlotDirty(signal_id);
  }
}

extern "C" void LyraStoreString(
    void* engine_ptr, void* slot_ptr, void* new_str, uint32_t signal_id) {
  auto** str_slot = static_cast<void**>(slot_ptr);
  void* old_str = *str_slot;
  bool value_changed = (old_str != new_str);

  *str_slot = new_str;

  if (value_changed && engine_ptr != nullptr) {
    auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
    engine->MarkSlotDirty(signal_id);
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

extern "C" void LyraSchedulePostponed(
    void* engine_ptr, LyraPostponedCallback callback, void* design_state) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->SchedulePostponed(callback, design_state);
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
        lyra::runtime::WriteOutput(
            std::format("$finish called at time {}\n", time));
        break;
      case 1:  // kFatal - "fatal: <msg>\n", NOT "called at time"
        lyra::runtime::WriteOutput("fatal: ");
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
        lyra::runtime::WriteOutput("\n");
        break;
      case 2:  // kStop
        // MVP: same as finish. Future: may pause for interactive debugger.
        lyra::runtime::WriteOutput(
            std::format("$stop called at time {}\n", time));
        break;
      case 3:  // kExit
        // MVP: same as finish. Future: may have program-block semantics.
        lyra::runtime::WriteOutput(
            std::format("$exit called at time {}\n", time));
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
  FsBaseDir() = base.lexically_normal();
  FinalTime() = 0;
}

namespace lyra::runtime {

auto GetFsBaseDir() -> const std::filesystem::path& {
  return FsBaseDir();
}

}  // namespace lyra::runtime

extern "C" void LyraReportTime() {
  lyra::runtime::WriteOutput(std::format("__LYRA_TIME__={}\n", FinalTime()));
  std::fflush(stdout);
}

extern "C" void LyraMonitorRegister(
    void* engine_ptr, LyraMonitorCheckCallback check_thunk, void* design_state,
    const void* initial_prev, uint32_t size) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->RegisterMonitor(check_thunk, design_state, initial_prev, size);
}

extern "C" void LyraMonitorSetEnabled(void* engine_ptr, bool enabled) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->SetMonitorEnabled(enabled);
}

extern "C" void LyraNotifySignal(
    void* engine_ptr, const void* slot_ptr, uint32_t signal_id) {
  if (engine_ptr == nullptr || slot_ptr == nullptr) return;

  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->MarkSlotDirty(signal_id);
}

extern "C" auto LyraRandom(void* engine_ptr) -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  return engine->Random();
}

extern "C" auto LyraUrandom(void* engine_ptr) -> uint32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  return engine->Urandom();
}

extern "C" void LyraApply4StatePatches8(
    void* base, const uint64_t* offsets, const uint8_t* masks, uint64_t count) {
  if (count == 0 || base == nullptr || offsets == nullptr || masks == nullptr)
    return;
  auto* base_bytes = static_cast<uint8_t*>(base);
  for (uint64_t i = 0; i < count; ++i) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    std::memcpy(base_bytes + offsets[i], &masks[i], sizeof(uint8_t));
  }
}

extern "C" void LyraApply4StatePatches16(
    void* base, const uint64_t* offsets, const uint16_t* masks,
    uint64_t count) {
  if (count == 0 || base == nullptr || offsets == nullptr || masks == nullptr)
    return;
  auto* base_bytes = static_cast<uint8_t*>(base);
  for (uint64_t i = 0; i < count; ++i) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    std::memcpy(base_bytes + offsets[i], &masks[i], sizeof(uint16_t));
  }
}

extern "C" void LyraApply4StatePatches32(
    void* base, const uint64_t* offsets, const uint32_t* masks,
    uint64_t count) {
  if (count == 0 || base == nullptr || offsets == nullptr || masks == nullptr)
    return;
  auto* base_bytes = static_cast<uint8_t*>(base);
  for (uint64_t i = 0; i < count; ++i) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    std::memcpy(base_bytes + offsets[i], &masks[i], sizeof(uint32_t));
  }
}

extern "C" void LyraApply4StatePatches64(
    void* base, const uint64_t* offsets, const uint64_t* masks,
    uint64_t count) {
  if (count == 0 || base == nullptr || offsets == nullptr || masks == nullptr)
    return;
  auto* base_bytes = static_cast<uint8_t*>(base);
  for (uint64_t i = 0; i < count; ++i) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    std::memcpy(base_bytes + offsets[i], &masks[i], sizeof(uint64_t));
  }
}

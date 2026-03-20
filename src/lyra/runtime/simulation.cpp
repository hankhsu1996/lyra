#include "lyra/runtime/simulation.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/diagnostic/print.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/back_edge_site_meta.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/feature_flags.hpp"
#include "lyra/runtime/format_spec_abi.hpp"
#include "lyra/runtime/iteration_limit.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/process_descriptor.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_meta.hpp"
#include "lyra/runtime/signal_dump.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/slot_meta_abi.hpp"
#include "lyra/runtime/string.hpp"
#include "lyra/runtime/suspend_record.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"
#include "lyra/trace/text_trace_sink.hpp"

namespace {

auto FsBaseDir() -> std::filesystem::path& {
  static std::filesystem::path value;
  return value;
}

// Process state header layout. Must match ProcessFrameHeader in
// process_frame.hpp and the LLVM struct emitted by BuildHeaderType.
using StateHeader = lyra::runtime::ProcessFrameHeader;

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
      if (suspend->num_triggers > 0 && suspend->triggers_ptr == nullptr) {
        throw lyra::common::InternalError(
            "HandleSuspendRecord",
            "triggers_ptr null with non-zero num_triggers");
      }
      auto triggers = std::span(suspend->triggers_ptr, suspend->num_triggers);

      bool has_late_bound =
          suspend->num_late_bound > 0 && suspend->late_bound_ptr != nullptr;
      auto late_bound =
          has_late_bound
              ? std::span(suspend->late_bound_ptr, suspend->num_late_bound)
              : std::span<const lyra::runtime::LateBoundHeader>{};
      auto plan_ops =
          (suspend->plan_ops_ptr != nullptr)
              ? std::span(suspend->plan_ops_ptr, suspend->num_plan_ops)
              : std::span<const lyra::runtime::IndexPlanOp>{};
      auto dep_slots =
          (suspend->dep_slots_ptr != nullptr)
              ? std::span(suspend->dep_slots_ptr, suspend->num_dep_slots)
              : std::span<const uint32_t>{};

      eng.InstallTriggers(
          handle, resume, triggers, late_bound, plan_ops, dep_slots);
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
  if (suspend->late_bound_ptr != nullptr) {
    delete[] suspend
        ->late_bound_ptr;  // NOLINT(cppcoreguidelines-owning-memory)
  }
  suspend->num_late_bound = 0;
  suspend->late_bound_ptr = nullptr;
  if (suspend->plan_ops_ptr != nullptr) {
    delete[] suspend->plan_ops_ptr;  // NOLINT(cppcoreguidelines-owning-memory)
  }
  suspend->num_plan_ops = 0;
  suspend->plan_ops_ptr = nullptr;
  if (suspend->dep_slots_ptr != nullptr) {
    delete[] suspend->dep_slots_ptr;  // NOLINT(cppcoreguidelines-owning-memory)
  }
  suspend->num_dep_slots = 0;
  suspend->dep_slots_ptr = nullptr;
}

void SuspendReset(lyra::runtime::SuspendRecord* suspend) {
  ReleaseTriggerOverflow(suspend);
  *suspend = lyra::runtime::SuspendRecord{};
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
    uint32_t num_triggers, uint32_t wait_site_id) {
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  ReleaseTriggerOverflow(suspend);  // Clean up any previous wait

  suspend->tag = lyra::runtime::SuspendTag::kWait;
  suspend->resume_block = resume_block;
  suspend->wait_site_id = wait_site_id;
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

extern "C" void LyraSuspendWaitWithLateBound(
    void* state, uint32_t resume_block, const void* triggers,
    uint32_t num_triggers, const void* headers, uint32_t num_headers,
    const void* plan_ops, uint32_t num_plan_ops, const void* dep_slots,
    uint32_t num_dep_slots, uint32_t wait_site_id) {
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  ReleaseTriggerOverflow(suspend);

  suspend->tag = lyra::runtime::SuspendTag::kWait;
  suspend->resume_block = resume_block;
  suspend->wait_site_id = wait_site_id;
  suspend->num_triggers = num_triggers;

  if (num_triggers <= lyra::runtime::kInlineTriggerCapacity) {
    suspend->triggers_ptr = suspend->inline_triggers.data();
  } else {
    suspend->triggers_ptr = new lyra::runtime::WaitTriggerRecord[num_triggers];
  }

  if (num_triggers > 0) {
    std::memcpy(
        suspend->triggers_ptr, triggers,
        num_triggers * sizeof(lyra::runtime::WaitTriggerRecord));
  }

  suspend->num_late_bound = num_headers;
  if (num_headers > 0 && headers != nullptr) {
    suspend->late_bound_ptr = new lyra::runtime::LateBoundHeader[num_headers];
    std::memcpy(
        suspend->late_bound_ptr, headers,
        num_headers * sizeof(lyra::runtime::LateBoundHeader));
  } else {
    suspend->late_bound_ptr = nullptr;
  }

  suspend->num_plan_ops = num_plan_ops;
  if (num_plan_ops > 0 && plan_ops != nullptr) {
    suspend->plan_ops_ptr = new lyra::runtime::IndexPlanOp[num_plan_ops];
    std::memcpy(
        suspend->plan_ops_ptr, plan_ops,
        num_plan_ops * sizeof(lyra::runtime::IndexPlanOp));
  } else {
    suspend->plan_ops_ptr = nullptr;
  }

  suspend->num_dep_slots = num_dep_slots;
  if (num_dep_slots > 0 && dep_slots != nullptr) {
    suspend->dep_slots_ptr = new uint32_t[num_dep_slots];
    std::memcpy(
        suspend->dep_slots_ptr, dep_slots, num_dep_slots * sizeof(uint32_t));
  } else {
    suspend->dep_slots_ptr = nullptr;
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

  // Reset suspend record and iteration limit before execution.
  // LyraGetIterationLimit() returns the process-global configured limit.
  // 0 = unlimited: set counter to UINT32_MAX so the guard never fires.
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  suspend->tag = lyra::runtime::SuspendTag::kFinished;
  uint32_t limit = LyraGetIterationLimit();
  LyraResetIterationLimit(limit > 0 ? limit : UINT32_MAX);

  // Pointer-out ABI contract: caller owns the outcome buffer, callee must
  // write exactly one valid ProcessOutcome before returning. Sentinel tag
  // (UINT32_MAX) detects codegen bugs where a process path misses the write.
  lyra::runtime::ProcessOutcome outcome{};
  outcome.tag = UINT32_MAX;
  process(state, kEntryBlock, &outcome);

  switch (static_cast<lyra::runtime::ProcessExitCode>(outcome.tag)) {
    case lyra::runtime::ProcessExitCode::kOk:
      break;
    case lyra::runtime::ProcessExitCode::kTrap:
      lyra::PrintError(
          std::format(
              "init process trapped (reason={}, a={}, b={}), aborting",
              outcome.reason, outcome.a, outcome.b));
      std::abort();
    default:
      throw lyra::common::InternalError(
          "LyraRunProcessSync",
          std::format(
              "process returned invalid exit tag {} (sentinel=codegen bug, "
              "other=unknown tag)",
              outcome.tag));
  }

  // Init processes must not suspend - they run to completion
  if (suspend->tag != lyra::runtime::SuspendTag::kFinished) {
    lyra::PrintError(
        std::format(
            "init process suspended (tag={}), aborting",
            static_cast<int>(suspend->tag)));
    std::abort();
  }
}

namespace {

using ProcessDescriptorEntry = lyra::runtime::ProcessDescriptorEntry;
using SharedBodyFn = lyra::runtime::SharedBodyFn;

struct ProcessDispatchContext {
  std::span<LyraProcessFunc> standalone_procs;
  std::span<void*> states;
  uint32_t num_standalone;
};

void DescriptorProcessDispatch(
    void* ctx, lyra::runtime::Engine& eng, lyra::runtime::ProcessHandle handle,
    lyra::runtime::ResumePoint resume) {
  auto* dctx = static_cast<ProcessDispatchContext*>(ctx);
  uint32_t proc_idx = handle.process_id;
  void* state = dctx->states[proc_idx];
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  SuspendReset(suspend);

  lyra::runtime::ProcessOutcome outcome{};
  outcome.tag = UINT32_MAX;

  // Partition invariant (established by BuildLayout, validated at init):
  //   [0, num_standalone) = standalone processes (direct 3-arg call)
  //   [num_standalone, num_processes) = module processes (frame-header
  //   dispatch)
  if (proc_idx < dctx->num_standalone) {
    dctx->standalone_procs[proc_idx](state, resume.block_index, &outcome);
  } else {
    // Module process: 2-arg shared body call.
    // Body pointer and all instance binding are in the frame header,
    // populated at init time from descriptor data.
    auto* header = static_cast<StateHeader*>(state);
    header->outcome.tag = UINT32_MAX;
    header->body(state, resume.block_index);
    outcome = header->outcome;
  }

  switch (static_cast<lyra::runtime::ProcessExitCode>(outcome.tag)) {
    case lyra::runtime::ProcessExitCode::kOk:
      break;
    case lyra::runtime::ProcessExitCode::kTrap: {
      lyra::runtime::TrapPayload payload{
          .reason = static_cast<lyra::runtime::TrapReason>(outcome.reason),
          .a = outcome.a,
          .b = outcome.b,
      };
      eng.HandleTrap(proc_idx, payload);
      return;
    }
    default:
      throw lyra::common::InternalError(
          "DescriptorProcessDispatch",
          std::format(
              "process returned invalid exit tag {} (sentinel=codegen bug, "
              "other=unknown tag)",
              outcome.tag));
  }

  if (!eng.HasPostActivationReconciliation()) {
    HandleSuspendRecord(eng, handle, suspend);
  }
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

  // Propagate initial values through connections and comb kernels before Run().
  // Init processes have already set initial values and marked slots dirty.
  // EvaluateAllConnections seeds dirty marks for all changed destinations.
  // SeedCombKernelDirtyMarks ensures comb kernels fire even when source values
  // match the default (memset 0 = no LyraStorePacked dirty mark).
  // FlushAndPropagateConnections then converges connections + comb kernels
  // using the dirty-driven propagation loop (handles cascading).
  engine.EvaluateAllConnections();
  engine.SeedCombKernelDirtyMarks();
  engine.FlushAndPropagateConnections();

  auto final_time = engine.Run();
  FinalTime() = std::max(FinalTime(), final_time);
}

// Canonical path for all persistent simulation-process header binding
// initialization. Runtime is the sole owner of this contract.
//
// For every process: writes design_ptr (cached binding derived from
// design_state_base, which is the ABI-level source of truth).
// For module processes: also writes body, this_ptr, instance_id,
// signal_id_offset, unstable_offsets from descriptor data.
void InitSimulationProcessBindings(
    std::span<void*> states, void* design_state_base,
    std::span<const ProcessDescriptorEntry> descriptors,
    uint32_t num_standalone) {
  using StateHeader = lyra::runtime::ProcessFrameHeader;
  using SharedBodyFn = lyra::runtime::SharedBodyFn;
  for (size_t i = 0; i < states.size(); ++i) {
    auto* header = static_cast<StateHeader*>(states[i]);
    header->design_ptr = design_state_base;
  }
  for (uint32_t i = 0; i < descriptors.size(); ++i) {
    uint32_t proc_idx = num_standalone + i;
    auto* header = static_cast<StateHeader*>(states[proc_idx]);
    const auto& desc = descriptors[i];
    header->body = reinterpret_cast<SharedBodyFn>(desc.shared_body);
    header->this_ptr =
        static_cast<char*>(design_state_base) + desc.base_byte_offset;
    header->instance_id = desc.instance_id;
    header->signal_id_offset = desc.signal_id_offset;
    header->unstable_offsets = desc.unstable_offsets;
  }
}

}  // namespace

extern "C" void LyraRunSimulation(
    LyraProcessFunc* processes, void** states_raw, uint32_t num_processes,
    const char** plusargs_raw, uint32_t num_plusargs,
    const char** instance_paths_raw, uint32_t num_instance_paths,
    const LyraRuntimeAbi* abi) {
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

  uint32_t feature_flags = (abi != nullptr) ? abi->feature_flags : 0;
  using lyra::runtime::FeatureFlag;
  auto flags = static_cast<FeatureFlag>(feature_flags);

  // Parse v7 descriptor data from ABI and build the dispatch context.
  //
  // Process index partition contract (established by BuildLayout):
  //   post-init indices [0, num_standalone) = standalone (connection) processes
  //   post-init indices [num_standalone, num_processes) = module processes
  // DescriptorProcessDispatch depends on this exact partition shape:
  //   proc_idx < num_standalone -> standalone direct call
  //   proc_idx >= num_standalone -> descriptor lookup at (proc_idx -
  //   num_standalone)
  uint32_t num_standalone = num_processes;
  std::span<const ProcessDescriptorEntry> descriptors;
  if (abi != nullptr && abi->version >= 7) {
    num_standalone = abi->num_standalone_processes;
    if (abi->process_descriptors != nullptr &&
        abi->num_process_descriptors > 0) {
      descriptors = std::span(
          static_cast<const ProcessDescriptorEntry*>(abi->process_descriptors),
          abi->num_process_descriptors);
    }
    if (num_standalone + descriptors.size() != num_processes) {
      throw lyra::common::InternalError(
          "LyraRunSimulation",
          std::format(
              "descriptor partition mismatch: {} standalone + {} descriptors "
              "!= {} total",
              num_standalone, descriptors.size(), num_processes));
    }
  }

  // design_state is the ABI-level source of truth for design-state binding.
  // Runtime owns all persistent simulation-process header initialization.
  if (abi == nullptr) {
    throw lyra::common::InternalError(
        "LyraRunSimulation", "abi is null (v9 required)");
  }
  void* design_state_base = abi->design_state;
  if (design_state_base == nullptr && !states.empty()) {
    throw lyra::common::InternalError(
        "LyraRunSimulation", "abi->design_state is null");
  }

  // Initialize all persistent simulation-process headers through one
  // canonical path. After this, the descriptor table is not needed for
  // dispatch -- all binding lives in the frame headers.
  InitSimulationProcessBindings(
      states, design_state_base, descriptors, num_standalone);

  auto standalone_procs = std::span(processes, num_standalone);
  ProcessDispatchContext dispatch_ctx{
      .standalone_procs = standalone_procs,
      .states = states,
      .num_standalone = num_standalone,
  };
  lyra::runtime::Engine engine(
      lyra::runtime::ProcessDispatch{
          .fn = DescriptorProcessDispatch, .ctx = &dispatch_ctx},
      num_processes, std::span(plusargs_vec), std::move(instance_paths_vec),
      feature_flags);

  if (abi != nullptr) {
    if (abi->version != kRuntimeAbiVersion) {
      throw lyra::common::InternalError(
          "LyraRunSimulation",
          std::format(
              "unsupported RuntimeAbi version {} (expected {})", abi->version,
              kRuntimeAbiVersion));
    }

    // Slot metadata
    if (abi->slot_meta_words != nullptr && abi->slot_meta_word_count > 0) {
      engine.InitSlotMeta(
          lyra::runtime::SlotMetaRegistry(
              abi->slot_meta_words, abi->slot_meta_word_count));
    }

    // Process metadata
    if (abi->process_meta_words != nullptr &&
        abi->process_meta_word_count > 0) {
      engine.InitProcessMeta(
          lyra::runtime::ProcessMetaRegistry(
              abi->process_meta_words, abi->process_meta_word_count,
              abi->process_meta_string_pool,
              abi->process_meta_string_pool_size));
    }

    // Back-edge site metadata
    if (abi->back_edge_site_meta_words != nullptr &&
        abi->back_edge_site_meta_word_count > 0) {
      engine.InitBackEdgeSiteMeta(
          lyra::runtime::BackEdgeSiteRegistry(
              abi->back_edge_site_meta_words,
              abi->back_edge_site_meta_word_count,
              abi->back_edge_site_meta_string_pool,
              abi->back_edge_site_meta_string_pool_size));
    }
    // Connection descriptors
    if (abi->conn_descs != nullptr && abi->num_conn_descs > 0) {
      auto conn_descs = std::span(
          static_cast<const lyra::runtime::ConnectionDescriptor*>(
              abi->conn_descs),
          abi->num_conn_descs);
      engine.InitConnectionBatch(conn_descs);
    }

    // Comb kernel word table. Body pointers resolved from descriptor table.
    if (abi->comb_kernel_words != nullptr && abi->num_comb_kernel_words > 0) {
      engine.InitCombKernels(
          std::span(abi->comb_kernel_words, abi->num_comb_kernel_words),
          descriptors, num_standalone, states_raw);
    }

    // Process trigger metadata and constructor-time trigger groups (G13).
    if (abi->process_trigger_words != nullptr &&
        abi->num_process_trigger_words > 0) {
      engine.InitProcessTriggerRegistry(
          std::span(
              abi->process_trigger_words, abi->num_process_trigger_words));
    }

    // Wait-site metadata
    if (abi->wait_site_words != nullptr && abi->wait_site_word_count > 0) {
      engine.InitWaitSiteMeta(
          lyra::runtime::WaitSiteRegistry(
              abi->wait_site_words, abi->wait_site_word_count));
    }

    // Trace signal metadata
    if (abi->trace_signal_meta_words != nullptr &&
        abi->trace_signal_meta_word_count > 0) {
      engine.InitTraceSignalMeta(
          lyra::runtime::TraceSignalMetaRegistry(
              abi->trace_signal_meta_words, abi->trace_signal_meta_word_count,
              abi->trace_signal_meta_string_pool,
              abi->trace_signal_meta_string_pool_size));
    }
  }

  // Register suspend records for post-activation reconciliation.
  // When both wait-site metadata and suspend records are present,
  // HasPostActivationReconciliation() gates the new flow everywhere.
  if (abi != nullptr && abi->wait_site_words != nullptr &&
      abi->wait_site_word_count > 0) {
    std::vector<lyra::runtime::SuspendRecord*> suspend_ptrs;
    suspend_ptrs.reserve(states.size());
    for (auto* state : states) {
      suspend_ptrs.push_back(static_cast<lyra::runtime::SuspendRecord*>(state));
    }
    engine.RegisterSuspendRecords(suspend_ptrs);
  }

  if (HasFlag(flags, FeatureFlag::kDumpSlotMeta)) {
    engine.GetSlotMetaRegistry().DumpSummary();
  }
  if (HasFlag(flags, FeatureFlag::kDumpProcessMeta)) {
    engine.GetProcessMetaRegistry().DumpSummary();
  }
  // Register trace sinks before enabling TraceManager.
  if (HasFlag(flags, FeatureFlag::kEnableSignalTrace)) {
    const auto* meta = engine.GetTraceManager().GetSignalMeta();
    if (abi != nullptr && abi->signal_trace_path != nullptr) {
      engine.GetTraceManager().AddSink(
          std::make_unique<lyra::trace::TextTraceSink>(
              meta, std::string(abi->signal_trace_path)));
    } else {
      engine.GetTraceManager().AddSink(
          std::make_unique<lyra::trace::TextTraceSink>(meta));
    }
  }

  if (HasFlag(flags, FeatureFlag::kEnableTrace)) {
    engine.GetTraceManager().SetEnabled(true);
  }

  // Install SIGUSR1 dump handler for last-resort visibility
  lyra::runtime::InstallSignalDumpHandler(&engine);

  SetupAndRunSimulation(engine, states, num_processes);

  lyra::runtime::RemoveSignalDumpHandler();

  if (HasFlag(flags, FeatureFlag::kEnableTraceSummary)) {
    engine.GetTraceManager().PrintSummary();
  }

  if (HasFlag(flags, FeatureFlag::kDumpRuntimeStats)) {
    engine.DumpRuntimeStats(stderr);
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

extern "C" auto LyraSystemCmd(void* engine_ptr, LyraStringHandle cmd_handle)
    -> int32_t {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  if (!engine->IsFeatureEnabled(lyra::runtime::FeatureFlag::kEnableSystem)) {
    return -1;
  }
  if (cmd_handle == nullptr) {
    return static_cast<int32_t>(std::system(nullptr));
  }
  std::string_view sv = LyraStringAsView(cmd_handle);
  std::string cmd(sv);
  return static_cast<int32_t>(std::system(cmd.c_str()));
}

extern "C" void LyraStorePacked(
    void* engine_ptr, void* slot_ptr, const void* new_value_ptr,
    uint32_t byte_size, uint32_t signal_id, uint32_t dirty_off,
    uint32_t dirty_size) {
  bool value_changed = std::memcmp(slot_ptr, new_value_ptr, byte_size) != 0;
  std::memcpy(slot_ptr, new_value_ptr, byte_size);

  if (value_changed && engine_ptr != nullptr) {
    auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
    if (dirty_size > 0) {
      engine->MarkDirtyRange(signal_id, dirty_off, dirty_size);
    } else {
      engine->MarkSlotDirty(signal_id);
    }
  }
}

extern "C" auto LyraGetFirstDirtySeenPtr(void* engine_ptr) -> uint8_t* {
  if (engine_ptr == nullptr) return nullptr;
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  if (!engine->IsFirstDirtyFastPathAllowed()) return nullptr;
  return engine->GetFirstDirtySeenPtr();
}

extern "C" void LyraMarkDirtyFirst(void* engine_ptr, uint32_t slot_id) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->MarkSlotDirtyFirst(slot_id);
}

extern "C" void LyraMarkDirty(
    void* engine_ptr, uint32_t signal_id, uint32_t dirty_off,
    uint32_t dirty_size) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  if (dirty_size > 0) {
    engine->MarkDirtyRange(signal_id, dirty_off, dirty_size);
  } else {
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
  // mask_ptr == nullptr means full overwrite; non-null means masked merge.
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->ScheduleNba(
      write_ptr, notify_base_ptr, value_ptr, mask_ptr, byte_size,
      notify_slot_id);
}

extern "C" void LyraScheduleNbaCanonicalPacked(
    void* engine_ptr, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* unk_ptr, uint32_t region_byte_size,
    uint32_t second_region_offset, uint32_t notify_slot_id) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->ScheduleNbaCanonicalPacked(
      write_ptr, notify_base_ptr, value_ptr, unk_ptr, region_byte_size,
      second_region_offset, notify_slot_id);
}

extern "C" void LyraRegisterStrobe(
    void* engine_ptr, LyraStrobeProgramFn program, void* design_state,
    void* this_ptr, uint32_t instance_id, uint32_t signal_id_offset,
    const void* unstable_offsets) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  lyra::runtime::ObserverContext ctx{
      .this_ptr = this_ptr,
      .instance_id = instance_id,
      .signal_id_offset = signal_id_offset,
      .unstable_offsets = unstable_offsets,
  };
  engine->RegisterStrobe(program, design_state, ctx);
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

extern "C" auto LyraResolveBaseDir(const char* argv0) -> const char* {
  static std::string resolved;

  // Priority 1: explicit env var (set by `lyra run` for temp-dir bundles)
  if (const char* env = std::getenv("LYRA_FS_BASE_DIR");
      env != nullptr && env[0] != '\0') {
    resolved = env;
    return resolved.c_str();
  }

  // Priority 2: derive from executable directory
  // Layout: <dir>/<name> (produced by `lyra compile`)
  if (argv0 != nullptr && argv0[0] != '\0') {
    std::error_code ec;
    auto exe_path = std::filesystem::canonical(argv0, ec);
    if (ec) {
      // canonical requires the file to exist; try absolute as fallback
      // (handles relative argv0 in PATH-resolved scenarios)
      exe_path = std::filesystem::absolute(argv0, ec);
    }
    if (!ec) {
      resolved = exe_path.parent_path().string();
      return resolved.c_str();
    }
  }

  // Fallback: current working directory
  std::error_code ec;
  auto cwd = std::filesystem::current_path(ec);
  resolved = ec ? "." : cwd.string();
  return resolved.c_str();
}

extern "C" void LyraInitRuntime(
    const char* fs_base_dir, uint32_t iteration_limit) {
  std::filesystem::path base(fs_base_dir);
  if (!base.is_absolute()) {
    throw lyra::common::InternalError(
        "LyraInitRuntime", "fs_base_dir must be absolute");
  }
  FsBaseDir() = base.lexically_normal();
  FinalTime() = 0;
  LyraSetIterationLimit(iteration_limit);
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
    void* engine_ptr, LyraMonitorCheckProgramFn program, void* design_state,
    void* this_ptr, uint32_t instance_id, uint32_t signal_id_offset,
    const void* unstable_offsets, const void* initial_prev, uint32_t size) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  lyra::runtime::ObserverContext ctx{
      .this_ptr = this_ptr,
      .instance_id = instance_id,
      .signal_id_offset = signal_id_offset,
      .unstable_offsets = unstable_offsets,
  };
  engine->RegisterMonitor(program, design_state, ctx, initial_prev, size);
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

extern "C" void LyraNotifyContainerMutation(
    void* engine_ptr, uint32_t signal_id, uint32_t kind, uint32_t off,
    uint32_t size) {
  if (engine_ptr == nullptr) return;
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->MarkSlotDirty(signal_id);
  switch (kind) {
    case 0:  // kElementWrite
      if (size == 0) {
        throw lyra::common::InternalError(
            "LyraNotifyContainerMutation", "element write size must be > 0");
      }
      engine->MarkExternalDirtyRange(signal_id, off, size);
      break;
    case 1:  // kStructural
    case 2:  // kDelete
      if (off != 0 || size != 0) {
        throw lyra::common::InternalError(
            "LyraNotifyContainerMutation",
            "structural/delete must have off=0, size=0");
      }
      engine->MarkExternalDirtyRange(signal_id, 0, 0);
      break;
    default:
      throw lyra::common::InternalError(
          "LyraNotifyContainerMutation", "unknown mutation kind");
  }
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

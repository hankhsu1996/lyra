#include "lyra/runtime/simulation.hpp"

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

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/assertions.hpp"
#include "lyra/runtime/back_edge_site_meta.hpp"
#include "lyra/runtime/cover_hook.hpp"
#include "lyra/runtime/dpi_export_context.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/feature_flags.hpp"
#include "lyra/runtime/iteration_limit.hpp"
#include "lyra/runtime/nba_stats_hook.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/process_envelope.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_meta.hpp"
#include "lyra/runtime/reporting.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/signal_dump.hpp"
#include "lyra/runtime/signal_interrupt.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/string.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"
#include "lyra/trace/text_trace_sink.hpp"

namespace {

// Process state header layout. Must match ProcessFrameHeader in
// process_frame.hpp and the LLVM struct emitted by BuildHeaderType.
using StateHeader = lyra::runtime::ProcessFrameHeader;

auto FinalTime() -> uint64_t& {
  static uint64_t value = 0;
  return value;
}

// Minimal scope-exit guard for cleanup on all exit paths.
template <typename F>
class ScopeExit {
 public:
  explicit ScopeExit(F fn) : fn_(std::move(fn)) {
  }
  ~ScopeExit() noexcept {
    fn_();
  }
  ScopeExit(const ScopeExit&) = delete;
  ScopeExit(ScopeExit&&) = delete;
  auto operator=(const ScopeExit&) -> ScopeExit& = delete;
  auto operator=(ScopeExit&&) -> ScopeExit& = delete;

 private:
  F fn_;
};


// Legacy post-activation dispatch: reads SuspendRecord and installs
// waiter/delay state. Called by DescriptorProcessDispatch when
// HasPostActivationReconciliation() is false. The new-path equivalent
// is Engine::ReconcilePostActivation. Exactly one of these two runs
// per activation -- never both.
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
      auto dep_records =
          (suspend->dep_slots_ptr != nullptr)
              ? std::span(suspend->dep_slots_ptr, suspend->num_dep_slots)
              : std::span<const lyra::runtime::DepSignalRecord>{};

      eng.InstallTriggers(
          handle, resume, triggers, late_bound, plan_ops, dep_records);
      break;
    }

    case lyra::runtime::SuspendTag::kRepeat:
      eng.ScheduleNextDelta(
          handle, lyra::runtime::ResumePoint{.block_index = 0});
      break;

    case lyra::runtime::SuspendTag::kWaitEvent: {
      auto& inst = eng.GetProcessInstance(handle.process_id);
      lyra::runtime::Engine::AddInstanceEventWaiter(
          inst, suspend->event_id,
          lyra::runtime::EventWaiter{
              .process_id = handle.process_id,
              .instance = &inst,
              .resume_block = suspend->resume_block,
          });
      break;
    }
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
  // NOLINTBEGIN(cppcoreguidelines-owning-memory)
  delete[] suspend->late_bound_ptr;
  suspend->num_late_bound = 0;
  suspend->late_bound_ptr = nullptr;
  delete[] suspend->plan_ops_ptr;
  suspend->num_plan_ops = 0;
  suspend->plan_ops_ptr = nullptr;
  delete[] suspend->dep_slots_ptr;
  // NOLINTEND(cppcoreguidelines-owning-memory)
  suspend->num_dep_slots = 0;
  suspend->dep_slots_ptr = nullptr;
}

void SuspendReset(lyra::runtime::SuspendRecord* suspend) {
  ReleaseTriggerOverflow(suspend);
  *suspend = lyra::runtime::SuspendRecord{};
}

// Defense-in-depth guard: abort if a non-suspending DPI export task
// attempts to suspend. The static gate in design.cpp prevents direct timing
// controls; this catches unforeseen indirect paths.
void FailIfSuspensionDisallowed(const char* api) {
  if (LyraIsDpiExportSuspensionDisallowed()) {
    throw lyra::common::InternalError(
        api, "non-suspending DPI export task attempted to suspend");
  }
}

}  // namespace

extern "C" void LyraTriggerEvent(
    void* engine_ptr, uint32_t instance_id, uint32_t local_event_id) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  auto& inst = engine->GetInstanceMut(lyra::runtime::InstanceId{instance_id});
  engine->TriggerInstanceEvent(inst, local_event_id);
}

namespace {

struct ProcessDispatchContext {
  std::span<LyraProcessFunc> connection_procs;
  std::span<void*> states;
  uint32_t num_connection;
};

void DescriptorProcessDispatch(
    void* ctx, lyra::runtime::Engine& eng, lyra::runtime::ProcessHandle handle,
    lyra::runtime::ResumePoint resume) {
  auto* dctx = static_cast<ProcessDispatchContext*>(ctx);
  lyra::runtime::DispatchAndHandleActivation(
      dctx->connection_procs, dctx->states, dctx->num_connection, eng, handle,
      resume);
}

auto SetupAndRunSimulation(
    lyra::runtime::Engine& engine, std::span<void*> states,
    uint32_t num_processes) -> uint64_t {
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
    engine.ScheduleInitial(lyra::runtime::ProcessHandle{.process_id = i});
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

  return engine.Run();
}

}  // namespace

extern "C" void LyraRunSimulation(
    LyraProcessFunc* connection_funcs, void** states_raw,
    uint32_t num_processes, const char** plusargs_raw, uint32_t num_plusargs,
    const LyraRuntimeAbi* abi, void* run_session_ptr) {
  if (num_processes > 0 && states_raw == nullptr) {
    throw lyra::common::InternalError(
        "LyraRunSimulation", "states_raw is null");
  }
  auto states = std::span(states_raw, num_processes);

  // Convert plusargs to vector<string> for Engine
  std::vector<std::string> plusargs_vec;
  if (plusargs_raw != nullptr && num_plusargs > 0) {
    auto plusargs_span = std::span(plusargs_raw, num_plusargs);
    plusargs_vec.reserve(num_plusargs);
    for (const char* arg : plusargs_span) {
      plusargs_vec.emplace_back(arg != nullptr ? arg : "");
    }
  }

  uint32_t feature_flags = (abi != nullptr) ? abi->feature_flags : 0;
  using lyra::runtime::FeatureFlag;
  auto flags = static_cast<FeatureFlag>(feature_flags);

  // Dispatch partition from ABI. Process binding is constructor-owned;
  // this is the only process topology fact read from the ABI after H2.
  //
  // Dispatch partition contract:
  //   [0, num_connection) = connection processes (3-arg direct call)
  //   [num_connection, num_processes) = module processes (frame-header
  //   dispatch)
  if (abi == nullptr) {
    throw lyra::common::InternalError("LyraRunSimulation", "abi is null");
  }

  uint32_t num_connection = abi->num_connection_processes;

  if (num_connection > 0 && connection_funcs == nullptr) {
    throw lyra::common::InternalError(
        "LyraRunSimulation", "connection_funcs is null");
  }
  auto connection_procs = std::span(connection_funcs, num_connection);
  ProcessDispatchContext dispatch_ctx{
      .connection_procs = connection_procs,
      .states = states,
      .num_connection = num_connection,
  };
  auto* session = static_cast<lyra::runtime::RunSession*>(run_session_ptr);
  lyra::runtime::Engine engine(
      lyra::runtime::ProcessDispatch{
          .fn = DescriptorProcessDispatch, .ctx = &dispatch_ctx},
      session->output, num_processes, std::span(plusargs_vec),
      feature_flags);

  if (abi != nullptr) {
    if (abi->version != kRuntimeAbiVersion) {
      throw lyra::common::InternalError(
          "LyraRunSimulation",
          std::format(
              "unsupported RuntimeAbi version {} (expected {})", abi->version,
              kRuntimeAbiVersion));
    }

    // v25: Filesystem base directory for relative path resolution.
    if (abi->fs_base_dir != nullptr) {
      std::filesystem::path base(abi->fs_base_dir);
      if (!base.is_absolute()) {
        throw lyra::common::InternalError(
            "LyraRunSimulation", "fs_base_dir must be absolute");
      }
      engine.SetFsBaseDir(base.lexically_normal());
    }

    // D6d: Set simulation-global precision from emitted ABI.
    engine.SetGlobalPrecision(abi->global_precision_power);

    // Instance pointer list for slot storage resolution and observability.
    if (abi->instance_ptrs != nullptr && abi->num_instances > 0) {
      engine.SetInstances(std::span(abi->instance_ptrs, abi->num_instances));
    }

    // Build DPI scope registry for svGetScope/svSetScope family (D6b).
    engine.BuildDpiScopeRegistry();

    // R4: Initialize module-instance registries from per-instance bundles.
    // Builds slot meta, coordination bases, trigger and comb registries
    // from structured bundle data. Connection/design-global metadata
    // remains flat through separate init paths below.
    if (abi->instance_bundles != nullptr && abi->num_instance_bundles > 0) {
      engine.InitModuleInstancesFromBundles(
          std::span(abi->instance_bundles, abi->num_instance_bundles),
          std::span(abi->slot_meta_words, abi->slot_meta_word_count),
          states_raw);
    } else if (
        abi->slot_meta_words != nullptr && abi->slot_meta_word_count > 0) {
      // No bundles: fall back to flat slot meta init (no module instances).
      engine.InitSlotMeta(
          lyra::runtime::SlotMetaRegistry(
              abi->slot_meta_words, abi->slot_meta_word_count));
    }

    // Process metadata. When bundles are present, the flat ABI path
    // contains connection-only process meta. InitProcessMeta merges
    // it with module process meta built from bundles.
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

    // Comb kernel metadata (flat path). Only used when no bundles exist.
    // When bundles are present, module-instance comb kernels are built
    // from bundle body templates in InitModuleInstancesFromBundles.
    if (abi->comb_kernel_words != nullptr && abi->num_comb_kernel_words > 0 &&
        abi->instance_bundles == nullptr) {
      engine.InitCombKernels(
          std::span(abi->comb_kernel_words, abi->num_comb_kernel_words),
          num_connection, states_raw);
    }

    // Trigger registry finalization. Always called: merges pending
    // module triggers (from bundles) with connection triggers (from flat).
    engine.InitProcessTriggerRegistry(
        abi->process_trigger_words != nullptr
            ? std::span(
                  abi->process_trigger_words, abi->num_process_trigger_words)
            : std::span<const uint32_t>{},
        num_connection, states_raw);

    // Wait-site metadata
    if (abi->wait_site_words != nullptr && abi->wait_site_word_count > 0) {
      engine.InitWaitSiteMeta(
          lyra::runtime::WaitSiteRegistry(
              abi->wait_site_words, abi->wait_site_word_count));
    }

    // A1b: Immediate cover site hit-count array.
    if (abi->num_immediate_cover_sites > 0) {
      engine.InitImmediateCoverSites(abi->num_immediate_cover_sites);
    }

    // L8a: Per-instance event state is initialized in
    // InitModuleInstancesFromBundles via BodyRealizationDesc::event_count.
    // num_events ABI field is a reserved global max for diagnostics.

    // A2: Deferred assertion site metadata table.
    if (abi->num_deferred_assertion_sites > 0) {
      engine.InitDeferredAssertionSites(
          static_cast<const LyraDeferredAssertionSiteMeta*>(
              abi->deferred_assertion_site_meta),
          abi->num_deferred_assertion_sites);
    }

    // R5: Global-only trace signal metadata from constructor.
    // Instance-owned trace metadata lives in BodyObservableLayout::trace_meta.
    if (abi->trace_signal_meta_words != nullptr &&
        abi->trace_signal_meta_word_count > 0) {
      engine.InitTraceSignalMeta(
          lyra::runtime::TraceSignalMetaRegistry(
              abi->trace_signal_meta_words, abi->trace_signal_meta_word_count,
              abi->trace_signal_meta_string_pool,
              abi->trace_signal_meta_string_pool_size));
    }

    // R5: Trace selection must cover all flat slot_ids (global +
    // instance-owned). Called unconditionally after all init paths.
    engine.InitTraceSelection();
  }

  if (HasFlag(flags, FeatureFlag::kDumpSlotMeta)) {
    engine.GetSlotMetaRegistry().DumpSummary(engine.Output());
  }
  if (HasFlag(flags, FeatureFlag::kDumpProcessMeta)) {
    engine.GetProcessMetaRegistry().DumpSummary(engine.Output());
  }
  // Register trace sinks before enabling TraceManager.
  if (HasFlag(flags, FeatureFlag::kEnableSignalTrace)) {
    const auto* meta = engine.GetTraceManager().GetSignalMeta();
    std::unique_ptr<lyra::trace::TextTraceSink> text_sink;
    if (abi != nullptr && abi->signal_trace_path != nullptr) {
      text_sink = std::make_unique<lyra::trace::TextTraceSink>(
          meta, std::string(abi->signal_trace_path));
    } else {
      text_sink =
          std::make_unique<lyra::trace::TextTraceSink>(meta, &engine.Output());
    }
    text_sink->SetInstanceResolver(&engine.GetInstanceTraceResolver());
    engine.GetTraceManager().AddSink(std::move(text_sink));
  }

  if (HasFlag(flags, FeatureFlag::kEnableTrace)) {
    engine.GetTraceManager().SetEnabled(true);
  }

  // Install signal handlers for simulation lifecycle.
  lyra::runtime::InstallSignalDumpHandler(&engine);
  lyra::runtime::InstallSimulationInterruptHandlers();
  ScopeExit signal_cleanup([] {
    lyra::runtime::RemoveSimulationInterruptHandlers();
    lyra::runtime::RemoveSignalDumpHandler();
  });

  // Install DPI export-call context so export wrappers can borrow
  // DesignState* and Engine* during simulation.
  lyra::runtime::DpiExportCallContext export_ctx{
      .design_state = abi->design_state,
      .engine = &engine,
      .decision_owner = {},
  };
  lyra::runtime::ScopedDpiExportCallContext export_scope(export_ctx);

  uint64_t final_time = SetupAndRunSimulation(engine, states, num_processes);

  if (HasFlag(flags, FeatureFlag::kEnableTraceSummary)) {
    engine.GetTraceManager().PrintSummary(engine.Output());
  }

  if (HasFlag(flags, FeatureFlag::kDumpRuntimeStats)) {
    engine.DumpRuntimeStats(stderr);
  }

  // Centralized scheduler snapshot dump. Two policies, one render path:
  // 1. Auto-on-abnormal: dump when simulation ended for any reason other
  //    than $finish (delta-cycle limit, empty queues, max time, trap).
  // 2. Force-on-flag: --dump-suspended dumps regardless of end reason.
  {
    const auto reason = engine.EndReason();
    const bool abnormal =
        (reason != lyra::runtime::SimulationEndReason::kFinish);
    const bool forced = HasFlag(flags, FeatureFlag::kDumpSuspended);
    if (abnormal || forced) {
      auto snapshot = engine.TakeSchedulerSnapshot();
      lyra::runtime::Engine::RenderSchedulerSnapshot(stderr, snapshot);
    }
  }

  // Invoke cover hit callback if registered (test framework hook).
  if (engine.NumImmediateCoverSites() > 0) {
    auto callback = lyra::runtime::GetCoverHitCallback();
    if (callback) {
      std::vector<uint64_t> hits(engine.NumImmediateCoverSites());
      for (uint32_t i = 0; i < engine.NumImmediateCoverSites(); ++i) {
        hits[i] = engine.GetImmediateCoverHitCount(i);
      }
      callback(std::move(hits));
    }
  }

  // Invoke NBA stats callback if registered (test framework hook).
  {
    auto callback = lyra::runtime::GetNbaStatsCallback();
    if (callback) {
      const auto& stats = engine.GetStats().core;
      callback(
          lyra::runtime::NbaRoutingStats{
              .generic_queue = stats.nba_generic_queue,
              .deferred_local = stats.nba_deferred_local,
          });
    }
  }

  // Release string-typed slot handles before engine destruction.
  // The engine owns the slot meta registry needed for address resolution.
  engine.ReleaseStringSlots();

  return final_time;
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

extern "C" void LyraRegisterStrobe(
    void* engine_ptr, LyraStrobeProgramFn program, void* design_state,
    void* this_ptr, uint32_t instance_id) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  lyra::runtime::ObserverContext ctx{
      .this_ptr = this_ptr,
      .instance_id = lyra::runtime::InstanceId{instance_id},
  };
  engine->RegisterStrobe(program, design_state, ctx);
}

extern "C" void LyraTerminate(
    void* engine_ptr, uint32_t kind, int32_t level,
    LyraStringHandle /*message*/) {
  if (engine_ptr == nullptr) {
    throw lyra::common::InternalError(
        "LyraTerminate", "engine_ptr must not be null");
  }
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  uint64_t time = engine->CurrentTime();

  auto& out = engine->Output();
  out.DrainSimOutputBuffer();

  // Negative level treated as silent
  if (level >= 1) {
    // Out-of-range kind defaults to kFinish behavior
    switch (kind) {
      case 0:  // kFinish
      default:
        out.AppendSimOutputFragment(
            std::format("$finish called at time {}", time));
        out.FinishSimOutputRecord();
        break;
      case 1:
        throw lyra::common::InternalError(
            "LyraTerminate",
            "kFatal must be lowered via LyraEmitReport, not LyraTerminate");
      case 2:  // kStop
        // MVP: same as finish. Future: may pause for interactive debugger.
        out.AppendSimOutputFragment(
            std::format("$stop called at time {}", time));
        out.FinishSimOutputRecord();
        break;
      case 3:  // kExit
        // MVP: same as finish. Future: may have program-block semantics.
        out.AppendSimOutputFragment(
            std::format("$exit called at time {}", time));
        out.FinishSimOutputRecord();
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
      env != nullptr && std::string_view(env) != "") {
    resolved = env;
    return resolved.c_str();
  }

  // Priority 2: derive from executable directory
  // Layout: <dir>/<name> (produced by `lyra compile`)
  if (argv0 != nullptr && std::string_view(argv0) != "") {
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

extern "C" void LyraInitRuntime(uint32_t iteration_limit) {
  LyraSetIterationLimit(iteration_limit);
}

extern "C" void LyraReportTime(void* run_session_ptr) {
  if (run_session_ptr == nullptr) {
    throw lyra::common::InternalError(
        "LyraReportTime", "run_session_ptr must not be null");
  }
  auto* session = static_cast<lyra::runtime::RunSession*>(run_session_ptr);
  session->output.DrainSimOutputBuffer();
  session->output.WriteProtocolRecord(
      std::format("__LYRA_TIME__={}\n", FinalTime()));
  std::fflush(stdout);
}

extern "C" void LyraMonitorRegister(
    void* engine_ptr, LyraMonitorCheckProgramFn program, void* design_state,
    void* this_ptr, uint32_t instance_id, const void* initial_prev,
    uint32_t size) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  lyra::runtime::ObserverContext ctx{
      .this_ptr = this_ptr,
      .instance_id = lyra::runtime::InstanceId{instance_id},
  };
  engine->RegisterMonitor(program, design_state, ctx, initial_prev, size);
}

extern "C" void LyraMonitorSetEnabled(void* engine_ptr, bool enabled) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  engine->SetMonitorEnabled(enabled);
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
  auto offset_span = std::span(offsets, count);
  auto mask_span = std::span(masks, count);
  auto* base_bytes = static_cast<uint8_t*>(base);
  for (uint64_t i = 0; i < count; ++i) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    std::memcpy(base_bytes + offset_span[i], &mask_span[i], sizeof(uint8_t));
  }
}

extern "C" void LyraApply4StatePatches16(
    void* base, const uint64_t* offsets, const uint16_t* masks,
    uint64_t count) {
  if (count == 0 || base == nullptr || offsets == nullptr || masks == nullptr)
    return;
  auto offset_span = std::span(offsets, count);
  auto mask_span = std::span(masks, count);
  auto* base_bytes = static_cast<uint8_t*>(base);
  for (uint64_t i = 0; i < count; ++i) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    std::memcpy(base_bytes + offset_span[i], &mask_span[i], sizeof(uint16_t));
  }
}

extern "C" void LyraApply4StatePatches32(
    void* base, const uint64_t* offsets, const uint32_t* masks,
    uint64_t count) {
  if (count == 0 || base == nullptr || offsets == nullptr || masks == nullptr)
    return;
  auto offset_span = std::span(offsets, count);
  auto mask_span = std::span(masks, count);
  auto* base_bytes = static_cast<uint8_t*>(base);
  for (uint64_t i = 0; i < count; ++i) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    std::memcpy(base_bytes + offset_span[i], &mask_span[i], sizeof(uint32_t));
  }
}

extern "C" void LyraApply4StatePatches64(
    void* base, const uint64_t* offsets, const uint64_t* masks,
    uint64_t count) {
  if (count == 0 || base == nullptr || offsets == nullptr || masks == nullptr)
    return;
  auto offset_span = std::span(offsets, count);
  auto mask_span = std::span(masks, count);
  auto* base_bytes = static_cast<uint8_t*>(base);
  for (uint64_t i = 0; i < count; ++i) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    std::memcpy(base_bytes + offset_span[i], &mask_span[i], sizeof(uint64_t));
  }
}

extern "C" auto LyraResolveSlotPtr(void* engine_ptr, uint32_t slot_id)
    -> void* {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  return engine->ResolveSlotBytesMut(slot_id);
}

// R3 typed coordination helpers.
// Local helpers take (engine_ptr, instance_ptr, local_id, ...).
// Global helpers take (engine_ptr, global_id, ...).
// These do not assume a process frame -- they work in any module-scoped
// context (process bodies, user functions, comb kernels).
//
// Shared implementations are templated on Signal (ObjectSignalRef or
// GlobalSignalId) so local/global externs are thin wrappers with no
// duplicated mutation semantics.

namespace {

using lyra::runtime::Engine;
using lyra::runtime::GlobalSignalId;
using lyra::runtime::LocalSignalId;
using lyra::runtime::ObjectSignalRef;
using lyra::runtime::RuntimeInstance;

auto AsEngine(void* engine_ptr) -> Engine* {
  return static_cast<Engine*>(engine_ptr);
}

auto MakeLocalRef(void* instance_ptr, uint32_t local_id) -> ObjectSignalRef {
  return ObjectSignalRef{
      .instance = static_cast<RuntimeInstance*>(instance_ptr),
      .local = LocalSignalId{local_id}};
}

template <class Signal>
void MarkDirtyTyped(
    Engine* engine, Signal signal, uint32_t dirty_off, uint32_t dirty_size) {
  if (dirty_size > 0) {
    engine->MarkDirtyRange(signal, dirty_off, dirty_size);
  } else {
    engine->MarkDirty(signal);
  }
}

template <class Signal>
void StorePackedTyped(
    void* engine_ptr, Signal signal, void* slot_ptr, const void* new_value_ptr,
    uint32_t byte_size, uint32_t dirty_off, uint32_t dirty_size) {
  bool value_changed = std::memcmp(slot_ptr, new_value_ptr, byte_size) != 0;
  std::memcpy(slot_ptr, new_value_ptr, byte_size);
  if (!value_changed || engine_ptr == nullptr) return;
  MarkDirtyTyped(AsEngine(engine_ptr), signal, dirty_off, dirty_size);
}

template <class Signal>
void StoreStringTyped(
    void* engine_ptr, Signal signal, void* slot_ptr, void* new_str) {
  auto** str_slot = static_cast<void**>(slot_ptr);
  void* old_str = *str_slot;
  *str_slot = new_str;
  if (old_str == new_str || engine_ptr == nullptr) return;
  AsEngine(engine_ptr)->MarkDirty(signal);
}

}  // namespace

// Local helpers: (engine_ptr, instance_ptr, local_id, ...)
// Global helpers: (engine_ptr, global_id, ...)

extern "C" void LyraMarkDirtyLocal(
    void* eng, void* inst, uint32_t id, uint32_t off, uint32_t size) {
  MarkDirtyTyped(AsEngine(eng), MakeLocalRef(inst, id), off, size);
}
extern "C" void LyraMarkDirtyGlobal(
    void* eng, uint32_t id, uint32_t off, uint32_t size) {
  MarkDirtyTyped(AsEngine(eng), GlobalSignalId{id}, off, size);
}

extern "C" void LyraMarkDirtyExtRef(
    void* eng, void* inst, uint32_t ref_id, uint32_t off, uint32_t size) {
  auto* instance = static_cast<lyra::runtime::RuntimeInstance*>(inst);
  auto bindings =
      std::span(instance->ext_ref_bindings, instance->ext_ref_binding_count);
  const auto& binding = bindings[ref_id];
  auto target_instance_id =
      lyra::runtime::InstanceId{binding.target_instance_id};
  auto* engine = AsEngine(eng);
  auto& target = engine->GetInstanceMut(target_instance_id);
  MarkDirtyTyped(
      engine, MakeLocalRef(&target, binding.target_local_signal.value), off,
      size);
}

extern "C" void LyraStorePackedLocal(
    void* eng, void* inst, void* slot, const void* val, uint32_t bsz,
    uint32_t id, uint32_t off, uint32_t dsz) {
  StorePackedTyped(eng, MakeLocalRef(inst, id), slot, val, bsz, off, dsz);
}
extern "C" void LyraStorePackedGlobal(
    void* eng, void* slot, const void* val, uint32_t bsz, uint32_t id,
    uint32_t off, uint32_t dsz) {
  StorePackedTyped(eng, GlobalSignalId{id}, slot, val, bsz, off, dsz);
}

extern "C" void LyraStoreStringLocal(
    void* eng, void* inst, void* slot, void* str, uint32_t id) {
  StoreStringTyped(eng, MakeLocalRef(inst, id), slot, str);
}
extern "C" void LyraStoreStringGlobal(
    void* eng, void* slot, void* str, uint32_t id) {
  StoreStringTyped(eng, GlobalSignalId{id}, slot, str);
}

// Resolve a body-relative byte offset to a subspan in the deferred region.
// Offsets < inline_size resolve to deferred_inline_base.
// Offsets >= inline_size resolve to deferred_appendix_base.
inline auto ResolveDeferredSubspan(
    lyra::runtime::RuntimeInstanceStorage& storage, uint32_t body_offset,
    uint32_t size) -> std::span<std::byte> {
  auto inline_size = static_cast<uint32_t>(storage.inline_size);
  if (body_offset < inline_size) {
    return storage.DeferredInlineRegion().subspan(body_offset, size);
  }
  return storage.DeferredAppendixRegion().subspan(
      body_offset - inline_size, size);
}

// Resolve a body-relative byte offset to a subspan in the current region.
inline auto ResolveCurrentSubspan(
    lyra::runtime::RuntimeInstanceStorage& storage, uint32_t body_offset,
    uint32_t size) -> std::span<std::byte> {
  auto inline_size = static_cast<uint32_t>(storage.inline_size);
  if (body_offset < inline_size) {
    return storage.InlineRegion().subspan(body_offset, size);
  }
  return storage.AppendixRegion().subspan(body_offset - inline_size, size);
}

// Copy-on-first-touch: ensure deferred storage for signal `id` contains a
// valid full-slot snapshot. Called before any partial write to a signal that
// hasn't been touched yet this delta. After this, all bytes in the deferred
// slot match the current slot, so subsequent partial writes accumulate
// correctly and the commit can compare the full slot.
// Works for both inline and container-backed slots.
inline void EnsureDeferredSlotInitialized(
    RuntimeInstance* instance, lyra::runtime::NbaPendingSet& pending,
    uint32_t id) {
  if (pending.slot_initialized[id] != 0) return;
  const auto& meta = instance->observability.layout->slot_meta[id];
  uint32_t off =
      meta.is_container ? meta.backing_rel_off : meta.instance_rel_off;
  uint32_t bytes = meta.is_container ? meta.backing_bytes : meta.total_bytes;
  auto current = ResolveCurrentSubspan(instance->storage, off, bytes);
  auto deferred = ResolveDeferredSubspan(instance->storage, off, bytes);
  std::memcpy(deferred.data(), current.data(), bytes);
  pending.slot_initialized[id] = 1;
}

// Instance-owned deferred byte-range write for local NBA.
// Handles both whole-slot and sub-slot (field, array element, byte-addressable
// packed subview) writes. Identity-based: no current-storage pointers.
extern "C" void LyraDeferredWriteLocal(
    void* eng, void* inst, const void* vp, uint32_t bsz, uint32_t id,
    uint32_t body_offset, uint32_t is_partial) {
  auto* instance = static_cast<RuntimeInstance*>(inst);
  auto& pending = instance->nba_pending;

  if (is_partial != 0) {
    EnsureDeferredSlotInitialized(instance, pending, id);
  } else {
    pending.slot_initialized[id] = 1;
  }

  auto deferred_slot =
      ResolveDeferredSubspan(instance->storage, body_offset, bsz);
  std::memcpy(deferred_slot.data(), vp, bsz);
  pending.MarkPending(LocalSignalId{id});
  AsEngine(eng)->MarkInstanceNbaPending(*instance);
}

// Instance-owned deferred masked-merge write for local NBA.
// For bit-addressable packed subview writes where the store uses a mask.
// Always partial: copy-on-first-touch, then apply mask merge in deferred
// storage.
extern "C" void LyraDeferredMaskedWriteLocal(
    void* eng, void* inst, const void* vp, const void* mp, uint32_t bsz,
    uint32_t id, uint32_t body_offset) {
  auto* instance = static_cast<RuntimeInstance*>(inst);
  auto& pending = instance->nba_pending;

  EnsureDeferredSlotInitialized(instance, pending, id);

  auto deferred_slot =
      ResolveDeferredSubspan(instance->storage, body_offset, bsz);
  auto val_span = std::span(static_cast<const std::byte*>(vp), bsz);
  auto mask_span = std::span(static_cast<const std::byte*>(mp), bsz);
  for (uint32_t i = 0; i < bsz; ++i) {
    deferred_slot[i] =
        (deferred_slot[i] & ~mask_span[i]) | (val_span[i] & mask_span[i]);
  }
  pending.MarkPending(LocalSignalId{id});
  AsEngine(eng)->MarkInstanceNbaPending(*instance);
}

// Instance-owned deferred canonical packed two-plane write for local NBA.
// For byte-addressable 4-state packed subview writes with val+unk planes.
// Always partial: copy-on-first-touch, then write both planes in deferred
// storage.
extern "C" void LyraDeferredCanonicalPackedWriteLocal(
    void* eng, void* inst, const void* val, const void* unk,
    uint32_t region_bsz, uint32_t id, uint32_t body_offset,
    uint32_t second_region_offset) {
  auto* instance = static_cast<RuntimeInstance*>(inst);
  auto& pending = instance->nba_pending;

  EnsureDeferredSlotInitialized(instance, pending, id);

  auto deferred_val =
      ResolveDeferredSubspan(instance->storage, body_offset, region_bsz);
  std::memcpy(deferred_val.data(), val, region_bsz);
  auto deferred_unk = ResolveDeferredSubspan(
      instance->storage, body_offset + second_region_offset, region_bsz);
  std::memcpy(deferred_unk.data(), unk, region_bsz);
  pending.MarkPending(LocalSignalId{id});
  AsEngine(eng)->MarkInstanceNbaPending(*instance);
}

// Generic NBA queue entry points for cross-instance local and global targets.
// Instance-owned local targets use LyraDeferredWriteLocal and friends above.
extern "C" void LyraScheduleNbaLocal(
    void* eng, void* inst, void* wp, const void* nb, const void* vp,
    const void* mp, uint32_t bsz, uint32_t id) {
  AsEngine(eng)->ScheduleNbaCrossInstanceLocal(
      MakeLocalRef(inst, id), wp, nb, vp, mp, bsz);
}
extern "C" void LyraScheduleNbaGlobal(
    void* eng, void* wp, const void* nb, const void* vp, const void* mp,
    uint32_t bsz, uint32_t id) {
  AsEngine(eng)->ScheduleNbaGlobal(GlobalSignalId{id}, wp, nb, vp, mp, bsz);
}

extern "C" void LyraScheduleNbaCanonicalPackedLocal(
    void* eng, void* inst, void* wp, const void* nb, const void* vp,
    const void* up, uint32_t rsz, uint32_t sro, uint32_t id) {
  AsEngine(eng)->ScheduleNbaCanonicalPackedCrossInstanceLocal(
      MakeLocalRef(inst, id), wp, nb, vp, up, rsz, sro);
}
extern "C" void LyraScheduleNbaCanonicalPackedGlobal(
    void* eng, void* wp, const void* nb, const void* vp, const void* up,
    uint32_t rsz, uint32_t sro, uint32_t id) {
  AsEngine(eng)->ScheduleNbaCanonicalPackedGlobal(
      GlobalSignalId{id}, wp, nb, vp, up, rsz, sro);
}

extern "C" void LyraScheduleNbaExtRef(
    void* eng, void* inst, uint32_t ref_id, void* wp, const void* nb,
    const void* vp, const void* mp, uint32_t bsz) {
  auto* owner = static_cast<lyra::runtime::RuntimeInstance*>(inst);
  auto bindings =
      std::span(owner->ext_ref_bindings, owner->ext_ref_binding_count);
  const auto& binding = bindings[ref_id];
  auto* target = AsEngine(eng)->FindInstanceMut(
      lyra::runtime::InstanceId{binding.target_instance_id});
  if (target == nullptr || !target->nba_pending.IsInitialized()) {
    throw lyra::common::InternalError(
        "LyraScheduleNbaExtRef",
        std::format(
            "target instance_id {} not found or not initialized",
            binding.target_instance_id));
  }
  lyra::runtime::NbaNotifySignal notify{lyra::runtime::NbaNotifyLocal{
      .instance = target,
      .signal = lyra::runtime::LocalSignalId{binding.target_local_signal.value},
  }};
  AsEngine(eng)->ScheduleNba(wp, nb, vp, mp, bsz, notify);
}

extern "C" void LyraScheduleNbaCanonicalPackedExtRef(
    void* eng, void* inst, uint32_t ref_id, void* wp, const void* nb,
    const void* vp, const void* up, uint32_t rsz, uint32_t sro) {
  auto* owner = static_cast<lyra::runtime::RuntimeInstance*>(inst);
  auto bindings =
      std::span(owner->ext_ref_bindings, owner->ext_ref_binding_count);
  const auto& binding = bindings[ref_id];
  auto* target = AsEngine(eng)->FindInstanceMut(
      lyra::runtime::InstanceId{binding.target_instance_id});
  if (target == nullptr || !target->nba_pending.IsInitialized()) {
    throw lyra::common::InternalError(
        "LyraScheduleNbaCanonicalPackedExtRef",
        std::format(
            "target instance_id {} not found or not initialized",
            binding.target_instance_id));
  }
  lyra::runtime::NbaNotifySignal notify{lyra::runtime::NbaNotifyLocal{
      .instance = target,
      .signal = lyra::runtime::LocalSignalId{binding.target_local_signal.value},
  }};
  AsEngine(eng)->ScheduleNbaCanonicalPacked(wp, nb, vp, up, rsz, sro, notify);
}

// R5: Resolve RuntimeInstance* from InstanceId at runtime.
// Used by codegen for cross-instance writes (design-level connection
// processes writing to child instance signals).
extern "C" auto LyraResolveInstancePtr(void* eng, uint32_t instance_id)
    -> void* {
  if (eng == nullptr) return nullptr;
  return AsEngine(eng)->FindInstanceMut(lyra::runtime::InstanceId{instance_id});
}

extern "C" auto LyraIsTraceObservedLocal(void* eng, void* inst, uint32_t id)
    -> bool {
  if (eng == nullptr) return false;
  return AsEngine(eng)->IsTraceObserved(MakeLocalRef(inst, id));
}
extern "C" auto LyraIsTraceObservedGlobal(void* eng, uint32_t id) -> bool {
  if (eng == nullptr) return false;
  return AsEngine(eng)->IsTraceObserved(GlobalSignalId{id});
}

extern "C" void LyraNotifyContainerMutationLocal(
    void* eng, void* inst, uint32_t id, uint32_t off, uint32_t size) {
  MarkDirtyTyped(AsEngine(eng), MakeLocalRef(inst, id), off, size);
}
extern "C" void LyraNotifyContainerMutationGlobal(
    void* eng, uint32_t id, uint32_t off, uint32_t size) {
  MarkDirtyTyped(AsEngine(eng), GlobalSignalId{id}, off, size);
}

extern "C" void LyraNotifySignalLocal(void* eng, void* inst, uint32_t id) {
  if (eng == nullptr) return;
  AsEngine(eng)->MarkDirty(MakeLocalRef(inst, id));
}
extern "C" void LyraNotifySignalGlobal(void* eng, uint32_t id) {
  if (eng == nullptr) return;
  AsEngine(eng)->MarkDirty(GlobalSignalId{id});
}

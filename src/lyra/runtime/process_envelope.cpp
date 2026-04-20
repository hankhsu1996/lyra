#include "lyra/runtime/process_envelope.hpp"

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <format>
#include <span>
#include <variant>

#include "lyra/common/diagnostic/print.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/dpi_export_context.hpp"
#include "lyra/runtime/engine_process_envelope_access.hpp"
#include "lyra/runtime/instance_event_state.hpp"
#include "lyra/runtime/iteration_limit.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_requests.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace {

using Access = lyra::runtime::ProcessEnvelopeAccess;
using StateHeader = lyra::runtime::ProcessFrameHeader;

void ReleaseTriggerOverflow(lyra::runtime::SuspendRecord* suspend) {
  if (suspend->triggers_ptr != nullptr &&
      suspend->triggers_ptr != suspend->inline_triggers.data()) {
    delete[] suspend->triggers_ptr;
  }
  suspend->num_triggers = 0;
  suspend->triggers_ptr = nullptr;
  delete[] suspend->late_bound_ptr;
  suspend->num_late_bound = 0;
  suspend->late_bound_ptr = nullptr;
  delete[] suspend->plan_ops_ptr;
  suspend->num_plan_ops = 0;
  suspend->plan_ops_ptr = nullptr;
  delete[] suspend->dep_slots_ptr;
  suspend->num_dep_slots = 0;
  suspend->dep_slots_ptr = nullptr;
}

void SuspendReset(lyra::runtime::SuspendRecord* suspend) {
  ReleaseTriggerOverflow(suspend);
  *suspend = lyra::runtime::SuspendRecord{};
}

void FailIfSuspensionDisallowed(const char* api) {
  if (LyraIsDpiExportSuspensionDisallowed()) {
    throw lyra::common::InternalError(
        api, "non-suspending DPI export task attempted to suspend");
  }
}

// Envelope-internal process request variant. Never exposed publicly.
struct CompletedRequest {};
struct TrapRequest {
  lyra::runtime::TrapPayload payload = {};
};
using ProcessRequest = std::variant<
    CompletedRequest, TrapRequest, lyra::runtime::DelayRequest,
    lyra::runtime::WaitRequest, lyra::runtime::RepeatRequest,
    lyra::runtime::EventWaitRequest>;

// Whether the request requires resetting the installed wait state.
// All non-wait requests must tear down prior subscriptions.
auto NeedsWaitReset(const ProcessRequest& request) -> bool {
  return !std::holds_alternative<lyra::runtime::WaitRequest>(request) &&
         !std::holds_alternative<TrapRequest>(request);
}

// Decode a SuspendRecord into a semantic ProcessRequest.
// Spans in WaitRequest point into still-alive SuspendRecord storage.
auto DecodeSuspendRecord(lyra::runtime::SuspendRecord* suspend)
    -> ProcessRequest {
  using lyra::runtime::DelayRequest;
  using lyra::runtime::DepSignalRecord;
  using lyra::runtime::EventWaitRequest;
  using lyra::runtime::IndexPlanOp;
  using lyra::runtime::LateBoundHeader;
  using lyra::runtime::RepeatRequest;
  using lyra::runtime::ResumePoint;
  using lyra::runtime::SuspendTag;
  using lyra::runtime::WaitRequest;

  auto resume =
      ResumePoint{.block_index = suspend->resume_block, .instruction_index = 0};

  switch (suspend->tag) {
    case SuspendTag::kFinished:
      return CompletedRequest{};

    case SuspendTag::kDelay:
      return DelayRequest{.resume = resume, .ticks = suspend->delay_ticks};

    case SuspendTag::kWait: {
      if (suspend->num_triggers > 0 && suspend->triggers_ptr == nullptr) {
        throw lyra::common::InternalError(
            "DecodeSuspendRecord",
            "triggers_ptr null with non-zero num_triggers");
      }
      auto triggers = std::span(suspend->triggers_ptr, suspend->num_triggers);
      bool has_late_bound =
          suspend->num_late_bound > 0 && suspend->late_bound_ptr != nullptr;
      auto headers =
          has_late_bound
              ? std::span(suspend->late_bound_ptr, suspend->num_late_bound)
              : std::span<const LateBoundHeader>{};
      auto plan_ops =
          (suspend->plan_ops_ptr != nullptr)
              ? std::span(suspend->plan_ops_ptr, suspend->num_plan_ops)
              : std::span<const IndexPlanOp>{};
      auto dep_records =
          (suspend->dep_slots_ptr != nullptr)
              ? std::span(suspend->dep_slots_ptr, suspend->num_dep_slots)
              : std::span<const DepSignalRecord>{};
      return WaitRequest{
          .resume = resume,
          .wait_site_id = suspend->wait_site_id,
          .triggers = triggers,
          .late_bound = {
              .headers = headers,
              .plan_ops = plan_ops,
              .dep_slots = dep_records}};
    }

    case SuspendTag::kRepeat:
      return RepeatRequest{
          .resume = ResumePoint{.block_index = 0, .instruction_index = 0}};

    case SuspendTag::kWaitEvent:
      return EventWaitRequest{.resume = resume, .event_id = suspend->event_id};
  }

  throw lyra::common::InternalError(
      "DecodeSuspendRecord",
      std::format("unknown SuspendTag {}", static_cast<int>(suspend->tag)));
}

// Dispatch a process body and decode the raw protocol into a ProcessRequest.
auto DispatchProcess(
    std::span<void*> states, lyra::runtime::ProcessHandle handle,
    lyra::runtime::ResumePoint resume) -> ProcessRequest {
  using lyra::runtime::ProcessExitCode;
  using lyra::runtime::ProcessOutcome;
  using lyra::runtime::SuspendRecord;
  using lyra::runtime::SuspendTag;
  using lyra::runtime::TrapReason;

  uint32_t proc_idx = handle.process_id;
  void* state = states[proc_idx];
  auto* suspend = static_cast<SuspendRecord*>(state);

  if (resume.block_index != 0 && suspend->tag == SuspendTag::kWait &&
      suspend->triggers_ptr == suspend->inline_triggers.data()) {
    suspend->tag = SuspendTag::kFinished;
  } else {
    SuspendReset(suspend);
  }

  ProcessOutcome outcome{};
  outcome.tag = UINT32_MAX;

  // Uniform shared-body dispatch for all processes.
  auto* header = static_cast<StateHeader*>(state);
  header->outcome.tag = UINT32_MAX;
  header->body(state, resume.block_index);
  outcome = header->outcome;

  switch (static_cast<ProcessExitCode>(outcome.tag)) {
    case ProcessExitCode::kOk:
      return DecodeSuspendRecord(suspend);

    case ProcessExitCode::kTrap:
      return TrapRequest{
          .payload = {
              .reason = static_cast<TrapReason>(outcome.reason),
              .a = outcome.a,
              .b = outcome.b}};

    default:
      throw lyra::common::InternalError(
          "DispatchProcess",
          std::format(
              "process returned invalid exit tag {} (sentinel=codegen bug, "
              "other=unknown tag)",
              outcome.tag));
  }
}

// Envelope-owned wait lifecycle orchestration. Decides whether to do a
// direct trigger install, a full wait-site install, or an in-place refresh.
void HandleWaitRequest(
    lyra::runtime::Engine& engine, lyra::runtime::ProcessHandle handle,
    const lyra::runtime::WaitRequest& req) {
  // When post-activation reconciliation is active, the engine handles
  // wait lifecycle after the activation returns. The envelope must not
  // duplicate that work.
  if (Access::HasPostActivationReconciliation(engine)) {
    return;
  }
  if (!Access::UsesWaitSiteLifecycle(engine)) {
    Access::InstallTriggers(engine, handle, req);
    return;
  }

  if (req.wait_site_id == lyra::runtime::kInvalidWaitSiteId) {
    throw lyra::common::InternalError(
        "HandleWaitRequest",
        std::format(
            "process {} suspended with kWait but wait_site_id is invalid",
            handle.process_id));
  }

  if (!Access::CanRefreshInstalledWait(engine, handle, req.wait_site_id)) {
    Access::ResetInstalledWait(engine, handle);
    Access::InstallWaitSite(engine, handle, req);
    return;
  }

  if (!Access::HasPendingDirtyState(engine)) {
    return;
  }

  if (Access::RefreshInstalledSnapshots(engine, handle)) {
    Access::ResetInstalledWait(engine, handle);
    Access::InstallWaitSite(engine, handle, req);
  }
}

// Handle the decoded process request by calling narrow engine primitives.
void HandleProcessRequest(
    lyra::runtime::Engine& engine, lyra::runtime::ProcessHandle handle,
    const ProcessRequest& request) {
  using lyra::runtime::DelayRequest;
  using lyra::runtime::Engine;
  using lyra::runtime::EventWaiter;
  using lyra::runtime::EventWaitRequest;
  using lyra::runtime::RepeatRequest;
  using lyra::runtime::WaitRequest;

  // Single structural reset point: all non-wait, non-trap requests must
  // tear down prior subscriptions when wait-site lifecycle is active.
  // When post-activation reconciliation is active, it handles resets.
  if (Access::UsesWaitSiteLifecycle(engine) &&
      !Access::HasPostActivationReconciliation(engine) &&
      NeedsWaitReset(request)) {
    Access::ResetInstalledWait(engine, handle);
  }

  std::visit(
      [&](const auto& v) {
        using T = std::decay_t<decltype(v)>;

        if constexpr (std::is_same_v<T, CompletedRequest>) {
          // No scheduling action needed.

        } else if constexpr (std::is_same_v<T, TrapRequest>) {
          engine.HandleTrap(Access::GetProcess(engine, handle), v.payload);

        } else if constexpr (std::is_same_v<T, DelayRequest>) {
          engine.Delay(handle, v.resume, v.ticks);

        } else if constexpr (std::is_same_v<T, WaitRequest>) {
          HandleWaitRequest(engine, handle, v);

        } else if constexpr (std::is_same_v<T, RepeatRequest>) {
          engine.ScheduleNextDelta(handle, v.resume);

        } else if constexpr (std::is_same_v<T, EventWaitRequest>) {
          auto& inst = engine.GetProcessInstance(handle.process_id);
          Engine::AddInstanceEventWaiter(
              inst, v.event_id,
              EventWaiter{
                  .process_id = handle.process_id,
                  .instance = &inst,
                  .resume_block = v.resume.block_index,
              });
        }
      },
      request);
}

}  // namespace

namespace lyra::runtime {

void DispatchAndHandleActivation(
    std::span<void*> states, Engine& engine, ProcessHandle handle,
    ResumePoint resume) {
  auto request = DispatchProcess(states, handle, resume);
  HandleProcessRequest(engine, handle, request);
}

}  // namespace lyra::runtime

// All suspend protocol C ABI functions. These are the only code outside
// the envelope header that touches SuspendRecord fields directly.

extern "C" auto LyraAllocTriggers(uint32_t count)
    -> lyra::runtime::WaitTriggerRecord* {
  return new lyra::runtime::WaitTriggerRecord[count];
}

extern "C" void LyraFreeTriggers(lyra::runtime::WaitTriggerRecord* ptr) {
  delete[] ptr;
}

extern "C" void LyraSuspendDelay(
    void* state, uint64_t ticks, uint32_t resume_block) {
  FailIfSuspensionDisallowed("LyraSuspendDelay");
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  ReleaseTriggerOverflow(suspend);
  suspend->tag = lyra::runtime::SuspendTag::kDelay;
  suspend->delay_ticks = ticks;
  suspend->resume_block = resume_block;
}

extern "C" void LyraSuspendWait(
    void* state, uint32_t resume_block, const void* triggers,
    uint32_t num_triggers, uint32_t wait_site_id) {
  FailIfSuspensionDisallowed("LyraSuspendWait");
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
}

extern "C" void LyraSuspendWaitStatic(
    void* state, uint32_t resume_block, uint32_t wait_site_id) {
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  suspend->tag = lyra::runtime::SuspendTag::kWait;
  suspend->resume_block = resume_block;
  suspend->wait_site_id = wait_site_id;
}

extern "C" void LyraSuspendWaitWithLateBound(
    void* state, uint32_t resume_block, const void* triggers,
    uint32_t num_triggers, const void* headers, uint32_t num_headers,
    const void* plan_ops, uint32_t num_plan_ops, const void* dep_slots,
    uint32_t num_dep_slots, uint32_t wait_site_id) {
  FailIfSuspensionDisallowed("LyraSuspendWaitWithLateBound");
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
    suspend->dep_slots_ptr = new lyra::runtime::DepSignalRecord[num_dep_slots];
    std::memcpy(
        suspend->dep_slots_ptr, dep_slots,
        num_dep_slots * sizeof(lyra::runtime::DepSignalRecord));
  } else {
    suspend->dep_slots_ptr = nullptr;
  }
}

extern "C" void LyraSuspendRepeat(void* state) {
  FailIfSuspensionDisallowed("LyraSuspendRepeat");
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  ReleaseTriggerOverflow(suspend);
  suspend->tag = lyra::runtime::SuspendTag::kRepeat;
  suspend->resume_block = 0;
}

extern "C" void LyraSuspendWaitEvent(
    void* state, uint32_t resume_block, uint32_t event_id) {
  FailIfSuspensionDisallowed("LyraSuspendWaitEvent");
  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  ReleaseTriggerOverflow(suspend);
  suspend->tag = lyra::runtime::SuspendTag::kWaitEvent;
  suspend->resume_block = resume_block;
  suspend->event_id = event_id;
}

extern "C" void LyraRunProcessSync(LyraProcessFunc process, void* state) {
  constexpr uint32_t kEntryBlock = 0;

  auto* suspend = static_cast<lyra::runtime::SuspendRecord*>(state);
  suspend->tag = lyra::runtime::SuspendTag::kFinished;
  uint32_t limit = LyraGetIterationLimit();
  LyraResetIterationLimit(limit > 0 ? limit : UINT32_MAX);

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

  if (suspend->tag != lyra::runtime::SuspendTag::kFinished) {
    lyra::PrintError(
        std::format(
            "init process suspended (tag={}), aborting",
            static_cast<int>(suspend->tag)));
    std::abort();
  }
}

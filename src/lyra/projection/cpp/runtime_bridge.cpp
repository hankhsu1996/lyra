#include "lyra/projection/cpp/runtime_bridge.hpp"

#include <string>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_process_envelope_access.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/runtime_instance.hpp"

namespace lyra::projection::cpp {

namespace {

struct BridgeCtx {
  void* object_ptr = nullptr;
  SingleEntryFn entry_fn = nullptr;
};

// One-shot process completion adapter. Calls the single projected process
// body and marks it finished. The process is not re-scheduled.
// This bridge shape is valid only because this prototype has exactly one
// process. Hard-fail if the engine dispatches any other process_id.
void BridgeDispatch(
    void* raw_ctx, runtime::Engine& engine, runtime::ProcessHandle handle,
    runtime::ResumePoint /*resume*/) {
  if (handle.process_id != 0) {
    throw common::InternalError(
        "BridgeDispatch",
        "single-process prototype bridge dispatched with process_id != 0");
  }
  auto* ctx = static_cast<BridgeCtx*>(raw_ctx);
  ctx->entry_fn(ctx->object_ptr);
  runtime::ProcessEnvelopeAccess::SetProcessWaitKind(
      engine, handle.process_id, runtime::ProcessWaitKind::kFinished);
}

}  // namespace

auto RunSingleInitial(void* object_ptr, SingleEntryFn entry_fn) -> int {
  BridgeCtx bridge_ctx{.object_ptr = object_ptr, .entry_fn = entry_fn};

  // RuntimeInstance with default storage (all nullptr/zero).
  // The projected process body accesses object_ptr directly via the
  // self_void parameter in entry_fn. RuntimeInstance exists only to
  // satisfy Engine::SetInstances(). Its storage fields are not accessed
  // by projected code. We must NOT set inline_base to object_ptr because
  // RuntimeInstance destructor calls delete[] on inline_base.
  runtime::RuntimeInstance instance;
  instance.instance_id = runtime::InstanceId{0};

  const runtime::RuntimeInstance* inst_ptr = &instance;
  runtime::Engine engine(
      runtime::ProcessDispatch{.fn = &BridgeDispatch, .ctx = &bridge_ctx}, 1,
      /*plusargs=*/{},
      /*instance_paths=*/std::vector<std::string>{"unit"},
      /*feature_flags=*/0);

  engine.SetDesignStateBase(object_ptr);
  engine.SetInstances(
      std::span<const runtime::RuntimeInstance* const>{&inst_ptr, 1});

  engine.ScheduleInitial(
      runtime::ProcessHandle{
          .process_id = 0, .instance_id = runtime::InstanceId{0}});

  engine.Run();
  return 0;
}

}  // namespace lyra::projection::cpp

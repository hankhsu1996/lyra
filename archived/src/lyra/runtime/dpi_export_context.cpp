#include "lyra/runtime/dpi_export_context.hpp"

#include <cstdio>
#include <cstdlib>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/reporting.hpp"
#include "lyra/runtime/runtime_instance.hpp"

namespace {

// Context stack head. nullptr when no simulation is active.
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
lyra::runtime::DpiExportCallContext* g_context_head = nullptr;

// Root context frame owned by ScopedDpiExportCallContext.
// Avoids heap allocation for the simulation-lifetime root.
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
lyra::runtime::DpiExportCallContext g_root_context{};

}  // namespace

namespace lyra::runtime {

ScopedDpiExportCallContext::ScopedDpiExportCallContext(
    DpiExportCallContext ctx) {
  if (g_context_head != nullptr) {
    throw common::InternalError(
        "ScopedDpiExportCallContext",
        "nested DPI export-call context installation");
  }
  if (ctx.design_state == nullptr || ctx.engine == nullptr) {
    throw common::InternalError(
        "ScopedDpiExportCallContext",
        "DPI export-call context requires non-null design_state and engine");
  }
  g_root_context = ctx;
  g_root_context.prev = nullptr;
  g_context_head = &g_root_context;
}

ScopedDpiExportCallContext::~ScopedDpiExportCallContext() {
  g_root_context = {};
  g_context_head = nullptr;
}

}  // namespace lyra::runtime

extern "C" auto LyraGetDpiExportCallContext()
    -> const lyra::runtime::DpiExportCallContext* {
  return g_context_head;
}

extern "C" auto LyraGetDpiExportCallContextMut()
    -> lyra::runtime::DpiExportCallContext* {
  return g_context_head;
}

extern "C" [[noreturn]] auto LyraFailMissingDpiExportCallContext() -> void {
  std::fputs(
      "fatal: DPI export function called without active Lyra simulation "
      "context\n",
      stderr);
  std::abort();
}

extern "C" void LyraResolvePackageExportBinding(
    lyra::runtime::DpiResolvedPackageBinding* out) {
  auto* ctx = LyraGetDpiExportCallContextMut();
  if (ctx == nullptr) {
    LyraFailMissingDpiExportCallContext();
  }
  out->design_state = ctx->design_state;
  out->engine = ctx->engine;
  out->decision_owner_id_raw = ctx->decision_owner.has_value
                                   ? ctx->decision_owner.value.Index()
                                   : UINT32_MAX;
  out->has_decision_owner = ctx->decision_owner.has_value;
}

// NOLINTBEGIN(cppcoreguidelines-pro-type-const-cast)

extern "C" void LyraResolveModuleInstanceBinding(
    lyra::runtime::DpiResolvedModuleBinding* out) {
  auto* ctx = LyraGetDpiExportCallContextMut();
  if (ctx == nullptr) {
    LyraFailMissingDpiExportCallContext();
  }
  if (ctx->active_scope == nullptr) {
    LyraFailMissingModuleExportScope();
  }
  auto* engine = static_cast<lyra::runtime::Engine*>(ctx->engine);
  // Validate the scope handle, then narrow to an instance -- DPI module
  // exports only bind to module instances, not generate scopes.
  auto* scope = const_cast<lyra::runtime::RuntimeScope*>(
      engine->ValidateScopeHandle(ctx->active_scope));
  auto* inst = lyra::runtime::ScopeAsInstanceChecked(scope);
  out->design_state = ctx->design_state;
  out->engine = ctx->engine;
  out->this_ptr = inst->storage.inline_base;
  out->instance_ptr = inst;
  out->decision_owner_id_raw = ctx->decision_owner.has_value
                                   ? ctx->decision_owner.value.Index()
                                   : UINT32_MAX;
  out->has_decision_owner = ctx->decision_owner.has_value;
}

// NOLINTEND(cppcoreguidelines-pro-type-const-cast)

extern "C" [[noreturn]] auto LyraFailMissingModuleExportScope() -> void {
  std::fputs(
      "fatal: module-scoped DPI export function called without active "
      "instance scope (svSetScope required)\n",
      stderr);
  std::abort();
}

extern "C" void LyraPushCurrentDpiScope(
    lyra::runtime::DpiContextSnapshot* out_prev, svScope new_scope,
    uint32_t owner_id_raw, bool has_owner) {
  auto* ctx = LyraGetDpiExportCallContextMut();
  if (ctx == nullptr) {
    LyraFailMissingDpiExportCallContext();
  }
  out_prev->active_scope = ctx->active_scope;
  out_prev->decision_owner = ctx->decision_owner;
  ctx->active_scope = new_scope;
  ctx->decision_owner =
      has_owner
          ? lyra::semantic::
                OptionalDecisionOwnerId{.value = lyra::semantic::DecisionOwnerId::FromIndex(owner_id_raw), .has_value = true}
          : lyra::semantic::OptionalDecisionOwnerId{};
}

extern "C" void LyraPopCurrentDpiScope(
    const lyra::runtime::DpiContextSnapshot* prev) {
  auto* ctx = LyraGetDpiExportCallContextMut();
  if (ctx == nullptr) {
    LyraFailMissingDpiExportCallContext();
  }
  ctx->active_scope = prev->active_scope;
  ctx->decision_owner = prev->decision_owner;
}

extern "C" void LyraPushDpiExportCallContext(bool suspension_disallowed) {
  if (g_context_head == nullptr) {
    throw lyra::common::InternalError(
        "LyraPushDpiExportCallContext",
        "push without active simulation context");
  }
  // Inherit design_state, engine, active_scope, and decision_owner from
  // current head. Only suspension_disallowed is set per-call.
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  auto* frame = new lyra::runtime::DpiExportCallContext{
      .prev = g_context_head,
      .design_state = g_context_head->design_state,
      .engine = g_context_head->engine,
      .active_scope = g_context_head->active_scope,
      .suspension_disallowed = suspension_disallowed,
      .decision_owner = g_context_head->decision_owner,
  };
  g_context_head = frame;
}

extern "C" void LyraPopDpiExportCallContext() {
  if (g_context_head == nullptr) {
    throw lyra::common::InternalError(
        "LyraPopDpiExportCallContext", "pop without matching push");
  }
  auto* frame = g_context_head;
  g_context_head = frame->prev;
  // Do not delete the root frame (it's stack-allocated).
  if (frame != &g_root_context) {
    // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
    delete frame;
  }
}

extern "C" auto LyraIsDpiExportSuspensionDisallowed() -> bool {
  return g_context_head != nullptr && g_context_head->suspension_disallowed;
}

extern "C" void LyraReportMissingDecisionOwnerFatal(
    void* engine_ptr, const char* export_name) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  lyra::runtime::EmitReport(
      engine,
      lyra::runtime::ReportRequest{
          .kind = lyra::runtime::ReportKind::kFatalTermination,
          .severity = lyra::Severity::kError,
          .origin = std::nullopt,
          .message = std::format(
              "DPI export '{}' requires an active decision owner context; "
              "no owner is available in the current DPI call context",
              export_name),
          .action = lyra::runtime::ReportAction::kFinish,
      });
}

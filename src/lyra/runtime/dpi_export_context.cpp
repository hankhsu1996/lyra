#include "lyra/runtime/dpi_export_context.hpp"

#include <cstdio>
#include <cstdlib>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/runtime_instance.hpp"

namespace {

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
bool g_context_active = false;
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
lyra::runtime::DpiExportCallContext g_context{};

}  // namespace

namespace lyra::runtime {

ScopedDpiExportCallContext::ScopedDpiExportCallContext(
    DpiExportCallContext ctx) {
  if (g_context_active) {
    throw common::InternalError(
        "ScopedDpiExportCallContext",
        "nested DPI export-call context installation");
  }
  if (ctx.design_state == nullptr || ctx.engine == nullptr) {
    throw common::InternalError(
        "ScopedDpiExportCallContext",
        "DPI export-call context requires non-null design_state and engine");
  }
  g_context = ctx;
  g_context_active = true;
}

ScopedDpiExportCallContext::~ScopedDpiExportCallContext() {
  g_context = {};
  g_context_active = false;
}

}  // namespace lyra::runtime

extern "C" auto LyraGetDpiExportCallContext()
    -> const lyra::runtime::DpiExportCallContext* {
  if (!g_context_active) {
    return nullptr;
  }
  return &g_context;
}

extern "C" auto LyraGetDpiExportCallContextMut()
    -> lyra::runtime::DpiExportCallContext* {
  if (!g_context_active) {
    return nullptr;
  }
  return &g_context;
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
  auto* inst = const_cast<lyra::runtime::RuntimeInstance*>(
      engine->ValidateScopeHandle(ctx->active_scope));
  out->design_state = ctx->design_state;
  out->engine = ctx->engine;
  out->this_ptr = inst->storage.inline_base;
  out->instance_ptr = inst;
  out->instance_id = inst->instance_id.value;
}

// NOLINTEND(cppcoreguidelines-pro-type-const-cast)

extern "C" [[noreturn]] auto LyraFailMissingModuleExportScope() -> void {
  std::fputs(
      "fatal: module-scoped DPI export function called without active "
      "instance scope (svSetScope required)\n",
      stderr);
  std::abort();
}

extern "C" auto LyraPushCurrentDpiScope(svScope new_scope) -> svScope {
  auto* ctx = LyraGetDpiExportCallContextMut();
  if (ctx == nullptr) {
    LyraFailMissingDpiExportCallContext();
  }
  svScope prev = ctx->active_scope;
  ctx->active_scope = new_scope;
  return prev;
}

extern "C" auto LyraPopCurrentDpiScope(svScope prev_scope) -> void {
  auto* ctx = LyraGetDpiExportCallContextMut();
  if (ctx == nullptr) {
    LyraFailMissingDpiExportCallContext();
  }
  ctx->active_scope = prev_scope;
}

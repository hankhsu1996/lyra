#include "lyra/runtime/dpi_export_context.hpp"

#include <cstdio>
#include <cstdlib>

#include "lyra/common/internal_error.hpp"

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

extern "C" [[noreturn]] void LyraFailMissingDpiExportCallContext() {
  std::fputs(
      "fatal: DPI export function called without active Lyra simulation "
      "context\n",
      stderr);
  std::abort();
}

extern "C" svScope LyraPushCurrentDpiScope(svScope new_scope) {
  auto* ctx = LyraGetDpiExportCallContextMut();
  if (ctx == nullptr) {
    LyraFailMissingDpiExportCallContext();
  }
  svScope prev = ctx->active_scope;
  ctx->active_scope = new_scope;
  return prev;
}

extern "C" void LyraPopCurrentDpiScope(svScope prev_scope) {
  auto* ctx = LyraGetDpiExportCallContextMut();
  if (ctx == nullptr) {
    LyraFailMissingDpiExportCallContext();
  }
  ctx->active_scope = prev_scope;
}

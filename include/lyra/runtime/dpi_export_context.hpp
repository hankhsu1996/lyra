#pragma once

#include "svdpi.h"

// Runtime export-call context for DPI export wrappers.
//
// DPI export wrappers need DesignState* and Engine* to call internal SV
// functions, but foreign callers must not provide those. This facility
// installs a narrow context during simulation that wrappers can borrow.
//
// Contract:
// - ScopedDpiExportCallContext is installed around LyraRunSimulation.
// - LyraGetDpiExportCallContext returns the active context, or nullptr.
// - LyraFailMissingDpiExportCallContext traps if called outside simulation.
// - Single-threaded: no concurrent installation, no TLS.
// - active_scope is set/read by svSetScope/svGetScope (D6b).

namespace lyra::runtime {

// Active export-call context.
// design_state and engine are simulation-lifetime base context.
// active_scope is dynamic DPI scope state set by svSetScope (D6b).
struct DpiExportCallContext {
  void* design_state = nullptr;
  void* engine = nullptr;
  svScope active_scope = nullptr;
};

// RAII guard: installs context on construction, removes on destruction.
// Asserts no nested installation (single-threaded model).
class ScopedDpiExportCallContext {
 public:
  explicit ScopedDpiExportCallContext(DpiExportCallContext ctx);
  ~ScopedDpiExportCallContext();

  ScopedDpiExportCallContext(const ScopedDpiExportCallContext&) = delete;
  auto operator=(const ScopedDpiExportCallContext&)
      -> ScopedDpiExportCallContext& = delete;
  ScopedDpiExportCallContext(ScopedDpiExportCallContext&&) = delete;
  auto operator=(ScopedDpiExportCallContext&&)
      -> ScopedDpiExportCallContext& = delete;
};

}  // namespace lyra::runtime

extern "C" {

// Returns the active DPI export-call context, or nullptr if no simulation
// is active. Called by LLVM-generated export wrappers.
auto LyraGetDpiExportCallContext()
    -> const lyra::runtime::DpiExportCallContext*;

// Mutable variant for svSetScope (D6b). Returns nullptr if no active
// simulation. Used by scope APIs that need to mutate active_scope.
auto LyraGetDpiExportCallContextMut() -> lyra::runtime::DpiExportCallContext*;

// Deterministic trap when an export wrapper is called without an active
// simulation context. Called by LLVM-generated export wrappers when
// LyraGetDpiExportCallContext returns nullptr.
[[noreturn]] void LyraFailMissingDpiExportCallContext();

// Compiler-internal current-DPI-scope push/pop.
// Used by compiler-generated DPI call boundaries that must expose a
// specific call-site scope through svGetScope() during foreign execution.
// Do not call svSetScope() from compiler-generated IR.

// Sets active_scope to new_scope, returns previous scope for pop.
// Does NOT validate new_scope (compiler-generated: scope is either a valid
// RuntimeInstance* from instance_ptr or nullptr).
svScope LyraPushCurrentDpiScope(svScope new_scope);

// Restores active_scope to prev_scope after context import returns.
void LyraPopCurrentDpiScope(svScope prev_scope);
}

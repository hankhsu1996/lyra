#pragma once

#include "lyra/semantic/decision.hpp"
#include "svdpi.h"

// Runtime export-call context for DPI export wrappers.
//
// DPI export wrappers need DesignState* and Engine* to call internal SV
// functions, but foreign callers must not provide those. This facility
// installs a narrow context during simulation that wrappers can borrow.
//
// Context frames form a stack (linked via prev pointer). The root frame
// is pushed by ScopedDpiExportCallContext around LyraRunSimulation.
// Per-wrapper-call frames are pushed by LyraPushDpiExportCallContext
// to carry suspension_disallowed for D7a export tasks.
//
// Contract:
// - LyraGetDpiExportCallContext returns the top frame, or nullptr.
// - LyraFailMissingDpiExportCallContext traps if called outside simulation.
// - Single-threaded: no concurrent installation, no TLS.
// - active_scope is set/read by svSetScope/svGetScope (D6b).

namespace lyra::runtime {

// Export-call context frame (stack node).
// design_state and engine are simulation-lifetime base context.
// active_scope is dynamic DPI scope state set by svSetScope (D6b).
// suspension_disallowed is true for D7a non-suspending export tasks.
struct DpiExportCallContext {
  DpiExportCallContext* prev = nullptr;
  void* design_state = nullptr;
  void* engine = nullptr;
  svScope active_scope = nullptr;
  bool suspension_disallowed = false;
  semantic::OptionalDecisionOwnerId decision_owner;
};

// Snapshot of mutable DPI context fields for save/restore across import calls.
// Scope and owner move together so the foreign boundary is one atomic
// operation.
struct DpiContextSnapshot {
  svScope active_scope = nullptr;
  semantic::OptionalDecisionOwnerId decision_owner;
};

// Resolved binding for package-scoped DPI export wrappers.
// Contains simulation-lifetime base context + decision owner from DPI context.
struct DpiResolvedPackageBinding {
  void* design_state = nullptr;
  void* engine = nullptr;
  uint32_t decision_owner_id_raw = UINT32_MAX;
  bool has_decision_owner = false;
};

// Resolved binding for module-scoped DPI export wrappers (D4a).
// Contains simulation-lifetime base context, instance-binding triple,
// and decision owner from DPI context.
struct DpiResolvedModuleBinding {
  void* design_state = nullptr;
  void* engine = nullptr;
  void* this_ptr = nullptr;
  void* instance_ptr = nullptr;
  uint32_t instance_id = 0;
  uint32_t decision_owner_id_raw = UINT32_MAX;
  bool has_decision_owner = false;
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

// Resolve package-scoped export binding: design_state + engine only.
// Called by LLVM-generated package export wrappers.
// Writes result to caller-provided output pointer.
void LyraResolvePackageExportBinding(
    lyra::runtime::DpiResolvedPackageBinding* out);

// Resolve module-scoped export instance binding: design_state + engine +
// this_ptr + instance_ptr + instance_id.
// Fails fatally if no active simulation context or no active scope.
// Called by LLVM-generated module export wrappers (D4a).
// Writes result to caller-provided output pointer.
void LyraResolveModuleInstanceBinding(
    lyra::runtime::DpiResolvedModuleBinding* out);

// Deterministic trap when a module-scoped export wrapper is called without
// an active instance scope (svSetScope not called).
[[noreturn]] void LyraFailMissingModuleExportScope();

// Compiler-internal current-DPI-scope push/pop.
// Used by compiler-generated DPI call boundaries that must expose a
// specific call-site scope through svGetScope() during foreign execution.
// Do not call svSetScope() from compiler-generated IR.
//
// Push saves scope + decision owner into *out_prev, installs new values.
// Pop restores from snapshot. Scope and owner move together atomically.

// Saves current scope + owner into *out_prev, installs new values.
void LyraPushCurrentDpiScope(
    lyra::runtime::DpiContextSnapshot* out_prev, svScope new_scope,
    uint32_t owner_id_raw, bool has_owner);

// Restores scope + owner from snapshot after context import returns.
void LyraPopCurrentDpiScope(const lyra::runtime::DpiContextSnapshot* prev);

// Per-wrapper-call context push/pop for DPI export wrappers (D7a).
// Pushes a nested frame that inherits design_state, engine, and
// active_scope from the current head context. Only suspension_disallowed
// is set per-call. This avoids passing raw pointers through codegen for
// state that the runtime already owns.
void LyraPushDpiExportCallContext(bool suspension_disallowed);
void LyraPopDpiExportCallContext();

// Returns true if the current export-call context disallows suspension.
// Used by suspension entrypoints as defense-in-depth guard.
auto LyraIsDpiExportSuspensionDisallowed() -> bool;
}

#include "lyra/runtime/dpi_context.hpp"

#include <cstdint>
#include <optional>
#include <variant>

#include "lyra/abi/records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/dpi_scope_handles.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/finish.hpp"
#include "lyra/runtime/foreign_execution.hpp"
#include "lyra/runtime/import_call_chain.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/vpi_time.hpp"
#include "svdpi.h"

namespace lyra::runtime {

void EnterDpiScope(
    RuntimeEffects& effects, Scope* decl_scope, base::SourceLocation at) {
  effects.Running().import_calls.Enter(decl_scope, at);
}

void LeaveDpiScope(RuntimeEffects& effects) {
  // The extent is left on every way out of it, and one of those ways runs with
  // nothing of the design executing: the frame of an execution still suspended
  // when the run ends is released where it stands, which runs the cleanups it
  // was holding open. The chain that entry belonged to is going with it.
  if (RunningState* running = effects.TryRunning(); running != nullptr) {
    running->import_calls.Leave();
  }
}

auto CurrentDpiScope() -> CurrentDpiHandle {
  RunningState* running = AmbientRunContext::Current().Effects().TryRunning();
  if (running == nullptr) {
    return static_cast<Scope*>(nullptr);
  }
  return running->import_calls.Current();
}

auto ReplaceDpiScope(CurrentDpiHandle handle) -> CurrentDpiHandle {
  RunningState* running = AmbientRunContext::Current().Effects().TryRunning();
  if (running == nullptr) {
    return static_cast<Scope*>(nullptr);
  }
  return running->import_calls.Replace(handle);
}

auto HandStringToForeignCaller(
    RuntimeEffects& effects, const value::String& text) -> const char* {
  return effects.Running().import_calls.HandToForeignCaller(text);
}

auto DisableIsActive(RuntimeEffects& effects) -> std::int32_t {
  return RunningIsDisabled(effects) ? 1 : 0;
}

auto ExportedTaskReturnValue(RuntimeEffects& effects) -> std::int32_t {
  if (const RunningState* running = effects.TryRunning();
      running != nullptr && running->ended_the_run &&
      ForeignExecutionIsRunning()) {
    for (;;) {
      YieldForeignExecution();
    }
  }
  return DisableIsActive(effects);
}

void AcknowledgeStop(RuntimeEffects& effects) {
  if (RunningState* running = effects.TryRunning(); running != nullptr) {
    running->import_calls.AcknowledgeStop();
  }
}

namespace {

// What foreign code asked of the run while the call returning now was the
// innermost, done now that it returns (LRM 38.4), as the design's own task
// would be done where the call is written. A task import returns on a stack of
// its own, so nothing is raised here: the execution leaves at the boundary the
// call returns through, as it does for a departure an export's body owed.
void ActOnRequestedControl(RuntimeEffects& effects, base::SourceLocation at) {
  RunningState* running = effects.TryRunning();
  if (running == nullptr) {
    return;
  }
  const std::optional<ForeignControl> requested =
      running->import_calls.TakeRequestedControl();
  if (!requested.has_value()) {
    return;
  }
  switch (requested->task) {
    case ForeignControl::Task::kFinish:
      FinishWithoutRaising(effects, requested->finish_number, at);
      return;
    case ForeignControl::Task::kStop:
      StopWithoutRaising(effects, requested->finish_number, at);
      return;
  }
  throw InternalError("a requested control names no task");
}

}  // namespace

void ContextImportTaskReturned(
    RuntimeEffects& effects, std::int32_t returned, base::SourceLocation at) {
  if (RunningIsDisabled(effects) && returned != 1) {
    // LRM 35.9 item b.
    const base::SourceLocationScope bound(at);
    effects.ReportDesignFailure(
        base::DiagMessage(
            "an imported task returned while its execution thread was in the "
            "disabled state without returning 1"));
  }
  ActOnRequestedControl(effects, at);
}

void ContextImportFunctionReturned(
    RuntimeEffects& effects, base::SourceLocation at) {
  RunningState* running = effects.TryRunning();
  if (RunningIsDisabled(effects) &&
      (running == nullptr ||
       !running->import_calls.InnermostAcknowledgedStop())) {
    // LRM 35.9 item c.
    const base::SourceLocationScope bound(at);
    effects.ReportDesignFailure(
        base::DiagMessage(
            "an imported function returned while its execution thread was in "
            "the disabled state without calling 'svAckDisabledState'"));
  }
  ActOnRequestedControl(effects, at);
}

auto RequestControl(RuntimeEffects& effects, ForeignControl control) -> bool {
  RunningState* running = effects.TryRunning();
  return running != nullptr && running->import_calls.RequestControl(control);
}

void CheckExportReachable(RuntimeEffects& effects) {
  RequireAContextImportCall(effects);
  if (!RunningIsDisabled(effects)) {
    return;
  }
  // LRM 35.9 item d.
  const base::SourceLocationScope bound(ImportCallWrittenAt(effects));
  effects.ReportDesignFailure(
      base::DiagMessage(
          "foreign code called an exported subroutine after its execution "
          "thread entered the disabled state"));
}

void BeforeForeignCodeRuns(RuntimeEffects& effects) {
  effects.Files().BeforeForeignCodeRuns();
}

auto ImportCallWrittenAt(RuntimeEffects& effects) -> base::SourceLocation {
  return effects.Running().import_calls.WrittenAt();
}

void RequireAContextImportCall(RuntimeEffects& effects) {
  if (const RunningState* running = effects.TryRunning();
      running != nullptr && running->import_calls.InsideACall()) {
    return;
  }
  throw SimulationError(
      base::kNoSourceLocation,
      "foreign code called an exported subroutine from an imported "
      "subroutine that is not declared 'context'");
}

}  // namespace lyra::runtime

namespace {

// The scope handles this run has given foreign code, which answer the name and
// user-data queries.
auto Handles() -> lyra::runtime::DpiScopeHandles& {
  return lyra::runtime::AmbientRunContext::Current().ScopeHandles();
}

// What foreign code is given for the handle an import call has current: a
// scope, which it may pass back from then on, or the address it set, which
// stays no scope.
auto HandOut(const lyra::runtime::CurrentDpiHandle& handle) -> void* {
  return std::visit(
      lyra::Overloaded{
          [](lyra::runtime::Scope* scope) -> void* {
            return Handles().Issue(scope);
          },
          [](const lyra::runtime::UnissuedHandle& unissued) {
            return unissued.address;
          }},
      handle);
}

// The run a time query reads its clock and its precision from.
auto Effects() -> lyra::runtime::RuntimeEffects& {
  return lyra::runtime::AmbientRunContext::Current().Effects();
}

// Resolves a time-query scope handle: a null handle is legal (the query is at
// the simulation level), a non-null one must have been handed out. Returns
// false for any other, which the query reports as an error.
auto ResolveTimeScope(svScope scope, const lyra::runtime::Scope** resolved)
    -> bool {
  if (scope == nullptr) {
    *resolved = nullptr;
    return true;
  }
  const auto* handle = static_cast<const lyra::runtime::Scope*>(scope);
  if (!Handles().IsIssued(handle)) {
    return false;
  }
  *resolved = handle;
  return true;
}

// The definition of the scope a time query names, none where it names none.
auto DefinitionOf(const lyra::runtime::Scope* scope)
    -> const lyra::abi::DefinitionRecord* {
  return scope == nullptr ? nullptr : &scope->Definition();
}

}  // namespace

// The Annex H context, caller, time and disable surface, linked into the
// simulation binary and resolved against the user's C by name. An `svScope` is
// a `runtime::Scope*` the C layer handed out. The current scope, and the
// caller's place, are those of the innermost entry of the running import call
// chain; the handles handed out answer the name and user-data queries; a time
// query reports the scope's unit or precision, or the simulation time unit for
// a null scope (LRM H.13); and the two disable entries are how foreign code
// reads and answers the protocol of LRM 35.9. Every entry that answers a scope
// hands it out, which is what makes it one foreign code may pass back. Errors
// follow the svdpi contract -- a null handle, null out slot, or invalid handle
// yields the documented null / -1 -- and nothing here throws across the C
// boundary.
extern "C" {

auto svGetScope() -> svScope {
  return HandOut(lyra::runtime::CurrentDpiScope());
}

// Whether the address is a scope is decided here, where it arrives, because
// only a scope this run handed out is one (LRM Annex H.9.3).
auto svSetScope(const svScope scope) -> svScope {
  auto* given = static_cast<lyra::runtime::Scope*>(scope);
  const lyra::runtime::CurrentDpiHandle installed =
      given == nullptr || Handles().IsIssued(given)
          ? lyra::runtime::CurrentDpiHandle{given}
          : lyra::runtime::CurrentDpiHandle{
                lyra::runtime::UnissuedHandle{.address = scope}};
  return HandOut(lyra::runtime::ReplaceDpiScope(installed));
}

auto svGetNameFromScope(const svScope scope) -> const char* {
  return Handles().NameOf(static_cast<const lyra::runtime::Scope*>(scope));
}

auto svGetScopeFromName(const char* scopeName) -> svScope {
  return scopeName == nullptr
             ? nullptr
             : Handles().Issue(Handles().ScopeOfName(scopeName));
}

auto svPutUserData(const svScope scope, void* userKey, void* userData) -> int {
  return Handles().PutUserData(
      static_cast<const lyra::runtime::Scope*>(scope), userKey, userData);
}

auto svGetUserData(const svScope scope, void* userKey) -> void* {
  return Handles().GetUserData(
      static_cast<const lyra::runtime::Scope*>(scope), userKey);
}

// Where the design wrote the context import call running now, which the call
// chain keeps for every such call (LRM Annex H.9.3). Outside one there is no
// call to answer for, and a call the design did not write has no place.
auto svGetCallerInfo(const char** fileName, int* lineNumber) -> int {
  const lyra::runtime::RunningState* running = Effects().TryRunning();
  if (running == nullptr || !running->import_calls.InsideACall()) {
    return 0;
  }
  const lyra::base::SourceLocation written_at =
      running->import_calls.WrittenAt();
  if (!written_at.IsValid()) {
    return 0;
  }
  *fileName = written_at.File();
  *lineNumber = static_cast<int>(written_at.Line());
  return 1;
}

auto svGetTime(const svScope scope, svTimeVal* time) -> int {
  if (time == nullptr) {
    return -1;
  }
  const lyra::runtime::Scope* resolved = nullptr;
  if (!ResolveTimeScope(scope, &resolved)) {
    return -1;
  }
  // An svTimeVal is an s_vpi_time (LRM H.13).
  return lyra::runtime::FillTime(
             Effects(),
             lyra::runtime::TimeResolutionOf(Effects(), DefinitionOf(resolved))
                 .unit_power,
             *time)
             ? 0
             : -1;
}

auto svGetTimeUnit(const svScope scope, std::int32_t* time_unit) -> int {
  if (time_unit == nullptr) {
    return -1;
  }
  const lyra::runtime::Scope* resolved = nullptr;
  if (!ResolveTimeScope(scope, &resolved)) {
    return -1;
  }
  *time_unit =
      lyra::runtime::TimeResolutionOf(Effects(), DefinitionOf(resolved))
          .unit_power;
  return 0;
}

auto svGetTimePrecision(const svScope scope, std::int32_t* time_precision)
    -> int {
  if (time_precision == nullptr) {
    return -1;
  }
  const lyra::runtime::Scope* resolved = nullptr;
  if (!ResolveTimeScope(scope, &resolved)) {
    return -1;
  }
  *time_precision =
      lyra::runtime::TimeResolutionOf(Effects(), DefinitionOf(resolved))
          .precision_power;
  return 0;
}

auto svIsDisabledState() -> int {
  return lyra::runtime::DisableIsActive(Effects());
}

void svAckDisabledState() {
  lyra::runtime::AcknowledgeStop(Effects());
}

}  // extern "C"

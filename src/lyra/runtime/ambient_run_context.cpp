#include "lyra/runtime/ambient_run_context.hpp"

#include <string_view>

#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/dpi_context.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

namespace {

auto CurrentSlot() -> AmbientRunContext*& {
  static thread_local AmbientRunContext* context = nullptr;
  return context;
}

}  // namespace

AmbientRunContext::AmbientRunContext(Scope* root, RuntimeEffects& effects)
    : effects_(&effects), scope_registry_(root), previous_(CurrentSlot()) {
  CurrentSlot() = this;
}

AmbientRunContext::~AmbientRunContext() {
  CurrentSlot() = previous_;
}

auto AmbientRunContext::Current() -> AmbientRunContext& {
  if (CurrentSlot() == nullptr) {
    // LRM 35.5.3.
    throw SimulationError(
        "a DPI export was reached with no simulation running, and an exported "
        "subroutine is callable only from a foreign call the simulation "
        "made");
  }
  return *CurrentSlot();
}

auto CurrentExportScope() -> Scope* {
  Scope* scope = CurrentDpiScope();
  if (scope == nullptr) {
    // The foreign side reached an instance-bound export with no scope
    // established. An import declared in a package or at `$unit` scope observes
    // no scope of its own (LRM 35.5.3), so reaching such an export from one
    // requires svSetScope first -- the foreign side's contract to meet, not an
    // invariant this compiler established.
    throw SimulationError(
        "a DPI export was reached without a scope; an export is callable only "
        "from a context import declared in an instantiated scope, or after "
        "'svSetScope' names one");
  }
  return scope;
}

auto FindExportEntry(Scope* scope, const char* subroutine) -> ErasedEntry {
  const std::string_view wanted{subroutine};
  if (ErasedEntry entry = FindInCallableTable(scope->Info().exports, wanted)) {
    return entry;
  }
  const char* entered =
      AmbientRunContext::Current().ScopeRegistry().NameOf(scope);
  // LRM 35.5.3.
  throw SimulationError(
      "DPI export '{}' was entered under scope '{}', which declares no such "
      "export; an export is callable directly only from an import of its own "
      "scope, and 'svSetScope' must name a scope that declares it",
      subroutine, entered == nullptr ? "<unregistered>" : entered);
}

}  // namespace lyra::runtime

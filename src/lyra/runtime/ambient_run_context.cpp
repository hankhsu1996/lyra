#include "lyra/runtime/ambient_run_context.hpp"

#include <format>
#include <string_view>

#include "lyra/base/simulation_error.hpp"
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
    throw SimulationError(
        "DPI export reached with no simulation running: an exported subroutine "
        "is callable only from a foreign call the simulation made (LRM "
        "35.5.3)");
  }
  return *CurrentSlot();
}

auto CurrentExportScope() -> Scope* {
  RuntimeProcess* process =
      AmbientRunContext::Current().Effects().TryCurrentProcess();
  Scope* scope = process == nullptr ? nullptr : process->CurrentDpiScope();
  if (scope == nullptr) {
    // The foreign side reached an instance-bound export with no scope
    // established. An import declared in a package or at `$unit` scope observes
    // no scope of its own (LRM 35.5.3), so reaching such an export from one
    // requires svSetScope first -- the foreign side's contract to meet, not an
    // invariant this compiler established.
    throw SimulationError(
        "DPI export reached without a scope context: the calling import must "
        "be a context import declared in an instantiated scope, or set the "
        "scope with svSetScope (LRM 35.5.3)");
  }
  return scope;
}

auto FindExportEntry(Scope* scope, const char* subroutine)
    -> ErasedScopeExportEntry {
  const std::string_view wanted{subroutine};
  for (const ScopeExport& published : scope->Program().exports.Entries()) {
    if (std::string_view{published.name.data, published.name.size} == wanted) {
      return published.entry;
    }
  }
  const char* entered =
      AmbientRunContext::Current().ScopeRegistry().NameOf(scope);
  throw SimulationError(
      std::format(
          "DPI export '{}' was entered under scope '{}', which declares no "
          "such export: an export is callable directly only from an import of "
          "its own scope, and svSetScope must name a scope that declares it "
          "(LRM 35.5.3)",
          subroutine, entered == nullptr ? "<unregistered>" : entered));
}

}  // namespace lyra::runtime

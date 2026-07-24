#include "lyra/runtime/ambient_run_context.hpp"

#include "lyra/base/internal_error.hpp"
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
    throw InternalError("DPI export: no active run context");
  }
  return *CurrentSlot();
}

auto CurrentExportScope() -> Scope* {
  RuntimeProcess* process =
      AmbientRunContext::Current().Effects().TryCurrentProcess();
  Scope* scope = process == nullptr ? nullptr : process->CurrentDpiScope();
  if (scope == nullptr) {
    throw InternalError(
        "DPI export reached without a scope context: the calling import must "
        "be a context import or set the scope with svSetScope (LRM 35.5.3)");
  }
  return scope;
}

}  // namespace lyra::runtime

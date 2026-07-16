#include "lyra/runtime/ambient_run_context.hpp"

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/scope.hpp"

namespace lyra::runtime {

namespace {

auto CurrentSlot() -> AmbientRunContext*& {
  static thread_local AmbientRunContext* context = nullptr;
  return context;
}

}  // namespace

AmbientRunContext::AmbientRunContext(Scope* root)
    : root_(root), previous_(CurrentSlot()) {
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

auto ResolveExportInstance(const char* instance_name) -> Scope* {
  Scope* instance =
      AmbientRunContext::Current().Root()->GetChild(instance_name, {});
  if (instance == nullptr) {
    throw InternalError("DPI export: exported subroutine's instance not found");
  }
  return instance;
}

}  // namespace lyra::runtime

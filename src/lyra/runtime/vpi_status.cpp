#include "lyra/runtime/vpi_status.hpp"

#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/version.hpp"
#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/vpi_callbacks.hpp"
#include "vpi_user.h"

namespace lyra::runtime {

namespace {

// The words of the error are handed out as the structure's characters, so
// they are kept here until the next routine clears them.
auto LastError() -> std::optional<VpiErrorReport>& {
  static thread_local std::optional<VpiErrorReport> error;
  return error;
}

}  // namespace

void ClearVpiError() {
  LastError().reset();
}

void SetVpiError(VpiErrorReport report) {
  LastError() = std::move(report);
}

void ReportVpiError(
    std::string_view routine, std::string message, PLI_INT32 level) {
  const VpiErrorReport report{
      .message = std::move(message),
      .routine = std::string(routine),
      .level = level,
      .file = std::string(),
      .line = 0};
  SetVpiError(report);
  if (level < vpiError) return;
  AmbientRunContext* context = AmbientRunContext::TryCurrent();
  if (context == nullptr) return;
  VpiCallbacks* callbacks = AsRuntime(context->Effects()).CallbacksIfMade();
  if (callbacks == nullptr) return;
  callbacks->ActionOccurred(SimulatorAction::kRoutineError, report);
  // What a callback for the error called cleared it, and the routine that
  // failed still reports it.
  SetVpiError(report);
}

auto CheckVpiError(p_vpi_error_info error_info_p) -> PLI_INT32 {
  std::optional<VpiErrorReport>& error = LastError();
  if (!error.has_value()) {
    return 0;
  }
  if (error_info_p != nullptr) {
    error_info_p->state = vpiRun;
    error_info_p->level = error->level;
    error_info_p->message = error->message.data();
    error_info_p->product = VpiProduct();
    error_info_p->code = error->routine.data();
    // An empty name rather than none where the error arose in no source text,
    // since a program prints what it is handed.
    error_info_p->file = error->file.data();
    error_info_p->line = error->line;
  }
  return error->level;
}

auto VpiProduct() -> PLI_BYTE8* {
  static std::string product{base::kProductName};
  return product.data();
}

auto VpiVersion() -> PLI_BYTE8* {
  static std::string version{base::LyraVersion()};
  return version.data();
}

}  // namespace lyra::runtime

#include "lyra/runtime/vpi_status.hpp"

#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/version.hpp"
#include "vpi_user.h"

namespace lyra::runtime {

namespace {

// The words of the error are handed out as the structure's characters, so
// they are kept here until the next routine clears them. Every error these
// routines report is of level `vpiError`.
struct VpiError {
  std::string message;
  std::string routine;
};

auto LastError() -> std::optional<VpiError>& {
  static thread_local std::optional<VpiError> error;
  return error;
}

// Where in a source an error arose, which none of these routines' errors has;
// an empty name rather than none, since a program prints what it is handed.
auto NoFile() -> PLI_BYTE8* {
  static std::string none;
  return none.data();
}

}  // namespace

void ClearVpiError() {
  LastError().reset();
}

void ReportVpiError(std::string_view routine, std::string message) {
  LastError() =
      VpiError{.message = std::move(message), .routine = std::string(routine)};
}

auto CheckVpiError(p_vpi_error_info error_info_p) -> PLI_INT32 {
  std::optional<VpiError>& error = LastError();
  if (!error.has_value()) {
    return 0;
  }
  if (error_info_p != nullptr) {
    error_info_p->state = vpiRun;
    error_info_p->level = vpiError;
    error_info_p->message = error->message.data();
    error_info_p->product = VpiProduct();
    error_info_p->code = error->routine.data();
    error_info_p->file = NoFile();
    error_info_p->line = 0;
  }
  return vpiError;
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

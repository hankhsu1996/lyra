#pragma once

#include <string>
#include <string_view>

#include "vpi_user.h"

namespace lyra::runtime {

// An error `vpi_chk_error` reports (LRM 38.2): what went wrong, at what level
// (LRM Table 38-1), in which routine, and where the design wrote what failed. A
// routine's error arose in no source text, and a run-time error of the design
// in no routine.
struct VpiErrorReport {
  std::string message;
  std::string routine;
  PLI_INT32 level = vpiError;
  std::string file;
  PLI_INT32 line = 0;
};

// What went wrong in the last VPI routine foreign code called on this thread
// (LRM 38.2). Every routine but `vpi_chk_error` clears it as it starts, so what
// `vpi_chk_error` reports is always about the call before it.
void ClearVpiError();

// Makes `report` what `vpi_chk_error` reports until a routine clears it.
void SetVpiError(VpiErrorReport report);

// The routine named `routine` could not do what it was asked, for the reason
// `message` says, which a program reads at `level` (LRM Table 38-1): an error
// of the request (`vpiError`), or a fault of the tool's own (`vpiInternal`).
// A callback placed for an error in a routine is called for one at an error's
// level or above (LRM 38.36.3 cbPLIError).
void ReportVpiError(
    std::string_view routine, std::string message, PLI_INT32 level = vpiError);

// `vpi_chk_error`: the severity of the last routine's error, or 0 where it met
// none, with what is known of it written to `error_info_p` where one is given.
auto CheckVpiError(p_vpi_error_info error_info_p) -> PLI_INT32;

// The product a VPI program is told it runs under and its version (LRM 38.17,
// Figure 38-1), as characters that stay where they are for the program's life.
auto VpiProduct() -> PLI_BYTE8*;
auto VpiVersion() -> PLI_BYTE8*;

}  // namespace lyra::runtime

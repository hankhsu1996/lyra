#pragma once

#include <string>
#include <string_view>

#include "vpi_user.h"

namespace lyra::runtime {

// What went wrong in the last VPI routine foreign code called on this thread
// (LRM 38.2). Every routine but `vpi_chk_error` clears it as it starts, so what
// `vpi_chk_error` reports is always about the call before it.
void ClearVpiError();

// The routine named `routine` could not do what it was asked, for the reason
// `message` says, which a program reads as an error (`vpiError`).
void ReportVpiError(std::string_view routine, std::string message);

// `vpi_chk_error`: the severity of the last routine's error, or 0 where it met
// none, with what is known of it written to `error_info_p` where one is given.
auto CheckVpiError(p_vpi_error_info error_info_p) -> PLI_INT32;

// The product a VPI program is told it runs under and its version (LRM 38.17,
// Figure 38-1), as characters that stay where they are for the program's life.
auto VpiProduct() -> PLI_BYTE8*;
auto VpiVersion() -> PLI_BYTE8*;

}  // namespace lyra::runtime

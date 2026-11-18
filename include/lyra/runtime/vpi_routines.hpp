#pragma once

// The VPI routines whose answers the version of the standard an application
// was compiled for decides (LRM 36.12.2.1), as that application calls them:
// what an object is and what a relation holds differ for the objects of LRM
// Table 36-10. Every other routine answers alike in every version.

#include "lyra/runtime/vpi_object.hpp"
#include "vpi_user.h"

namespace lyra::runtime {

[[nodiscard]] auto VpiGet(VpiMode mode, PLI_INT32 property, vpiHandle object)
    -> PLI_INT32;
[[nodiscard]] auto VpiGetStr(VpiMode mode, PLI_INT32 property, vpiHandle object)
    -> PLI_BYTE8*;
[[nodiscard]] auto VpiIterate(VpiMode mode, PLI_INT32 type, vpiHandle refHandle)
    -> vpiHandle;

}  // namespace lyra::runtime

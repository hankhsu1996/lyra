#pragma once

#include <string>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/local.hpp"

namespace lyra::mir {

// A foreign-linkage wrapper realizing a DPI-C export (LRM 35.5): the C entry
// point foreign code calls to reach an exported SV method. `code` is a
// receiver-less callable whose C-ABI parameters are the marshaled carriers and
// whose body marshals each carrier to its SV value, calls the exported method
// on the recovered receiver, and marshals the result back. The marshaling is
// stated in the body as ordinary calls so a backend renders it mechanically;
// only the receiver recovery and the external linkage are the backend's shell.
//
// `foreign_name` is the C linkage name (LRM 35.5.3); `instance_name` names the
// design instance the exported subroutine runs against, recovered when foreign
// code enters; `self_local` is the receiver binding the recovery fills, read by
// the body's method call. A backend emits one external-linkage definition per
// wrapper.
struct ForeignExportWrapper {
  std::string foreign_name;
  std::string instance_name;
  CallableCode code;
  LocalId self_local;
};

}  // namespace lyra::mir

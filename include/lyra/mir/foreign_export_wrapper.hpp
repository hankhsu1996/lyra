#pragma once

#include <string>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/local.hpp"

namespace lyra::mir {

// A foreign-linkage wrapper realizing a DPI-C export (LRM 35.5): the C entry
// point foreign code calls to reach an exported SV method. `code` is a
// receiver-less callable whose parameters are the marshaled carriers and whose
// body marshals each argument to its SV value, calls the exported method on the
// recovered receiver, and marshals the result and any `output` / `inout`
// argument back. The marshaling is stated in the body as ordinary calls so a
// backend renders it mechanically; only the receiver recovery and the external
// linkage are the backend's shell. Each parameter's C ABI type is carried by
// its MIR type (the carrier realized as a machine value or a canonical-chunk
// pointer), so a backend spells the signature through ordinary type mapping,
// with no per-parameter ABI decision in render.
//
// `foreign_name` is the C linkage name (LRM 35.5.3); `instance_name` names the
// design instance the exported subroutine runs against, recovered when foreign
// code enters; `self_local` is the receiver binding the recovery fills, read by
// the body's method call. The foreign caller owns the memory behind every
// by-pointer argument and is trusted to pass valid pointers per the DPI
// contract, as the runtime is otherwise explicit-pointer-threaded with no null
// checks. A backend emits one external-linkage definition per wrapper.
struct ForeignExportWrapper {
  std::string foreign_name;
  std::string instance_name;
  CallableCode code;
  LocalId self_local;
};

}  // namespace lyra::mir

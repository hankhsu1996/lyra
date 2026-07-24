#pragma once

#include <string>

#include "lyra/mir/callable_code.hpp"

namespace lyra::mir {

// A foreign-linkage wrapper realizing a DPI-C export (LRM 35.5): the C entry
// point foreign code calls to reach an exported SV subroutine. A DPI export
// defines a program-global foreign-linkage symbol, in its own name space
// distinct from any compilation-unit scope and never a class member (LRM 35.4,
// 35.7), so the wrapper is a unit-level contribution owned by the compilation
// unit, not by any class it may dispatch into.
//
// `foreign_name` is the C linkage name (LRM 35.5.3). `code` is a self-contained
// body: it recovers the exported subroutine's leading context argument from the
// running design (the current DPI scope as a module export's receiver, or the
// run's services for a receiver-less package export), marshals each C carrier
// to its SV value, calls the exported subroutine, and marshals the result and
// any `output` / `inout` argument back -- all as ordinary MIR a backend renders
// mechanically. Only the external linkage of the entry point is the backend's
// shell; the body decides nothing at render. Each parameter's C ABI type is
// carried by its MIR type (the carrier realized as a machine value or a
// canonical-chunk pointer), so a backend spells the signature through ordinary
// type mapping. The foreign caller owns the memory behind every by-pointer
// argument and is trusted to pass valid pointers per the DPI contract, as the
// runtime is otherwise explicit-pointer-threaded with no null checks.
struct ForeignExportWrapper {
  std::string foreign_name;
  CallableCode code;
};

}  // namespace lyra::mir

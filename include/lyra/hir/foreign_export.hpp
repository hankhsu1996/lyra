#pragma once

#include <string>
#include <vector>

#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/subroutine_id.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::hir {

// A subroutine exposed to foreign C by `export "DPI-C"` (LRM 35.5). Unlike an
// import, the subroutine keeps its ordinary SV body and is lowered as a normal
// method; this record additionally asks for a C entry point that marshals the
// ABI arguments, recovers the design instance the subroutine runs against,
// calls it, and marshals the result back. `subroutine` names the one that entry
// point calls, by the identity the declaring scope gave it; `foreign_name` is
// the C linkage name (LRM 35.5.3). The ABI projection of the signature is
// resolved once here, where slang types are available.
struct ForeignExportDecl {
  StructuralSubroutineId subroutine;
  std::string foreign_name;
  support::DpiScalarAbi ret_abi = support::DpiScalarAbi::kVoid;
  TypeId ret_sv_type{};
  std::vector<DpiParamAbi> params;
};

}  // namespace lyra::hir

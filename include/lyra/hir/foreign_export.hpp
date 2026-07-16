#pragma once

#include <string>
#include <vector>

#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::hir {

// A subroutine exposed to foreign C by `export "DPI-C"` (LRM 35.5). Unlike an
// import, the subroutine keeps its ordinary SV body and is lowered as a normal
// method; this record additionally asks a backend to materialize a
// foreign-linkage wrapper that marshals the C ABI arguments, recovers the
// design instance the subroutine runs against, calls the method, and marshals
// the result back. `sv_name` names the method the wrapper calls; `foreign_name`
// is the C linkage name (LRM 35.5.3). The ABI projection of the signature is
// resolved once here, where slang types are available.
struct ForeignExportDecl {
  std::string sv_name;
  std::string foreign_name;
  support::DpiScalarAbi ret_abi = support::DpiScalarAbi::kVoid;
  TypeId ret_sv_type{};
  std::vector<DpiParamAbi> params;
};

}  // namespace lyra::hir

#pragma once

#include <string>
#include <vector>

#include "lyra/hir/type_id.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::hir {

// One argument of a DPI-C import's ABI projection (LRM 35.5.6): the SV type it
// was declared with, the C ABI carrier it crosses the boundary as, and its
// direction (LRM 35.5.1.2), which decides whether the boundary copies the value
// in, back, or both.
struct DpiParamAbi {
  TypeId sv_type{};
  support::DpiCarrier carrier =
      support::ScalarCarrier{support::DpiScalarAbi::kVoid};
  support::DpiDirection direction = support::DpiDirection::kInput;
};

// A subroutine declared `import "DPI-C"` (LRM 35.4). It has no SV body, so it
// is a bodyless external callable, not a body-bearing structural subroutine.
// The foreign linkage name, the pure property, whether it is a task or a
// function (LRM 35.5), whether it is a context import (LRM 35.5.3), and the ABI
// projection of the signature are resolved once here (where slang types are
// available) and are the sole downstream source for the import; no layer below
// reads slang.
// Lowered to a MIR static external callable at HIR-to-MIR.
struct ForeignImportDecl {
  std::string name;
  std::string foreign_name;
  bool is_pure = false;
  bool is_task = false;
  bool is_context = false;
  support::DpiScalarAbi ret_abi = support::DpiScalarAbi::kVoid;
  TypeId ret_sv_type{};
  std::vector<DpiParamAbi> params;
};

}  // namespace lyra::hir

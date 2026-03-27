#pragma once

// Normalized import declarations for DPI-C function imports.

#include <string>
#include <vector>

#include "lyra/common/dpi_types.hpp"
#include "lyra/common/parameter_direction.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"

namespace lyra::hir {

// Re-export from common for convenience within the HIR layer.
using lyra::DpiAbiTypeClass;
using lyra::IsNormalizedDpiType;
using lyra::IsValidDpiParamType;
using lyra::IsValidDpiReturnType;

// A single parameter of a DPI import declaration.
// Invariant: dpi_type must satisfy IsValidDpiParamType().
struct DpiParam {
  SymbolId symbol;  // SV declaration symbol for this parameter
  SourceSpan span;
  TypeId type_id;  // Original Lyra semantic type
  DpiAbiTypeClass dpi_type = DpiAbiTypeClass::kInvalid;
  ParameterDirection direction = ParameterDirection::kInput;
};

// Normalized DPI-C import declaration.
// Produced by AST-to-HIR normalization from slang's DPIImportSyntax.
// These records are the sole downstream source for DPI import semantics.
// No layer below AST-to-HIR may access slang syntax or MethodFlags for DPI
// information; all decisions are keyed on the fields in this struct.
//
// Not stored in hir::Arena (DPI imports have no HIR statement body).
// Owned by ModuleBody::dpi_imports and Package::dpi_imports.
struct DpiImportDecl {
  SymbolId symbol;  // SV declaration symbol, not the foreign C identity
  SourceSpan span;
  std::string sv_name;
  std::string c_name;  // Resolved final C symbol name; never empty.
  TypeId return_type_id;
  DpiAbiTypeClass return_dpi_type = DpiAbiTypeClass::kInvalid;
  std::vector<DpiParam> params;
};

}  // namespace lyra::hir

#pragma once

// Normalized import declarations for DPI-C function imports.

#include <string>
#include <unordered_map>
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
  // True for `import "DPI-C" context ...`.
  // Records the hidden call-context requirement only. The hidden svScope
  // parameter is not represented in the user-visible DPI signature fields.
  bool is_context = false;
};

// Normalized DPI-C export declaration.
// Lean declaration metadata only. Does not carry ABI-classified types.
// The canonical DPI signature for each export is produced as a separate
// artifact (DpiSignatureCache) at AST-to-HIR time and consumed at MIR time.
//
// Not stored in hir::Arena (DPI exports have no HIR statement body).
// Owned by ModuleBody::dpi_exports and Package::dpi_exports.
struct DpiExportDecl {
  SymbolId symbol;  // SV function/task symbol being exported
  SourceSpan span;
  std::string c_name;  // Resolved final C symbol name; never empty.
  bool is_task = false;
  bool is_context = false;
  bool is_module_scoped = false;
};

// Pre-classified DPI signature for an exported subroutine.
// Produced at AST-to-HIR time (when slang types are available) as a
// separate artifact from the lean DpiExportDecl. Consumed at MIR time
// to build the canonical mir::DpiSignature without a second classifier.
//
// Reuses DpiParam and DpiAbiTypeClass from the import path -- same
// classification model, same canonical type rules.
struct DpiExportSignature {
  TypeId return_type_id;
  DpiAbiTypeClass return_dpi_type = DpiAbiTypeClass::kInvalid;
  std::vector<DpiParam> params;
};

// Preclassified DPI export signatures, keyed by export symbol.
// Produced at AST-to-HIR time (when slang types are available) as a
// separate artifact from the lean DpiExportDecl. Consumed at MIR time
// to build the canonical mir::DpiSignature without a second classifier.
using DpiExportSignatureCache =
    std::unordered_map<SymbolId, DpiExportSignature, SymbolIdHash>;

}  // namespace lyra::hir

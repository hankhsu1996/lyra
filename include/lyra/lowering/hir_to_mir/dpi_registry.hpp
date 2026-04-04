#pragma once

// Design-level DPI import and export registries.
// Populated during CollectDesignDeclarations from body/package DPI
// declarations. Single source of truth for DPI resolution during HIR-to-MIR
// lowering.

#include <cstddef>
#include <string>
#include <unordered_map>
#include <vector>

#include "lyra/common/dpi_types.hpp"
#include "lyra/common/parameter_direction.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/call.hpp"

namespace lyra::lowering::hir_to_mir {

// Frozen parameter info for a design-level DPI import.
struct DpiParamInfo {
  TypeId type_id;
  DpiAbiTypeClass abi_type = DpiAbiTypeClass::kInvalid;
  ParameterDirection direction = ParameterDirection::kInput;
};

// Design-level DPI import info, frozen after collection.
// Contains the full foreign-boundary metadata needed for MIR signature
// construction and LLVM lowering. Nothing D2-critical should be dropped here.
struct DpiImportInfo {
  SymbolId symbol;
  SourceSpan span;
  std::string sv_name;
  std::string c_name;
  TypeId return_type_id;
  DpiAbiTypeClass return_abi_type = DpiAbiTypeClass::kInvalid;
  std::vector<DpiParamInfo> params;
  // True for `import "DPI-C" context ...`.
  // Records the hidden call-context requirement only. The hidden svScope
  // parameter is not represented in the user-visible DPI signature fields.
  bool is_context = false;
};

// Registry of all design-visible DPI imports, keyed by declaration SymbolId.
// Frozen after CollectDesignDeclarations; immutable during lowering.
class DesignDpiImports {
 public:
  // Insert a DPI import. Returns true on success, false if symbol already
  // registered (duplicate).
  auto Insert(DpiImportInfo info) -> bool;

  [[nodiscard]] auto Find(SymbolId symbol) const -> const DpiImportInfo*;

  [[nodiscard]] auto Entries() const
      -> const std::unordered_map<SymbolId, DpiImportInfo, SymbolIdHash>& {
    return entries_;
  }

  [[nodiscard]] auto Size() const -> size_t {
    return entries_.size();
  }

  [[nodiscard]] auto Empty() const -> bool {
    return entries_.empty();
  }

 private:
  std::unordered_map<SymbolId, DpiImportInfo, SymbolIdHash> entries_;
};

// Design-level DPI export info, frozen after collection.
// Binds a lean export declaration to the canonical DpiSignature.
// The signature is the single source of ABI truth for wrapper emission
// and header generation.
struct DpiExportInfo {
  SymbolId symbol;
  SourceSpan span;
  std::string c_name;
  mir::DpiSignature signature;
  bool is_module_scoped = false;
  bool is_task = false;
};

// Registry of all design-visible DPI exports, keyed by declaration SymbolId.
// Frozen after CollectDesignDeclarations; immutable during lowering.
// Export-side registry with per-symbol and per-c_name collision detection.
// Design-wide visible-name checking (import/export cross-check) is done
// in CollectDesignDeclarations after both registries are populated.
class DesignDpiExports {
 public:
  // Insert a DPI export. Returns true on success.
  // Returns false if the symbol is already registered (duplicate symbol).
  // Throws InternalError if the c_name collides with a different export symbol.
  auto Insert(DpiExportInfo info) -> bool;

  [[nodiscard]] auto Find(SymbolId symbol) const -> const DpiExportInfo*;

  [[nodiscard]] auto Entries() const
      -> const std::unordered_map<SymbolId, DpiExportInfo, SymbolIdHash>& {
    return entries_;
  }

  [[nodiscard]] auto Size() const -> size_t {
    return entries_.size();
  }

  [[nodiscard]] auto Empty() const -> bool {
    return entries_.empty();
  }

 private:
  std::unordered_map<SymbolId, DpiExportInfo, SymbolIdHash> entries_;
  // Secondary index: c_name -> symbol, for collision detection.
  std::unordered_map<std::string, SymbolId> c_name_to_symbol_;
};

}  // namespace lyra::lowering::hir_to_mir

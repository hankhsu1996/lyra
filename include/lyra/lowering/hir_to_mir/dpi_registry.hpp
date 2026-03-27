#pragma once

// Design-level DPI import registry.
// Populated during CollectDesignDeclarations from body/package dpi_imports.
// Single source of truth for DPI import resolution during HIR-to-MIR lowering.

#include <cstddef>
#include <string>
#include <unordered_map>
#include <vector>

#include "lyra/common/dpi_types.hpp"
#include "lyra/common/parameter_direction.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"

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
};

// Registry of all design-visible DPI imports, keyed by declaration SymbolId.
// Frozen after CollectDesignDeclarations; immutable during lowering.
class DesignDpiImports {
 public:
  // Insert a DPI import. Returns true on success, false if symbol already
  // registered (duplicate).
  auto Insert(DpiImportInfo info) -> bool;

  [[nodiscard]] auto Find(SymbolId symbol) const -> const DpiImportInfo*;

  [[nodiscard]] auto Size() const -> size_t {
    return entries_.size();
  }

  [[nodiscard]] auto Empty() const -> bool {
    return entries_.empty();
  }

 private:
  std::unordered_map<SymbolId, DpiImportInfo, SymbolIdHash> entries_;
};

}  // namespace lyra::lowering::hir_to_mir

#pragma once

#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/dpi.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/package.hpp"

namespace lyra::hir {

using DesignElement = std::variant<Module, Package>;

// Preclassified DPI export signatures, keyed by export symbol.
// Produced at AST-to-HIR time (when slang types are available) as a
// separate artifact from the lean DpiExportDecl. Lives in Design so
// that declaration collection can consume it without widening pipeline
// interfaces.
using DpiExportSignatureCache =
    std::unordered_map<SymbolId, DpiExportSignature, SymbolIdHash>;

struct Design {
  std::vector<DesignElement> elements;
  // Specialization-owned shared module bodies, indexed by ModuleBodyId.
  // One body per specialization group.
  std::vector<ModuleBody> module_bodies;

  // Preclassified DPI export signatures for MIR-time signature construction.
  // Populated at AST-to-HIR time alongside lean DpiExportDecl records.
  DpiExportSignatureCache dpi_export_signatures;
};

}  // namespace lyra::hir

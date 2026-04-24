#pragma once

#include <vector>

#include "lyra/common/integral_constant.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// Source-level port direction (HIR layer -- no MIR dependency).
enum class HirPortDirection : uint8_t {
  kInput,
  kOutput,
  kInOut,
};

// Port declaration: symbol + direction, extracted from slang PortSymbol.
struct PortDecl {
  SymbolId sym;
  HirPortDirection dir;
};

// Instance/realization record for one elaborated module instance.
//
// This is NOT a behavioral container. Behavioral content (processes,
// functions, tasks) lives in hir::ModuleBody, referenced via body_id.
//
// The fields here are per-instance registration and realization artifacts:
// - symbol/span: instance identity
// - variables/nets/param_slots: per-instance SymbolIds from registration,
//   used by design-global place allocation and port binding compilation
// - param_init_values: per-instance parameter values for const blocks
// - module_def_id: definition identity for specialization grouping
struct Module {
  SymbolId symbol;
  SourceSpan span;
  // Per-instance SymbolIds from registration. These are needed by
  // CollectDesignDeclarations for design-global place allocation and
  // by port binding compilation for cross-instance wiring.
  std::vector<SymbolId> variables;
  std::vector<SymbolId> nets;
  // Parameters promoted to runtime slots (StorageClass::kDesignStorage).
  // Per-instance SymbolIds, not body-owned declarations.
  std::vector<SymbolId> param_slots;
  // Constant values for promoted params (parallel to param_slots).
  // Type comes from the symbol table via param_slots[i].
  std::vector<IntegralConstant> param_init_values;
  common::ModuleDefId module_def_id;
  // Reference to shared specialization-owned behavioral content.
  ModuleBodyId body_id;
  // Declared ports with directions, extracted from slang's port list.
  // Used by design_lower to build CompiledModuleHeaders from declared
  // port data (not binding plan).
  std::vector<PortDecl> ports;
};

}  // namespace lyra::hir

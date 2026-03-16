#pragma once

#include <cstdint>
#include <memory>
#include <unordered_map>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/compiled_bindings.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/instance.hpp"

namespace lyra::lowering::hir_to_mir {

struct LoweringInput {
  const hir::Design* design = nullptr;
  const hir::Arena* hir_arena = nullptr;
  TypeArena* type_arena = nullptr;
  const ConstantArena* constant_arena = nullptr;
  const SymbolTable* symbol_table = nullptr;
  BuiltinTypes builtin_types;
  const ast_to_hir::DesignBindingPlan* binding_plan = nullptr;
  int8_t global_precision_power =
      -9;  // Finest timeprecision across all modules
  const mir::InstanceTable* instance_table = nullptr;  // For %m support
  const common::SpecializationMap* specialization_map;
};

// Statistics collected during HIR->MIR lowering (for --stats output).
struct LoweringStats {
  uint64_t place_temps = 0;
  uint64_t value_temps = 0;
  // Times a value-representation operand (UseTemp/Const) was converted to a
  // place temp via allocate+assign, so that a projection (bit-range, field,
  // element access) could address it.  Most SV selections operate on lvalues
  // that are already places, so this is expected to be 0 on simple tests;
  // real designs with complex expressions may show non-zero counts.
  uint64_t materialize_to_place = 0;
  uint64_t mir_stmts = 0;
};

struct LoweringResult {
  mir::Design design;
  // Design-level arena for design-global MIR (package places, package
  // functions, init processes, connection processes). Body-local MIR
  // is in each ModuleBody's embedded arena.
  std::unique_ptr<mir::Arena> design_arena;
  // Design-global origins (package init processes, generated functions).
  OriginMap design_origins;
  // Per-body origins, indexed by ModuleBodyId. Body-local MIR origins
  // stay body-local -- not merged into design_origins.
  std::vector<std::vector<OriginEntry>> body_origins;
  LoweringStats stats;
  mir::CompiledBindingPlan compiled_bindings;
};

auto LowerHirToMir(const LoweringInput& input) -> Result<LoweringResult>;

}  // namespace lyra::lowering::hir_to_mir

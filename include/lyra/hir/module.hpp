#pragma once

#include <vector>

#include "lyra/common/integral_constant.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

struct Module {
  SymbolId symbol;
  SourceSpan span;
  std::vector<SymbolId> variables;
  std::vector<SymbolId> nets;
  // Parameters promoted to runtime slots (StorageClass::kDesignStorage).
  // These are value-only params that differ across instances of the same
  // definition, enabling template dedup.
  std::vector<SymbolId> param_slots;
  // Constant values for promoted params (parallel to param_slots).
  // Type comes from the symbol table via param_slots[i].
  std::vector<IntegralConstant> param_init_values;
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;
  std::vector<TaskId> tasks;
  common::ModuleDefId module_def_id;
};

}  // namespace lyra::hir

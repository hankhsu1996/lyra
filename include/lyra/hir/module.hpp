#pragma once

#include <vector>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

struct Module {
  SymbolId symbol;
  SourceSpan span;
  std::vector<SymbolId> variables;
  std::vector<SymbolId> nets;
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;
  std::vector<TaskId> tasks;
  // In-run grouping key for process template pre-filtering.
  // Assigned from DefinitionSymbol* during AST->HIR lowering.
  // NOT a stable identity -- valid only within a single compilation run.
  uint64_t module_def_key = 0;
};

}  // namespace lyra::hir

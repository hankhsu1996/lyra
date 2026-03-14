#pragma once

#include <vector>

#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// Specialization-owned shared behavioral HIR for a module body.
//
// This owns only behavioral content that is identical across all instances
// of the same specialization group. AST->HIR lowering produces one ModuleBody
// per specialization group using the representative instance.
//
// Invariants:
// - No instance identity (no instance symbol, no instance path)
// - No per-instance registration artifacts (no per-instance SymbolIds)
// - No per-instance parameter values
struct ModuleBody {
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;
  std::vector<TaskId> tasks;
};

}  // namespace lyra::hir

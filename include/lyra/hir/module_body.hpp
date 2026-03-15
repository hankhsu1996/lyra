#pragma once

#include <vector>

#include "lyra/hir/arena.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// Per-specialization-body owned artifact.
//
// The primary unit of body-local HIR compilation. Owns both the body structure
// (processes, functions, tasks) and the body-local HIR storage (arena).
// All body-local HIR IDs are scoped to this body's arena.
//
// AST->HIR lowering produces one ModuleBody per specialization group.
// Downstream consumers (HIR->MIR, dumper) resolve body-local HIR through
// this unit by body_id.
//
// Invariants:
// - No instance identity (no instance symbol, no instance path)
// - No per-instance registration artifacts (no per-instance SymbolIds)
// - No per-instance parameter values
// - All body-local HIR IDs resolve against this body's arena
struct ModuleBody {
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;
  std::vector<TaskId> tasks;

  // Body-local HIR storage. All body-local HIR IDs in this ModuleBody
  // are resolved against this arena.
  Arena arena;
};

}  // namespace lyra::hir

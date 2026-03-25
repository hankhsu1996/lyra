#pragma once

#include <vector>

#include "lyra/common/constant_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/dpi.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// Per-specialization-body owned artifact.
//
// The primary unit of body-local HIR compilation. Owns the body structure
// (processes, functions, tasks) and the body-local artifact domains:
// HIR node storage (arena) and constant storage (constant_arena).
// All body-local HIR IDs and ConstIds are scoped to this body's stores.
//
// AST->HIR lowering produces one ModuleBody per specialization group.
// Downstream consumers (HIR->MIR, dumper) resolve body-local HIR and
// constants through this unit by body_id.
//
// Invariants:
// - No instance identity (no instance symbol, no instance path)
// - No per-instance registration artifacts (no per-instance SymbolIds)
// - No per-instance parameter values
// - All body-local HIR IDs resolve against this body's arena
// - All body-local ConstIds resolve against this body's constant_arena
struct ModuleBody {
  std::vector<ProcessId> processes;
  std::vector<FunctionId> functions;
  std::vector<TaskId> tasks;
  // DPI-C import declarations owned by this body.
  // Downstream design-level resolution is built from this owned collection.
  std::vector<DpiImportDecl> dpi_imports;

  // Body-local HIR node storage.
  Arena arena;

  // Body-local constant storage. All ConstIds in this body's HIR nodes
  // resolve against this arena, not the design-global constant arena.
  ConstantArena constant_arena;
};

}  // namespace lyra::hir

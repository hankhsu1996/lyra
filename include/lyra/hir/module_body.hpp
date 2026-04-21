#pragma once

#include <vector>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/constructor.hpp"
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
  // Permanent body-owned constructor artifact. Exactly one per body.
  // constructor.body is a root StatementId in `arena` whose kind is
  // always kBlock.
  Constructor constructor;
  // DPI-C import declarations owned by this body.
  // Downstream design-level resolution is built from this owned collection.
  std::vector<DpiImportDecl> dpi_imports;
  // DPI-C export declarations owned by this body.
  std::vector<DpiExportDecl> dpi_exports;

  // Plain-child object-handle members declared directly on this body.
  //
  // Restricted subset carrier, NOT a general object-handle graph:
  //   - Only child-instance members whose handle slot lives directly on
  //     the owning body (no enclosing generate-scope, no dynamic-scope
  //     nesting, no class/dynamic-array-hosted object handles).
  //   - Only children that matched the "plain child" subset at AST->HIR
  //     time (no ports, etc.). Children on the non-plain path are not
  //     recorded here.
  //
  // Populated at AST->HIR alongside constructor body synthesis.
  // HIR->MIR enrolls these as kVariable module slots; the constructor
  // body writes the corresponding handle values through ordinary
  // assignment statements. This is declaration state: MIR lowering
  // must not reconstruct the set by scanning constructor statements.
  //
  // New code that needs a broader "all object handles in this body"
  // view (generate scopes, class members, etc.) must introduce its
  // own model rather than extending this list.
  std::vector<SymbolId> plain_child_object_handle_members;

  // Body-local HIR node storage.
  Arena arena;

  // Body-local constant storage. All ConstIds in this body's HIR nodes
  // resolve against this arena, not the design-global constant arena.
  ConstantArena constant_arena;
};

}  // namespace lyra::hir

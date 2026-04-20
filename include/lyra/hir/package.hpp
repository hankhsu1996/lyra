#pragma once

#include <vector>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/dpi.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// Per-compilation-unit package record. Owns package-local HIR node storage
// and package-local constant storage. All FunctionIds in `functions` and the
// ProcessId in `init_process` resolve against this package's `arena`; all
// ConstIds reachable from package HIR resolve against `constant_arena`.
struct Package {
  SymbolId symbol;
  SourceSpan span;
  std::vector<SymbolId> variables;
  std::vector<FunctionId> functions;
  // DPI-C import declarations owned by this package.
  // Downstream design-level resolution is built from this owned collection.
  std::vector<DpiImportDecl> dpi_imports;
  // DPI-C export declarations owned by this package.
  std::vector<DpiExportDecl> dpi_exports;
  ProcessId init_process = kInvalidProcessId;

  // Package-local HIR node storage. All FunctionIds in `functions` and the
  // ProcessId in `init_process` index into this arena.
  Arena arena;
  // Package-local constant storage. All ConstIds reachable from package HIR
  // resolve against this arena.
  ConstantArena constant_arena;
};

}  // namespace lyra::hir

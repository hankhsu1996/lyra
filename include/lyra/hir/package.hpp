#pragma once

#include <vector>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/dpi.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

struct Package {
  SymbolId symbol;
  SourceSpan span;
  std::vector<SymbolId> variables;
  std::vector<FunctionId> functions;
  // DPI-C import declarations owned by this package.
  // Downstream design-level resolution is built from this owned collection.
  std::vector<DpiImportDecl> dpi_imports;
  ProcessId init_process = kInvalidProcessId;
};

}  // namespace lyra::hir

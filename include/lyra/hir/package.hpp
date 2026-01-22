#pragma once

#include <vector>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

struct Package {
  SymbolId symbol;
  SourceSpan span;
  std::vector<SymbolId> variables;
  ProcessId init_process = kInvalidProcessId;
};

}  // namespace lyra::hir

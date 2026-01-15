#pragma once

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"

namespace lyra::hir {

struct Package {
  SymbolId symbol;
  SourceSpan span;
};

}  // namespace lyra::hir

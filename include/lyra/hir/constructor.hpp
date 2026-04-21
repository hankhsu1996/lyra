#pragma once

#include <vector>

#include "lyra/common/parameter_direction.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// Constructor formal parameter. Symbol is a synthesized constructor-scope
// local, distinct from any body-owned member symbol. Body-level bindings
// to a body-owned member are expressed as ordinary assignment statements
// in Constructor::body (not through a side table).
struct ConstructorFormal {
  SymbolId symbol;
  ParameterDirection direction = ParameterDirection::kInput;

  auto operator==(const ConstructorFormal&) const -> bool = default;
};

// Body-owned semantic artifact for specialization construction.
//
// Every ModuleBody owns exactly one Constructor. `body` is a root kBlock
// StatementId in the owning body's arena. Formals are scope-local to the
// constructor; any binding into body-owned storage happens through
// ordinary assignment statements at the top of the body.
struct Constructor {
  SourceSpan span;
  StatementId body;
  std::vector<ConstructorFormal> parameters;

  auto operator==(const Constructor&) const -> bool = default;
};

}  // namespace lyra::hir

#pragma once

#include "lyra/common/source_span.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// Body-owned semantic artifact for specialization construction.
//
// Every ModuleBody owns exactly one Constructor. Its `body` is a root
// StatementId in the owning body's arena whose kind is always kBlock.
// Constructor is a peer to Process/Function/Task: function-like in body
// shape (single root statement) but semantically distinct from user-written
// functions and simulation-triggered processes.
struct Constructor {
  SourceSpan span;
  StatementId body;

  auto operator==(const Constructor&) const -> bool = default;
};

}  // namespace lyra::hir

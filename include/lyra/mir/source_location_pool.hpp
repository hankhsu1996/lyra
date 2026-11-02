#pragma once

#include <cstddef>

#include "lyra/base/hash.hpp"
#include "lyra/base/interner.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/mir/source_location_id.hpp"

namespace lyra::mir {

struct SourceLocationHash {
  auto operator()(const diag::SourcePosition& position) const -> std::size_t {
    std::size_t seed = 0;
    base::HashField(seed, position.file);
    base::HashField(seed, position.line);
    base::HashField(seed, position.column);
    return seed;
  }
};

// The locations a unit's program hands the library, where an operation that can
// fail the design was written, each stated once as a constant however many
// calls start there, as clang's `ASTContext` holds one unnamed global constant
// per location `__builtin_source_location` names. An entry is what the location
// says, settled before either backend reads it, and each target writes the
// constant from that.
using SourceLocationPool =
    base::Interner<diag::SourcePosition, SourceLocationId, SourceLocationHash>;

}  // namespace lyra::mir

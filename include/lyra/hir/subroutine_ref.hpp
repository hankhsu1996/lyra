#pragma once

#include <variant>

#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::hir {

struct StructuralSubroutineRef {
  StructuralHops hops;
  StructuralSubroutineId subroutine;
};

struct SystemSubroutineRef {
  support::SystemSubroutineId id;
};

using SubroutineRef =
    std::variant<StructuralSubroutineRef, SystemSubroutineRef>;

}  // namespace lyra::hir

#pragma once

#include <variant>

#include "lyra/hir/parent_scope_hops.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::hir {

struct UserSubroutineRef {
  ParentScopeHops parent_scope_hops;
  SubroutineId id;
};

struct SystemSubroutineRef {
  support::SystemSubroutineId id;
};

using SubroutineRef = std::variant<UserSubroutineRef, SystemSubroutineRef>;

}  // namespace lyra::hir

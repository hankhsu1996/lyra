#pragma once

#include <cstdint>
#include <variant>

#include "lyra/hir/local_var.hpp"
#include "lyra/hir/member_var.hpp"

namespace lyra::hir {

struct ParentScopeHops {
  std::uint32_t value;

  auto operator<=>(const ParentScopeHops&) const = default;
};

struct MemberVarRef {
  ParentScopeHops parent_scope_hops;
  MemberVarId target;

  auto operator==(const MemberVarRef&) const -> bool = default;
};

struct LocalVarRef {
  LocalVarId target;

  auto operator==(const LocalVarRef&) const -> bool = default;
};

using ValueRef = std::variant<MemberVarRef, LocalVarRef>;

}  // namespace lyra::hir

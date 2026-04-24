#pragma once

#include <cstdint>
#include <variant>

#include "lyra/hir/var_decl.hpp"

namespace lyra::hir {

struct ParentScopeHops {
  std::uint32_t value;

  auto operator<=>(const ParentScopeHops&) const = default;
};

struct VarDeclRef {
  ParentScopeHops parent_scope_hops;
  VarDeclId local_id;

  auto operator==(const VarDeclRef&) const -> bool = default;
};

using ValueDeclRef = std::variant<VarDeclRef>;

struct LocalValueRef {
  ValueDeclRef target;
};

}  // namespace lyra::hir

#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct LocalVarId {
  std::uint32_t value;

  auto operator<=>(const LocalVarId&) const -> std::strong_ordering = default;
};

struct LocalScopeId {
  std::uint32_t value;

  auto operator<=>(const LocalScopeId&) const -> std::strong_ordering = default;
};

struct LocalVar {
  std::string name;
  TypeId type;
};

struct LocalScope {
  std::optional<LocalScopeId> parent;
  std::vector<LocalVar> locals;
};

}  // namespace lyra::mir

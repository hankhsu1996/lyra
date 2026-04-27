#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/hir/type.hpp"

namespace lyra::hir {

struct SubroutineId {
  std::uint32_t value;

  auto operator<=>(const SubroutineId&) const -> std::strong_ordering = default;
};

enum class SubroutineKind : std::uint8_t {
  kTask,
  kFunction,
};

struct UserSubroutineDecl {
  std::string name;
  SubroutineKind kind;
  TypeId result_type;
};

}  // namespace lyra::hir

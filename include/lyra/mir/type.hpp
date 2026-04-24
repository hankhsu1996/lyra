#pragma once

#include <compare>
#include <cstdint>
#include <variant>

namespace lyra::mir {

struct TypeId {
  std::uint32_t value;

  auto operator<=>(const TypeId&) const -> std::strong_ordering = default;
};

struct BuiltinIntType {};
struct BuiltinLogicType {};

using TypeData = std::variant<BuiltinIntType, BuiltinLogicType>;

struct Type {
  TypeData data;
};

}  // namespace lyra::mir

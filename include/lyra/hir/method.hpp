#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <variant>

#include "lyra/hir/subroutine_id.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

enum class BuiltinMethodKind : std::uint8_t {
  kEnumFirst,
  kEnumLast,
  kEnumNum,
  kEnumNext,
  kEnumPrev,
  kEnumName,
};

struct StructuralMethod {
  StructuralSubroutineId subroutine;
};

struct BuiltinMethod {
  BuiltinMethodKind kind;
};

using MethodData = std::variant<StructuralMethod, BuiltinMethod>;

struct Method {
  std::string name;
  MethodData data;
};

struct MethodId {
  std::uint32_t value;

  auto operator<=>(const MethodId&) const -> std::strong_ordering = default;
};

struct MethodRef {
  TypeId receiver_type;
  MethodId method;
};

}  // namespace lyra::hir

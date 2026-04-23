#pragma once

#include <compare>
#include <cstdint>
#include <variant>

#include "lyra/mir/member.hpp"

namespace lyra::mir {

struct ExprId {
  std::uint32_t value;

  auto operator<=>(const ExprId&) const -> std::strong_ordering = default;
};

struct IntegerLiteral {
  std::int64_t value;
};

struct MemberRef {
  MemberId target;
};

using ExprData = std::variant<IntegerLiteral, MemberRef>;

struct Expr {
  ExprData data;
};

}  // namespace lyra::mir

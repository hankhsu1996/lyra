#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <variant>

#include "lyra/mir/class_decl_id.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct MemberVarId {
  std::uint32_t value;

  auto operator<=>(const MemberVarId&) const -> std::strong_ordering = default;
};

struct ValueMember {
  TypeId type;
};

struct ChildClassMember {
  ClassDeclId target;
};

using MemberKind = std::variant<ValueMember, ChildClassMember>;

struct MemberVar {
  std::string name;
  MemberKind kind;
};

}  // namespace lyra::mir

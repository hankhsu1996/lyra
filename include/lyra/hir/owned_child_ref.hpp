#pragma once

#include <compare>
#include <cstdint>
#include <variant>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

struct GenerateId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const GenerateId&) const -> std::strong_ordering = default;
};

struct StructuralScopeId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const StructuralScopeId&) const
      -> std::strong_ordering = default;
};

struct InstanceMemberId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const InstanceMemberId&) const
      -> std::strong_ordering = default;
};

// A generate block (LRM 27) as a child of the scope that declares it: the
// generate construct it belongs to, plus which of that construct's elaborated
// blocks it is.
struct GenerateChildRef {
  GenerateId generate;
  StructuralScopeId scope;

  auto operator==(const GenerateChildRef&) const -> bool = default;
};

// A child object the referrer's compilation unit declares, named by the
// declaring scope's own identity for it.
using OwnedChildRef = std::variant<InstanceMemberId, GenerateChildRef>;

}  // namespace lyra::hir

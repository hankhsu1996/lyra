#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/base/pool_id.hpp"
#include "lyra/hir/published_scope.hpp"

namespace lyra::hir {

struct ExternalScopeClassId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ExternalScopeClassId&) const
      -> std::strong_ordering = default;
};

// The class of a scope of a unit this one references -- an instance of that
// unit, or a generate block inside one -- as that unit's signature published
// it, with the unit that defines it. Every type the signature names is this
// unit's own -- taken into its pool where the signature was consumed -- so
// nothing below this record reads a signature or a type it does not own.
//
// This unit compiles none of it; it holds what it compiled against.
struct ExternalScopeClass {
  std::string unit_name;
  ScopeClassSignature signature;

  auto operator==(const ExternalScopeClass&) const -> bool = default;
};

}  // namespace lyra::hir

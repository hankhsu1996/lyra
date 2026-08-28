#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <variant>

#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// Where a published member sits in the object that publishes it. The position
// is the signature's own order, so the unit that publishes and the unit that
// reads both count it out of the same list and neither states it to the other.
struct PublishedMemberId {
  std::uint32_t value;

  auto operator<=>(const PublishedMemberId&) const
      -> std::strong_ordering = default;
};

// A member holding its own mutable cell (LRM 6.5).
struct VariableStorage {
  auto operator==(const VariableStorage&) const -> bool = default;
};

// A member whose value is the resolution of its drivers, folding under the net
// type its declaration states (LRM 6.6).
struct NetStorage {
  NetType net_type{};

  auto operator==(const NetStorage&) const -> bool = default;
};

// A member holding no cell of its own: a `ref` / `const ref` port aliases
// whatever the connection binds it to, and the binding decides whether writing
// through it is permitted (LRM 23.3.3.2).
struct ReferenceStorage {
  ReferenceBinding binding{};

  auto operator==(const ReferenceStorage&) const -> bool = default;
};

// Which storage a published member is, and so which cell a reference to it
// reaches. The publishing unit's own declaration is the only source of this, so
// the signature states it and a referrer never reads that declaration to learn
// it -- which is what the external name exists to prevent.
using PublishedStorage =
    std::variant<VariableStorage, NetStorage, ReferenceStorage>;

// One declaration an instance of a unit exposes to another unit by name.
struct PublishedMember {
  std::string name;
  TypeId type;
  PublishedStorage storage;
};

// A unit states a declaration's storage on its signature and builds its own
// object from it, so the two cannot describe different storage.
[[nodiscard]] inline auto StorageOf(const StructuralDataObjectDecl& decl)
    -> PublishedStorage {
  if (const auto* variable = std::get_if<StructuralVariableDecl>(&decl.kind)) {
    if (variable->reference.has_value()) {
      return ReferenceStorage{.binding = *variable->reference};
    }
    return VariableStorage{};
  }
  return NetStorage{
      .net_type = std::get<StructuralNetDecl>(decl.kind).net_type};
}

}  // namespace lyra::hir

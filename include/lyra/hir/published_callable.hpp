#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/base/pool_id.hpp"
#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// Where a published callable sits in the list its unit published. A callable is
// reached by the symbol its declaring unit emits it under rather than by a
// position in an object, so this orders the promise and nothing else.
struct PublishedCallableId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PublishedCallableId&) const
      -> std::strong_ordering = default;
};

// One subroutine a unit exposes to another by name: one an instance of it
// offers (LRM 25.7), one its namespace declares (LRM 26.3), or a method of a
// class it publishes (LRM 8.6). What a caller needs beyond the name is the
// interface a call to it is made through and the result its completion yields.
struct PublishedCallable {
  std::string name;
  ExternalCalleeInterface interface;
  TypeId result_type;

  auto operator==(const PublishedCallable&) const -> bool = default;
};

}  // namespace lyra::hir

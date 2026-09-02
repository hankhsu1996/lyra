#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <vector>

#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// Where a published callable sits in the list its unit published. A callable is
// reached by the symbol its declaring unit emits it under rather than by a
// position in an object, so this orders the promise and nothing else.
struct PublishedCallableId {
  std::uint32_t value;

  auto operator<=>(const PublishedCallableId&) const
      -> std::strong_ordering = default;
};

// One subroutine an instance of a unit exposes to another unit by name (LRM
// 25.7). What a caller needs beyond the name is the call protocol -- a task
// enable suspends the caller until completion (LRM 13.3) -- the result the
// completion yields, and per formal a direction and a type, which is what
// shapes the arguments the call passes and the writeback it performs.
struct PublishedCallable {
  std::string name;
  SubroutineKind kind;
  TypeId result_type;
  std::vector<ExternalCalleeParam> params;
};

}  // namespace lyra::hir

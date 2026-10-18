#pragma once

#include <cstdint>
#include <string>

#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/with_clause_id.hpp"

namespace lyra::hir {

// The methods LRM 7.12 defines on every unpacked array that evaluate an
// expression at each entry: what a `with` clause answers for it, or the
// element itself where the call writes none (LRM 7.12.1). Only a layer that
// still holds the clause's expression can write that evaluation, so no layer
// below names these.
enum class ArrayIteratorMethod : std::uint8_t {
  kSort,
  kRsort,
  kSum,
  kProduct,
  kAnd,
  kOr,
  kXor,
  kFind,
  kFindIndex,
  kFindFirst,
  kFindFirstIndex,
  kFindLast,
  kFindLastIndex,
  kMin,
  kMax,
  kUnique,
  kUniqueIndex,
  kMap,
};

// An array method's `with` clause (LRM 7.12, Syntax 7-5). `id` names this
// clause; the element (`item`) and its index (`item.index`) are referenced
// inside `expr` by that identity and role, so a clause nested in `expr` that
// reads an outer iterator still names the outer clause. `element_name` is the
// source iterator name, kept so the loop's element variable reads as the
// source named it.
struct WithClause {
  WithClauseId id;
  std::string element_name;
  ExprId expr;

  auto operator==(const WithClause&) const -> bool = default;
};

}  // namespace lyra::hir

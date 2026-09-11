#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>

#include "lyra/base/pool_id.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct FieldId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const FieldId&) const -> std::strong_ordering = default;
};

// A field carries no name. Its identity is the position its declaration sits
// at, which is already what every access to it names; being reachable by an
// identifier is a separate relation, held by whatever answers that identifier
// (`NamedField` below). A field the source never wrote simply does not take
// part in it, rather than taking part with a spelling of the compiler's own --
// which it could not have, since a SystemVerilog identifier admits every
// printable character but white space (LRM 5.6.1) and so leaves no word
// reserved to mint one from.
//
// LRM 10.5 variable initialization (the user-supplied `= value` or the LRM
// Table 6-7 type default) lowers to an `AssignExpr` statement in the enclosing
// class's constructor body; every backend renders construction-time state from
// that statement list.
struct FieldDecl {
  TypeId type;
};

// One entry of the relation between a name space and the storage it answers:
// the identifier written in the source, and the field it reaches. A class's
// declared variables, its instances and the signals a scope offers are each
// such a relation, held by the class rather than by the fields, so the fields
// nothing names carry nothing.
struct NamedField {
  std::string name;
  FieldId slot;
};

// The identifier `slot` answers to among `named`, or nothing where nothing
// names it. Answering nothing is the answer, not a case to work around.
[[nodiscard]] inline auto NameOf(
    std::span<const NamedField> named, FieldId slot)
    -> std::optional<std::string_view> {
  for (const NamedField& entry : named) {
    if (entry.slot == slot) {
      return std::string_view{entry.name};
    }
  }
  return std::nullopt;
}

// A member another unit's promise describes -- the referrer's own slot-aligned
// copy of the declaring unit's storage, which an access counts a position out
// of exactly as the declaring unit does.
//
// The name sits on the member rather than in a relation because a promise has
// no member that answers to none: a unit publishes what its source declared and
// never what its own lowering synthesized. The declaring unit's arena needs the
// relation for the opposite reason -- it also holds the cells its bodies keep
// -- and putting one here would oblige every consumer to handle an absence that
// cannot arise.
//
// Not `AggregateMember`, which carries the same two fields for a different
// reason: there the name is content of a declared type, printed by LRM
// 21.2.1.6, and it travels with the type into the runtime.
struct PromisedField {
  std::string name;
  TypeId type;
};

// One field of a construction: which field (`target`, a stable `FieldId`)
// receives which value (`value`). The vocabulary every field-bearing
// construction states its initializers in. The value is a pure read of an
// already-materialized source, so the order of these entries is the
// source-semantic evaluation order, independent of a declaration's field order.
struct FieldInit {
  FieldId target;
  ExprId value;
};

}  // namespace lyra::mir

#pragma once

#include <cstddef>
#include <vector>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

struct RuntimeValue;

// The runtime-owned realization of a tagged union (LRM 7.3.2 / 11.9), MIR's
// generic `TaggedUnionType`. A type-checked sum: it holds one member at a time,
// identified by a declaration-order tag that is part of the value, and stores
// only that member.
//
// The execution backend's type-erased counterpart of the C++ backend's
// monomorphized `TaggedUnion<Ts...>`, and the tagged sibling of `RuntimeUnion`.
// It differs from `RuntimeUnion` in what a mismatched access does: the tag is
// observable, so reading or writing a member other than the active one is a
// run-time error (LRM 11.9), not the deterministic fallback an untagged union
// returns. Re-tagging goes through a whole-value build, never a member write. A
// `void` member carries an `Empty` payload like any other component, so nothing
// here treats it apart.
class RuntimeTaggedUnion {
 public:
  // LRM 11.9: an uninitialized tagged union is undefined; the deterministic
  // stand-in is tag 0 with the first member's default, supplied by the lowering
  // at each default-init site. No program may depend on it.
  RuntimeTaggedUnion();
  RuntimeTaggedUnion(std::size_t tag_index, RuntimeValue payload);
  RuntimeTaggedUnion(const RuntimeTaggedUnion&);
  RuntimeTaggedUnion(RuntimeTaggedUnion&&) noexcept;
  auto operator=(const RuntimeTaggedUnion&) -> RuntimeTaggedUnion&;
  auto operator=(RuntimeTaggedUnion&&) noexcept -> RuntimeTaggedUnion&;
  ~RuntimeTaggedUnion();

  // The active tag, as the small non-negative integer the pattern-match guard
  // compares against a constant tag (LRM 12.6).
  [[nodiscard]] auto Tag() const -> std::size_t;

  // Reads member `index`. LRM 11.9: a read whose tag does not match the current
  // one is a run-time error, not the component default an untagged union
  // returns.
  [[nodiscard]] auto Member(std::size_t index) const -> RuntimeValue;

  // Replaces the payload of member `index`. LRM 11.9: a write whose tag does
  // not match the current one is a run-time error; re-tagging goes through a
  // whole-value build, never here.
  void SetMember(std::size_t index, RuntimeValue value);

  // LRM 11.4.5 `==` / `!=` (Any data type): equal only when the same member is
  // active and its values compare equal.
  [[nodiscard]] auto operator==(const RuntimeTaggedUnion& other) const
      -> PackedArray;
  [[nodiscard]] auto operator!=(const RuntimeTaggedUnion& other) const
      -> PackedArray;

  // LRM 11.4.5 `===` / `!==` (case equality): same active member and
  // bit-for-bit identical value.
  [[nodiscard]] auto CaseEqual(const RuntimeTaggedUnion& other) const
      -> PackedArray;

  // LRM 9.4.2 update-event predicate: changed when the active member changed or
  // the active value's bits changed.
  [[nodiscard]] auto IsBitIdentical(const RuntimeTaggedUnion& other) const
      -> bool;

  // LRM 20.9 `$isunknown`: the active member's unknown bits propagate up.
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> PackedArray;

 private:
  std::size_t tag_index_ = 0;
  // The active member's payload. Held in a vector so the header tolerates the
  // incomplete `RuntimeValue`, the same idiom the runtime aggregates share; it
  // always holds exactly one element.
  std::vector<RuntimeValue> payload_;
};

static_assert(LyraValue<RuntimeTaggedUnion>);
static_assert(CaseEqualComparable<RuntimeTaggedUnion>);

}  // namespace lyra::value

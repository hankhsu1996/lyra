#pragma once

#include <cstddef>
#include <vector>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

struct RuntimeValue;

// The runtime-owned realization of an untagged unpacked union (LRM 7.3), MIR's
// generic `UnionType`. An active-member value: it holds one member at a time,
// identified by a declaration-order index, and stores only that member -- the
// union's whole observable content is the pair (active index, active value).
//
// This is the execution backend's type-erased counterpart of the C++ backend's
// monomorphized `Union<Ts...>`. A compile-once runtime cannot instantiate a
// distinct C++ type per source union, so one `RuntimeUnion` holds a single
// type-erased `RuntimeValue` and composes the `LyraValue` contract by
// delegating to it. Because only the active member is stored, a read of an
// inactive member -- undefined in SV (LRM 7.3) -- has no value of the requested
// member's domain to return; this backend does not yet synthesize that member's
// default and reports the cross-member read instead.
class RuntimeUnion {
 public:
  // LRM Table 7-1: an unpacked union defaults to its first member. The default
  // construction is a placeholder; the lowering emits an explicit first-member
  // default value at each default-init site.
  RuntimeUnion();
  RuntimeUnion(std::size_t active_index, RuntimeValue active);
  RuntimeUnion(const RuntimeUnion&);
  RuntimeUnion(RuntimeUnion&&) noexcept;
  auto operator=(const RuntimeUnion&) -> RuntimeUnion&;
  auto operator=(RuntimeUnion&&) noexcept -> RuntimeUnion&;
  ~RuntimeUnion();

  // Reads member `index`, which must be the live one. A cross-member read is
  // undefined in SV (LRM 7.3); this backend stores only the active member and
  // does not yet synthesize an inactive member's default, so it reports the
  // read rather than inventing a value of that member's domain.
  [[nodiscard]] auto Member(std::size_t index) const -> RuntimeValue;

  // Makes `index` the live member, carrying `value` (the activating write of a
  // member, and the whole-value rebuild a build primitive produces).
  void SetActive(std::size_t index, RuntimeValue value);

  // LRM 11.4.5 `==` / `!=` (Any data type): equal only when the same member is
  // active and its values compare equal, never a cross-member comparison.
  [[nodiscard]] auto operator==(const RuntimeUnion& other) const -> PackedArray;
  [[nodiscard]] auto operator!=(const RuntimeUnion& other) const -> PackedArray;

  // LRM 11.4.5 `===` / `!==` (case equality): same active member and
  // bit-for-bit identical value.
  [[nodiscard]] auto CaseEqual(const RuntimeUnion& other) const -> PackedArray;

  // LRM 9.4.2 update-event predicate (engine change-detection hook): changed
  // when the active member changed or the active value's bits changed.
  [[nodiscard]] auto IsBitIdentical(const RuntimeUnion& other) const -> bool;

  // LRM 20.9 `$isunknown`: the active member's unknown bits propagate up.
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> PackedArray;

 private:
  std::size_t active_index_ = 0;
  // A single active member. Held in a vector so the header tolerates the
  // incomplete `RuntimeValue`, the same idiom the runtime aggregates share; it
  // always holds exactly one element.
  std::vector<RuntimeValue> active_;
};

static_assert(LyraValue<RuntimeUnion>);
static_assert(CaseEqualComparable<RuntimeUnion>);

}  // namespace lyra::value

#pragma once

#include <cstddef>

#include "lyra/value/any_value.hpp"
#include "lyra/value/basic_union.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/runtime_union.hpp"

namespace lyra::value {

// A tagged union (LRM 7.3.2 / 11.9) as the library holds one: a type-checked
// sum whose tag, the declaration-order index of the live member, is part of the
// value. The code reaching a member checks the tag first (LRM 11.9), so a
// member is reached here only where the tag names it. Re-tagging goes through a
// whole-value build, never a member write. A `void` member carries an `Empty`
// payload like any other component, so nothing here treats it apart.
class RuntimeTaggedUnion : public BasicUnion<RuntimeTaggedUnion, HeldMember> {
 public:
  // LRM 11.9: an uninitialized tagged union is undefined; the deterministic
  // stand-in is tag 0 with the first member's default, supplied by the lowering
  // at each default-init site. No program may depend on it.
  RuntimeTaggedUnion() = default;
  RuntimeTaggedUnion(std::size_t tag, AnyValue payload);

  // The live tag, as the small non-negative integer a check before a member
  // access and a pattern compare against a constant tag (LRM 11.9, 12.6).
  [[nodiscard]] auto Tag() const -> std::size_t;

  // Reads member `index`, which the tag names.
  [[nodiscard]] auto Component(std::size_t index) const -> const AnyValue&;

  // Replaces the payload of member `index`, which the tag names.
  void SetComponent(std::size_t index, AnyValue value);

 private:
  friend BasicUnion<RuntimeTaggedUnion, HeldMember>;

  explicit RuntimeTaggedUnion(HeldMember live);
};

static_assert(LyraValue<RuntimeTaggedUnion>);
static_assert(CaseEqualComparable<RuntimeTaggedUnion>);

}  // namespace lyra::value

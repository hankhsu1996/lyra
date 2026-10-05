#pragma once

#include <cstddef>
#include <utility>
#include <variant>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/basic_union.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// A tagged union (LRM 7.3.2 / 11.9) compiled with its members' C++ types: a
// type-checked sum whose tag, the declaration-order index of the live member,
// is part of the value. Every access -- read or write through the dot-notation
// surface -- requires the tag to match the current one, and a mismatch is a
// run-time error. Members are reached by index, never by type, because a
// tagged union may declare two members of the same type (`union tagged { int
// A; int B; }`). A `void` member -- allowed only in tagged unions (LRM 7.3.2)
// -- is an ordinary component whose type carries no bits, so nothing here
// treats it apart.
template <typename... Ts>
class TaggedUnion
    : public BasicUnion<TaggedUnion<Ts...>, VariantMember<Ts...>> {
  using Base = BasicUnion<TaggedUnion<Ts...>, VariantMember<Ts...>>;

 public:
  // LRM 11.9: an uninitialized variable of tagged union type is undefined,
  // including its tag bits. The deterministic stand-in is tag 0 with the first
  // component's default value, which is what `std::variant`'s default
  // construction already yields. No program may depend on it.
  TaggedUnion() = default;

  // A tagged value whose live member is component `I`, carrying `value`. The
  // index form (not a type form) is mandatory because components may repeat.
  template <std::size_t I, typename V>
  [[nodiscard]] static auto Make(V&& value) -> TaggedUnion {
    TaggedUnion u;
    u.Live().Alternatives().template emplace<I>(std::forward<V>(value));
    return u;
  }

  // Asks whether component `I` is the live one, without the runtime error a
  // mismatched read raises. This is the query a caller uses to decide whether
  // a read is legal, so that the error path stays reserved for a program that
  // reads without asking (LRM 11.9).
  template <std::size_t I>
  [[nodiscard]] auto IsTagged() const -> bool {
    return this->Live().Index() == I;
  }

  // Read component `I`. LRM 11.9: reading a member whose type is inconsistent
  // with the current tag results in a run-time error.
  template <std::size_t I>
  [[nodiscard]] auto Component() const
      -> const std::variant_alternative_t<I, std::variant<Ts...>>& {
    const auto* live = std::get_if<I>(&this->Live().Alternatives());
    if (live == nullptr) {
      throw SimulationError(
          "read of a tagged union member inconsistent with the current tag "
          "(LRM 11.9)");
    }
    return *live;
  }

  // The writable location of component `I`. LRM 11.9: assigning a member whose
  // type is inconsistent with the current tag is a run-time error, so
  // re-tagging goes through a whole-value `tagged` construction.
  template <std::size_t I>
  [[nodiscard]] auto ComponentRef()
      -> std::variant_alternative_t<I, std::variant<Ts...>>& {
    auto* live = std::get_if<I>(&this->Live().Alternatives());
    if (live == nullptr) {
      throw SimulationError(
          "write to a tagged union member inconsistent with the current tag "
          "(LRM 11.9)");
    }
    return *live;
  }

 private:
  friend Base;

  explicit TaggedUnion(VariantMember<Ts...> live) : Base(std::move(live)) {
  }
};

static_assert(LyraValue<TaggedUnion<PackedArray, PackedArray>>);
static_assert(LyraValue<TaggedUnion<Empty, PackedArray>>);
static_assert(CaseEqualComparable<TaggedUnion<PackedArray, PackedArray>>);

}  // namespace lyra::value

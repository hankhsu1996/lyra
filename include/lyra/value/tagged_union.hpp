#pragma once

#include <cstddef>
#include <utility>
#include <variant>

#include "lyra/value/basic_union.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::value {

// A tagged union (LRM 7.3.2 / 11.9) compiled with its members' C++ types: a
// type-checked sum whose tag, the declaration-order index of the live member,
// is part of the value. The code reaching a member checks the tag first, so a
// member is reached here only where the tag names it. Members are reached by
// index, never by type, because a tagged union may declare two members of the
// same type (`union tagged { int A; int B; }`). A `void` member -- allowed
// only in tagged unions (LRM 7.3.2) -- is an ordinary component whose type
// carries no bits, so nothing here treats it apart.
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

  // Whether the tag names component `I`: what a check before an access and a
  // pattern ask (LRM 11.9, 12.6).
  template <std::size_t I>
  [[nodiscard]] auto IsTagged() const -> bool {
    return this->Live().Index() == I;
  }

  // Read component `I`, which the tag names.
  template <std::size_t I>
  [[nodiscard]] auto Component() const
      -> const std::variant_alternative_t<I, std::variant<Ts...>>& {
    const auto* live = std::get_if<I>(&this->Live().Alternatives());
    if (live == nullptr) {
      RefuseMemberTheTagDoesNotName();
    }
    return *live;
  }

  // The writable location of component `I`, which the tag names. Re-tagging
  // goes through a whole-value `tagged` construction (LRM 11.9).
  template <std::size_t I>
  [[nodiscard]] auto ComponentRef()
      -> std::variant_alternative_t<I, std::variant<Ts...>>& {
    auto* live = std::get_if<I>(&this->Live().Alternatives());
    if (live == nullptr) {
      RefuseMemberTheTagDoesNotName();
    }
    return *live;
  }

 private:
  friend Base;

  explicit TaggedUnion(VariantMember<Ts...> live) : Base(std::move(live)) {
  }
};

static_assert(LyraValue<TaggedUnion<Int, LogicVector<8>>>);
static_assert(LyraValue<TaggedUnion<Empty, LogicVector<8>>>);
static_assert(CaseEqualComparable<TaggedUnion<Int, LogicVector<8>>>);

}  // namespace lyra::value

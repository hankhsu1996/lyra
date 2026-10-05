#pragma once

#include <cstddef>
#include <utility>
#include <variant>

#include "lyra/value/basic_union.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// An untagged unpacked union (LRM 7.3) compiled with its members' C++ types:
// holds one member at a time, identified by a declaration-order index, since a
// union may declare two members of the same type. SystemVerilog gives no
// reliable semantics to reading a member other than the one last written, so
// the union stores only the live member; a read of another returns that
// member's default -- a deterministic answer to an operation SV leaves
// undefined, not a value any program may depend on.
template <typename... Ts>
class Union : public BasicUnion<Union<Ts...>, VariantMember<Ts...>> {
  using Base = BasicUnion<Union<Ts...>, VariantMember<Ts...>>;

 public:
  // LRM Table 7-1: an unpacked union defaults to its first member. The default
  // construction is a placeholder; the lowering emits an explicit first-member
  // default value at each default-init site.
  Union() = default;

  // A union whose live member is component `I`, carrying `value`. The index
  // form (not a type form) is mandatory because components may repeat.
  template <std::size_t I, typename V>
  [[nodiscard]] static auto Make(V&& value) -> Union {
    Union u;
    u.Live().Alternatives().template emplace<I>(std::forward<V>(value));
    return u;
  }

  // Read component `I` (the read side of member access): the value where `I`
  // is the live member, and that component's default otherwise.
  template <std::size_t I>
  [[nodiscard]] auto Component() const
      -> std::variant_alternative_t<I, std::variant<Ts...>> {
    using Alternative = std::variant_alternative_t<I, std::variant<Ts...>>;
    if (const auto* live = std::get_if<I>(&this->Live().Alternatives())) {
      return *live;
    }
    return Alternative{};
  }

  // The writable location of component `I` (the write side of member access),
  // making `I` live first if it is not -- so a write activates the member it
  // targets. A read never activates; only a write takes this reference, so
  // `u.f = v`, `u.f op= v`, and a nested `u.f.g = v` all compose on it the way
  // a struct member's reference does.
  template <std::size_t I>
  [[nodiscard]] auto ComponentRef()
      -> std::variant_alternative_t<I, std::variant<Ts...>>& {
    auto& alternatives = this->Live().Alternatives();
    if (auto* live = std::get_if<I>(&alternatives)) {
      return *live;
    }
    return alternatives.template emplace<I>();
  }

 private:
  friend Base;

  explicit Union(VariantMember<Ts...> live) : Base(std::move(live)) {
  }
};

static_assert(LyraValue<Union<PackedArray, PackedArray>>);
static_assert(CaseEqualComparable<Union<PackedArray, PackedArray>>);
static_assert(NetResolvable<Union<PackedArray, PackedArray>>);

}  // namespace lyra::value

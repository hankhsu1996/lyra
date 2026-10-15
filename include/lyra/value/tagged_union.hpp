#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <span>
#include <type_traits>
#include <utility>

#include "lyra/value/concepts.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/union.hpp"
#include "lyra/value/union_layout.hpp"

namespace lyra::value {

// A tagged union (LRM 7.3.2, 11.9): its members overlaid as an untagged
// union's are, each at the start of one storage, and after them the tag, the
// declaration-order position of the member it holds. A member is reached where
// it lies only once the code reaching it has checked the tag, so nothing here
// checks it. The tag is the one thing that knows which member is there, so it
// is what copying, moving and ending a union whose member may own something
// follow, as a sum type's own operations follow its discriminant; one whose
// members own nothing is its bytes, copied and ended as bytes are, as a C++
// union of such members is. A union built holding a member has every byte
// the member does not take zero, and a copy carries every byte over. A `void`
// member is one whose type carries no bits. A tag naming no member is bits
// another member of an untagged union holding this one wrote, and only a
// union whose members own nothing is held so (LRM 7.3), so it equals nothing
// and holds no unknown.
template <typename... Ts>
class TaggedUnion {
  using Storage = detail::UnionStorage<Ts...>;
  // The smallest unsigned integer that counts the members.
  using Tag = std::conditional_t<
      TagBytesFor(sizeof...(Ts)) == 1, std::uint8_t,
      std::conditional_t<
          TagBytesFor(sizeof...(Ts)) == 2, std::uint16_t, std::uint32_t>>;
  // Whether comparing each member answers in four states, one entry per
  // member, so a union of any member count states it without one expression
  // nesting over all of them.
  static constexpr std::array<StateDomain, sizeof...(Ts)>
      kMemberEqualityDomains{
          decltype(std::declval<const Ts&>() == std::declval<const Ts&>())::
              kDomain...};
  // Whether no member owns anything, so the union is its bytes. One entry per
  // member, for the reason the entry above is.
  static constexpr std::array<bool, sizeof...(Ts)> kMembersOwningNothing{
      std::is_trivially_copyable_v<Ts>...};
  static constexpr bool kOwnsNothing =
      std::ranges::all_of(kMembersOwningNothing, std::identity{});

 public:
  template <std::size_t I>
  using Member = typename Storage::template Member<I>;

  // What comparing two tagged unions answers (LRM 11.4.5): one bit, unknown
  // where any member can hold x or z.
  using Equality = OneBit<
      std::ranges::find(kMemberEqualityDomains, StateDomain::kFourState) !=
              kMemberEqualityDomains.end()
          ? StateDomain::kFourState
          : StateDomain::kTwoState>;

  // LRM 11.9 leaves an uninitialized tagged union undefined, tag included; it
  // holds its first member at that member's default.
  TaggedUnion()
      : TaggedUnion(
            detail::Building<0, const Member<0>&>{
                detail::DefaultOf<Member<0>>()}) {
  }

  template <std::size_t I, typename V>
  [[nodiscard]] static auto Make(V&& value) -> TaggedUnion {
    return TaggedUnion(detail::Building<I, V>{std::forward<V>(value)});
  }

  TaggedUnion(const TaggedUnion&)
    requires kOwnsNothing
  = default;
  TaggedUnion(TaggedUnion&&) noexcept
    requires kOwnsNothing
  = default;
  auto operator=(const TaggedUnion&) -> TaggedUnion&
    requires kOwnsNothing
  = default;
  auto operator=(TaggedUnion&&) noexcept -> TaggedUnion&
    requires kOwnsNothing
  = default;
  ~TaggedUnion()
    requires kOwnsNothing
  = default;

  TaggedUnion(const TaggedUnion& other)
    requires(!kOwnsNothing)
      : storage_(other.storage_), tag_(other.tag_) {
    WithTag(
        [&]<std::size_t I>() {
          storage_.template Build<I>(other.storage_.template At<I>());
        },
        [] {});
  }
  TaggedUnion(TaggedUnion&& other) noexcept
    requires(!kOwnsNothing)
      : storage_(other.storage_), tag_(other.tag_) {
    WithTag(
        [&]<std::size_t I>() {
          storage_.template Build<I>(
              std::move(other.storage_.template At<I>()));
        },
        [] {});
  }
  auto operator=(const TaggedUnion& other) -> TaggedUnion&
    requires(!kOwnsNothing)
  {
    if (this != &other) {
      End();
      std::construct_at(this, other);
    }
    return *this;
  }
  auto operator=(TaggedUnion&& other) noexcept -> TaggedUnion&
    requires(!kOwnsNothing)
  {
    if (this != &other) {
      End();
      std::construct_at(this, std::move(other));
    }
    return *this;
  }
  ~TaggedUnion()
    requires(!kOwnsNothing)
  {
    End();
  }

  // Whether the tag names member `I`: what a check before an access and a
  // pattern ask (LRM 11.9, 12.6).
  template <std::size_t I>
  [[nodiscard]] auto TagMatches() const -> bool {
    return tag_ == I;
  }

  template <std::size_t I>
  [[nodiscard]] auto Component() & -> Member<I>& {
    return storage_.template At<I>();
  }
  template <std::size_t I>
  [[nodiscard]] auto Component() const& -> const Member<I>& {
    return storage_.template At<I>();
  }

  // LRM 11.4.5: two tagged unions are equal where they hold the same member
  // and its values compare equal.
  [[nodiscard]] auto operator==(const TaggedUnion& other) const -> Equality {
    if (tag_ != other.tag_) {
      return Equality::FromBool(false);
    }
    return WithTag(
        [&]<std::size_t I>() -> Equality {
          return Convert<Equality>(
              storage_.template At<I>() == other.storage_.template At<I>());
        },
        [] { return Equality::FromBool(false); });
  }
  [[nodiscard]] auto operator!=(const TaggedUnion& other) const -> Equality {
    return Inverted(*this == other);
  }
  [[nodiscard]] auto CaseEqual(const TaggedUnion& other) const -> Bit {
    return Bit::FromBool(
        tag_ == other.tag_ &&
        WithTag(
            [&]<std::size_t I>() -> bool {
              return storage_.template At<I>()
                  .CaseEqual(other.storage_.template At<I>())
                  .IsTruthy();
            },
            [] { return false; }));
  }
  [[nodiscard]] auto IsBitIdentical(const TaggedUnion& other) const -> bool {
    return tag_ == other.tag_ &&
           WithTag(
               [&]<std::size_t I>() -> bool {
                 return storage_.template At<I>().IsBitIdentical(
                     other.storage_.template At<I>());
               },
               [&] { return storage_.SameBytes(other.storage_); });
  }
  [[nodiscard]] auto HasUnknown() const -> bool {
    return WithTag(
        [&]<std::size_t I>() -> bool {
          return storage_.template At<I>().HasUnknown();
        },
        [] { return false; });
  }

  // The positions above its widths the member the tag names keeps made clear.
  void ClearAboveWidths() {
    WithTag(
        [&]<std::size_t I>() {
          lyra::value::ClearAboveWidths(storage_.template At<I>());
        },
        [] {});
  }
  [[nodiscard]] auto IsUnknown() const -> Bit {
    return Bit::FromBool(HasUnknown());
  }

 private:
  template <std::size_t I, typename V>
  explicit TaggedUnion(detail::Building<I, V> building) : tag_(I) {
    storage_.template Build<I>(std::forward<V>(building.value));
  }

  void End() {
    WithTag(
        [&]<std::size_t I>() { std::destroy_at(&storage_.template At<I>()); },
        [] {});
  }

  // `f` called with the member the tag names, through a table indexed by the
  // tag, so nothing here is an expression over every member at once; `none`
  // where the tag names no member.
  template <typename F, typename None>
  auto WithTag(F f, None none) const -> decltype(f.template operator()<0>()) {
    using Answer = decltype(f.template operator()<0>());
    using Entry = Answer (*)(F&);
    static constexpr auto kEntries =
        []<std::size_t... I>(std::index_sequence<I...>) {
          return std::array<Entry, sizeof...(I)>{
              +[](F& g) -> Answer { return g.template operator()<I>(); }...};
        }(std::index_sequence_for<Ts...>{});
    if (tag_ >= kEntries.size()) {
      return none();
    }
    return std::span<const Entry>(kEntries)[tag_](f);
  }

  Storage storage_;
  Tag tag_;
};

static_assert(LyraValue<TaggedUnion<Int, LogicVector<8>>>);
static_assert(LyraValue<TaggedUnion<Empty, LogicVector<8>>>);
static_assert(CaseEqualComparable<TaggedUnion<Int, LogicVector<8>>>);
static_assert(std::is_trivially_copyable_v<TaggedUnion<Int, LogicVector<8>>>);

}  // namespace lyra::value

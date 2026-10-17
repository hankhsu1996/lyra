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

#include "lyra/value/integral.hpp"
#include "lyra/value/layout.hpp"
#include "lyra/value/union.hpp"

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
// the member does not take zero, and a copy carries every byte over. A tag
// naming no member is bits another member of an untagged union holding this
// one wrote, and only a union whose members own nothing is held so (LRM 7.3).
// What the union answers as a whole value is its declaration's own methods, on
// the type the unit declares over this one.
template <typename... Ts>
class TaggedUnion {
  using Storage = detail::UnionStorage<Ts...>;
  // The smallest unsigned integer that counts the members.
  using Tag = std::conditional_t<
      TagBytesFor(sizeof...(Ts)) == 1, std::uint8_t,
      std::conditional_t<
          TagBytesFor(sizeof...(Ts)) == 2, std::uint16_t, std::uint32_t>>;
  // Whether no member owns anything, so the union is its bytes. One entry per
  // member, so a union of any member count states it without one expression
  // nesting over all of them.
  static constexpr std::array<bool, sizeof...(Ts)> kMembersOwningNothing{
      std::is_trivially_copyable_v<Ts>...};
  static constexpr bool kOwnsNothing =
      std::ranges::all_of(kMembersOwningNothing, std::identity{});

 public:
  template <std::size_t I>
  using Member = typename Storage::template Member<I>;

  // LRM 11.9 leaves an uninitialized tagged union undefined, tag included; it
  // holds its first member at that member's default.
  TaggedUnion()
      : TaggedUnion(std::in_place_index<0>, detail::DefaultOf<Member<0>>()) {
  }

  // The union holding member `I` as `value`.
  template <std::size_t I, typename V>
  TaggedUnion(std::in_place_index_t<I>, V&& value) : tag_(I) {
    storage_.template Build<I>(std::forward<V>(value));
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
    WithTag([&]<std::size_t I>() {
      storage_.template Build<I>(other.storage_.template At<I>());
    });
  }
  TaggedUnion(TaggedUnion&& other) noexcept
    requires(!kOwnsNothing)
      : storage_(other.storage_), tag_(other.tag_) {
    WithTag([&]<std::size_t I>() {
      storage_.template Build<I>(std::move(other.storage_.template At<I>()));
    });
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

  // The position of the member the tag names, which a check before an access
  // and a pattern compare with the member they name (LRM 11.9, 12.6).
  [[nodiscard]] auto Discriminant() const -> std::int64_t {
    return tag_;
  }

  template <std::size_t I>
  [[nodiscard]] auto Component() & -> Member<I>& {
    return storage_.template At<I>();
  }
  template <std::size_t I>
  [[nodiscard]] auto Component() const& -> const Member<I>& {
    return storage_.template At<I>();
  }

  // The positions above its widths the member the tag names keeps made clear.
  void ClearAboveWidths() {
    WithTag([&]<std::size_t I>() {
      lyra::value::ClearAboveWidths(storage_.template At<I>());
    });
  }

 private:
  void End() {
    WithTag(
        [&]<std::size_t I>() { std::destroy_at(&storage_.template At<I>()); });
  }

  // `f` called with the member the tag names, through a table indexed by the
  // tag, so nothing here is an expression over every member at once; nothing
  // where the tag names no member, whose bytes are all there is of it.
  template <typename F>
  void WithTag(F f) const {
    using Entry = void (*)(F&);
    static constexpr auto kEntries =
        []<std::size_t... I>(std::index_sequence<I...>) {
          return std::array<Entry, sizeof...(I)>{
              +[](F& g) { g.template operator()<I>(); }...};
        }(std::index_sequence_for<Ts...>{});
    const std::span<const Entry> entries(kEntries);
    if (tag_ < entries.size()) {
      entries[tag_](f);
    }
  }

  Storage storage_;
  Tag tag_;
};

static_assert(std::is_trivially_copyable_v<TaggedUnion<Int, LogicVector<8>>>);

}  // namespace lyra::value

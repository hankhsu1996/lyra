#pragma once

#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <memory>
#include <new>
#include <tuple>
#include <type_traits>
#include <utility>

#include "lyra/value/clear_above_widths.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::value {

namespace detail {

// The storage every member of a union lies at the start of, as large as the
// largest and aligned as the strictest (LRM 7.3). A member is reached where it
// lies by retyping that start, as C reaches one.
template <typename... Ts>
class UnionStorage {
 public:
  template <std::size_t I>
  using Member = std::tuple_element_t<I, std::tuple<Ts...>>;

  template <std::size_t I>
  [[nodiscard]] auto At() -> Member<I>& {
    return *std::launder(std::bit_cast<Member<I>*>(bytes_.data()));
  }
  template <std::size_t I>
  [[nodiscard]] auto At() const -> const Member<I>& {
    return *std::launder(std::bit_cast<const Member<I>*>(bytes_.data()));
  }

  // Member `I` built from `value` over storage holding no member.
  template <std::size_t I, typename V>
  void Build(V&& value) {
    std::construct_at(
        std::bit_cast<Member<I>*>(bytes_.data()), std::forward<V>(value));
  }

 private:
  alignas(Ts...) std::array<std::byte, std::max({sizeof(Ts)...})> bytes_{};
};

}  // namespace detail

// The value of `T` the bits lying where `held` lies stand for, which is what
// reading a member of an untagged union answers whatever member wrote them
// (LRM 7.3.2).
template <typename T>
[[nodiscard]] auto ReadOverlaid(const T& held) -> T {
  T value = held;
  ClearAboveWidths(value);
  return value;
}

// An untagged unpacked union (LRM 7.3): one storage its members overlay, each
// at its start. A member is written where it lies and read as the bits there,
// whichever member wrote them, so the union records nothing of which member it
// holds. Its members own nothing, so copying it copies its bytes and ending it
// does nothing. What the union answers as a whole value is its declaration's
// own methods, on the type the unit declares over this one.
template <typename... Ts>
class Union {
  using Storage = detail::UnionStorage<Ts...>;

 public:
  template <std::size_t I>
  using Member = typename Storage::template Member<I>;

  // LRM 7.3: the first member at its default.
  Union() {
    storage_.template Build<0>(detail::DefaultOf<Member<0>>());
  }

  // The union holding member `I` as `value` and every other byte zero, which
  // a read through another member may read.
  template <std::size_t I, typename V>
  Union(std::in_place_index_t<I>, V&& value) {
    storage_.template Build<I>(std::forward<V>(value));
  }

  template <std::size_t I>
  [[nodiscard]] auto Component() & -> Member<I>& {
    return storage_.template At<I>();
  }
  template <std::size_t I>
  [[nodiscard]] auto Component() const& -> const Member<I>& {
    return storage_.template At<I>();
  }

 private:
  Storage storage_;
};

static_assert(std::is_trivially_copyable_v<Union<Int, LogicVector<33>>>);

}  // namespace lyra::value

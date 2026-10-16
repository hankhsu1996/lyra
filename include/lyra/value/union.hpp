#pragma once

#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <memory>
#include <new>
#include <tuple>
#include <type_traits>
#include <utility>

#include "lyra/value/clear_above_widths.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/small_box.hpp"

namespace lyra::value {

namespace detail {

// What a union is built from: the member it holds and the value that member
// takes.
template <std::size_t I, typename V>
struct Building {
  V&& value;
};

// What a union is built from where its first member is what `make` answers,
// built where the member lies rather than first in a frame.
template <typename Make>
struct BuiltBy {
  Make make;
};

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

  // Member `I` built from what `make` answers over storage holding no member,
  // the answer made where the member lies.
  template <std::size_t I, typename Make>
  void BuildFrom(Make& make) {
    value::BuildFrom<Member<I>>(bytes_.data(), make);
  }

  // Every byte zero, which a union whose members own nothing holds before a
  // member is built over it.
  void Clear() {
    bytes_.fill(std::byte{0});
  }

  [[nodiscard]] auto SameBytes(const UnionStorage& other) const -> bool {
    return std::memcmp(bytes_.data(), other.bytes_.data(), bytes_.size()) == 0;
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
// holds; and every whole-value operation of the union is its first member's,
// as LRM 21.2.1.6 and 11.4.14.1 have printing and streaming read that member.
// Whether the union changed is whether its bytes did, since those are what a
// read through any member sees. Its members own nothing, so copying it copies
// its bytes and ending it does nothing.
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
  [[nodiscard]] static auto Make(V&& value) -> Union {
    return Union(detail::Building<I, V>{std::forward<V>(value)});
  }

  template <std::size_t I>
  [[nodiscard]] auto Component() & -> Member<I>& {
    return storage_.template At<I>();
  }
  template <std::size_t I>
  [[nodiscard]] auto Component() const& -> const Member<I>& {
    return storage_.template At<I>();
  }

  [[nodiscard]] auto operator==(const Union& other) const {
    return *First() == *other.First();
  }
  [[nodiscard]] auto operator!=(const Union& other) const {
    return Inverted(*this == other);
  }
  [[nodiscard]] auto CaseEqual(const Union& other) const -> Bit {
    return First()->CaseEqual(*other.First());
  }
  [[nodiscard]] auto IsBitIdentical(const Union& other) const -> bool {
    return storage_.SameBytes(other.storage_);
  }
  [[nodiscard]] auto HasUnknown() const -> bool {
    return First()->HasUnknown();
  }
  [[nodiscard]] auto IsUnknown() const -> Bit {
    return Bit::FromBool(HasUnknown());
  }

  // LRM 6.7.1 admits a union whose members are all valid for a net as a net's
  // type; it resolves as its first member does.
  [[nodiscard]] auto ResolveTriState(const Union& other) const -> Union {
    return Resolved(NetResolution::kTriState, other);
  }
  [[nodiscard]] auto ResolveWiredAnd(const Union& other) const -> Union {
    return Resolved(NetResolution::kWiredAnd, other);
  }
  [[nodiscard]] auto ResolveWiredOr(const Union& other) const -> Union {
    return Resolved(NetResolution::kWiredOr, other);
  }
  [[nodiscard]] auto Dominating(const Union& weaker) const -> Union {
    return Union(
        detail::BuiltBy{[&] { return First()->Dominating(*weaker.First()); }});
  }
  template <IntegralValue Fill>
  [[nodiscard]] static auto FilledLike(const Union& prototype, const Fill& fill)
      -> Union {
    return Union(
        detail::BuiltBy{[&] { return FilledAs(*prototype.First(), fill); }});
  }

  [[nodiscard]] auto BitstreamWidth() const -> Int {
    const SmallBox<Member<0>> first = First();
    return Int::FromInt(StaticElem<Member<0>>::BitstreamWidth(&*first));
  }
  template <IntegralValue Control>
  [[nodiscard]] auto CountBits(const Control& control_bits) const -> Int {
    const SmallBox<Member<0>> first = First();
    return Int::FromInt(
        StaticElem<Member<0>>::CountBits(&*first, control_bits));
  }
  [[nodiscard]] auto WriteToStream(
      const IntegralView& stream, std::uint64_t filled) const -> std::uint64_t {
    const SmallBox<Member<0>> first = First();
    return StaticElem<Member<0>>::WriteToStream(&*first, stream, filled);
  }
  // The union a stream holds below its `taken` most significant positions:
  // its first member read out of it, every other byte zero.
  auto ReadFromStream(const ConstIntegralView& stream, std::uint64_t taken)
      -> std::uint64_t {
    storage_.Clear();
    storage_.template Build<0>(detail::DefaultOf<Member<0>>());
    return StaticElem<Member<0>>::ReadFromStream(
        stream, taken, storage_.template At<0>());
  }
  template <IntegralValue R>
  [[nodiscard]] auto ToBitstream() const -> R {
    R out = R::Zero();
    WriteToStream(out.AsMutableWords().MutableView(), 0);
    return out;
  }
  template <IntegralValue Bits>
  [[nodiscard]] static auto FromBitstream(const Bits& bits) -> Union {
    Union out;
    out.ReadFromStream(bits.AsWords().View(), 0);
    return out;
  }

 private:
  template <std::size_t I, typename V>
  explicit Union(detail::Building<I, V> building) {
    storage_.template Build<I>(std::forward<V>(building.value));
  }
  template <typename Make>
  explicit Union(detail::BuiltBy<Make> built) {
    storage_.template BuildFrom<0>(built.make);
  }

  // Member 0 read as its type holds a value, kept where a value of its type
  // may be kept, so a union whose first member is too large for a frame keeps
  // it off the stack.
  [[nodiscard]] auto First() const -> SmallBox<Member<0>> {
    SmallBox<Member<0>> first(storage_.template At<0>());
    ClearAboveWidths(*first);
    return first;
  }

  [[nodiscard]] auto Resolved(NetResolution fold, const Union& other) const
      -> Union {
    return Union(detail::BuiltBy{[&] {
      return lyra::value::Resolve(fold, *First(), *other.First());
    }});
  }

  Storage storage_;
};

static_assert(LyraValue<Union<LogicVector<4>, LogicVector<8>>>);
static_assert(CaseEqualComparable<Union<LogicVector<4>, LogicVector<8>>>);
static_assert(NetResolvable<Union<LogicVector<4>, LogicVector<8>>>);
static_assert(std::is_trivially_copyable_v<Union<Int, LogicVector<33>>>);

}  // namespace lyra::value

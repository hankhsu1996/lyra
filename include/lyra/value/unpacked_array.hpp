#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string_view>
#include <type_traits>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/queue.hpp"
#include "lyra/value/small_box.hpp"

namespace lyra::value {

template <typename T, std::size_t N>
class UnpackedArray;
template <typename T>
class ArraySliceRef;
class String;

// LRM 7.4.5: how many elements a slice takes, which the type the select
// produces fixes. A select never names an empty run, so a count below one is a
// lowering defect rather than a value.
[[nodiscard]] inline auto SliceCount(std::int64_t count) -> std::size_t {
  if (count < 1) {
    throw InternalError("an unpacked slice names at least one element");
  }
  return static_cast<std::size_t>(count);
}

namespace detail {

// A run of elements as the whole-value operations of a sequence read one: the
// element's operations, the count, and each element by its address.
template <typename T>
class ElementRun {
 public:
  explicit ElementRun(std::span<const T> elements) : elements_(elements) {
  }

  [[nodiscard]] static auto Element() -> const StaticElem<T>& {
    static const StaticElem<T> operations{};
    return operations;
  }
  [[nodiscard]] auto Count() const -> std::size_t {
    return elements_.size();
  }
  [[nodiscard]] auto At(std::size_t i) const -> const void* {
    return &elements_[i];
  }

 private:
  std::span<const T> elements_;
};

}  // namespace detail

// SystemVerilog fixed-size unpacked array (LRM 7.4.2): `N` elements of the C++
// type `T`, one after another, which is all an array is. One C++ layer per
// declared unpacked dimension; multi-dim composes as
// `UnpackedArray<UnpackedArray<...>, N>`, each inner array lying in its place
// in the outer. A default-constructed array holds every element at its type's
// default (LRM Table 7-1). `Element` / `Slice` are the indexed and range access
// (no `operator[]`), and `operator==` answers in the state class an element's
// own comparison does, so equality on aggregates propagates through the same
// one-bit type the integral surface uses.
//
// The elements lie in the array, and an element keeps its address for as long
// as the array holds it: an assignment writes each element where it lies.
//
// An access names an element by its ordinal, counted from the left (LRM 7.6),
// because the declared range is a fact of the static type the select was
// written against and is read there. Whole-array movement is ordinal-wise and
// range-agnostic.
template <typename T, std::size_t N>
class UnpackedArray : public OrdinalArrayMethods<UnpackedArray<T, N>, T> {
  static_assert(N >= 1, "a fixed-size unpacked array has at least one element");

 public:
  using ElementType = T;
  template <typename U>
  using Rebound = UnpackedArray<U, N>;

  UnpackedArray() = default;

  // An array of structures defaults to each element at the default its
  // structure's declaration states (LRM 7.4.5, 7.2.2), so the array's default
  // is stated by its element's as a structure's is by its declaration.
  [[nodiscard]] static auto DeclaredDefault() -> UnpackedArray
    requires HasDeclaredDefault<T>
  {
    UnpackedArray array;
    std::ranges::fill(array.items_, detail::DefaultOf<T>());
    return array;
  }

  // The array `unit` fills when laid down again and again from the left:
  // element `i` is a copy of `unit[i % unit.size()]`. Every element stated
  // once (LRM 10.9), one element default (LRM Table 7-1) and the items of a
  // `'{n{...}}` pattern (LRM 10.9.1) are each such a unit. How many elements
  // the array has is the type's, so a unit that is empty, or of which no whole
  // number fills the array, is a lowering defect.
  explicit UnpackedArray(std::span<const T> unit) {
    if (unit.empty() || N % unit.size() != 0) {
      throw InternalError(
          std::format(
              "an unpacked array of {} elements was built from a unit of {}", N,
              unit.size()));
    }
    for (std::size_t i = 0; i < N; ++i) {
      Items()[i] = unit[i % unit.size()];
    }
  }

  // LRM 5.9 / 21.3.3: a string value assigned to an unpacked array of bytes is
  // left-justified -- the first character lands at the array's left bound and
  // runs toward the right bound, an element past the end of the text keeps the
  // element type's default, and text beyond the array's last element is
  // dropped. The clause admits this form for an array of bytes alone, so the
  // element is integral.
  [[nodiscard]] static auto FromString(const String& text) -> UnpackedArray;

  // LRM 5.9: a string literal assigned to an unpacked array of bytes, under the
  // same left justification. A literal is a packed bit-vector constant, not a
  // string value, so its bytes arrive whole -- a NUL among them is a byte like
  // any other, where building a string value would have removed it (LRM 6.16).
  template <IntegralValue Bits>
  [[nodiscard]] static auto FromIntegral(const Bits& bits) -> UnpackedArray {
    return OfBytes(BytesOf(bits.AsWords().View()));
  }

  // LRM 10.10: adopt an unpacked concatenation's parts, accumulated into a
  // growable array by the concatenation chain, into this fixed-size type. The
  // element counts must agree; a mismatch the front end could not rule out --
  // because a spread part is sized at run time -- is a run-time error.
  // Templated on the accumulator so the fixed-size type needs no dependency on
  // the growable one that feeds it.
  template <typename C>
  [[nodiscard]] static auto ConformSize(const C& parts) -> UnpackedArray {
    if (parts.RawSize() != N) {
      throw SimulationError(
          std::format(
              "unpacked array concatenation yields {} elements but the "
              "fixed-size target has {} (LRM 10.10)",
              parts.RawSize(), N));
    }
    return Of(parts);
  }

  // LRM 7.6: a fixed-size unpacked array assigned an array of another unpacked
  // kind takes its elements in left-to-right order, and how many elements it
  // has is a declared property of the variable being written rather than
  // anything the source decides -- so a source of another size is a run-time
  // error and the assignment does not happen.
  template <OrdinalElements C>
  [[nodiscard]] static auto FromArray(const C& source) -> UnpackedArray {
    if (source.RawSize() != N) {
      throw SimulationError(
          std::format(
              "a fixed-size unpacked array of {} elements cannot be assigned "
              "an array of {} (LRM 7.6)",
              N, source.RawSize()));
    }
    return Of(source);
  }

  // An array whose element at ordinal `i` is a copy of what `element_at(i)`
  // names.
  template <typename ElementAt>
  [[nodiscard]] static auto Gathered(ElementAt element_at) -> UnpackedArray {
    UnpackedArray out;
    for (std::size_t i = 0; i < N; ++i) {
      out.Items()[i] = element_at(i);
    }
    return out;
  }

  UnpackedArray(const UnpackedArray&) = default;
  UnpackedArray(UnpackedArray&&) noexcept = default;
  auto operator=(const UnpackedArray&) -> UnpackedArray& = default;
  auto operator=(UnpackedArray&&) noexcept -> UnpackedArray& = default;
  ~UnpackedArray() = default;

  // LRM 7.4.2: size() yields an SV int.
  [[nodiscard]] auto Size() const -> Int {
    return Int::FromInt(static_cast<std::int64_t>(N));
  }

  [[nodiscard]] static constexpr auto RawSize() -> std::size_t {
    return N;
  }

  // The element at storage ordinal `i`, in [0, N), for a traversal that walks
  // storage in ordinal order; a position the program computed is read through
  // `Element` instead, which answers one naming no element.
  [[nodiscard]] auto RawAt(std::size_t i) const -> const T& {
    return Items()[i];
  }

  // LRM 11.4.11: the two arms of a conditional operator whose condition is
  // ambiguous, combined element by element -- an element the arms agree on
  // survives, and one they disagree on, or cannot know, takes the element
  // default (Table 7-1).
  [[nodiscard]] auto MergeConditional(const UnpackedArray& other) const
      -> UnpackedArray {
    return Gathered([&](std::size_t i) -> const T& {
      return StaticElem<T>::Agree(&RawAt(i), &other.RawAt(i))
                 ? RawAt(i)
                 : ElementDefault();
    });
  }

  // LRM 7.4.5: an invalid-index write lands where no read reaches, which is no
  // element of the array.
  [[nodiscard]] auto ElementMut(const Position& position, Formation& formed)
      -> T& {
    const auto ordinal = ElementOrdinal(position, N);
    formed = ordinal ? Formation::kExisting : Formation::kNowhere;
    return ordinal ? Items()[*ordinal] : DiscardedWrite();
  }
  [[nodiscard]] auto ElementMut(const Position& position) -> T& {
    Formation formed{};
    return ElementMut(position, formed);
  }

  // LRM 7.4.5: an invalid-index read returns the element default (LRM Table
  // 7-1).
  [[nodiscard]] auto Element(const Position& position) const -> const T& {
    const auto ordinal = ElementOrdinal(position, N);
    return ordinal ? Items()[*ordinal] : ElementDefault();
  }

  // LRM 7.4.5 contiguous-range selector: the elements from `start` that
  // `Result`, an array of this element type, has room for. An element outside
  // the array reads the element default, and a start that names no position
  // reads a wholly-default sub-array.
  template <typename Result>
  [[nodiscard]] auto Slice(const Position& start, std::int64_t count) const
      -> Result {
    Result::RequireDeclared(count);
    return Result::SliceOf(Elements(), ReadPosition(start));
  }

  [[nodiscard]] auto SliceRef(const Position& start, std::int64_t count)
      -> ArraySliceRef<T> {
    return ArraySliceRef<T>{
        MutableElements(), ReadPosition(start), SliceCount(count)};
  }

  // The slice of `elements` from `start` as an array of this type: what a
  // container of any unpacked kind answers its own slice with.
  [[nodiscard]] static auto SliceOf(
      std::span<const T> elements, std::optional<std::int64_t> start)
      -> UnpackedArray {
    const auto size = static_cast<std::int64_t>(elements.size());
    return Gathered([&](std::size_t i) -> const T& {
      const std::int64_t at =
          start.has_value() ? *start + static_cast<std::int64_t>(i) : -1;
      return at >= 0 && at < size ? elements[static_cast<std::size_t>(at)]
                                  : ElementDefault();
    });
  }

  // A slice select states how many elements it takes (LRM 7.4.5), and the
  // array type it produces fixes the same number, so a count that is not this
  // type's is a lowering defect.
  static void RequireDeclared(std::int64_t count) {
    if (count < 0 || static_cast<std::uint64_t>(count) != N) {
      throw InternalError(
          std::format(
              "an unpacked array of {} elements was stated as one of {}", N,
              count));
    }
  }

  // LRM 11.2.2 + 11.4.5 aggregate equality / case-equality. `==` / `!=`
  // propagate X / Z; `CaseEqual` returns a deterministic 0/1.
  [[nodiscard]] auto operator==(const UnpackedArray& other) const ->
      typename StaticElem<T>::Equality {
    return StaticElem<T>::Equality::Filled(
        detail::SequenceEqual(Run(), other.Run()));
  }
  [[nodiscard]] auto operator!=(const UnpackedArray& other) const ->
      typename StaticElem<T>::Equality {
    return !(*this == other);
  }
  [[nodiscard]] auto CaseEqual(const UnpackedArray& other) const -> Bit {
    return detail::SequenceCaseEqual(Run(), other.Run());
  }

  // LRM 9.4.2 update event predicate (engine change-detection hook): are the
  // two arrays element-wise bit-identical.
  [[nodiscard]] auto IsBitIdentical(const UnpackedArray& other) const -> bool {
    return detail::SequenceBitIdentical(Run(), other.Run());
  }

  // Net resolution under each of the three truth tables (LRM 6.6), and what a
  // stronger contribution leaves a weaker one (LRM 28.12.1), element by
  // element.
  [[nodiscard]] auto ResolveTriState(const UnpackedArray& other) const
      -> UnpackedArray {
    return Resolved(NetResolution::kTriState, other);
  }
  [[nodiscard]] auto ResolveWiredAnd(const UnpackedArray& other) const
      -> UnpackedArray {
    return Resolved(NetResolution::kWiredAnd, other);
  }
  [[nodiscard]] auto ResolveWiredOr(const UnpackedArray& other) const
      -> UnpackedArray {
    return Resolved(NetResolution::kWiredOr, other);
  }
  [[nodiscard]] auto Dominating(const UnpackedArray& weaker) const
      -> UnpackedArray {
    UnpackedArray out;
    for (std::size_t i = 0; i < N; ++i) {
      out.Items()[i] = RawAt(i).Dominating(weaker.RawAt(i));
    }
    return out;
  }

  // The bits the array streams as, held as a value of `R`, which is exactly as
  // wide (LRM 6.24.3).
  template <IntegralValue R>
  [[nodiscard]] auto ToBitstream() const -> R {
    R out = R::Zero();
    const std::uint64_t filled =
        WriteToStream(out.AsMutableWords().MutableView(), 0);
    if (filled != R::kWidth) {
      throw InternalError("an unpacked array streamed as other than its bits");
    }
    return out;
  }

  // `prototype`'s shape with every bit set to `fill` (LRM 6.7.1).
  template <IntegralValue Fill>
  [[nodiscard]] static auto FilledLike(
      const UnpackedArray& prototype, const Fill& fill) -> UnpackedArray {
    UnpackedArray out;
    for (std::size_t i = 0; i < N; ++i) {
      out.Items()[i] = FilledAs(prototype.RawAt(i), fill);
    }
    return out;
  }

  // LRM 20.9: any element carrying an unknown bit propagates up.
  [[nodiscard]] auto HasUnknown() const -> bool {
    return detail::SequenceHasUnknown(Run());
  }

  [[nodiscard]] auto IsUnknown() const -> Bit {
    return Bit::FromBool(HasUnknown());
  }

  // LRM 20.6.2 `$bits`: the current bit count is the sum of the elements' own
  // bit counts. A fixed-size unpacked array folds at elaboration; this path
  // serves the case where an element is itself dynamically sized.
  [[nodiscard]] auto BitstreamWidth() const -> Int {
    return Int::FromInt(detail::SequenceBitstreamWidth(Run()));
  }

  // LRM 20.9 `$countbits`: the bit stream this value contributes is its
  // elements' streams laid end to end, so the count over it is the sum of the
  // elements' own counts under the same control bits.
  template <IntegralValue Control>
  [[nodiscard]] auto CountBits(const Control& control_bits) const -> Int {
    return Int::FromInt(detail::SequenceCountBits(Run(), control_bits));
  }

  // LRM 6.24.3: the elements' own streams laid end to end into a stream below
  // its `filled` most significant positions, answering how many are filled
  // after them; and the inverse, which reads each element in turn where it
  // lies and answers how many positions it took.
  [[nodiscard]] auto WriteToStream(
      const IntegralView& stream, std::uint64_t filled) const -> std::uint64_t {
    for (std::size_t i = 0; i < N; ++i) {
      filled = StaticElem<T>::WriteToStream(&RawAt(i), stream, filled);
    }
    return filled;
  }
  auto ReadFromStream(const ConstIntegralView& stream, std::uint64_t taken)
      -> std::uint64_t {
    for (std::size_t i = 0; i < N; ++i) {
      taken = StaticElem<T>::ReadFromStream(stream, taken, Items()[i]);
    }
    return taken;
  }

  // The array a stream of exactly its bits holds (LRM 11.4.14.3).
  template <IntegralValue Bits>
  [[nodiscard]] static auto FromBitstream(const Bits& bits) -> UnpackedArray {
    UnpackedArray out;
    out.ReadFromStream(bits.AsWords().View(), 0);
    return out;
  }

  // The elements, in ordinal order, where they lie.
  [[nodiscard]] auto Elements() const -> std::span<const T> {
    return {Items(), N};
  }
  [[nodiscard]] auto MutableElements() -> std::span<T> {
    return {Items(), N};
  }

 private:
  friend class OrdinalArrayMethods<UnpackedArray<T, N>, T>;

  [[nodiscard]] auto Items() const -> const T* {
    return items_.data();
  }
  [[nodiscard]] auto Items() -> T* {
    return items_.data();
  }
  [[nodiscard]] auto Run() const -> detail::ElementRun<T> {
    return detail::ElementRun<T>{Elements()};
  }

  // The element type's default (LRM Table 7-1), which a read answers with
  // where no element of the array is named.
  [[nodiscard]] static auto ElementDefault() -> const T& {
    return detail::DefaultOf<T>();
  }

  // Where a write through an invalid index lands (LRM 7.4.5): storage no read
  // reaches, holding the element default again each time it is handed out so
  // a discarded write never shows through a later access.
  [[nodiscard]] static auto DiscardedWrite() -> T& {
    return detail::DiscardedWriteOf<T>();
  }

  // An array holding copies of `source`'s elements, in order.
  template <typename C>
  [[nodiscard]] static auto Of(const C& source) -> UnpackedArray {
    return Gathered([&](std::size_t i) -> const T& { return source.RawAt(i); });
  }

  // Left-justifies a byte sequence: the first byte lands at the array's left
  // bound and runs toward the right, an element past the end of the sequence
  // keeps the element type's default, and bytes beyond the last element are
  // dropped.
  [[nodiscard]] static auto OfBytes(std::string_view bytes) -> UnpackedArray {
    UnpackedArray out;
    for (std::size_t i = 0; i < std::min(N, bytes.size()); ++i) {
      out.Items()[i] = T::FromInt(static_cast<unsigned char>(bytes[i]));
    }
    return out;
  }

  [[nodiscard]] auto Resolved(
      NetResolution fold, const UnpackedArray& other) const -> UnpackedArray {
    UnpackedArray out;
    for (std::size_t i = 0; i < N; ++i) {
      out.Items()[i] = Resolve(fold, RawAt(i), other.RawAt(i));
    }
    return out;
  }

  // Puts the value that was at `order[k]` at ordinal `k` (LRM 7.12.2). The
  // values as they were are kept aside while they are put back, as a frame
  // keeps a value aside, since a copy of them is as large as the array.
  void Permute(std::span<const std::size_t> order) {
    const SmallBox<std::array<T, N>> before(items_);
    for (std::size_t k = 0; k < order.size(); ++k) {
      Items()[k] = (*before)[order[k]];
    }
  }

  std::array<T, N> items_{};
};

// LRM 7.6: an assignment to an unpacked slice is a single assignment to the
// entire slice. The proxy aliases the elements of the array it was taken from,
// plus the window's start and count, so a fixed-size unpacked array and a
// dynamic array share one slice-write surface. A start that names no position
// makes the assignment a no-op; partial-OOB behaves per-element. Move-only so
// the proxy cannot outlive what it aliases.
template <typename T>
class ArraySliceRef {
 public:
  ArraySliceRef(
      std::span<T> elements, std::optional<std::int64_t> start,
      std::size_t count)
      : elements_(elements), start_(start), count_(count) {
  }
  ArraySliceRef(const ArraySliceRef&) = delete;
  auto operator=(const ArraySliceRef&) -> ArraySliceRef& = delete;
  ArraySliceRef(ArraySliceRef&&) noexcept = default;
  auto operator=(ArraySliceRef&&) noexcept -> ArraySliceRef& = default;
  ~ArraySliceRef() = default;

  template <OrdinalElements C>
  auto operator=(const C& value) -> ArraySliceRef& {
    Assign(value);
    return *this;
  }

  // The assignment, answering whether any element of the window took a
  // different value (LRM 4.3).
  template <OrdinalElements C>
  auto Assign(const C& value) -> bool {
    if (!start_.has_value()) {
      return false;
    }
    const auto size = static_cast<std::int64_t>(elements_.size());
    bool moved = false;
    for (std::size_t i = 0; i < count_; ++i) {
      const std::int64_t at = *start_ + static_cast<std::int64_t>(i);
      if (at < 0 || at >= size) {
        continue;
      }
      T& element = elements_[static_cast<std::size_t>(at)];
      if (element.IsBitIdentical(value.RawAt(i))) {
        continue;
      }
      element = value.RawAt(i);
      moved = true;
    }
    return moved;
  }

 private:
  std::span<T> elements_;
  std::optional<std::int64_t> start_;
  std::size_t count_;
};

static_assert(LyraValue<UnpackedArray<LogicVector<4>, 3>>);
static_assert(Sized<UnpackedArray<LogicVector<4>, 3>>);
static_assert(BitstreamSizable<UnpackedArray<LogicVector<4>, 3>>);
static_assert(Indexable<UnpackedArray<LogicVector<4>, 3>>);
static_assert(SliceableRef<UnpackedArray<LogicVector<4>, 3>>);
static_assert(ConditionallyMergeable<UnpackedArray<LogicVector<4>, 3>>);
static_assert(Sortable<UnpackedArray<LogicVector<4>, 3>>);
static_assert(NetResolvable<UnpackedArray<LogicVector<4>, 3>>);
static_assert(
    NetResolvable<UnpackedArray<UnpackedArray<LogicVector<4>, 3>, 2>>);
static_assert(OrdinalElements<UnpackedArray<LogicVector<4>, 3>>);
static_assert(std::is_trivially_copyable_v<UnpackedArray<LogicVector<4>, 3>>);
static_assert(
    sizeof(UnpackedArray<LogicVector<8>, 4>) == 4 * sizeof(LogicVector<8>));
static_assert(
    sizeof(UnpackedArray<LogicVector<8>, kMaxInlineBytes>) ==
    kMaxInlineBytes * sizeof(LogicVector<8>));

}  // namespace lyra::value

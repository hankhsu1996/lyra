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
#include "lyra/base/source_location.hpp"
#include "lyra/value/array_ordering.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/positions.hpp"
#include "lyra/value/small_box.hpp"
#include "lyra/value/tuple.hpp"
#include "lyra/value/value_type_kind.hpp"

namespace lyra::value {

template <typename T, std::size_t N>
class UnpackedArray;
class String;

// LRM 7.6: `value` written into the `count` of `elements` from `start` as one
// assignment, answering the positions from the first element that took a
// different value to one past the last, the same position twice where none
// did (LRM 4.3). A start that names no position assigns nothing, and an
// element outside the array is skipped. The elements are written in order, so
// the first to change is the one found while nothing has yet.
template <typename T, OrdinalElements C>
auto AssignRun(
    std::span<T> elements, std::optional<std::int64_t> start, std::size_t count,
    const C& value) -> Tuple<Position, Position> {
  std::int64_t begin = 0;
  std::int64_t end = 0;
  const auto size = static_cast<std::int64_t>(elements.size());
  // A start at or past the end names no element of the window, which also
  // keeps the positions below from passing the largest one.
  if (!start.has_value() || *start >= size) {
    return Tuple<Position, Position>{
        Position::FromInt(begin), Position::FromInt(end)};
  }
  for (std::size_t i = 0; i < count; ++i) {
    const std::int64_t at = *start + static_cast<std::int64_t>(i);
    if (at < 0 || at >= size) {
      continue;
    }
    T& element = elements[static_cast<std::size_t>(at)];
    if (element.IsBitIdentical(value.GetUnchecked(i))) {
      continue;
    }
    element = value.GetUnchecked(i);
    if (end == 0) {
      begin = at;
    }
    end = at + 1;
  }
  return Tuple<Position, Position>{
      Position::FromInt(begin), Position::FromInt(end)};
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
class UnpackedArray : public OrdinalArrayMethods<UnpackedArray<T, N>> {
  static_assert(N >= 1, "a fixed-size unpacked array has at least one element");

 public:
  static constexpr ValueTypeKind kKind = ValueTypeKind::kFixedArray;

  UnpackedArray() = default;

  // An array at the value every element starts at, then written by `fill`,
  // which is handed it: made where whoever asked for it keeps it, since what
  // this answers is the array being made and never a copy of one made here
  // (C++17 guaranteed copy elision), so an array too large for a frame is
  // never one in the frame of the operation computing it.
  template <typename Fill>
  [[nodiscard]] static auto Built(Fill fill) -> UnpackedArray {
    return UnpackedArray(WrittenBy<Fill>{.fill = &fill});
  }
  // The same, starting from a copy of `start`.
  template <typename Fill>
  [[nodiscard]] static auto BuiltFrom(const UnpackedArray& start, Fill fill)
      -> UnpackedArray {
    return UnpackedArray(start, WrittenBy<Fill>{.fill = &fill});
  }

  // An array of structures or unions defaults to each element at the default
  // its declaration states (LRM 7.4.5, 7.2.2), so the array's default is stated
  // by its element's as a record's is by its declaration.
  [[nodiscard]] static auto DeclaredDefault() -> UnpackedArray
    requires HasDeclaredDefault<T>
  {
    return Built([](UnpackedArray& array) {
      std::ranges::fill(array.items_, DefaultOf<T>());
    });
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

  // An array whose element at ordinal `i` is a copy of what `element_at(i)`
  // names.
  template <typename ElementAt>
  [[nodiscard]] static auto Gathered(ElementAt element_at) -> UnpackedArray {
    return Built([&](UnpackedArray& out) {
      for (std::size_t i = 0; i < N; ++i) {
        out.Items()[i] = element_at(i);
      }
    });
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

  [[nodiscard]] static constexpr auto Count() -> std::size_t {
    return N;
  }

  // The element at the index `i`, which names one the array holds, as Rust's
  // `get_unchecked` reaches one: nothing checks it, so only a caller that
  // counts below `N` asks; a position the program computed goes through the
  // checked element access, which answers the element default where it names
  // none (LRM 7.4.5).
  [[nodiscard]] auto GetUnchecked(std::size_t i) const -> const T& {
    return Items()[i];
  }
  [[nodiscard]] auto GetUnchecked(std::size_t i) -> T& {
    return Items()[i];
  }

  // LRM 11.4.11: the two arms of a conditional operator whose condition is
  // ambiguous, combined element by element -- an element the arms agree on
  // survives, and one they disagree on, or cannot know, takes the element
  // default (Table 7-1).
  [[nodiscard]] auto MergeConditional(const UnpackedArray& other) const
      -> UnpackedArray {
    return Gathered([&](std::size_t i) -> const T& {
      return StaticElem<T>::Agree(&GetUnchecked(i), &other.GetUnchecked(i))
                 ? GetUnchecked(i)
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
  [[nodiscard]] auto Slice(const Position& start) const -> Result {
    return Result::SliceOf(Elements(), ReadPosition(start));
  }

  // LRM 7.6: `value` written into as many elements from `start` as it holds,
  // one assignment to the whole window, answering the positions it changed
  // (LRM 4.3). Every run write is handed where it was written, which it binds
  // for what it runs.
  template <OrdinalElements C>
  auto AssignSlice(
      const Position& start, const C& value, base::SourceLocation at)
      -> Tuple<Position, Position> {
    const base::SourceLocationScope bound(at);
    return AssignRun(
        MutableElements(), ReadPosition(start), value.Count(), value);
  }

  // LRM 11.4.14.4: the stream of the `count` elements from `start`, an element
  // outside the array streaming as the element default; and this array with
  // those elements taking `values`, where a range reaching outside the array
  // is an error.
  [[nodiscard]] auto WindowToBitVec(
      const Position& start, std::int64_t count, base::SourceLocation at) const
      -> BitVec {
    const base::SourceLocationScope bound(at);
    return detail::StreamOfElements(
        StaticElem<T>{}, detail::SequenceSliceElements(
                             Run(), ReadPosition(start), WindowCount(count)));
  }
  template <OrdinalElements C>
  [[nodiscard]] auto WithWindow(
      const Position& start, std::int64_t count, const C& values,
      base::SourceLocation at) const -> UnpackedArray {
    const std::optional<std::int64_t> first = ReadPosition(start);
    if (count > 0 && (!first.has_value() || *first < 0 ||
                      *first + count > static_cast<std::int64_t>(N))) {
      const base::SourceLocationScope bound(at);
      throw SimulationError(
          "the range a 'with' names reaches outside the array it is unpacked "
          "into");
    }
    return BuiltFrom(*this, [&](UnpackedArray& out) {
      AssignRun(out.MutableElements(), first, WindowCount(count), values);
    });
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

  // LRM 11.2.2 + 11.4.5 aggregate equality / case-equality. `==` / `!=`
  // propagate X / Z; `CaseEqual` returns a deterministic 0/1.
  [[nodiscard]] auto operator==(const UnpackedArray& other) const {
    return EqualityOf<T>::Filled(
        detail::SequenceEqual(Run(), other.Run(), StaticElem<T>{}));
  }
  [[nodiscard]] auto operator!=(const UnpackedArray& other) const {
    return !(*this == other);
  }
  [[nodiscard]] auto CaseEqual(const UnpackedArray& other) const -> Bit {
    return detail::SequenceCaseEqual(Run(), other.Run(), StaticElem<T>{});
  }

  // LRM 9.4.2 update event predicate (engine change-detection hook): are the
  // two arrays element-wise bit-identical.
  [[nodiscard]] auto IsBitIdentical(const UnpackedArray& other) const -> bool {
    return detail::SequenceBitIdentical(Run(), other.Run());
  }

  // The bits the array streams as, held as a value of `R`, which is exactly as
  // wide (LRM 6.24.3).
  template <IntegralValue R>
  [[nodiscard]] auto ToBitstream(base::SourceLocation at) const -> R {
    const base::SourceLocationScope bound(at);
    return R::Built([&](R& out) {
      const std::uint64_t filled =
          WriteToStream(out.AsMutableWords().MutableView(), 0);
      if (filled != R::kWidth) {
        throw InternalError(
            "an unpacked array streamed as other than its bits");
      }
    });
  }

  // The positions a procedural continuous assignment may hold some of (LRM
  // 10.6.2): each element's, the one at ordinal 0 first.
  [[nodiscard]] static auto PositionCount() -> std::int64_t {
    return static_cast<std::int64_t>(N) * PositionCountOf<T>();
  }
  [[nodiscard]] auto TakePositions(
      const UnpackedArray& from, std::int64_t first, std::int64_t count) const
      -> UnpackedArray {
    const std::int64_t per_element = PositionCountOf<T>();
    return BuiltFrom(*this, [&](UnpackedArray& out) {
      for (std::size_t i = 0; i < N; ++i) {
        const auto at = static_cast<std::int64_t>(i) * per_element;
        const std::int64_t start = std::max(first, at);
        const std::int64_t stop = std::min(first + count, at + per_element);
        if (start < stop) {
          out.Items()[i] = PositionsTaken(
              GetUnchecked(i), from.GetUnchecked(i), start - at, stop - start);
        }
      }
    });
  }

  // The positions a view reaches of the array, its least significant position
  // standing at position `start` of the array and `count` of them, replaced
  // in place by the view's; answers whether any changed. Only the elements the
  // positions lie in are reached, by where each element's positions begin, so
  // a run over one element costs that element whatever the array's size.
  auto WritePositions(
      const ConstIntegralView& in, std::int64_t start, std::int64_t count)
      -> bool {
    const std::int64_t per_element = PositionCountOf<T>();
    const std::int64_t first = std::max<std::int64_t>(start, 0);
    const std::int64_t end =
        std::min<std::int64_t>(start + count, PositionCount());
    bool changed = false;
    for (std::int64_t element = first / per_element;
         element * per_element < end; ++element) {
      changed = WritePositionsOf(
                    GetUnchecked(static_cast<std::size_t>(element)), in,
                    start - (element * per_element), count) ||
                changed;
    }
    return changed;
  }

  // All of the array's positions as one integral value, element 0's first.
  [[nodiscard]] auto ToPositions() const {
    using Element = decltype(ToPositionsOf(std::declval<const T&>()));
    using Positions = LogicVector<N * Element::kWidth>;
    return Positions::Built([&](Positions& out) {
      for (std::size_t i = 0; i < N; ++i) {
        detail::WriteRun(
            out.MutableView(), ToPositionsOf(GetUnchecked(i)).View(),
            static_cast<std::int64_t>(i * Element::kWidth),
            {.lsb = i * Element::kWidth, .width = Element::kWidth});
      }
    });
  }

  // The array an integral value of its positions holds, element 0's first.
  template <class Positions>
  [[nodiscard]] static auto FromPositions(const Positions& positions)
      -> UnpackedArray {
    using Element = decltype(ToPositionsOf(std::declval<const T&>()));
    return Built([&](UnpackedArray& out) {
      for (std::size_t i = 0; i < N; ++i) {
        const Element part = Element::Built([&](Element& bits) {
          Extract(
              bits.MutableView(), positions.View(),
              static_cast<std::int64_t>(i * Element::kWidth));
        });
        out.GetUnchecked(i) = FromPositionsOf<T>(part);
      }
    });
  }

  // LRM 20.9: any element carrying an unknown bit propagates up.
  [[nodiscard]] auto HasUnknown() const -> bool {
    return detail::SequenceHasUnknown(Run(), StaticElem<T>{});
  }

  // LRM 20.6.2 `$bits`: the current bit count is the sum of the elements' own
  // bit counts. A fixed-size unpacked array folds at elaboration; this path
  // serves the case where an element is itself dynamically sized.
  [[nodiscard]] auto BitstreamWidth(base::SourceLocation at) const -> Int {
    const base::SourceLocationScope bound(at);
    return Int::FromInt(detail::SequenceBitstreamWidth(Run(), StaticElem<T>{}));
  }

  // LRM 20.9 `$countbits`: the bit stream this value contributes is its
  // elements' streams laid end to end, so the count over it is the sum of the
  // elements' own counts under the same control bits.
  [[nodiscard]] auto CountBits(
      const LogicVector<4>& control_bits, base::SourceLocation at) const
      -> Int {
    const base::SourceLocationScope bound(at);
    return Int::FromInt(
        detail::SequenceCountBits(Run(), control_bits, StaticElem<T>{}));
  }

  // LRM 6.24.3: the elements' own streams laid end to end into a stream below
  // its `filled` most significant positions, answering how many are filled
  // after them; and the inverse, which reads each element in turn where it
  // lies and answers how many positions it took.
  [[nodiscard]] auto WriteToStream(
      const IntegralView& stream, std::uint64_t filled) const -> std::uint64_t {
    for (std::size_t i = 0; i < N; ++i) {
      filled = StaticElem<T>::WriteToStream(&GetUnchecked(i), stream, filled);
    }
    return filled;
  }
  // An element whose width the running program decides takes every position
  // up to `until` but those the elements after it take where they are empty,
  // so the first such element takes what the rest leave (LRM 11.4.14.4).
  auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, std::uint64_t until)
      -> std::uint64_t {
    const std::uint64_t empty = StaticElem<T>::EmptyStreamWidth();
    for (std::size_t i = 0; i < N; ++i) {
      const std::uint64_t after = empty * (N - 1 - i);
      if (until - taken < after) {
        throw SimulationError(
            "a stream of bits holds fewer bits than the value it is read into "
            "takes");
      }
      taken = StaticElem<T>::ReadFromStream(
          stream, taken, until - after, &Items()[i]);
    }
    return taken;
  }

  // The array a stream of exactly its bits holds (LRM 11.4.14.3).
  template <IntegralValue Bits>
  [[nodiscard]] static auto FromBitstream(
      const Bits& bits, base::SourceLocation at) -> UnpackedArray {
    const base::SourceLocationScope bound(at);
    return Built([&](UnpackedArray& out) {
      out.ReadFromStream(bits.AsWords().View(), 0, Bits::kWidth);
    });
  }

  // The same where an element's width is the running program's: the stream as
  // a run of bits of its own, and the array exactly such a run holds.
  [[nodiscard]] auto ToBitVec(base::SourceLocation at) const -> BitVec {
    const base::SourceLocationScope bound(at);
    return StreamOf(*this, BitstreamWidth(at));
  }
  [[nodiscard]] static auto FromBitVec(
      const BitVec& run, base::SourceLocation at) -> UnpackedArray {
    const base::SourceLocationScope bound(at);
    return Built([&](UnpackedArray& out) {
      if (out.ReadFromStream(run.View(), 0, run.View().width) !=
          run.View().width) {
        throw SimulationError(
            "a stream of bits does not divide into the elements of the array "
            "it is read into");
      }
    });
  }

  // The elements, in ordinal order, where they lie.
  [[nodiscard]] auto Elements() const -> std::span<const T> {
    return Items();
  }
  [[nodiscard]] auto MutableElements() -> std::span<T> {
    return Items();
  }

 private:
  friend class OrdinalArrayMethods<UnpackedArray<T, N>>;

  // What writes an array `Built` or `BuiltFrom` makes.
  template <typename Fill>
  struct WrittenBy {
    Fill* fill;
  };
  template <typename Fill>
  explicit UnpackedArray(WrittenBy<Fill> written) : UnpackedArray() {
    (*written.fill)(*this);
  }
  template <typename Fill>
  UnpackedArray(const UnpackedArray& start, WrittenBy<Fill> written)
      : UnpackedArray(start) {
    (*written.fill)(*this);
  }

  [[nodiscard]] auto Items() const -> std::span<const T, N> {
    return items_;
  }
  [[nodiscard]] auto Items() -> std::span<T, N> {
    return items_;
  }
  [[nodiscard]] auto Run() const -> detail::ElementRun<T> {
    return detail::ElementRun<T>{Elements()};
  }

  // The element type's default (LRM Table 7-1), which a read answers with
  // where no element of the array is named.
  [[nodiscard]] static auto ElementDefault() -> const T& {
    return DefaultOf<T>();
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
    return Gathered(
        [&](std::size_t i) -> const T& { return source.GetUnchecked(i); });
  }

  // Left-justifies a byte sequence: the first byte lands at the array's left
  // bound and runs toward the right, an element past the end of the sequence
  // keeps the element type's default, and bytes beyond the last element are
  // dropped.
  [[nodiscard]] static auto OfBytes(std::string_view bytes) -> UnpackedArray {
    return Built([&](UnpackedArray& out) {
      for (std::size_t i = 0; i < std::min(N, bytes.size()); ++i) {
        out.Items()[i] = T::FromInt(static_cast<unsigned char>(bytes[i]));
      }
    });
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

static_assert(LyraValue<UnpackedArray<LogicVector<4>, 3>>);
static_assert(Sized<UnpackedArray<LogicVector<4>, 3>>);
static_assert(BitstreamSizable<UnpackedArray<LogicVector<4>, 3>>);
static_assert(Indexable<UnpackedArray<LogicVector<4>, 3>>);
static_assert(ConditionallyMergeable<UnpackedArray<LogicVector<4>, 3>>);
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

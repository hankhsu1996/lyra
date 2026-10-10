#pragma once

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/basic_dynamic_array.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/queue.hpp"

namespace lyra::value {

template <typename T>
class UnpackedArray;
template <typename T>
class ArraySliceRef;
class String;

// LRM 7.4.6: how many elements a slice takes, which the type the select
// produces fixes. A select never names an empty run, so a count below one is a
// lowering defect rather than a value.
[[nodiscard]] inline auto SliceCount(std::int64_t count) -> std::size_t {
  if (count < 1) {
    throw InternalError("an unpacked slice names at least one element");
  }
  return static_cast<std::size_t>(count);
}

// SystemVerilog fixed-size unpacked array (LRM 7.4.2): the dynamic array's run
// of elements, compiled with the element's C++ type, whose count the array's
// type fixes rather than the running program. One C++ container layer per
// declared unpacked dimension; multi-dim composes as
// `UnpackedArray<UnpackedArray<...>>`. `Element` / `Slice` are the indexed and
// range access (no `operator[]`), and `operator==` answers in the state class
// an element's own comparison does, so equality on aggregates propagates
// through the same one-bit type the integral surface uses.
//
// The payload is ordinal-only: it does not carry a declared range. An access
// names an element by its ordinal, counted from the left (LRM 7.6), because
// the declared range is a fact of the static type the select was written
// against and is read there. Whole-array movement is ordinal-wise and
// range-agnostic.
template <typename T>
class UnpackedArray : public OrdinalArrayMethods<UnpackedArray<T>, T> {
 public:
  using ElementType = T;
  template <typename U>
  using Rebound = UnpackedArray<U>;

  // Sentinel "uninitialized" form -- an empty container with no element
  // default. Used as the declared default state of a `Var<UnpackedArray<T>>`
  // field; the first MIR-level assignment overwrites the whole array (LRM 10.5
  // variable initialization).
  UnpackedArray() = default;

  // Element-list construction (LRM 10.9 assignment pattern lowering). The
  // element list is taken as a span so the emit side can hand in a
  // `std::array<T, N>{...}` literal whose self-determined type is unambiguous;
  // the element count sizes the payload.
  UnpackedArray(T element_default, std::span<const T> init)
      : UnpackedArray(std::move(element_default), init, 1) {
  }

  // LRM 10.9.1: `count` replications of `unit`, where a replication stands for
  // an entire dimension. Covers both a fixed array's all-default state (`unit`
  // is one element default, LRM Table 7-1) and an `'{count{...}}` pattern
  // (`unit` is the replicated items), which are the same repeat-and-count shape
  // and so construct through one path. Taking the two separately keeps a
  // uniform array O(unit) to build where an enumerated element list would be
  // O(unit * count).
  UnpackedArray(T element_default, std::span<const T> unit, std::size_t count)
      : core_(
            Core::Built(
                StaticElem<T>(std::move(element_default)), unit.size() * count,
                [&](std::size_t i, void* out) {
                  StaticElem<T>::Copy(&unit[i % unit.size()], out);
                })) {
  }

  // LRM 5.9 / 21.3.3: a string value assigned to an unpacked array of bytes is
  // left-justified -- the first character lands at the array's left bound and
  // runs toward the right bound, an element past the end of the text keeps the
  // element type's default, and text beyond the array's last element is
  // dropped. The clause admits this form for an array of bytes alone, so the
  // element is integral; `count` is the destination's element count.
  [[nodiscard]] static auto FromString(const String& text, std::int64_t count)
      -> UnpackedArray;

  // LRM 5.9: a string literal assigned to an unpacked array of bytes, under the
  // same left justification. A literal is a packed bit-vector constant, not a
  // string value, so its bytes arrive whole -- a NUL among them is a byte like
  // any other, where building a string value would have removed it (LRM 6.16).
  template <IntegralValue Bits>
  [[nodiscard]] static auto FromIntegral(const Bits& bits, std::int64_t count)
      -> UnpackedArray {
    return OfBytes(BytesOf(bits.Load().View()), count);
  }

  // LRM 10.10: adopt an unpacked concatenation's parts, accumulated into a
  // growable array by the concatenation chain, into this fixed-size type. The
  // element counts must agree; a mismatch the front end could not rule out --
  // because a spread part is sized at run time -- is a run-time error.
  // Templated on the accumulator so the fixed-size type needs no dependency on
  // the growable one that feeds it.
  template <typename C>
  [[nodiscard]] static auto ConformSize(const C& parts, std::int64_t count)
      -> UnpackedArray {
    if (static_cast<std::int64_t>(parts.RawSize()) != count) {
      throw SimulationError(
          std::format(
              "unpacked array concatenation yields {} elements but the "
              "fixed-size target has {} (LRM 10.10)",
              parts.RawSize(), count));
    }
    return Of(parts.ElementDefault(), parts);
  }

  // LRM 7.6: a fixed-size unpacked array assigned an array of another unpacked
  // kind takes its elements in left-to-right order, and how many elements it
  // has is a declared property of the variable being written rather than
  // anything the source decides -- so a source of another size is a run-time
  // error and the assignment does not happen. The count arrives as an operand
  // because the value carries no declared range of its own. Named because the
  // argument list cannot tell it from building over an element list.
  template <OrdinalElements C>
  [[nodiscard]] static auto FromArray(
      const C& source, T element_default, std::int64_t declared)
      -> UnpackedArray {
    if (static_cast<std::int64_t>(source.RawSize()) != declared) {
      throw SimulationError(
          std::format(
              "a fixed-size unpacked array of {} elements cannot be assigned "
              "an array of {} (LRM 7.6)",
              declared, source.RawSize()));
    }
    return Of(std::move(element_default), source);
  }

  UnpackedArray(const UnpackedArray&) = default;
  UnpackedArray(UnpackedArray&&) noexcept = default;
  auto operator=(const UnpackedArray&) -> UnpackedArray& = default;
  auto operator=(UnpackedArray&&) noexcept -> UnpackedArray& = default;
  ~UnpackedArray() = default;

  // LRM 7.4.2: size() yields an SV int.
  [[nodiscard]] auto Size() const -> Int {
    return Int::FromInt(static_cast<std::int64_t>(RawSize()));
  }

  [[nodiscard]] auto RawSize() const -> std::size_t {
    return core_.Count();
  }

  // The element at storage ordinal `i`, in [0, RawSize()), for a traversal that
  // walks storage in ordinal order; a position the program computed is read
  // through `Element` instead, which answers one naming no element.
  [[nodiscard]] auto RawAt(std::size_t i) const -> const T& {
    return *static_cast<const T*>(core_.At(i));
  }

  // The element type's default (LRM Table 7-1), the shape an out-of-range read
  // returns and a derived container seeds its own out-of-range source with.
  [[nodiscard]] auto ElementDefault() const -> const T& {
    return core_.Element().DefaultValue();
  }

  // LRM 11.4.11: the two arms of a conditional operator whose condition is
  // ambiguous, combined element by element -- an element the arms agree on
  // survives, and one they disagree on, or cannot know, takes the element
  // default (Table 7-1). Arms of unequal size put no elements in
  // correspondence, so every element takes that default.
  [[nodiscard]] auto MergeConditional(const UnpackedArray& other) const
      -> UnpackedArray {
    return UnpackedArray(core_.MergeConditional(other.core_));
  }

  // LRM 7.4.5: an invalid-index write lands where no read reaches, which is no
  // element of the array.
  [[nodiscard]] auto ElementRef(const Position& position, Formation& formed)
      -> T& {
    return *static_cast<T*>(core_.ElementRef(ReadPosition(position), formed));
  }
  [[nodiscard]] auto ElementRef(const Position& position) -> T& {
    return *static_cast<T*>(core_.ExistingAt(ReadPosition(position)));
  }

  // LRM 7.4.5: an invalid-index read returns the element default (LRM Table
  // 7-1).
  [[nodiscard]] auto Element(const Position& position) const -> const T& {
    return *static_cast<const T*>(core_.ElementAt(ReadPosition(position)));
  }

  // LRM 7.4.5 contiguous-range selector: `count` elements from `start`. An
  // element outside the array reads the element default, and a start that names
  // no position reads a wholly-default sub-array. The result is ordinal-only
  // payload.
  [[nodiscard]] auto Slice(const Position& start, std::int64_t count) const
      -> UnpackedArray {
    return SliceOf(core_, ReadPosition(start), SliceCount(count));
  }

  [[nodiscard]] auto SliceRef(const Position& start, std::int64_t count)
      -> ArraySliceRef<T> {
    return ArraySliceRef<T>{core_, ReadPosition(start), SliceCount(count)};
  }

  // LRM 11.2.2 + 11.4.5 aggregate equality / case-equality. `==` / `!=`
  // propagate X / Z; `CaseEqual` returns a deterministic 0/1.
  [[nodiscard]] auto operator==(const UnpackedArray& other) const ->
      typename StaticElem<T>::Equality {
    return StaticElem<T>::Equality::Filled(
        detail::SequenceEqual(core_, other.core_));
  }
  [[nodiscard]] auto operator!=(const UnpackedArray& other) const ->
      typename StaticElem<T>::Equality {
    return !(*this == other);
  }
  [[nodiscard]] auto CaseEqual(const UnpackedArray& other) const -> Bit {
    return detail::SequenceCaseEqual(core_, other.core_);
  }

  // LRM 9.4.2 update event predicate (engine change-detection hook): are the
  // two arrays element-wise bit-identical. A size mismatch is a change -- this
  // is how the empty default of a fresh `Var<UnpackedArray>` is detected as
  // different from the first sized write, so the declared-shape initializer
  // commits.
  [[nodiscard]] auto IsBitIdentical(const UnpackedArray& other) const -> bool {
    return detail::SequenceBitIdentical(core_, other.core_);
  }

  // Net resolution under each of the three truth tables (LRM 6.6), and what a
  // stronger contribution leaves a weaker one (LRM 28.12.1), element by
  // element.
  [[nodiscard]] auto ResolveTriState(const UnpackedArray& other) const
      -> UnpackedArray {
    return UnpackedArray(core_.Resolved(NetResolution::kTriState, other.core_));
  }
  [[nodiscard]] auto ResolveWiredAnd(const UnpackedArray& other) const
      -> UnpackedArray {
    return UnpackedArray(core_.Resolved(NetResolution::kWiredAnd, other.core_));
  }
  [[nodiscard]] auto ResolveWiredOr(const UnpackedArray& other) const
      -> UnpackedArray {
    return UnpackedArray(core_.Resolved(NetResolution::kWiredOr, other.core_));
  }
  [[nodiscard]] auto Dominating(const UnpackedArray& weaker) const
      -> UnpackedArray {
    return UnpackedArray(core_.Dominating(weaker.core_));
  }

  // The bits the array streams as, held as a value of `R`, which is exactly as
  // wide (LRM 6.24.3).
  template <IntegralValue R>
  [[nodiscard]] auto ToBitstream() const -> R {
    typename R::Words words;
    WriteToStream(words.Write(), R::kWidth, 0);
    return R::FromWords(words);
  }

  // `prototype`'s shape with every bit set to `fill` (LRM 6.7.1).
  template <IntegralValue Fill>
  [[nodiscard]] static auto FilledLike(
      const UnpackedArray& prototype, const Fill& fill) -> UnpackedArray {
    return UnpackedArray(prototype.core_.FilledLike(fill));
  }

  // LRM 20.9: any element carrying an unknown bit propagates up.
  [[nodiscard]] auto HasUnknown() const -> bool {
    return detail::SequenceHasUnknown(core_);
  }

  [[nodiscard]] auto IsUnknown() const -> Bit {
    return Bit::FromBool(HasUnknown());
  }

  // LRM 20.6.2 `$bits`: the current bit count is the sum of the elements' own
  // bit counts. A fixed-size unpacked array folds at elaboration; this path
  // serves the case where an element is itself dynamically sized.
  [[nodiscard]] auto BitstreamWidth() const -> Int {
    return Int::FromInt(detail::SequenceBitstreamWidth(core_));
  }

  // LRM 20.9 `$countbits`: the bit stream this value contributes is its
  // elements' streams laid end to end, so the count over it is the sum of the
  // elements' own counts under the same control bits.
  template <IntegralValue Control>
  [[nodiscard]] auto CountBits(const Control& control_bits) const -> Int {
    return Int::FromInt(detail::SequenceCountBits(core_, control_bits));
  }

  // LRM 6.24.3: the elements' own streams laid end to end into a stream below
  // its `filled` most significant positions, answering how many are filled
  // after them; and the inverse, which reads an array of this one's count and
  // element shapes and answers how many positions it took.
  auto WriteToStream(
      Planes stream, std::uint64_t stream_width, std::uint64_t filled) const
      -> std::uint64_t {
    return core_.WriteToStream(stream, stream_width, filled);
  }
  [[nodiscard]] auto ReadFromStream(
      ConstPlanes stream, std::uint64_t stream_width, std::uint64_t taken) const
      -> std::pair<UnpackedArray, std::uint64_t> {
    auto [read, after] = core_.ReadFromStream(stream, stream_width, taken);
    return {UnpackedArray(std::move(read)), after};
  }

  // The array a stream of exactly its bits holds, `prototype` stating the
  // element count and every element's shape (LRM 11.4.14.3).
  template <IntegralValue Bits>
  [[nodiscard]] static auto FromBitstream(
      const Bits& bits, const UnpackedArray& prototype) -> UnpackedArray {
    const typename Bits::Words words = bits.Load();
    return prototype.ReadFromStream(words.Read(), Bits::kWidth, 0).first;
  }

 private:
  friend class OrdinalArrayMethods<UnpackedArray<T>, T>;

  using Core = BasicDynamicArray<StaticElem<T>>;

  explicit UnpackedArray(Core core) : core_(std::move(core)) {
  }

  // An array of `element_default`'s type holding copies of `source`'s
  // elements, in order.
  template <typename C>
  [[nodiscard]] static auto Of(T element_default, const C& source)
      -> UnpackedArray {
    return UnpackedArray(
        Core::Built(
            StaticElem<T>(std::move(element_default)), source.RawSize(),
            [&](std::size_t i, void* out) {
              StaticElem<T>::Copy(&source.RawAt(i), out);
            }));
  }

  // Left-justifies a byte sequence into `count` elements: the first byte lands
  // at the array's left bound and runs toward the right, an element past the
  // end of the sequence keeps the element type's default, and bytes beyond the
  // last element are dropped.
  [[nodiscard]] static auto OfBytes(std::string_view bytes, std::int64_t count)
      -> UnpackedArray {
    const auto element_count = static_cast<std::size_t>(count);
    std::vector<T> elements;
    elements.reserve(element_count);
    for (std::size_t i = 0; i < element_count; ++i) {
      elements.push_back(
          i < bytes.size() ? T::FromInt(static_cast<unsigned char>(bytes[i]))
                           : T{});
    }
    return UnpackedArray{T{}, std::span<const T>{elements}};
  }

  // The `count` elements of `core` from `start` (LRM 7.4.5 / 7.4.6), as an
  // array of their own.
  [[nodiscard]] static auto SliceOf(
      const Core& core, std::optional<std::int64_t> start, std::size_t count)
      -> UnpackedArray {
    return UnpackedArray(
        Core(core.Element()).Extended(core.SliceElements(start, count)));
  }

  Core core_;

  friend class ArraySliceRef<T>;
  template <typename U>
  friend class DynamicArray;
};

// LRM 7.6: an assignment to an unpacked slice is a single assignment to the
// entire slice. The proxy aliases the elements of the array it was taken from,
// plus the window's start and count, so a fixed-size unpacked array and a
// dynamic array share one slice-write surface. A start that names no position
// makes `operator=` a no-op; partial-OOB behaves per-element. Move-only so the
// proxy cannot outlive what it aliases.
template <typename T>
class ArraySliceRef {
 public:
  ArraySliceRef(
      BasicDynamicArray<StaticElem<T>>& elements,
      std::optional<std::int64_t> start, std::size_t count)
      : elements_(&elements), start_(start), count_(count) {
  }
  ArraySliceRef(const ArraySliceRef&) = delete;
  auto operator=(const ArraySliceRef&) -> ArraySliceRef& = delete;
  ArraySliceRef(ArraySliceRef&&) noexcept = default;
  auto operator=(ArraySliceRef&&) noexcept -> ArraySliceRef& = default;
  ~ArraySliceRef() = default;

  auto operator=(const UnpackedArray<T>& value) -> ArraySliceRef& {
    Assign(value);
    return *this;
  }

  // The assignment, answering whether any element of the window took a
  // different value.
  auto Assign(const UnpackedArray<T>& value) -> bool {
    return elements_->AssignSlice(
        start_, count_, detail::OrdinalAddresses(value));
  }

 private:
  BasicDynamicArray<StaticElem<T>>* elements_;
  std::optional<std::int64_t> start_;
  std::size_t count_;
};

static_assert(LyraValue<UnpackedArray<LogicVector<4>>>);
static_assert(Sized<UnpackedArray<LogicVector<4>>>);
static_assert(BitstreamSizable<UnpackedArray<LogicVector<4>>>);
static_assert(Indexable<UnpackedArray<LogicVector<4>>>);
static_assert(Sliceable<UnpackedArray<LogicVector<4>>>);
static_assert(SliceableRef<UnpackedArray<LogicVector<4>>>);
static_assert(ConditionallyMergeable<UnpackedArray<LogicVector<4>>>);
static_assert(Sortable<UnpackedArray<LogicVector<4>>>);
static_assert(NetResolvable<UnpackedArray<LogicVector<4>>>);
static_assert(NetResolvable<UnpackedArray<UnpackedArray<LogicVector<4>>>>);
static_assert(OrdinalElements<UnpackedArray<LogicVector<4>>>);

}  // namespace lyra::value

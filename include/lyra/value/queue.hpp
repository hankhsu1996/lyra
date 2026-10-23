#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>

#include "lyra/value/array_ordering.hpp"
#include "lyra/value/basic_queue.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/take_built.hpp"

namespace lyra::value {

template <typename T, std::int64_t kBound = -1>
class Queue;

// The LRM 7.12.2 ordering methods of an unpacked array whose index is the
// ordinal position -- a fixed-size array, a dynamic array, a queue. A sort is
// handed whether the element at one position comes before the one at another,
// which the program answers, and moves the elements into that order; how they
// move is the container's own.
template <typename Self>
class OrdinalArrayMethods {
 public:
  auto Reverse() -> void {
    MutableThis().Permute(detail::ReversedPositions(Count()));
  }
  template <typename Before>
  auto Sort(Before before) -> void {
    MutableThis().Permute(detail::SortedPositions(Count(), before));
  }

 private:
  friend Self;
  OrdinalArrayMethods() = default;

  [[nodiscard]] auto MutableThis() -> Self& {
    return static_cast<Self&>(*this);
  }
  [[nodiscard]] auto Count() const -> std::size_t {
    return static_cast<const Self&>(*this).RawSize();
  }
};

// The bound a queue's C++ type states (LRM 7.10.5), `kBound`.
template <std::int64_t kBound>
struct StaticBound {
  [[nodiscard]] static constexpr auto Bound() -> std::int64_t {
    return kBound;
  }
};

// SystemVerilog queue (LRM 7.10) of the C++ type `T`, holding no element whose
// index exceeds `kBound` (LRM 7.10.5), a bound below zero being none: the
// queue every element type shares, with its elements read and written as `T`.
// Default value is the empty queue (LRM Table 6-7). What an invalid-index read
// returns, and what growth (the `q[$+1]` append) and an empty-queue pop start
// as, is the element type's default (LRM Table 7-1), which `T`'s own default
// construction builds.
template <typename T, std::int64_t kBound>
class Queue : public OrdinalArrayMethods<Queue<T, kBound>> {
 public:
  using ElementType = T;

  Queue() = default;

  // LRM 10.9.1 assignment-pattern construction: the elements of the pattern's
  // list, in order.
  explicit Queue(std::span<const T> init) {
    core_.Assign(detail::ReplicatedAddresses(init, 1));
  }

  // LRM 10.9.1: a queue built from an assignment pattern or a replication,
  // `count` replications of `unit`, held to the bound. Taking the repeat unit
  // and the count separately keeps the value O(unit) to build rather than
  // O(unit * count).
  Queue(std::span<const T> unit, std::size_t count) {
    core_.Assign(detail::ReplicatedAddresses(unit, count));
  }

  // LRM 7.6: a queue assigned an array of any of the three unpacked kinds is
  // resized to the source's element count and takes its elements in
  // left-to-right order, held to the bound as any other write is. The clause
  // admits the assignment only where the element types are equivalent, so
  // each element crosses as it stands. Named because the argument list cannot
  // tell it from building over an element list.
  template <OrdinalElements C>
  [[nodiscard]] static auto FromArray(const C& source) -> Queue {
    Queue result;
    result.core_.Assign(detail::OrdinalAddresses(source));
    return result;
  }

  // LRM 7.10.5: a queue of another bound assigned to one of this type keeps
  // only what this bound admits. The bound is the destination's, so the two
  // types never convert without being asked to.
  template <std::int64_t kFrom>
  [[nodiscard]] static auto ConformBound(const Queue<T, kFrom>& source)
      -> Queue {
    return FromArray(source);
  }

  Queue(const Queue&) = default;
  Queue(Queue&&) noexcept = default;
  auto operator=(const Queue&) -> Queue& = default;
  auto operator=(Queue&&) noexcept -> Queue& = default;
  ~Queue() = default;

  // LRM 7.10.2.1: size() yields an SV int.
  [[nodiscard]] auto Size() const -> Int {
    return Int::FromInt(static_cast<std::int64_t>(RawSize()));
  }

  [[nodiscard]] auto RawSize() const -> std::size_t {
    return core_.Count();
  }

  [[nodiscard]] auto RawAt(std::size_t i) const -> const T& {
    return *static_cast<const T*>(core_.At(i));
  }

  [[nodiscard]] auto operator==(const Queue& other) const ->
      typename StaticElem<T>::Equality {
    return StaticElem<T>::Equality::Filled(
        detail::SequenceEqual(core_, other.core_));
  }
  [[nodiscard]] auto operator!=(const Queue& other) const ->
      typename StaticElem<T>::Equality {
    return !(*this == other);
  }
  [[nodiscard]] auto CaseEqual(const Queue& other) const -> Bit {
    return detail::SequenceCaseEqual(core_, other.core_);
  }
  [[nodiscard]] auto IsBitIdentical(const Queue& other) const -> bool {
    return detail::SequenceBitIdentical(core_, other.core_);
  }
  [[nodiscard]] auto HasUnknown() const -> bool {
    return detail::SequenceHasUnknown(core_);
  }
  [[nodiscard]] auto IsUnknown() const -> Bit {
    return Bit::FromBool(HasUnknown());
  }
  [[nodiscard]] auto BitstreamWidth() const -> Int {
    return Int::FromInt(detail::SequenceBitstreamWidth(core_));
  }
  // LRM 6.24.3: the elements' own streams laid end to end below the `filled`
  // most significant positions of `stream`, the one at index 0 first, and the
  // inverse, as many elements as fill the stream up to `until`.
  [[nodiscard]] auto WriteToStream(
      const IntegralView& stream, std::uint64_t filled) const -> std::uint64_t {
    return core_.WriteToStream(stream, filled);
  }
  auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, std::uint64_t until)
      -> std::uint64_t {
    return core_.ReadFromStream(stream, taken, until);
  }
  // The same as a run of bits of its own, and the queue exactly that run holds.
  [[nodiscard]] auto ToBitVec() const -> BitVec {
    return StreamOf(*this);
  }
  [[nodiscard]] static auto FromBitVec(const BitVec& run) -> Queue {
    Queue out;
    ReadWholeStream(out, run);
    return out;
  }
  template <IntegralValue Control>
  [[nodiscard]] auto CountBits(const Control& control_bits) const -> Int {
    return Int::FromInt(detail::SequenceCountBits(core_, control_bits));
  }
  [[nodiscard]] auto MergeConditional(const Queue& other) const -> Queue {
    return Queue(core_.MergeConditional(other.core_));
  }

  // LRM 7.10.1 / 7.4.5: the element a position names, the element default
  // where it names none; the non-const form is where a write lands, which is
  // nowhere a read sees where the position names no element. A read never
  // grows the queue -- only the write form appends at `$+1`.
  [[nodiscard]] auto Element(const Position& position) -> T& {
    return *static_cast<T*>(core_.ExistingAt(ReadPosition(position)));
  }
  [[nodiscard]] auto Element(const Position& position) const -> const T& {
    return *static_cast<const T*>(core_.ElementAt(ReadPosition(position)));
  }
  [[nodiscard]] auto ElementMut(const Position& position, Formation& formed)
      -> T& {
    return *static_cast<T*>(core_.ElementMut(ReadPosition(position), formed));
  }
  [[nodiscard]] auto ElementMut(const Position& position) -> T& {
    Formation formed{};
    return ElementMut(position, formed);
  }

  // LRM 7.10.1 queue slice: the elements from position `lo` through `hi`, as
  // the queue declaring no bound that the selection is typed as.
  [[nodiscard]] auto Slice(const Position& lo, const Position& hi) const
      -> Queue<T> {
    return Queue<T>(
        core_.Slice(ReadPosition(lo), ReadPosition(hi), StaticBound<-1>{}));
  }

  // LRM 7.6 / 7.10.1: the elements of that slice take `value`, element for
  // element, an array of another count being a run-time error. Answers the
  // elements written where any of them took a different value (LRM 4.3).
  template <OrdinalElements C>
  auto AssignSlice(const Position& lo, const Position& hi, const C& value)
      -> std::optional<SliceWindow> {
    return core_.AssignSlice(
        ReadPosition(lo), ReadPosition(hi), detail::OrdinalAddresses(value));
  }

  // LRM 11.4.14.4: the stream of the `count` elements from `lo`, an element
  // outside the queue streaming as the element default; and this queue with
  // those elements taking `values`, grown first to hold them.
  [[nodiscard]] auto WindowToBitVec(
      const Position& lo, std::int64_t count) const -> BitVec {
    return core_.WindowToBitVec(ReadPosition(lo), WindowCount(count));
  }
  template <OrdinalElements C>
  [[nodiscard]] auto WithWindow(
      const Position& lo, std::int64_t count, const C& values) const -> Queue {
    return Queue(core_.WithWindow(
        ReadPosition(lo), WindowCount(count),
        detail::OrdinalAddresses(values)));
  }

  // LRM 7.10.2.7 / 7.10.2.6: append / prepend a single element.
  auto PushBack(const T& item) -> void {
    core_.PushBack(&item);
  }
  auto PushFront(const T& item) -> void {
    core_.PushFront(&item);
  }

  // LRM 7.10.2.4 / 7.10.2.5: remove and return the first / last element, the
  // element default on an empty queue.
  auto PopFront() -> T {
    return TakeBuilt<T>([&](void* out) { core_.PopFront(out); });
  }
  auto PopBack() -> T {
    return TakeBuilt<T>([&](void* out) { core_.PopBack(out); });
  }

  // LRM 7.10.2.2 / 7.10.2.3.
  auto Insert(const Position& position, const T& item) -> void {
    core_.Insert(ReadPosition(position), &item);
  }
  auto Delete() -> void {
    core_.Delete();
  }
  auto DeleteIndex(const Position& position) -> void {
    core_.DeleteIndex(ReadPosition(position));
  }

 private:
  friend class OrdinalArrayMethods<Queue<T, kBound>>;
  template <typename, std::int64_t>
  friend class Queue;

  using Core = BasicQueue<StaticElem<T>, StaticBound<kBound>>;

  explicit Queue(Core core) : core_(std::move(core)) {
  }

  // Puts the value that was at `order[k]` at position `k` (LRM 7.12.2).
  void Permute(std::span<const std::size_t> order) {
    core_.Permute(order);
  }

  Core core_;
};

static_assert(LyraValue<Queue<LogicVector<4>>>);
static_assert(Sized<Queue<LogicVector<4>>>);
static_assert(BitstreamSizable<Queue<LogicVector<4>>>);
static_assert(Indexable<Queue<LogicVector<4>>>);
// A queue's `Slice(lo, hi)` takes its element count from two bounds the
// running program can move (LRM 7.10.1), not the fixed count an array's slice
// has, so it answers a queue.
static_assert(ConditionallyMergeable<Queue<LogicVector<4>>>);
static_assert(Sortable<Queue<LogicVector<4>>>);
static_assert(OrdinalElements<Queue<LogicVector<4>>>);
static_assert(
    sizeof(Queue<LogicVector<4>, 3>) == sizeof(Queue<LogicVector<4>>));

}  // namespace lyra::value

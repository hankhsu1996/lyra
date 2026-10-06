#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <span>
#include <utility>
#include <vector>

#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/basic_queue.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

template <typename T>
class Queue;

// The LRM 7.12 methods of an unpacked array whose index is the ordinal
// position -- a fixed-size array, a dynamic array, a queue -- over the C++
// closures the C++ backend writes, each closure taking an element and its
// position. A located family answers with a queue (LRM 7.12.1); `map` answers
// with an array of the receiver's own kind over the closure's result type
// (LRM 7.12.5), which `Self` names as its `Rebound`. `proto` is what the
// producer supplies as the result's element default, and, for a reduction, its
// answer for no entries.
template <typename Self, typename T>
class OrdinalArrayMethods {
 public:
  template <typename F, typename R>
  [[nodiscard]] auto Sum(F key, R proto) const -> R {
    return detail::Folded(
        Count(), KeyOf(key), Reduction::kSum, std::move(proto));
  }
  template <typename F, typename R>
  [[nodiscard]] auto Product(F key, R proto) const -> R {
    return detail::Folded(
        Count(), KeyOf(key), Reduction::kProduct, std::move(proto));
  }
  template <typename F, typename R>
  [[nodiscard]] auto And(F key, R proto) const -> R {
    return detail::Folded(
        Count(), KeyOf(key), Reduction::kAnd, std::move(proto));
  }
  template <typename F, typename R>
  [[nodiscard]] auto Or(F key, R proto) const -> R {
    return detail::Folded(
        Count(), KeyOf(key), Reduction::kOr, std::move(proto));
  }
  template <typename F, typename R>
  [[nodiscard]] auto Xor(F key, R proto) const -> R {
    return detail::Folded(
        Count(), KeyOf(key), Reduction::kXor, std::move(proto));
  }

  template <typename F>
  [[nodiscard]] auto Find(F pred, T proto) const {
    return Elements(
        std::move(proto), detail::MatchingPositions(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindIndex(F pred, PackedArray proto) const {
    return Indices(
        std::move(proto), detail::MatchingPositions(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindFirst(F pred, T proto) const {
    return Elements(
        std::move(proto), detail::FirstMatching(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindFirstIndex(F pred, PackedArray proto) const {
    return Indices(
        std::move(proto), detail::FirstMatching(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindLast(F pred, T proto) const {
    return Elements(
        std::move(proto), detail::LastMatching(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindLastIndex(F pred, PackedArray proto) const {
    return Indices(
        std::move(proto), detail::LastMatching(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto Min(F key, T proto) const {
    return Elements(
        std::move(proto), detail::LeastPosition(Count(), KeyOf(key)));
  }
  template <typename F>
  [[nodiscard]] auto Max(F key, T proto) const {
    return Elements(
        std::move(proto), detail::GreatestPosition(Count(), KeyOf(key)));
  }
  template <typename F>
  [[nodiscard]] auto Unique(F key, T proto) const {
    return Elements(
        std::move(proto), detail::UniquePositions(Count(), KeyOf(key)));
  }
  template <typename F>
  [[nodiscard]] auto UniqueIndex(F key, PackedArray proto) const {
    return Indices(
        std::move(proto), detail::UniquePositions(Count(), KeyOf(key)));
  }

  template <typename F, typename U>
  [[nodiscard]] auto Map(F closure, U proto) const {
    return typename Self::template Rebound<U>(
        std::move(proto), detail::KeysOf(Count(), KeyOf(closure)));
  }

  // LRM 7.12.2 ordering: a positional permutation, by the closure-projected key
  // with the ordinal position as index for `sort` / `rsort`; `reverse` takes no
  // closure. How the elements move under it is the container's own.
  auto Reverse() -> void {
    MutableThis().core_.Permute(detail::ReversedPositions(Count()));
  }
  template <typename F>
  auto Sort(F key) -> void {
    MutableThis().core_.Permute(SortOrder(key, false));
  }
  template <typename F>
  auto Rsort(F key) -> void {
    MutableThis().core_.Permute(SortOrder(key, true));
  }

 private:
  friend Self;
  OrdinalArrayMethods() = default;

  [[nodiscard]] auto This() const -> const Self& {
    return static_cast<const Self&>(*this);
  }
  [[nodiscard]] auto MutableThis() -> Self& {
    return static_cast<Self&>(*this);
  }

  // The order `key` puts the elements in, ascending or descending.
  template <typename F>
  [[nodiscard]] auto SortOrder(F& key, bool descending) const
      -> std::vector<std::size_t> {
    return detail::SortedPositions(
        detail::KeysOf(Count(), KeyOf(key)), descending);
  }
  [[nodiscard]] auto Count() const -> std::size_t {
    return This().RawSize();
  }

  // What `closure` answers for the element at a position and that position.
  template <typename F>
  [[nodiscard]] auto KeyOf(F& closure) const {
    return [this, &closure](std::size_t i) {
      return closure(
          This().RawAt(i), PackedArray::Int(static_cast<std::int32_t>(i)));
    };
  }

  // The located elements, or positions, as the queue a locator answers with,
  // whose element default is the one the producer supplies. Each builds a
  // queue only where it is instantiated, which is after the queue is complete.
  template <typename Element>
  [[nodiscard]] auto Elements(
      Element proto, const std::vector<std::size_t>& positions) const
      -> Queue<Element> {
    std::vector<Element> found;
    found.reserve(positions.size());
    for (const std::size_t i : positions) {
      found.push_back(This().RawAt(i));
    }
    return Queue<Element>(std::move(proto), found);
  }
  template <typename Index>
  [[nodiscard]] static auto Indices(
      Index proto, const std::vector<std::size_t>& positions) -> Queue<Index> {
    std::vector<Index> found;
    found.reserve(positions.size());
    for (const std::size_t i : positions) {
      found.push_back(Index::Int(static_cast<std::int32_t>(i)));
    }
    return Queue<Index>(std::move(proto), found);
  }
};

// SystemVerilog queue (LRM 7.10) of the C++ type `T`: the queue every element
// type shares, with its elements read and written as `T` and the LRM 7.12
// methods run over the closures the C++ backend writes. Default value is the
// empty queue (LRM Table 6-7). The element default is what an invalid-index
// read returns and what growth (the `q[$+1]` append), slice results and an
// empty-queue pop are seeded with.
template <typename T>
class Queue : public OrdinalArrayMethods<Queue<T>, T> {
 public:
  using ElementType = T;
  template <typename U>
  using Rebound = Queue<U>;

  // Empty queue carrying no element shape, the unestablished state of a
  // freshly value-initialized cell before its declared representation is
  // installed. A declared queue is constructed through one of the seeding forms
  // below, or installed via the cell's initialization; the empty form exists
  // only for the default-constructed slots STL containers require.
  Queue() = default;

  // An empty queue of a known element shape, which a functional operation
  // yielding no element starts from.
  explicit Queue(T element_default)
      : core_(StaticElem<T>(std::move(element_default))) {
  }

  // LRM 10.9.1 assignment-pattern construction: the element default seeded,
  // elements taken from the pattern's list. The list is a span so the emit side
  // hands in a `std::array<T, N>{...}` literal, mirroring `DynamicArray`.
  Queue(T element_default, std::span<const T> init)
      : Queue(std::move(element_default)) {
    core_.Assign(detail::ReplicatedAddresses(init, 1));
  }

  // LRM 10.9.1 `'{count{...}}` pattern: `count` replications of `unit`.
  // Taking the repeat unit and the count separately keeps the value O(unit)
  // to build rather than O(unit * count).
  Queue(T element_default, std::span<const T> unit, std::size_t count)
      : Queue(std::move(element_default)) {
    core_.Assign(detail::ReplicatedAddresses(unit, count));
  }

  // LRM 7.10.5 bounded queue initialized by an assignment pattern or a
  // replication: the pattern's elements, held to the bound on entry.
  Queue(
      T element_default, std::span<const T> init, const PackedArray& max_bound)
      : Queue(std::move(element_default), init) {
    core_.SetBound(max_bound);
  }
  Queue(
      T element_default, std::span<const T> unit, std::size_t count,
      const PackedArray& max_bound)
      : Queue(std::move(element_default), unit, count) {
    core_.SetBound(max_bound);
  }

  // LRM 7.6: a queue assigned an array of any of the three unpacked kinds is
  // resized to the source's element count and takes its elements in
  // left-to-right order. The clause admits the assignment only where the
  // element types are equivalent, so each element crosses as it stands, and the
  // element default is the destination's own -- the element shape is a declared
  // property of the variable being written, which the source has no say in. LRM
  // 7.10.5 makes the bound another such property, and a bound below zero is the
  // unbounded queue, so one form covers both and the trimming is the same
  // trimming any other write gets. Named because the argument list cannot tell
  // it from building over an element list.
  template <OrdinalElements C>
  [[nodiscard]] static auto FromArray(
      const C& source, T element_default, const PackedArray& max_bound)
      -> Queue {
    Queue result(std::move(element_default));
    result.core_.SetBound(max_bound);
    result.core_.Assign(detail::OrdinalAddresses(source));
    return result;
  }

  Queue(const Queue&) = default;
  Queue(Queue&&) noexcept = default;
  auto operator=(const Queue&) -> Queue& = default;
  auto operator=(Queue&&) noexcept -> Queue& = default;
  ~Queue() = default;

  // The element shape and the LRM 7.10.5 bound are declared-type properties of
  // the destination variable, not value content. The store boundary brings the
  // right-hand side to the destination representation before a semantic store,
  // so a cross-representation source is conformed here rather than preserved by
  // a shape-keeping assignment. Returns a copy carrying `bound` (a negative
  // value means unbounded) and this queue's element shape and contents, trimmed
  // to the bound.
  [[nodiscard]] auto ConformBound(const PackedArray& bound) const -> Queue {
    return Queue(core_.WithBound(bound));
  }

  // LRM 7.10.2.1: size() yields an SV int.
  [[nodiscard]] auto Size() const -> PackedArray {
    return PackedArray::Int(static_cast<std::int32_t>(RawSize()));
  }

  [[nodiscard]] auto RawSize() const -> std::size_t {
    return core_.Count();
  }

  [[nodiscard]] auto RawAt(std::size_t i) const -> const T& {
    return *static_cast<const T*>(core_.At(i));
  }

  // The element type's default (LRM Table 7-1), the shape an out-of-range read
  // returns and a derived container seeds its own out-of-range source with.
  [[nodiscard]] auto ElementDefault() const -> const T& {
    return core_.Element().DefaultValue();
  }

  [[nodiscard]] auto ToOwned() const -> Queue {
    return *this;
  }

  [[nodiscard]] auto operator==(const Queue& other) const -> PackedArray {
    return detail::SequenceEqual(core_, other.core_);
  }
  [[nodiscard]] auto operator!=(const Queue& other) const -> PackedArray {
    return !(*this == other);
  }
  [[nodiscard]] auto CaseEqual(const Queue& other) const -> PackedArray {
    return detail::SequenceCaseEqual(core_, other.core_);
  }
  [[nodiscard]] auto IsBitIdentical(const Queue& other) const -> bool {
    return detail::SequenceBitIdentical(core_, other.core_);
  }
  [[nodiscard]] auto HasUnknown() const -> bool {
    return detail::SequenceHasUnknown(core_);
  }
  [[nodiscard]] auto IsUnknown() const -> PackedArray {
    return PackedArray::Bit(HasUnknown());
  }
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray {
    return detail::SequenceBitstreamWidth(core_);
  }
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray {
    return detail::SequenceCountBits(core_, control_bits);
  }
  [[nodiscard]] auto MergeConditional(const Queue& other) const -> Queue {
    return Queue(core_.MergeConditional(other.core_));
  }

  // LRM 7.10.1 / 7.4.5: the element a position names, the element default
  // where it names none; the non-const form is where a write lands, which is
  // nowhere a read sees where the position names no element. A read never
  // grows the queue -- only the write form appends at `$+1`.
  [[nodiscard]] auto Element(const PackedArray& position) -> T& {
    return *static_cast<T*>(core_.ExistingAt(position));
  }
  [[nodiscard]] auto Element(const PackedArray& position) const -> const T& {
    return *static_cast<const T*>(core_.ElementAt(position));
  }
  [[nodiscard]] auto ElementRef(const PackedArray& position, Formation& formed)
      -> T& {
    return *static_cast<T*>(core_.ElementRef(position, formed));
  }
  [[nodiscard]] auto ElementRef(const PackedArray& position) -> T& {
    Formation formed{};
    return ElementRef(position, formed);
  }

  // LRM 7.10.1 queue slice: the elements from position `lo` through `hi`.
  [[nodiscard]] auto Slice(const PackedArray& lo, const PackedArray& hi) const
      -> Queue {
    return Queue(core_.Slice(lo, hi));
  }

  // LRM 7.10.2.7 / 7.10.2.6: append / prepend a single element.
  auto PushBack(const T& item) -> void {
    core_.PushBack(&item);
  }
  auto PushFront(const T& item) -> void {
    core_.PushFront(&item);
  }

  // LRM 10.10 unpacked concatenation, as the two-operand steps a join folds to:
  // this queue with one element appended, or with every element of a spread
  // part appended in order.
  [[nodiscard]] auto ConcatElement(const T& item) const -> Queue {
    const std::array<const void*, 1> items{&item};
    return Queue(core_.Concat(items));
  }
  template <typename C>
  [[nodiscard]] auto ConcatSpread(const C& part) const -> Queue {
    return Queue(core_.Concat(detail::OrdinalAddresses(part)));
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
  auto Insert(const PackedArray& index, const T& item) -> void {
    core_.Insert(index, &item);
  }
  auto Delete() -> void {
    core_.Delete();
  }
  auto DeleteIndex(const PackedArray& index) -> void {
    core_.DeleteIndex(index);
  }

 private:
  friend class OrdinalArrayMethods<Queue<T>, T>;

  explicit Queue(BasicQueue<StaticElem<T>> core) : core_(std::move(core)) {
  }

  BasicQueue<StaticElem<T>> core_;
};

static_assert(LyraValue<Queue<PackedArray>>);
static_assert(Sized<Queue<PackedArray>>);
static_assert(BitstreamSizable<Queue<PackedArray>>);
static_assert(Indexable<Queue<PackedArray>>);
// A queue's `Slice(lo, hi)` takes its element count from two bounds the
// running program can move (LRM 7.10.1), not the fixed count `Sliceable` names,
// so despite the matching arity it carries its own `Slice` rather than claiming
// that concept.
static_assert(Ownable<Queue<PackedArray>>);
static_assert(ConditionallyMergeable<Queue<PackedArray>>);
static_assert(Sortable<Queue<PackedArray>>);
static_assert(OrdinalElements<Queue<PackedArray>>);

}  // namespace lyra::value

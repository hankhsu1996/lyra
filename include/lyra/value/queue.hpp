#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <span>
#include <type_traits>
#include <utility>
#include <vector>

#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/basic_queue.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/reduction.hpp"
#include "lyra/value/take_built.hpp"

namespace lyra::value {

template <typename T, std::int64_t kBound = -1>
class Queue;

// The LRM 7.12 methods of an unpacked array whose index is the ordinal
// position -- a fixed-size array, a dynamic array, a queue -- over the C++
// closures the C++ backend writes, each closure taking an element and its
// position. A located family answers with a queue (LRM 7.12.1); `map` answers
// with an array of the receiver's own kind over the closure's result type
// (LRM 7.12.5), which `Self` names as its `Rebound`. A reduction of no entries
// answers the default of the closure's result type, LRM 7.12.3 stating no
// answer of its own.
template <typename Self, typename T>
class OrdinalArrayMethods {
 public:
  template <typename F>
  [[nodiscard]] auto Sum(F key) const {
    return Reduced(key, Reduction::kSum);
  }
  template <typename F>
  [[nodiscard]] auto Product(F key) const {
    return Reduced(key, Reduction::kProduct);
  }
  template <typename F>
  [[nodiscard]] auto And(F key) const {
    return Reduced(key, Reduction::kAnd);
  }
  template <typename F>
  [[nodiscard]] auto Or(F key) const {
    return Reduced(key, Reduction::kOr);
  }
  template <typename F>
  [[nodiscard]] auto Xor(F key) const {
    return Reduced(key, Reduction::kXor);
  }

  template <typename F>
  [[nodiscard]] auto Find(F pred) const {
    return Elements(detail::MatchingPositions(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindIndex(F pred) const {
    return Indices(detail::MatchingPositions(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindFirst(F pred) const {
    return Elements(detail::FirstMatching(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindFirstIndex(F pred) const {
    return Indices(detail::FirstMatching(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindLast(F pred) const {
    return Elements(detail::LastMatching(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindLastIndex(F pred) const {
    return Indices(detail::LastMatching(Count(), KeyOf(pred)));
  }
  template <typename F>
  [[nodiscard]] auto Min(F key) const {
    return Elements(detail::LeastPosition(Count(), KeyOf(key)));
  }
  template <typename F>
  [[nodiscard]] auto Max(F key) const {
    return Elements(detail::GreatestPosition(Count(), KeyOf(key)));
  }
  template <typename F>
  [[nodiscard]] auto Unique(F key) const {
    return Elements(detail::UniquePositions(Count(), KeyOf(key)));
  }
  template <typename F>
  [[nodiscard]] auto UniqueIndex(F key) const {
    return Indices(detail::UniquePositions(Count(), KeyOf(key)));
  }

  template <typename F>
  [[nodiscard]] auto Map(F closure) const {
    const auto mapped = detail::KeysOf(Count(), KeyOf(closure));
    using U = typename decltype(mapped)::value_type;
    return typename Self::template Rebound<U>(std::span<const U>{mapped});
  }

  // LRM 7.12.2 ordering: a positional permutation, by the closure-projected key
  // with the ordinal position as index for `sort` / `rsort`; `reverse` takes no
  // closure. How the elements move under it is the container's own.
  auto Reverse() -> void {
    MutableThis().Permute(detail::ReversedPositions(Count()));
  }
  template <typename F>
  auto Sort(F key) -> void {
    MutableThis().Permute(SortOrder(key, false));
  }
  template <typename F>
  auto Rsort(F key) -> void {
    MutableThis().Permute(SortOrder(key, true));
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
          This().RawAt(i), Int::FromInt(static_cast<std::int64_t>(i)));
    };
  }

  template <typename F>
  [[nodiscard]] auto Reduced(F& key, Reduction reduction) const {
    using R = std::remove_cvref_t<std::invoke_result_t<F&, const T&, Int>>;
    return detail::Folded(
        Count(), KeyOf(key), reduction, detail::DefaultOf<R>());
  }

  // The located elements, or positions, as the queue a locator answers with
  // (LRM 7.12.1), a position being an `int`. Each is a template so that it
  // builds a queue only where it is instantiated, which is after the queue is
  // complete.
  template <typename Element = T>
  [[nodiscard]] auto Elements(const std::vector<std::size_t>& positions) const
      -> Queue<Element> {
    std::vector<Element> found;
    found.reserve(positions.size());
    for (const std::size_t i : positions) {
      found.push_back(This().RawAt(i));
    }
    return Queue<Element>(std::span<const Element>{found});
  }
  template <IntegralValue Index = Int>
  [[nodiscard]] static auto Indices(const std::vector<std::size_t>& positions)
      -> Queue<Index> {
    std::vector<Index> found;
    found.reserve(positions.size());
    for (const std::size_t i : positions) {
      found.push_back(Index::FromInt(static_cast<std::int64_t>(i)));
    }
    return Queue<Index>(std::span<const Index>{found});
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
// queue every element type shares, with its elements read and written as `T`
// and the LRM 7.12 methods run over the closures the C++ backend writes.
// Default value is the empty queue (LRM Table 6-7). What an invalid-index read
// returns, and what growth (the `q[$+1]` append) and an empty-queue pop start
// as, is the element type's default (LRM Table 7-1), which `T`'s own default
// construction builds.
template <typename T, std::int64_t kBound>
class Queue : public OrdinalArrayMethods<Queue<T, kBound>, T> {
 public:
  using ElementType = T;
  template <typename U>
  using Rebound = Queue<U>;

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
  friend class OrdinalArrayMethods<Queue<T, kBound>, T>;
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

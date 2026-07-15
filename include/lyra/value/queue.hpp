#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <functional>
#include <iostream>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <utility>

#include "lyra/value/array_case_equal.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/oob_shield.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/slice_selector.hpp"

namespace lyra::value {

// SystemVerilog queue (LRM 7.10): a variable-size ordered collection with
// efficient insertion and removal at both ends, so the storage is a
// `std::deque<T>` rather than the `std::vector<T>` the dynamic array uses.
// Default value is the empty queue (LRM Table 6-7). The element default --
// returned on an invalid-index read and copied to seed growth (the `q[$+1]`
// append), slice results, and an empty-queue pop -- and the invalid-index
// discard target are carried by an `OobShield`.
template <typename T>
class Queue {
 public:
  using ElementType = T;

  // Empty queue carrying no element shape, the unestablished state of a
  // freshly value-initialized cell before its declared representation is
  // installed. A declared queue is constructed through one of the seeding forms
  // below, or installed via the cell's initialization; the empty form exists
  // only for the default-constructed slots STL containers require.
  Queue() = default;

  // Empty queue with the shield seeded. Used for declarations like `int q[$];`
  // where the queue starts empty but the element shape is known at lowering
  // time.
  explicit Queue(T element_default) : shield_(std::move(element_default)) {
  }

  // LRM 10.9.1 assignment-pattern construction: shield seeded, elements taken
  // from the pattern's list. The list is a span so the emit side hands in a
  // `std::array<T, N>{...}` literal, mirroring `DynamicArray`.
  Queue(T element_default, std::span<const T> init)
      : shield_(std::move(element_default)), data_(init.begin(), init.end()) {
  }

  // LRM 7.10.5 bounded queue `int q[$:N]`: the empty start is unchanged, but
  // the maximum index N is recorded so growth past N+1 elements is discarded
  // with a warning. The bound arrives as a PackedArray construction argument.
  Queue(T element_default, const PackedArray& max_bound)
      : shield_(std::move(element_default)),
        max_bound_(static_cast<std::uint64_t>(max_bound.ToInt64())) {
  }

  // LRM 7.10.5 bounded queue initialized by an assignment pattern: take the
  // pattern's elements, record the bound, and trim any overflow on entry.
  Queue(
      T element_default, std::span<const T> init, const PackedArray& max_bound)
      : shield_(std::move(element_default)),
        data_(init.begin(), init.end()),
        max_bound_(static_cast<std::uint64_t>(max_bound.ToInt64())) {
    EnforceBound();
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
    const std::int64_t b = bound.ToInt64();
    Queue result = *this;
    result.max_bound_ =
        b < 0 ? std::nullopt
              : std::optional<std::uint64_t>(static_cast<std::uint64_t>(b));
    result.EnforceBound();
    return result;
  }

  // LRM 7.10.2.1: size() yields an SV int.
  [[nodiscard]] auto Size() const -> PackedArray {
    return PackedArray::Int(static_cast<std::int32_t>(RawSize()));
  }

  [[nodiscard]] auto RawSize() const -> std::size_t {
    return data_.size();
  }

  [[nodiscard]] auto RawAt(std::size_t i) const -> const T& {
    return data_[i];
  }

  [[nodiscard]] auto ToOwned() const -> Queue {
    return *this;
  }

  // LRM 11.2.2 aggregate equality / 11.4.5: element-wise reduction over
  // matching positions. A size mismatch yields 0; matching empties yield 1
  // (LRM is silent on both, matching industry convention). `==` / `!=`
  // propagate X / Z; `CaseEqual` matches X / Z as values and is deterministic.
  [[nodiscard]] auto operator==(const Queue& other) const -> PackedArray {
    if (data_.size() != other.data_.size()) {
      return PackedArray::FromInt(0, 1, false, false);
    }
    if (data_.empty()) {
      return PackedArray::FromInt(1, 1, false, false);
    }
    PackedArray result = data_[0] == other.data_[0];
    for (std::size_t i = 1; i < data_.size(); ++i) {
      result = result && (data_[i] == other.data_[i]);
    }
    return result;
  }
  [[nodiscard]] auto operator!=(const Queue& other) const -> PackedArray {
    return !(*this == other);
  }

  [[nodiscard]] auto CaseEqual(const Queue& other) const -> PackedArray {
    if (data_.size() != other.data_.size()) {
      return PackedArray::FromInt(0, 1, false, false);
    }
    if (data_.empty()) {
      return PackedArray::FromInt(1, 1, false, false);
    }
    PackedArray result = detail::ArrayCaseEqElement(data_[0], other.data_[0]);
    for (std::size_t i = 1; i < data_.size(); ++i) {
      result = result && detail::ArrayCaseEqElement(data_[i], other.data_[i]);
    }
    return result;
  }

  // LRM 9.4.2 update event predicate (engine change-detection hook): are the
  // two queues element-wise bit-identical. A size mismatch is a change.
  [[nodiscard]] auto IsBitIdentical(const Queue& other) const -> bool {
    if (data_.size() != other.data_.size()) {
      return false;
    }
    for (std::size_t i = 0; i < data_.size(); ++i) {
      if (!data_[i].IsBitIdentical(other.data_[i])) {
        return false;
      }
    }
    return true;
  }

  // LRM 20.9: any element carrying an unknown bit propagates up.
  [[nodiscard]] auto HasUnknown() const -> bool {
    for (const auto& e : data_) {
      if (e.HasUnknown()) return true;
    }
    return false;
  }

  [[nodiscard]] auto IsUnknown() const -> PackedArray {
    return PackedArray::Bit(HasUnknown());
  }

  // LRM 20.6.2 `$bits`: the current bit count is the sum of the elements' own
  // bit counts, so a dynamically sized element contributes its current width.
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray {
    PackedArray total = PackedArray::Int(0);
    for (const auto& e : data_) {
      total = total + e.BitstreamWidth();
    }
    return total;
  }

  // LRM Table 6-7: a queue's default is the empty queue. When this container
  // is itself the discard sink of an outer container, the outer scrubs it to
  // canonical state before handing out a reference.
  auto ResetToDefault() -> void {
    data_.clear();
  }

  // LRM 7.10.1 / 7.4.5 read: an index outside `0..size-1` (or carrying x/z)
  // misses. Reads never grow the queue -- the write path owns the `q[$+1]`
  // append semantic. The non-const overload returns the shield's discard
  // target; the const overload returns the element default by direct reference.
  [[nodiscard]] auto Element(const PackedArray& idx) -> T& {
    if (IsInvalidIndex(idx)) {
      return shield_.DiscardTarget();
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  [[nodiscard]] auto Element(const PackedArray& idx) const -> const T& {
    if (IsInvalidIndex(idx)) {
      return shield_.Default();
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  // LRM 7.10.1 write: `q[$+1] = v` (index == size) appends a default-shaped
  // slot and returns it. An x/z, negative, or beyond-`$+1` index lands on
  // the discard sink so the write is ignored. The backend routes here
  // only for an element-write lvalue, so a read of `index == size` still
  // sees the default rather than growing the queue.
  [[nodiscard]] auto ElementRef(const PackedArray& idx) -> T& {
    if (!idx.HasUnknown()) {
      const auto v = idx.ToInt64();
      if (v >= 0 && static_cast<std::uint64_t>(v) < data_.size()) {
        return data_[static_cast<std::size_t>(v)];
      }
      if (v >= 0 && static_cast<std::uint64_t>(v) == data_.size()) {
        data_.push_back(shield_.Default());
        EnforceBound();
        if (static_cast<std::uint64_t>(v) < data_.size()) {
          return data_[static_cast<std::size_t>(v)];
        }
        return shield_.DiscardTarget();
      }
    }
    return shield_.DiscardTarget();
  }

  // LRM 7.10.1 queue slice. `form` selects the source shape from `(anchor,
  // extent)`: a constant `q[a:b]` is `anchor = a`, `extent = b`; an indexed
  // `q[base+:w]` is `anchor = base`, `extent = w`, growing upward; `q[base-:w]`
  // grows downward. The `base +/- (w - 1)` bound is computed here in the wide
  // int64 domain, never synthesized at lowering. An x/z bound, or `lo > hi`
  // after clamping, yields the empty queue; `lo` clamps up to 0 and `hi` down
  // to the last index. The result carries this queue's element shape. `form` is
  // the shared `value::SliceForm` (`slice_selector.hpp`).
  [[nodiscard]] auto Slice(
      const PackedArray& anchor, const PackedArray& extent,
      const PackedArray& form) const -> Queue {
    Queue out(shield_.Default());
    if (anchor.HasUnknown() || extent.HasUnknown() || data_.empty()) {
      return out;
    }
    const std::int64_t an = anchor.ToInt64();
    const std::int64_t ex = extent.ToInt64();
    std::int64_t lo = an;
    std::int64_t hi = ex;
    switch (static_cast<SliceForm>(form.ToInt64())) {
      case SliceForm::kIndexedUp:
        hi = an + ex - 1;
        break;
      case SliceForm::kIndexedDown:
        lo = an - ex + 1;
        hi = an;
        break;
      case SliceForm::kConstant:
        break;
    }
    const std::int64_t a = std::max<std::int64_t>(lo, 0);
    const auto last = static_cast<std::int64_t>(data_.size()) - 1;
    const std::int64_t b = std::min<std::int64_t>(hi, last);
    for (std::int64_t i = a; i <= b; ++i) {
      out.data_.push_back(data_[static_cast<std::size_t>(i)]);
    }
    return out;
  }

  // LRM 7.10.2.7 / 7.10.2.6: append / prepend a single element.
  auto PushBack(const T& item) -> void {
    data_.push_back(item);
    EnforceBound();
  }
  auto PushFront(const T& item) -> void {
    data_.push_front(item);
    EnforceBound();
  }

  // LRM 7.10.2.4 / 7.10.2.5: remove and return the first / last element. On an
  // empty queue the result is the element type's LRM Table 7-1 default and the
  // queue is left unchanged.
  auto PopFront() -> T {
    if (data_.empty()) {
      return shield_.Default();
    }
    T front = std::move(data_.front());
    data_.pop_front();
    return front;
  }
  auto PopBack() -> T {
    if (data_.empty()) {
      return shield_.Default();
    }
    T back = std::move(data_.back());
    data_.pop_back();
    return back;
  }

  // LRM 7.10.2.2: insert before `index`. The index is `integer` (4-state) so
  // x/z, negative, or `> size` is detectable and makes the call a no-op;
  // `index == size` is a valid append.
  auto Insert(const PackedArray& index, const T& item) -> void {
    if (index.HasUnknown()) {
      return;
    }
    const auto v = index.ToInt64();
    if (v < 0 || static_cast<std::uint64_t>(v) > data_.size()) {
      return;
    }
    data_.insert(data_.begin() + static_cast<std::ptrdiff_t>(v), item);
    EnforceBound();
  }

  // LRM 7.10.2.3: with no index, clear the queue; with an index, remove that
  // element. An x/z, negative, or `>= size` index is a no-op.
  auto Delete() -> void {
    data_.clear();
  }
  auto Delete(const PackedArray& index) -> void {
    if (IsInvalidIndex(index)) {
      return;
    }
    data_.erase(data_.begin() + static_cast<std::ptrdiff_t>(index.ToInt64()));
  }

  // LRM 7.12.2 ordering: an in-place positional permutation (queues are
  // unpacked arrays, LRM 7.10.1). `reverse` takes no closure; `sort` / `rsort`
  // order by the closure-projected key with the ordinal position as index.
  auto Reverse() -> void {
    detail::ArrayReverse(data_);
  }
  template <typename F>
  auto Sort(F&& key) -> void {
    detail::ArraySortByKey(data_, std::forward<F>(key), std::less<>{});
  }
  template <typename F>
  auto Rsort(F&& key) -> void {
    detail::ArraySortByKey(data_, std::forward<F>(key), std::greater<>{});
  }

  // LRM 7.12.3 reduction over the entry stream. `proto` is the
  // producer-supplied result default for an empty receiver and carries the
  // result shape otherwise.
  template <typename F, typename R>
  [[nodiscard]] auto Sum(F&& key, R proto) const -> R {
    return detail::ArrayFold(
        Entries(), std::move(proto), std::forward<F>(key),
        [](auto a, auto v) { return a + v; });
  }
  template <typename F, typename R>
  [[nodiscard]] auto Product(F&& key, R proto) const -> R {
    return detail::ArrayFold(
        Entries(), std::move(proto), std::forward<F>(key),
        [](auto a, auto v) { return a * v; });
  }
  template <typename F, typename R>
  [[nodiscard]] auto And(F&& key, R proto) const -> R {
    return detail::ArrayFold(
        Entries(), std::move(proto), std::forward<F>(key),
        [](auto a, auto v) { return a & v; });
  }
  template <typename F, typename R>
  [[nodiscard]] auto Or(F&& key, R proto) const -> R {
    return detail::ArrayFold(
        Entries(), std::move(proto), std::forward<F>(key),
        [](auto a, auto v) { return a | v; });
  }
  template <typename F, typename R>
  [[nodiscard]] auto Xor(F&& key, R proto) const -> R {
    return detail::ArrayFold(
        Entries(), std::move(proto), std::forward<F>(key),
        [](auto a, auto v) { return a ^ v; });
  }

  // LRM 7.12.1 locator methods over the entry stream. Value locators return a
  // queue of elements; index locators return a queue of the ordinal index.
  // Both seed the result with the producer-supplied `proto`. No match or an
  // empty receiver yields an empty queue.
  template <typename F>
  [[nodiscard]] auto Find(F pred, T proto) const -> Queue<T> {
    return Queue<T>(std::move(proto), detail::ArrayFind(Entries(), pred));
  }
  template <typename F>
  [[nodiscard]] auto FindIndex(F pred, PackedArray proto) const
      -> Queue<PackedArray> {
    return Queue<PackedArray>(
        std::move(proto), detail::ArrayFindIndex(Entries(), pred));
  }
  template <typename F>
  [[nodiscard]] auto FindFirst(F pred, T proto) const -> Queue<T> {
    return Queue<T>(std::move(proto), detail::ArrayFindFirst(Entries(), pred));
  }
  template <typename F>
  [[nodiscard]] auto FindFirstIndex(F pred, PackedArray proto) const
      -> Queue<PackedArray> {
    return Queue<PackedArray>(
        std::move(proto), detail::ArrayFindFirstIndex(Entries(), pred));
  }
  template <typename F>
  [[nodiscard]] auto FindLast(F pred, T proto) const -> Queue<T> {
    return Queue<T>(std::move(proto), detail::ArrayFindLast(Entries(), pred));
  }
  template <typename F>
  [[nodiscard]] auto FindLastIndex(F pred, PackedArray proto) const
      -> Queue<PackedArray> {
    return Queue<PackedArray>(
        std::move(proto), detail::ArrayFindLastIndex(Entries(), pred));
  }
  template <typename F>
  [[nodiscard]] auto Min(F&& key, T proto) const -> Queue<T> {
    return Queue<T>(
        std::move(proto), detail::ArrayMin(Entries(), std::forward<F>(key)));
  }
  template <typename F>
  [[nodiscard]] auto Max(F&& key, T proto) const -> Queue<T> {
    return Queue<T>(
        std::move(proto), detail::ArrayMax(Entries(), std::forward<F>(key)));
  }
  template <typename F>
  [[nodiscard]] auto Unique(F key, T proto) const -> Queue<T> {
    return Queue<T>(
        std::move(proto), detail::ArrayUnique(Entries(), std::move(key)));
  }
  template <typename F>
  [[nodiscard]] auto UniqueIndex(F key, PackedArray proto) const
      -> Queue<PackedArray> {
    return Queue<PackedArray>(
        std::move(proto), detail::ArrayUniqueIndex(Entries(), std::move(key)));
  }

  // LRM 7.12.5 projection into a same-shape queue; `proto` seeds the result
  // element type's canonical default (producer-supplied, since the result
  // element type may differ from this queue's).
  template <typename F, typename U>
  [[nodiscard]] auto Map(F closure, U proto) const -> Queue<U> {
    return Queue<U>(std::move(proto), detail::ArrayMap(Entries(), closure));
  }

 private:
  // The LRM 7.12 entry stream (decision: array-manipulation-entry-stream): a
  // lazy view pairing each element with its ordinal index, front-to-back.
  [[nodiscard]] auto Entries() const {
    return std::views::enumerate(data_) |
           std::views::transform([](auto&& pair) {
             auto&& [i, e] = pair;
             return detail::Entry<PackedArray, T>{
                 PackedArray::Int(static_cast<int>(i)), &e};
           });
  }

  [[nodiscard]] auto IsInvalidIndex(const PackedArray& idx) const -> bool {
    if (idx.HasUnknown()) {
      return true;
    }
    const auto v = idx.ToInt64();
    return v < 0 || static_cast<std::uint64_t>(v) >=
                        static_cast<std::uint64_t>(data_.size());
  }

  // LRM 7.10.5: a bounded queue holds no element whose index exceeds the bound,
  // so once a write grows it past `max_bound_ + 1` elements the overflow is
  // discarded and a warning is issued. A no-op for an unbounded queue.
  auto EnforceBound() -> void {
    if (!max_bound_.has_value()) {
      return;
    }
    const std::size_t cap = static_cast<std::size_t>(*max_bound_) + 1;
    if (data_.size() > cap) {
      data_.resize(cap);
      std::cerr << "warning: bounded queue exceeded its declared bound; "
                   "elements beyond the bound were discarded (LRM 7.10.5)\n";
    }
  }

  detail::OobShield<T> shield_;
  std::deque<T> data_;
  std::optional<std::uint64_t> max_bound_ = std::nullopt;
};

// LRM 10.10 unpacked array concatenation builder. The backend wraps each part
// as a single element (`QElem`) or an array to splice in element order
// (`QSpread`); `MakeQueueConcat` folds the parts onto a queue seeded with the
// element shape and bound of its result type, so even the empty `{}` carries
// the declared representation -- the concatenation value matches its type with
// no destination-side shape preservation.
template <typename T>
struct ConcatElemArg {
  T value;
};
template <typename C>
struct ConcatSpreadArg {
  const C* array;
};

template <typename T>
auto QElem(const T& value) -> ConcatElemArg<T> {
  return ConcatElemArg<T>{value};
}
template <typename C>
auto QSpread(const C& array) -> ConcatSpreadArg<C> {
  return ConcatSpreadArg<C>{&array};
}

template <typename T>
auto AppendConcatPart(Queue<T>& out, const ConcatElemArg<T>& part) -> void {
  out.PushBack(part.value);
}
template <typename T, typename C>
auto AppendConcatPart(Queue<T>& out, const ConcatSpreadArg<C>& part) -> void {
  for (std::size_t i = 0; i < part.array->RawSize(); ++i) {
    out.PushBack(part.array->RawAt(i));
  }
}

// The result carries its element shape and LRM 7.10.5 bound (a negative bound
// means unbounded) so the concatenation value matches its declared type, even
// for the empty `{}`. `element_default` cannot be deduced from an empty part
// list, so it is supplied explicitly; the parts are folded on and the bound
// enforced.
template <typename T, typename... Parts>
auto MakeQueueConcat(
    const T& element_default, std::int64_t max_bound, const Parts&... parts)
    -> Queue<T> {
  Queue<T> out =
      max_bound < 0
          ? Queue<T>(element_default)
          : Queue<T>(
                element_default,
                PackedArray::Int(static_cast<std::int32_t>(max_bound)));
  // Each PushBack enforces the bound, so an over-long concatenation trims as it
  // folds (LRM 7.10.5).
  (AppendConcatPart(out, parts), ...);
  return out;
}

// LRM 21.2.1.6 aggregate format. Mirrors `Formatter<DynamicArray<T>>`: walk
// the elements, deferring each to its own `Formatter`. Empty queues compose
// naturally because the loop runs zero times.
template <typename T>
struct Formatter<Queue<T>> {
  static auto Format(const FormatSpec& spec, const Queue<T>& value)
      -> std::string {
    std::string out = "'{";
    for (std::size_t i = 0; i < value.RawSize(); ++i) {
      if (i != 0) {
        out += ", ";
      }
      out += lyra::value::Format(spec, MakeFormatArg(value.RawAt(i)));
    }
    out += "}";
    return out;
  }
};

static_assert(LyraValue<Queue<PackedArray>>);
static_assert(Sized<Queue<PackedArray>>);
static_assert(BitstreamSizable<Queue<PackedArray>>);
static_assert(Indexable<Queue<PackedArray>>);
// A queue's `Slice(anchor, extent, form)` is dynamic-width -- the runtime
// derives the element count from the bounds (LRM 7.10.1) -- not the fixed-width
// `(anchor, count, shift)` contract `Sliceable` names, so despite the matching
// arity it carries its own `Slice` rather than claiming that concept.
static_assert(Ownable<Queue<PackedArray>>);
static_assert(Defaultable<Queue<PackedArray>>);
static_assert(Sortable<Queue<PackedArray>>);

}  // namespace lyra::value

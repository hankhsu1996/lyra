#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <functional>
#include <iostream>
#include <optional>
#include <span>
#include <string>
#include <type_traits>
#include <utility>

#include "lyra/value/array_case_equal.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/value_concept.hpp"

namespace lyra::value {

// SystemVerilog queue (LRM 7.10): a variable-size ordered collection with
// efficient insertion and removal at both ends, so the storage is a
// `std::deque<T>` rather than the `std::vector<T>` the dynamic array uses.
// Default value is the empty queue (LRM Table 6-7).
//
// `oob_slot_` is the LRM 7.4.5 invalid-index shield seed, supplied at
// construction because `T = PackedArray` carries runtime shape parameters
// (bit width, signedness, 2/4-state) that cannot be recovered from the C++
// type alone. See `docs/decisions/runtime-shape-and-default-value.md`. It
// holds the element's canonical default for construction and the OOB-shield
// protocol (`ResetToDefault`) shared with the other container wrappers.
template <typename T>
class Queue {
 public:
  using ElementType = T;

  // Sentinel "uninitialized" form -- empty queue with a default-constructed
  // OOB slot. Used as the declared default state of a queue field; the first
  // MIR-level assignment overwrites the whole queue (LRM 10.5 variable
  // initialization).
  Queue() = default;

  // Empty queue with the shield slot seeded. Used for declarations like
  // `int q[$];` where the queue starts empty but the element shape is known
  // at lowering time.
  explicit Queue(T oob_slot)
      : oob_slot_(std::move(oob_slot)), oob_seeded_(true) {
  }

  // LRM 10.9.1 assignment-pattern construction: shield slot seeded, elements
  // taken from the pattern's list. The list is a span so the emit side hands
  // in a `std::array<T, N>{...}` literal, mirroring `DynamicArray`.
  Queue(T oob_slot, std::span<const T> init)
      : oob_slot_(std::move(oob_slot)),
        data_(init.begin(), init.end()),
        oob_seeded_(true) {
  }

  // LRM 7.10.5 bounded queue `int q[$:N]`: the empty start is unchanged, but
  // the maximum index N is recorded so growth past N+1 elements is discarded
  // with a warning. The bound arrives as a PackedArray construction argument.
  Queue(T oob_slot, const PackedArray& max_bound)
      : oob_slot_(std::move(oob_slot)),
        max_bound_(static_cast<std::uint64_t>(max_bound.ToInt64())),
        oob_seeded_(true) {
  }

  // LRM 7.10.5 bounded queue initialized by an assignment pattern: take the
  // pattern's elements, record the bound, and trim any overflow on entry.
  Queue(T oob_slot, std::span<const T> init, const PackedArray& max_bound)
      : oob_slot_(std::move(oob_slot)),
        data_(init.begin(), init.end()),
        max_bound_(static_cast<std::uint64_t>(max_bound.ToInt64())),
        oob_seeded_(true) {
    EnforceBound();
  }

  Queue(const Queue&) = default;
  Queue(Queue&&) noexcept = default;
  // LRM 10.6.1 / `value-assignment-and-moved-from.md`: the element shape and
  // the LRM 7.10.5 bound are declared-type properties of the variable, not
  // value content. A declared queue is default-constructed and then initialized
  // by assignment, so an as-yet-unseeded target adopts the source's element
  // shape (and an unbounded target the source's bound) on that first store;
  // once seeded it keeps its own shape and bound on every later assignment and
  // copies only the elements in -- so assigning the empty `{}` or an unbounded
  // concat result preserves the variable's shape rather than overwriting it.
  auto operator=(const Queue& other) -> Queue& {
    if (this != &other) {
      if (!oob_seeded_ && other.oob_seeded_) {
        oob_slot_ = other.oob_slot_;
        oob_seeded_ = true;
      }
      data_ = other.data_;
      if (!max_bound_.has_value()) {
        max_bound_ = other.max_bound_;
      }
      EnforceBound();
    }
    return *this;
  }
  auto operator=(Queue&& other) noexcept -> Queue& {
    if (this != &other) {
      if (!oob_seeded_ && other.oob_seeded_) {
        oob_slot_ = std::move(other.oob_slot_);
        oob_seeded_ = true;
      }
      data_ = std::move(other.data_);
      if (!max_bound_.has_value()) {
        max_bound_ = other.max_bound_;
      }
      EnforceBound();
    }
    return *this;
  }
  ~Queue() = default;

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

  // LRM Table 6-7: a queue's default is the empty queue. When this container
  // is itself an OOB shield slot of an outer container, the outer calls this
  // to restore canonical state before handing out a reference.
  auto ResetToDefault() -> void {
    data_.clear();
  }

  // LRM 7.10.1 / 7.4.5 read: an index outside 0..size-1 (or carrying x/z)
  // returns the element default via `oob_slot_`, restored to canonical state
  // first. Reads never grow the queue; the backend routes writes to `WriteRef`.
  [[nodiscard]] auto ElementAt(const PackedArray& idx) -> T& {
    if (IsInvalidIndex(idx)) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  [[nodiscard]] auto ElementAt(const PackedArray& idx) const -> const T& {
    if (IsInvalidIndex(idx)) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  // LRM 7.10.1 write: `q[$+1] = v` (index == size) appends a default-shaped
  // slot and returns it. An x/z, negative, or beyond-`$+1` index lands on the
  // discarded shield so the write is ignored. The backend takes this path only
  // for an element-write lvalue, so a read of index == size still returns the
  // default rather than growing the queue.
  [[nodiscard]] auto WriteRef(const PackedArray& idx) -> T& {
    if (!idx.HasUnknown()) {
      const auto v = idx.ToInt64();
      if (v >= 0 && static_cast<std::uint64_t>(v) < data_.size()) {
        return data_[static_cast<std::size_t>(v)];
      }
      if (v >= 0 && static_cast<std::uint64_t>(v) == data_.size()) {
        oob_slot_.ResetToDefault();
        data_.push_back(oob_slot_);
        EnforceBound();
        if (static_cast<std::uint64_t>(v) < data_.size()) {
          return data_[static_cast<std::size_t>(v)];
        }
        oob_slot_.ResetToDefault();
        return oob_slot_;
      }
    }
    oob_slot_.ResetToDefault();
    return oob_slot_;
  }

  // LRM 7.10.1 queue slice `q[lo:hi]`. An x/z bound, or `lo > hi` after
  // clamping, yields the empty queue. `lo` clamps up to 0 and `hi` clamps down
  // to the last index (`q[a:b]` with `a < 0` is `q[0:b]`, with `b > $` is
  // `q[a:$]`). The result carries this queue's element shape via `oob_slot_`.
  [[nodiscard]] auto Slice(const PackedArray& lo, const PackedArray& hi) const
      -> Queue {
    Queue out(oob_slot_);
    if (lo.HasUnknown() || hi.HasUnknown() || data_.empty()) {
      return out;
    }
    const std::int64_t a = std::max<std::int64_t>(lo.ToInt64(), 0);
    const auto last = static_cast<std::int64_t>(data_.size()) - 1;
    const std::int64_t b = std::min<std::int64_t>(hi.ToInt64(), last);
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
  // empty queue the result is the element type's LRM Table 7-1 default (the
  // canonical seed held by `oob_slot_`) and the queue is left unchanged.
  auto PopFront() -> T {
    if (data_.empty()) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    T front = std::move(data_.front());
    data_.pop_front();
    return front;
  }
  auto PopBack() -> T {
    if (data_.empty()) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
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

  // LRM 7.12 ordering and reduction (queues are unpacked arrays, LRM 7.10.1),
  // each a thin wrapper over the shared `detail::Array*` algorithms. HIR-to-MIR
  // always supplies the closure (the `with`-clause body or the LRM-default
  // identity); `reverse` takes none -- its `with` clause is a compiler error
  // filtered upstream by slang. The reduction result type follows the closure's
  // return type, so a width-widening `with`-expression widens the result.
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
  template <typename F>
  [[nodiscard]] auto Sum(F&& key) const
      -> std::invoke_result_t<F&, const T&, const PackedArray&> {
    return detail::ArrayFold(
        data_, oob_slot_, std::forward<F>(key),
        [](auto& acc, auto& v) { acc = acc + v; });
  }
  template <typename F>
  [[nodiscard]] auto Product(F&& key) const
      -> std::invoke_result_t<F&, const T&, const PackedArray&> {
    return detail::ArrayFold(
        data_, oob_slot_, std::forward<F>(key),
        [](auto& acc, auto& v) { acc = acc * v; });
  }
  template <typename F>
  [[nodiscard]] auto And(F&& key) const
      -> std::invoke_result_t<F&, const T&, const PackedArray&> {
    return detail::ArrayFold(
        data_, oob_slot_, std::forward<F>(key),
        [](auto& acc, auto& v) { acc = acc & v; });
  }
  template <typename F>
  [[nodiscard]] auto Or(F&& key) const
      -> std::invoke_result_t<F&, const T&, const PackedArray&> {
    return detail::ArrayFold(
        data_, oob_slot_, std::forward<F>(key),
        [](auto& acc, auto& v) { acc = acc | v; });
  }
  template <typename F>
  [[nodiscard]] auto Xor(F&& key) const
      -> std::invoke_result_t<F&, const T&, const PackedArray&> {
    return detail::ArrayFold(
        data_, oob_slot_, std::forward<F>(key),
        [](auto& acc, auto& v) { acc = acc ^ v; });
  }

  // LRM 7.12.1 locator methods. Value locators return a queue of elements
  // carrying this queue's element shape via `oob_slot_`; index locators return
  // a queue of `int`. No match or an empty receiver yields an empty queue.
  template <typename F>
  [[nodiscard]] auto Find(F pred) const -> Queue<T> {
    return Queue<T>(oob_slot_, detail::ArrayFind(data_, std::move(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindIndex(F pred) const -> Queue<PackedArray> {
    return {
        PackedArray::Int(0), detail::ArrayFindIndex(data_, std::move(pred))};
  }
  template <typename F>
  [[nodiscard]] auto FindFirst(F pred) const -> Queue<T> {
    return Queue<T>(oob_slot_, detail::ArrayFindFirst(data_, std::move(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindFirstIndex(F pred) const -> Queue<PackedArray> {
    return {
        PackedArray::Int(0),
        detail::ArrayFindFirstIndex(data_, std::move(pred))};
  }
  template <typename F>
  [[nodiscard]] auto FindLast(F pred) const -> Queue<T> {
    return Queue<T>(oob_slot_, detail::ArrayFindLast(data_, std::move(pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindLastIndex(F pred) const -> Queue<PackedArray> {
    return {
        PackedArray::Int(0),
        detail::ArrayFindLastIndex(data_, std::move(pred))};
  }
  template <typename F>
  [[nodiscard]] auto Min(F&& key) const -> Queue<T> {
    return Queue<T>(oob_slot_, detail::ArrayMin(data_, std::forward<F>(key)));
  }
  template <typename F>
  [[nodiscard]] auto Max(F&& key) const -> Queue<T> {
    return Queue<T>(oob_slot_, detail::ArrayMax(data_, std::forward<F>(key)));
  }
  template <typename F>
  [[nodiscard]] auto Unique(F key) const -> Queue<T> {
    return Queue<T>(oob_slot_, detail::ArrayUnique(data_, std::move(key)));
  }
  template <typename F>
  [[nodiscard]] auto UniqueIndex(F key) const -> Queue<PackedArray> {
    return {
        PackedArray::Int(0), detail::ArrayUniqueIndex(data_, std::move(key))};
  }

 private:
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

  mutable T oob_slot_;
  std::deque<T> data_;
  std::optional<std::uint64_t> max_bound_ = std::nullopt;
  bool oob_seeded_ = false;
};

// LRM 10.10 unpacked array concatenation builder. The backend wraps each part
// as a single element (`QElem`) or an array to splice in element order
// (`QSpread`); `MakeQueueConcat` folds the parts onto a queue seeded with the
// element default, so even the empty `{}` carries the element shape.
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

// The result is default-constructed (unseeded): the destination of the
// enclosing assignment keeps its own element shape (LRM 10.6.1), so the concat
// value need not carry one. The element type is supplied explicitly because it
// cannot be deduced from an empty `{}` part list.
template <typename T, typename... Parts>
auto MakeQueueConcat(const Parts&... parts) -> Queue<T> {
  Queue<T> out;
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

}  // namespace lyra::value

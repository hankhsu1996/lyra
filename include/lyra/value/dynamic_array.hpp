#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <ranges>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/array_case_equal.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/oob_shield.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/queue.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::value {

// SystemVerilog dynamic array (LRM 7.5). Size set at run time via `new[N]` /
// `new[N](other)` constructors; default is the empty array (LRM Table 6-7). The
// element default and the invalid-index discard target are carried by an
// `OobShield`.
template <typename T>
class DynamicArray {
 public:
  using ElementType = T;

  // Sentinel "uninitialized" form -- empty container with a default-constructed
  // shield. Used as the declared default state of a `Var<DynamicArray<T>>`
  // field; the first MIR-level assignment overwrites the whole array (LRM 10.5
  // variable initialization).
  DynamicArray() = default;

  // Empty container with the shield seeded. Used for declarations like
  // `int arr[];` where the array starts empty but the element shape is known at
  // lowering time.
  explicit DynamicArray(T element_default)
      : shield_(std::move(element_default)) {
  }

  // LRM 7.5.1 `new[N]`: build `n` elements, each a copy of the element default.
  // `n` is a longint per LRM 7.5.1; the negative-N case throws at construction.
  DynamicArray(const PackedArray& n, T element_default)
      : shield_(std::move(element_default)) {
    const std::int64_t n_val = n.ToInt64();
    if (n_val < 0) {
      throw SimulationError(
          "dynamic array new[N]: size operand is negative (LRM 7.5.1)");
    }
    data_.assign(static_cast<std::size_t>(n_val), shield_.Default());
  }

  // LRM 7.5.1 `new[N](other)`: copy `other` then resize to `N` with the element
  // default -- `std::vector::resize` truncates when target is smaller and pads
  // with the fill value when target is larger, covering both halves of LRM
  // 7.5.1 in one call.
  DynamicArray(const PackedArray& n, T element_default, const DynamicArray& src)
      : shield_(std::move(element_default)), data_(src.data_) {
    const std::int64_t n_val = n.ToInt64();
    if (n_val < 0) {
      throw SimulationError(
          "dynamic array new[N](src): size operand is negative (LRM 7.5.1)");
    }
    data_.resize(static_cast<std::size_t>(n_val), shield_.Default());
  }

  // LRM 10.9.1 assignment-pattern construction: shield seeded, size taken from
  // the pattern's element list. Mirrors `UnpackedArray`'s span
  // ctor so a single emit path produces `std::array<T, N>{...}` as the
  // second argument for either container.
  DynamicArray(T element_default, std::span<const T> init)
      : shield_(std::move(element_default)), data_(init.begin(), init.end()) {
  }

  // LRM 10.9.1 `'{count{...}}` pattern: `count` replications of `unit`.
  // Taking the repeat unit and the count separately keeps the value O(unit)
  // to build rather than O(unit * count).
  DynamicArray(T element_default, std::span<const T> unit, std::size_t count)
      : shield_(std::move(element_default)) {
    data_.reserve(unit.size() * count);
    for (std::size_t i = 0; i < count; ++i) {
      data_.insert(data_.end(), unit.begin(), unit.end());
    }
  }

  // The LRM 7.5.1 run-time-sized forms, each named. Which one a source
  // construct means is decided where that construct is read, not by the
  // argument list: `new[N]` and an assignment pattern both carry two operands,
  // so a target with no overload resolution of its own has nothing to tell them
  // apart. The assignment pattern stays the constructor, since a container has
  // one way to be built from an element list.
  [[nodiscard]] static auto Default(T element_default) -> DynamicArray {
    return DynamicArray(std::move(element_default));
  }

  [[nodiscard]] static auto New(const PackedArray& n, T element_default)
      -> DynamicArray {
    return DynamicArray(n, std::move(element_default));
  }

  [[nodiscard]] static auto NewCopy(
      const PackedArray& n, T element_default, const DynamicArray& src)
      -> DynamicArray {
    return DynamicArray(n, std::move(element_default), src);
  }

  // LRM 7.6: a dynamic array assigned an array of any of the three unpacked
  // kinds is resized to the source's element count and takes its elements in
  // left-to-right order. The clause admits the assignment only where the
  // element types are equivalent, so each element crosses as it stands, and the
  // element default is the destination's own -- the element shape is a declared
  // property of the variable being written, which the source has no say in.
  // Named rather than left a constructor because an argument list of an element
  // default and one more operand does not say which of the run-time-sized forms
  // it is, and a target with no overload resolution has nothing else to read.
  template <OrdinalElements C>
  [[nodiscard]] static auto FromArray(const C& source, T element_default)
      -> DynamicArray {
    DynamicArray result(std::move(element_default));
    result.data_.reserve(source.RawSize());
    for (std::size_t i = 0; i < source.RawSize(); ++i) {
      result.data_.push_back(source.RawAt(i));
    }
    return result;
  }

  DynamicArray(const DynamicArray&) = default;
  DynamicArray(DynamicArray&&) noexcept = default;
  auto operator=(const DynamicArray&) -> DynamicArray& = default;
  auto operator=(DynamicArray&&) noexcept -> DynamicArray& = default;
  ~DynamicArray() = default;

  // LRM 7.5.1: size() yields an SV int.
  [[nodiscard]] auto Size() const -> PackedArray {
    return PackedArray::Int(static_cast<std::int32_t>(RawSize()));
  }

  [[nodiscard]] auto RawSize() const -> std::size_t {
    return data_.size();
  }

  [[nodiscard]] auto RawAt(std::size_t i) const -> const T& {
    return data_[i];
  }

  // The element type's default (LRM Table 7-1), the shape an out-of-range read
  // returns and a derived container seeds its own out-of-range source with.
  [[nodiscard]] auto ElementDefault() const -> const T& {
    return shield_.Default();
  }

  [[nodiscard]] auto ToOwned() const -> DynamicArray {
    return *this;
  }

  // LRM Table 6-7: dynamic array default is the empty array. When this
  // container is itself the discard sink of an outer container, the outer
  // scrubs it to canonical state before handing out a reference; any subsequent
  // inner access then re-misses naturally (data_ is empty), so chained access
  // through the sink never observes a stale
  // write.
  auto ResetToDefault() -> void {
    data_.clear();
  }

  // LRM 7.4.5: an invalid-index write lands on the shield's discard target.
  [[nodiscard]] auto ElementRef(const PackedArray& idx) -> T& {
    if (IsInvalidIndex(idx)) {
      return shield_.DiscardTarget();
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  // LRM 7.4.5: an invalid-index read returns the element default (LRM Table
  // 7-1).
  [[nodiscard]] auto Element(const PackedArray& idx) const -> const T& {
    if (IsInvalidIndex(idx)) {
      return shield_.Default();
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  // LRM 7.4.5 / 7.4.6 contiguous-range selector. A dynamic array is zero-based,
  // so the source index is the storage ordinal (identity rebase); the slice
  // result is a fixed-size unpacked array over `[base : base + count - 1]`.
  // Partial-OOB positions and an x / z selector yield the canonical default at
  // the type-fixed count's width; see `concepts.hpp` for the `Sliceable` shape.
  [[nodiscard]] auto Slice(
      const PackedArray& a, const PackedArray& b, const PackedArray& form) const
      -> UnpackedArray<T> {
    const SliceWindow win = ResolveSliceWindow(a, b, form);
    return UnpackedArray<T>(
        shield_.Default(),
        detail::ArraySliceGather(
            data_, shield_.Default(), win.base, win.count, win.base_known));
  }

  [[nodiscard]] auto SliceRef(
      const PackedArray& a, const PackedArray& b, const PackedArray& form)
      -> ArraySliceRef<T> {
    const SliceWindow win = ResolveSliceWindow(a, b, form);
    return ArraySliceRef<T>{
        data_, shield_.Default(), win.base, win.count, win.base_known};
  }

  // LRM 11.2.2 + 11.4.5 aggregate equality. Runtime size mismatch yields
  // 0 and matching-size empties yield 1; the LRM is silent on both, this
  // matches industry convention (Verilator). For matching non-empty sizes,
  // `==` / `!=` propagate X / Z through the `&&` reduction over element
  // comparisons; `CaseEqual` matches X / Z as values and is deterministic.
  [[nodiscard]] auto operator==(const DynamicArray& other) const
      -> PackedArray {
    // LRM 11.4.5: the answer carries the state class an element's own equality
    // produces, because that is what a run of them reduces to. Reading the
    // class off the element shape rather than off a first element leaves an
    // empty container no case of its own -- it is the run of no elements -- and
    // leaves a size mismatch, which is definitely unequal, answering in the
    // same class as every other comparison of this container.
    const bool four_state =
        (shield_.Default() == shield_.Default()).IsFourState();
    if (data_.size() != other.data_.size()) {
      return PackedArray::FromInt(0, 1, false, four_state);
    }
    PackedArray result = PackedArray::FromInt(1, 1, false, four_state);
    for (std::size_t i = 0; i < data_.size(); ++i) {
      result = result && (data_[i] == other.data_[i]);
    }
    return result;
  }
  [[nodiscard]] auto operator!=(const DynamicArray& other) const
      -> PackedArray {
    return !(*this == other);
  }

  [[nodiscard]] auto CaseEqual(const DynamicArray& other) const -> PackedArray {
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
  // two arrays element-wise bit-identical. A size mismatch is a change;
  // matching empties are identical; otherwise recurse into each element's own
  // predicate.
  [[nodiscard]] auto IsBitIdentical(const DynamicArray& other) const -> bool {
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

  // LRM 20.9 `$countbits`: the bit stream this value contributes is its
  // elements' streams laid end to end, so the count over it is the sum of the
  // elements' own counts under the same control bits.
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray {
    PackedArray total = PackedArray::Int(0);
    for (const auto& e : data_) {
      total = total + e.CountBits(control_bits);
    }
    return total;
  }

  // LRM 7.5.3: empties the array, resulting in a zero-sized array. Body is
  // identical to ResetToDefault (LRM Table 6-7 default for dynamic array is
  // the empty array), but the two surface names track distinct contracts:
  // Delete is the user-facing method name; ResetToDefault is the
  // canonical-reset protocol shared with PackedArray / UnpackedArray.
  auto Delete() -> void {
    data_.clear();
  }

  // LRM 10.10 unpacked concatenation, as the two-operand steps a join folds to:
  // this array with one element appended, or with every element of a spread
  // part appended in order. A part is one element unless the program spreads a
  // container. Value-returning so the fold chains them without mutating a
  // shared array, and unbounded, so every appended element is kept.
  [[nodiscard]] auto ConcatElement(const T& item) const -> DynamicArray {
    DynamicArray out = *this;
    out.data_.push_back(item);
    return out;
  }
  template <typename C>
  [[nodiscard]] auto ConcatSpread(const C& part) const -> DynamicArray {
    DynamicArray out = *this;
    for (std::size_t i = 0; i < part.RawSize(); ++i) {
      out.data_.push_back(part.RawAt(i));
    }
    return out;
  }

  // LRM 7.12.2 ordering: an in-place positional permutation. `reverse` takes no
  // closure (its `with` clause is a compiler error filtered upstream by slang);
  // `sort` / `rsort` order by the closure-projected key, the index passed to
  // the closure being the ordinal position.
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

  // LRM 7.12.3 reduction over the entry stream. The closure receives each
  // element and its ordinal index; `proto` is the producer-supplied result
  // default returned for an empty receiver, and otherwise carries the result
  // shape the fold accumulates into.
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

  // LRM 7.12.1 locator methods over the entry stream. The value locators
  // (`find`, `find_first`, `find_last`, `min`, `max`, `unique`) return a queue
  // of elements; the index locators return a queue of the ordinal index. Both
  // seed the result queue with the producer-supplied `proto`. No match or an
  // empty receiver yields an empty queue. The `with` clause is mandatory for
  // the find family (a Boolean predicate) and optional for `min` / `max` /
  // `unique` (a comparison key, defaulting to the element).
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

  // LRM 7.12.5 projection into a same-shape dynamic array; `proto` seeds the
  // result element type's canonical default (the producer supplies it because
  // the result element type may differ from this array's).
  template <typename F, typename U>
  [[nodiscard]] auto Map(F closure, U proto) const -> DynamicArray<U> {
    return DynamicArray<U>(
        std::move(proto), detail::ArrayMap(Entries(), closure));
  }

 private:
  // The LRM 7.12 entry stream: a lazy view pairing each element with its
  // ordinal index, in storage order.
  [[nodiscard]] auto Entries() const {
    return std::views::enumerate(data_) |
           std::views::transform([](auto&& pair) {
             auto&& [i, e] = pair;
             return detail::Entry<PackedArray, T>{
                 PackedArray::Int(static_cast<int>(i)), &e};
           });
  }

  [[nodiscard]] auto IsInvalidIndex(const PackedArray& idx) const -> bool {
    if (idx.HasUnknown()) return true;
    const auto v = idx.ToInt64();
    return v < 0 || static_cast<std::uint64_t>(v) >=
                        static_cast<std::uint64_t>(data_.size());
  }

  detail::OobShield<T> shield_;
  std::vector<T> data_;
};

static_assert(LyraValue<DynamicArray<PackedArray>>);
static_assert(Sized<DynamicArray<PackedArray>>);
static_assert(BitstreamSizable<DynamicArray<PackedArray>>);
static_assert(Indexable<DynamicArray<PackedArray>>);
static_assert(Sliceable<DynamicArray<PackedArray>>);
static_assert(SliceableRef<DynamicArray<PackedArray>>);
static_assert(Ownable<DynamicArray<PackedArray>>);
static_assert(Defaultable<DynamicArray<PackedArray>>);
static_assert(Sortable<DynamicArray<PackedArray>>);
static_assert(OrdinalElements<DynamicArray<PackedArray>>);

}  // namespace lyra::value

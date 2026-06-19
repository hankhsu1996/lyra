#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <span>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/array_case_equal.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/queue.hpp"
#include "lyra/value/unpacked_array.hpp"
#include "lyra/value/value_concept.hpp"

namespace lyra::value {

// SystemVerilog dynamic array (LRM 7.5). Size set at run time via `new[N]` /
// `new[N](other)` constructors; default is the empty array (LRM Table 6-7).
//
// `oob_slot_` is the LRM 7.4.5 invalid-index shield. Its state is restored
// to the canonical default on every OOB access via `T::ResetToDefault`, so
// reads after an OOB write observe the LRM Table 7-1 default rather than
// whatever the prior write left in the slot. It is supplied at construction
// because `T = PackedArray` requires runtime shape parameters (bit width,
// signedness, 2/4-state) that cannot be recovered from the C++ type alone.
// See `docs/decisions/runtime-shape-and-default-value.md`.
template <typename T>
class DynamicArray {
 public:
  using ElementType = T;

  // Sentinel "uninitialized" form -- empty container with default-constructed
  // OOB slot. Used as the declared default state of a `Var<DynamicArray<T>>`
  // field; the first MIR-level assignment overwrites the whole array (LRM
  // 10.5 variable initialization).
  DynamicArray() = default;

  // Empty container with the shield slot seeded. Used for declarations like
  // `int arr[];` where the array starts empty but the element shape is
  // known at lowering time.
  explicit DynamicArray(T oob_slot) : oob_slot_(std::move(oob_slot)) {
  }

  // LRM 7.5.1 `new[N]`: build `n` elements, each a copy of the shield-slot
  // seed (which is also the canonical default). `n` is a longint per LRM
  // 7.5.1; the negative-N case throws at construction.
  DynamicArray(const PackedArray& n, T oob_slot)
      : oob_slot_(std::move(oob_slot)) {
    const std::int64_t n_val = n.ToInt64();
    if (n_val < 0) {
      throw InternalError(
          "DynamicArray::new[N]: size operand is negative (LRM 7.5.1)");
    }
    data_.assign(static_cast<std::size_t>(n_val), oob_slot_);
  }

  // LRM 7.5.1 `new[N](other)`: copy `other` then `resize(N, oob_slot_)` --
  // `std::vector::resize` truncates when target is smaller and pads with
  // the fill value when target is larger, covering both halves of LRM
  // 7.5.1 in one call.
  DynamicArray(const PackedArray& n, T oob_slot, const DynamicArray& src)
      : oob_slot_(std::move(oob_slot)), data_(src.data_) {
    const std::int64_t n_val = n.ToInt64();
    if (n_val < 0) {
      throw InternalError(
          "DynamicArray::new[N](src): size operand is negative (LRM 7.5.1)");
    }
    data_.resize(static_cast<std::size_t>(n_val), oob_slot_);
  }

  // LRM 10.9.1 assignment-pattern construction: shield slot seeded, size
  // taken from the pattern's element list. Mirrors `UnpackedArray`'s span
  // ctor so a single emit path produces `std::array<T, N>{...}` as the
  // second argument for either container.
  DynamicArray(T oob_slot, std::span<const T> init)
      : oob_slot_(std::move(oob_slot)), data_(init.begin(), init.end()) {
  }

  DynamicArray(const DynamicArray&) = default;
  DynamicArray(DynamicArray&&) noexcept = default;
  auto operator=(const DynamicArray&) -> DynamicArray& = default;
  auto operator=(DynamicArray&&) noexcept -> DynamicArray& = default;
  ~DynamicArray() = default;

  // ADL swap so STL element-relocation primitives can shuffle nested
  // DynamicArrays inside an outer container (e.g. `matrix.reverse()` on
  // `int matrix[][]` swaps two row arrays). The default move-assign on
  // DynamicArray<PackedArray> trips PackedArray::AssignFrom's
  // shape-preservation rule because storage moves out without the
  // shape scalars being reset; swapping member-wise avoids the path.
  // Mirrors PackedArray's friend swap; same NOLINT rationale (ADL name
  // is mandated lowercase).
  // NOLINTNEXTLINE(readability-identifier-naming)
  friend auto swap(DynamicArray& a, DynamicArray& b) noexcept -> void {
    using std::swap;
    swap(a.oob_slot_, b.oob_slot_);
    swap(a.data_, b.data_);
  }

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

  [[nodiscard]] auto ToOwned() const -> DynamicArray {
    return *this;
  }

  // LRM Table 6-7: dynamic array default is the empty array. When this
  // container is itself an OOB shield slot of an outer container, the outer
  // calls this method to restore canonical state before handing out a
  // reference; any subsequent inner access then re-OOBs naturally (data_ is
  // empty), so chained access through the shield never observes a stale
  // write.
  auto ResetToDefault() -> void {
    data_.clear();
  }

  // LRM 7.4.5: an invalid index makes the access route through `oob_slot_`.
  // The slot is restored to canonical state before the reference is handed
  // out, so OOB writes that mutate it are invisible to any subsequent
  // access. The non-const overload returns a writable reference; writes
  // through it land on the shield rather than on real storage and are
  // erased on the next OOB access.
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

  // LRM 7.4.5 / 7.4.6 contiguous-range selector. A dynamic array slices like
  // any unpacked array; the constant width makes the result a fixed-size
  // unpacked array. The const overload materializes a fresh sub-array
  // (partial-OOB positions and an x / z offset yield the canonical default);
  // the non-const overload returns the shared write-back proxy with the same
  // invalid-index policy on `operator=`.
  [[nodiscard]] auto Slice(const PackedArray& offset, std::uint32_t count) const
      -> UnpackedArray<T> {
    const T canonical = MakeCanonicalElement();
    return UnpackedArray<T>(
        canonical, detail::ArraySliceGather(data_, canonical, offset, count));
  }

  [[nodiscard]] auto Slice(const PackedArray& offset, std::uint32_t count)
      -> ArraySliceRef<T> {
    return ArraySliceRef<T>{data_, MakeCanonicalElement(), offset, count};
  }

  // LRM 11.2.2 + 11.4.5 aggregate equality. Runtime size mismatch yields
  // 0 and matching-size empties yield 1; the LRM is silent on both, this
  // matches industry convention (Verilator). For matching non-empty sizes,
  // `==` / `!=` propagate X / Z through the `&&` reduction over element
  // comparisons; `CaseEqual` matches X / Z as values and is deterministic.
  [[nodiscard]] auto operator==(const DynamicArray& other) const
      -> PackedArray {
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

  // LRM 7.5.3: empties the array, resulting in a zero-sized array. Body is
  // identical to ResetToDefault (LRM Table 6-7 default for dynamic array is
  // the empty array), but the two surface names track distinct contracts:
  // Delete is the user-facing method name; ResetToDefault is the OOB-shield
  // protocol shared with PackedArray / UnpackedArray.
  auto Delete() -> void {
    data_.clear();
  }

  // LRM 7.12 ordering and reduction, each a thin wrapper over the shared
  // `detail::Array*` algorithms. HIR-to-MIR always supplies the closure (the
  // `with`-clause body or the LRM-default identity), so the runtime exposes
  // only the closure-taking form; `reverse` takes none -- its `with` clause is
  // a compiler error filtered upstream by slang. The closure receives each
  // element and its index and returns a key (ordering) or a summand
  // (reduction); the reduction result type follows the closure's return type,
  // so a width-widening `with`-expression widens the result.
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

  // LRM 7.12.1 locator methods, thin wrappers over the shared `detail::Array*`
  // algorithms. The value locators (`find`, `find_first`, `find_last`, `min`,
  // `max`, `unique`) return a queue of elements carrying this array's element
  // shape via `oob_slot_`; the index locators return a queue of `int` whose
  // shield is a plain zero. No match or an empty receiver yields an empty
  // queue. The `with` clause is mandatory for the find family (a Boolean
  // predicate) and optional for `min` / `max` / `unique` (a comparison key,
  // defaulting to the element).
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

  // LRM 7.12.5 projection into a same-shape dynamic array; `shield` seeds the
  // result element type's canonical default.
  template <typename F, typename U>
  [[nodiscard]] auto Map(F closure, U shield) const -> DynamicArray<U> {
    return DynamicArray<U>(std::move(shield), detail::ArrayMap(data_, closure));
  }

 private:
  [[nodiscard]] auto IsInvalidIndex(const PackedArray& idx) const -> bool {
    if (idx.HasUnknown()) return true;
    const auto v = idx.ToInt64();
    return v < 0 || static_cast<std::uint64_t>(v) >=
                        static_cast<std::uint64_t>(data_.size());
  }

  // Returns a fresh `T` at this container's element shape in LRM Table 7-1
  // canonical state, disconnected from the OOB shield identity (slice fill).
  [[nodiscard]] auto MakeCanonicalElement() const -> T {
    T fresh = oob_slot_;
    fresh.ResetToDefault();
    return fresh;
  }

  mutable T oob_slot_;
  std::vector<T> data_;
};

// LRM 21.2.1.6 aggregate format. Mirrors `Formatter<UnpackedArray<T>>` --
// no `FormatContext` thread-through (aggregates never carry context-bound
// kinds); empty arrays compose naturally because the loop runs zero times.
template <typename T>
struct Formatter<DynamicArray<T>> {
  static auto Format(const FormatSpec& spec, const DynamicArray<T>& value)
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

static_assert(LyraValue<DynamicArray<PackedArray>>);

}  // namespace lyra::value

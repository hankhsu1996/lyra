#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <span>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "lyra/value/array_case_equal.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/queue.hpp"
#include "lyra/value/value_concept.hpp"

namespace lyra::value {

template <typename T>
class UnpackedArray;
template <typename T>
class ArraySliceRef;

// SystemVerilog fixed-size unpacked array (LRM 7.4.2). One C++ container layer
// per declared unpacked dimension; multi-dim composes as
// `UnpackedArray<UnpackedArray<...>>`. Mirrors `PackedArray`'s surface for
// every op that crosses the SV / C++ boundary: `ElementAt(const PackedArray&)`
// for indexed access (no `operator[]`), `Slice(offset, count)` for the LRM
// 7.4.5 contiguous-range selector, and `operator==` / `CaseEqual` returning a
// 1-bit `PackedArray` so equality on aggregates propagates through the same
// value-type the integral surface uses.
//
// `oob_slot_` is the LRM 7.4.5 invalid-index shield. Its state is restored
// to the canonical default on every OOB access via `T::ResetToDefault`, so
// reads after an OOB write observe the LRM Table 7-1 default rather than
// whatever the prior write left in the slot. It is supplied at construction
// because `T = PackedArray` requires runtime shape parameters (bit width,
// signedness, 2/4-state) that cannot be recovered from the C++ type alone.
// See `docs/decisions/runtime-shape-and-default-value.md`.
template <typename T>
class UnpackedArray {
 public:
  using ElementType = T;

  // Sentinel "uninitialized" form -- empty container with default-constructed
  // OOB slot. Used as the declared default state of a `Var<UnpackedArray<T>>`
  // field; the first MIR-level assignment overwrites the whole array (LRM
  // 10.5 variable initialization).
  UnpackedArray() = default;

  // Empty container with the shield slot seeded. Internal use only -- SV
  // fixed-size arrays are always non-empty; this form serves `Slice` and
  // similar paths that build a fresh `UnpackedArray` element-by-element.
  explicit UnpackedArray(T oob_slot) : oob_slot_(std::move(oob_slot)) {
  }

  // Element-list construction: shield slot seeded, plus the explicit
  // initial elements (LRM 10.9 assignment pattern lowering). The element
  // list is taken as a span so the emit side can hand in a `std::array<T,
  // N>{...}` literal whose self-determined type is unambiguous, rather
  // than relying on context-dependent braced-init binding.
  UnpackedArray(T oob_slot, std::span<const T> init)
      : oob_slot_(std::move(oob_slot)), data_(init.begin(), init.end()) {
  }

  UnpackedArray(const UnpackedArray&) = default;
  UnpackedArray(UnpackedArray&&) noexcept = default;
  auto operator=(const UnpackedArray&) -> UnpackedArray& = default;
  auto operator=(UnpackedArray&&) noexcept -> UnpackedArray& = default;
  ~UnpackedArray() = default;

  // LRM 7.4.2: size() yields an SV int.
  [[nodiscard]] auto Size() const -> PackedArray {
    return PackedArray::Int(static_cast<std::int32_t>(RawSize()));
  }

  [[nodiscard]] auto RawSize() const -> std::size_t {
    return data_.size();
  }

  // Flat-storage element read. `i` is in [0, RawSize()); no SV-index
  // translation, no invalid-index handling. Sole consumer is the
  // aggregate-format path
  // (`Formatter<UnpackedArray<T>>::Format`), which traverses storage to defer
  // each element to its own `Formatter` specialization. SV-semantics access
  // goes through `ElementAt(PackedArray)`.
  [[nodiscard]] auto RawAt(std::size_t i) const -> const T& {
    return data_[i];
  }

  [[nodiscard]] auto ToOwned() const -> UnpackedArray {
    return *this;
  }

  // LRM Table 7-1 default for a fixed-size unpacked array is "Array, all of
  // whose elements have the value specified in this table for that array's
  // element type." When this container is itself the OOB shield slot of an
  // outer container, the outer calls this method to restore the canonical
  // all-defaults state before handing out a reference. This is O(N) in the
  // unpacked dim and is the LRM-mandated cost of "all elements at default."
  auto ResetToDefault() -> void {
    for (auto& elem : data_) {
      elem.ResetToDefault();
    }
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

  // LRM 7.4.5 contiguous-range selector. The const overload materializes a
  // fresh sub-array; partial-OOB positions yield the canonical default,
  // X / Z offset yields a wholly-default sub-array. The non-const overload
  // returns a write-back proxy with the same invalid-index policy on
  // `operator=`.
  [[nodiscard]] auto Slice(const PackedArray& offset, std::uint32_t count) const
      -> UnpackedArray {
    const T canonical = MakeCanonicalElement();
    return UnpackedArray(
        canonical, detail::ArraySliceGather(data_, canonical, offset, count));
  }

  [[nodiscard]] auto Slice(const PackedArray& offset, std::uint32_t count)
      -> ArraySliceRef<T> {
    return ArraySliceRef<T>{data_, MakeCanonicalElement(), offset, count};
  }

  // LRM 11.2.2 + 11.4.5 aggregate equality / case-equality. Slang's binding
  // enforces equivalent operand size, so the loops over `data_` are matched.
  // `==` / `!=` propagate X / Z; `CaseEqual` returns a deterministic 0/1.
  [[nodiscard]] auto operator==(const UnpackedArray& other) const
      -> PackedArray {
    PackedArray result = data_[0] == other.data_[0];
    for (std::size_t i = 1; i < data_.size(); ++i) {
      result = result && (data_[i] == other.data_[i]);
    }
    return result;
  }
  [[nodiscard]] auto operator!=(const UnpackedArray& other) const
      -> PackedArray {
    return !(*this == other);
  }

  [[nodiscard]] auto CaseEqual(const UnpackedArray& other) const
      -> PackedArray {
    PackedArray result = detail::ArrayCaseEqElement(data_[0], other.data_[0]);
    for (std::size_t i = 1; i < data_.size(); ++i) {
      result = result && detail::ArrayCaseEqElement(data_[i], other.data_[i]);
    }
    return result;
  }

  // LRM 9.4.2 update event predicate (engine change-detection hook): are the
  // two arrays element-wise bit-identical. A size mismatch is a change -- this
  // is how the empty default of a fresh `Var<UnpackedArray>` is detected as
  // different from the first sized write, so the declared-shape initializer
  // commits.
  [[nodiscard]] auto IsBitIdentical(const UnpackedArray& other) const -> bool {
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

  // LRM 7.12 ordering and reduction, each a thin wrapper over the shared
  // `detail::Array*` algorithms. HIR-to-MIR always supplies the closure (the
  // `with`-clause body or the LRM-default identity), so the runtime exposes
  // only the closure-taking form; `reverse` takes none -- its `with` clause is
  // a compiler error filtered upstream by slang. Ordering mutates in place at
  // constant size (a fixed array never grows or shrinks). The closure receives
  // each element and its index and returns a key (ordering) or a summand
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
  // shield is a plain zero. No match yields an empty queue. The `with` clause
  // is mandatory for the find family (a Boolean predicate) and optional for
  // `min` / `max` / `unique` (a comparison key, defaulting to the element).
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
    if (idx.HasUnknown()) return true;
    const auto v = idx.ToInt64();
    return v < 0 || static_cast<std::uint64_t>(v) >=
                        static_cast<std::uint64_t>(data_.size());
  }

  // Returns a fresh `T` at this container's element shape in LRM Table 7-1
  // canonical state. Used by paths that need a default element disconnected
  // from the OOB shield identity (Slice fill, Slice-ref Clone).
  [[nodiscard]] auto MakeCanonicalElement() const -> T {
    T fresh = oob_slot_;
    fresh.ResetToDefault();
    return fresh;
  }

  mutable T oob_slot_;
  std::vector<T> data_;

  friend class ArraySliceRef<T>;
};

// LRM 7.6: an assignment to an unpacked slice is a single assignment to the
// entire slice. The proxy aliases the source storage (a non-owning pointer to
// its element vector) plus the (offset, count) window, so a fixed-size unpacked
// array and a dynamic array share one slice-write surface. X / Z offset makes
// `ToOwned()` a wholly-default sub-array and `operator=` a no-op; partial-OOB
// behaves per-element. Move-only so the proxy cannot outlive what it aliases.
template <typename T>
class ArraySliceRef {
 public:
  ArraySliceRef(
      std::vector<T>& data, T canonical, PackedArray offset,
      std::uint32_t count)
      : data_(&data),
        canonical_(std::move(canonical)),
        offset_(std::move(offset)),
        count_(count) {
  }
  ArraySliceRef(const ArraySliceRef&) = delete;
  auto operator=(const ArraySliceRef&) -> ArraySliceRef& = delete;
  ArraySliceRef(ArraySliceRef&&) noexcept = default;
  auto operator=(ArraySliceRef&&) noexcept -> ArraySliceRef& = default;
  ~ArraySliceRef() = default;

  [[nodiscard]] auto ToOwned() const -> UnpackedArray<T> {
    return UnpackedArray<T>(
        canonical_,
        detail::ArraySliceGather(*data_, canonical_, offset_, count_));
  }

  auto operator=(const UnpackedArray<T>& value) -> ArraySliceRef& {
    detail::ArraySliceScatter(*data_, offset_, count_, value.data_);
    return *this;
  }

 private:
  std::vector<T>* data_;
  T canonical_;
  PackedArray offset_;
  std::uint32_t count_;
};

// LRM 21.2.1.6 aggregate format. Per the per-type Formatter trait the
// container walks its own elements and recursively defers each element to
// `Format(spec, MakeFormatArg(elem))`. Aggregates only ever carry
// `kAssignmentPattern`, which rewrites to `kDecimal` at the integral leaf
// and never reaches a context-bound kind (`%t` is rejected on aggregate
// operands upstream), so no `FormatContext` is threaded through. Multi-dim
// and mixed-container nesting (`int arr[3][]`) fall out because every
// nested element type carries its own Formatter.
template <typename T>
struct Formatter<UnpackedArray<T>> {
  static auto Format(const FormatSpec& spec, const UnpackedArray<T>& value)
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

static_assert(LyraValue<UnpackedArray<PackedArray>>);

}  // namespace lyra::value

#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <ranges>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/value/array_case_equal.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/oob_shield.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/queue.hpp"

namespace lyra::value {

template <typename T>
class UnpackedArray;
template <typename T>
class ArraySliceRef;

// SystemVerilog fixed-size unpacked array (LRM 7.4.2). One C++ container layer
// per declared unpacked dimension; multi-dim composes as
// `UnpackedArray<UnpackedArray<...>>`. Mirrors `PackedArray`'s surface for
// every op that crosses the SV / C++ boundary: `Element(const PackedArray&)`
// for indexed access (no `operator[]`), `Slice(offset, count)` for the LRM
// 7.4.5 contiguous-range selector, and `operator==` / `CaseEqual` returning a
// 1-bit `PackedArray` so equality on aggregates propagates through the same
// value-type the integral surface uses.
//
// The element default and the invalid-index discard target are carried by an
// `OobShield`.
template <typename T>
class UnpackedArray {
 public:
  using ElementType = T;

  // Sentinel "uninitialized" form -- empty container with a default-constructed
  // shield. Used as the declared default state of a `Var<UnpackedArray<T>>`
  // field; the first MIR-level assignment overwrites the whole array (LRM 10.5
  // variable initialization).
  UnpackedArray() = default;

  // Empty container with the shield seeded. Internal use only -- SV fixed-size
  // arrays are always non-empty; this form serves `Slice` and similar paths
  // that build a fresh `UnpackedArray` element-by-element.
  explicit UnpackedArray(T element_default)
      : shield_(std::move(element_default)) {
  }

  // Element-list construction: shield seeded, plus the explicit initial
  // elements (LRM 10.9 assignment pattern lowering). The element list is taken
  // as a span so the emit side can hand in a `std::array<T, N>{...}` literal
  // whose self-determined type is unambiguous, rather than relying on
  // context-dependent braced-init binding.
  UnpackedArray(T element_default, std::span<const T> init)
      : shield_(std::move(element_default)), data_(init.begin(), init.end()) {
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
  // goes through `Element(PackedArray)`.
  [[nodiscard]] auto RawAt(std::size_t i) const -> const T& {
    return data_[i];
  }

  [[nodiscard]] auto ToOwned() const -> UnpackedArray {
    return *this;
  }

  // LRM Table 7-1 default for a fixed-size unpacked array is "Array, all of
  // whose elements have the value specified in this table for that array's
  // element type." When this container is itself the discard sink of an outer
  // container, the outer scrubs it to the canonical all-defaults state before
  // handing out a reference. This is O(N) in the unpacked dim and is the
  // LRM-mandated cost of "all elements at default."
  auto ResetToDefault() -> void {
    for (auto& elem : data_) {
      elem.ResetToDefault();
    }
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

  // LRM 7.4.5 contiguous-range selector. Partial-OOB positions yield the
  // canonical default; X / Z offset yields a wholly-default sub-array at
  // the type-fixed count's width. See `concepts.hpp` for the `Sliceable`
  // protocol shape.
  [[nodiscard]] auto Slice(
      const PackedArray& offset, const PackedArray& count_pa) const
      -> UnpackedArray {
    const auto count = static_cast<std::uint32_t>(count_pa.ToInt64());
    return UnpackedArray(
        shield_.Default(),
        detail::ArraySliceGather(data_, shield_.Default(), offset, count));
  }

  [[nodiscard]] auto SliceRef(
      const PackedArray& offset, const PackedArray& count_pa)
      -> ArraySliceRef<T> {
    const auto count = static_cast<std::uint32_t>(count_pa.ToInt64());
    return ArraySliceRef<T>{data_, shield_.Default(), offset, count};
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

  // LRM 7.12.2 ordering: an in-place positional permutation at constant size (a
  // fixed array never grows or shrinks). `reverse` takes no closure; `sort` /
  // `rsort` order by the closure-projected key with the ordinal position as
  // index.
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
  // Both seed the result with the producer-supplied `proto`. No match yields an
  // empty queue. The `with` clause is mandatory for the find family (a Boolean
  // predicate) and optional for `min` / `max` / `unique` (a comparison key,
  // defaulting to the element).
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

  // LRM 7.12.5 projection into a same-range fixed unpacked array; `proto` seeds
  // the result element type's canonical default (producer-supplied, since the
  // result element type may differ from this array's).
  template <typename F, typename U>
  [[nodiscard]] auto Map(F closure, U proto) const -> UnpackedArray<U> {
    return UnpackedArray<U>(
        std::move(proto), detail::ArrayMap(Entries(), closure));
  }

 private:
  // The LRM 7.12 entry stream (decision: array-manipulation-entry-stream): a
  // lazy view pairing each element with its ordinal index, in declared order.
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
static_assert(Sized<UnpackedArray<PackedArray>>);
static_assert(Indexable<UnpackedArray<PackedArray>>);
static_assert(Sliceable<UnpackedArray<PackedArray>>);
static_assert(SliceableRef<UnpackedArray<PackedArray>>);
static_assert(Ownable<UnpackedArray<PackedArray>>);
static_assert(Defaultable<UnpackedArray<PackedArray>>);
static_assert(Sortable<UnpackedArray<PackedArray>>);

}  // namespace lyra::value

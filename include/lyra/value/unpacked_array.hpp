#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
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
#include "lyra/value/slice_selector.hpp"

namespace lyra::value {

template <typename T>
class UnpackedArray;
template <typename T>
class ArraySliceRef;
class String;

// The declared range of one unpacked dimension, the array's coordinate system.
// Element order runs left-to-right (LRM 7.6), so the leftmost element (index
// `left`) is storage ordinal 0. `ToOrdinal` maps a source-declared index onto
// the storage ordinal; `FromOrdinal` is its inverse. Unlike a packed bit range,
// the ordinal counts from the left, not the least-significant end.
struct UnpackedRange {
  std::int64_t left;
  std::int64_t right;

  [[nodiscard]] auto IsAscending() const -> bool {
    return left <= right;
  }
  [[nodiscard]] auto ToOrdinal(std::int64_t sv) const -> std::int64_t {
    return IsAscending() ? sv - left : left - sv;
  }
  [[nodiscard]] auto FromOrdinal(std::int64_t ordinal) const -> std::int64_t {
    return IsAscending() ? ordinal + left : left - ordinal;
  }
};

// SystemVerilog fixed-size unpacked array (LRM 7.4.2). One C++ container layer
// per declared unpacked dimension; multi-dim composes as
// `UnpackedArray<UnpackedArray<...>>`. Mirrors `PackedArray`'s surface for
// every op that crosses the SV / C++ boundary: `Element` / `Slice` for indexed
// and range access (no `operator[]`), and `operator==` / `CaseEqual` returning
// a 1-bit `PackedArray` so equality on aggregates propagates through the same
// value-type the integral surface uses.
//
// The payload is ordinal-only: it does not carry a declared range. The declared
// coordinate range is a fact of the receiver's static type, passed to element
// and slice access as a `[left:right]` operand pair against which the source
// index resolves to a storage ordinal. Whole-array movement is ordinal-wise and
// range-agnostic. The element default and the invalid-index discard target are
// carried by an `OobShield`.
template <typename T>
class UnpackedArray {
 public:
  using ElementType = T;

  // Sentinel "uninitialized" form -- empty container with a default-constructed
  // shield. Used as the declared default state of a `Var<UnpackedArray<T>>`
  // field; the first MIR-level assignment overwrites the whole array (LRM 10.5
  // variable initialization).
  UnpackedArray() = default;

  // Empty container with the shield seeded. Internal use only -- a fresh
  // `UnpackedArray` that a `Slice` fills element-by-element. The payload is
  // ordinal-only; a declared range is a fact of the receiver's static type, not
  // of the value.
  explicit UnpackedArray(T element_default)
      : shield_(std::move(element_default)) {
  }

  // Shield + element-list construction: the seeded shield and the explicit
  // initial elements (LRM 10.9 assignment pattern lowering). The element list
  // is taken as a span so the emit side can hand in a `std::array<T, N>{...}`
  // literal whose self-determined type is unambiguous; the element count sizes
  // the payload.
  UnpackedArray(T element_default, std::span<const T> init)
      : shield_(std::move(element_default)), data_(init.begin(), init.end()) {
  }

  // LRM 5.9 / 21.3.3: a string value assigned to an unpacked array of bytes is
  // left-justified -- the first character lands at the array's left bound and
  // runs toward the right bound, an element past the end of the text keeps the
  // element default, and text beyond the array's last element is dropped.
  // `element_prototype` carries the element's declared representation and
  // doubles as that default; `count` is the destination's element count.
  [[nodiscard]] static auto FromString(
      const String& text, const T& element_prototype, const PackedArray& count)
      -> UnpackedArray;

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

  // LRM 7.4.5: an invalid-index write lands on the shield's discard target. The
  // declared range `[left:right]` comes from the receiver's static type as a
  // select operand.
  [[nodiscard]] auto ElementRef(
      const PackedArray& sv_index, const PackedArray& left,
      const PackedArray& right) -> T& {
    const auto ordinal = ResolveOrdinal(sv_index, left, right);
    if (!ordinal) {
      return shield_.DiscardTarget();
    }
    return data_[*ordinal];
  }

  // LRM 7.4.5: an invalid-index read returns the element default (LRM Table
  // 7-1).
  [[nodiscard]] auto Element(
      const PackedArray& sv_index, const PackedArray& left,
      const PackedArray& right) const -> const T& {
    const auto ordinal = ResolveOrdinal(sv_index, left, right);
    if (!ordinal) {
      return shield_.Default();
    }
    return data_[*ordinal];
  }

  // LRM 7.4.5 contiguous-range selector. The raw selector `(a, b, form)` is
  // resolved to the storage-ordinal window against the receiver's declared
  // `[left:right]` range; a partial-OOB position yields the canonical default
  // and an X/Z base yields a wholly-default sub-array. The result is
  // ordinal-only payload.
  [[nodiscard]] auto Slice(
      const PackedArray& a, const PackedArray& b, const PackedArray& form,
      const PackedArray& left, const PackedArray& right) const
      -> UnpackedArray {
    const SliceWindow win = ResolveSliceWindow(a, b, form, left, right);
    return UnpackedArray(
        shield_.Default(),
        detail::ArraySliceGather(
            data_, shield_.Default(), win.base, win.count, win.base_known));
  }

  [[nodiscard]] auto SliceRef(
      const PackedArray& a, const PackedArray& b, const PackedArray& form,
      const PackedArray& left, const PackedArray& right) -> ArraySliceRef<T> {
    const SliceWindow win = ResolveSliceWindow(a, b, form, left, right);
    return ArraySliceRef<T>{
        data_, shield_.Default(), win.base, win.count, win.base_known};
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

  // LRM 20.6.2 `$bits`: the current bit count is the sum of the elements' own
  // bit counts. A fixed-size unpacked array folds at elaboration; this path
  // serves the case where an element is itself dynamically sized.
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray {
    PackedArray total = PackedArray::Int(0);
    for (const auto& e : data_) {
      total = total + e.BitstreamWidth();
    }
    return total;
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

  // LRM 7.12.5 projection into a same-size fixed unpacked array; `proto` seeds
  // the result element type's canonical default (producer-supplied, since the
  // result element type may differ from this array's).
  template <typename F, typename U>
  [[nodiscard]] auto Map(F closure, U proto) const -> UnpackedArray<U> {
    return UnpackedArray<U>(
        std::move(proto), detail::ArrayMap(Entries(), closure));
  }

 private:
  // The LRM 7.12 entry stream: a lazy view pairing each element with its
  // ordinal index, in declared order.
  [[nodiscard]] auto Entries() const {
    return std::views::enumerate(data_) |
           std::views::transform([](auto&& pair) {
             auto&& [i, e] = pair;
             return detail::Entry<PackedArray, T>{
                 PackedArray::Int(static_cast<int>(i)), &e};
           });
  }

  // LRM 7.4.5: resolve a source-declared index to a storage ordinal against the
  // declared range. An X / Z index, or one outside the range, is an invalid
  // access -- `nullopt`, the read-default / write-discard path.
  [[nodiscard]] auto ResolveOrdinal(
      const PackedArray& sv_index, const PackedArray& left,
      const PackedArray& right) const -> std::optional<std::size_t> {
    if (sv_index.HasUnknown()) {
      return std::nullopt;
    }
    const UnpackedRange range{.left = left.ToInt64(), .right = right.ToInt64()};
    const std::int64_t ordinal = range.ToOrdinal(sv_index.ToInt64());
    if (ordinal < 0 || static_cast<std::uint64_t>(ordinal) >= data_.size()) {
      return std::nullopt;
    }
    return static_cast<std::size_t>(ordinal);
  }

  struct SliceWindow {
    std::int64_t base;
    std::uint32_t count;
    bool base_known;
  };

  // Resolve a raw range selector to the storage-ordinal window. `(a, b)` are
  // source coordinates -- a constant range's two declared endpoints, or an
  // indexed part-select's base and (constant) width -- and `form` says which.
  // The receiver's declared range `[left:right]` comes from its static type as
  // a select operand. The low ordinal and the count fall out of the two source
  // endpoints rebased against that range; only the base coordinate `a` can
  // carry a runtime X / Z.
  [[nodiscard]] auto ResolveSliceWindow(
      const PackedArray& a, const PackedArray& b, const PackedArray& form,
      const PackedArray& left, const PackedArray& right) const -> SliceWindow {
    const std::int64_t base_coord = a.ToInt64();
    const std::int64_t extent = b.ToInt64();
    std::int64_t other = extent;
    switch (static_cast<SliceForm>(form.ToInt64())) {
      case SliceForm::kIndexedUp:
        other = base_coord + extent - 1;
        break;
      case SliceForm::kIndexedDown:
        other = base_coord - extent + 1;
        break;
      case SliceForm::kConstant:
        break;
    }
    const UnpackedRange range{.left = left.ToInt64(), .right = right.ToInt64()};
    const std::int64_t o1 = range.ToOrdinal(base_coord);
    const std::int64_t o2 = range.ToOrdinal(other);
    const std::int64_t lo = o1 < o2 ? o1 : o2;
    const std::int64_t span = o1 < o2 ? o2 - o1 : o1 - o2;
    return SliceWindow{
        .base = lo,
        .count = static_cast<std::uint32_t>(span + 1),
        .base_known = !a.HasUnknown()};
  }

  detail::OobShield<T> shield_;
  std::vector<T> data_;

  friend class ArraySliceRef<T>;
};

// LRM 7.6: an assignment to an unpacked slice is a single assignment to the
// entire slice. The proxy aliases the source storage (a non-owning pointer to
// its element vector) plus the resolved (base ordinal, count) window, so a
// fixed-size unpacked array and a dynamic array share one slice-write surface.
// An unresolved selector makes `ToOwned()` a wholly-default sub-array and
// `operator=` a no-op; partial-OOB behaves per-element. The materialized owned
// value is ordinal-only payload (no range). Move-only so the proxy cannot
// outlive what it aliases.
template <typename T>
class ArraySliceRef {
 public:
  ArraySliceRef(
      std::vector<T>& data, T canonical, std::int64_t base, std::uint32_t count,
      bool anchor_known)
      : data_(&data),
        canonical_(std::move(canonical)),
        base_(base),
        count_(count),
        anchor_known_(anchor_known) {
  }
  ArraySliceRef(const ArraySliceRef&) = delete;
  auto operator=(const ArraySliceRef&) -> ArraySliceRef& = delete;
  ArraySliceRef(ArraySliceRef&&) noexcept = default;
  auto operator=(ArraySliceRef&&) noexcept -> ArraySliceRef& = default;
  ~ArraySliceRef() = default;

  [[nodiscard]] auto ToOwned() const -> UnpackedArray<T> {
    return UnpackedArray<T>(
        canonical_, detail::ArraySliceGather(
                        *data_, canonical_, base_, count_, anchor_known_));
  }

  auto operator=(const UnpackedArray<T>& value) -> ArraySliceRef& {
    detail::ArraySliceScatter(
        *data_, base_, count_, value.data_, anchor_known_);
    return *this;
  }

 private:
  std::vector<T>* data_;
  T canonical_;
  std::int64_t base_;
  std::uint32_t count_;
  bool anchor_known_;
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
static_assert(BitstreamSizable<UnpackedArray<PackedArray>>);
static_assert(RangedIndexable<UnpackedArray<PackedArray>>);
static_assert(RangedSliceable<UnpackedArray<PackedArray>>);
static_assert(RangedSliceableRef<UnpackedArray<PackedArray>>);
static_assert(Ownable<UnpackedArray<PackedArray>>);
static_assert(Defaultable<UnpackedArray<PackedArray>>);
static_assert(Sortable<UnpackedArray<PackedArray>>);

}  // namespace lyra::value

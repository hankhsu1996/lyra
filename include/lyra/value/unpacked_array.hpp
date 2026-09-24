#pragma once

#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/array_case_equal.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/oob_shield.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/queue.hpp"

namespace lyra::value {

template <typename T>
class UnpackedArray;
template <typename T>
class ArraySliceRef;
class String;

// LRM 7.4.6: how many elements a slice takes, which the type the select
// produces fixes. A select never names an empty run, so a count below one is a
// lowering defect rather than a value.
[[nodiscard]] inline auto SliceCount(std::int64_t count) -> std::size_t {
  if (count < 1) {
    throw InternalError("an unpacked slice names at least one element");
  }
  return static_cast<std::size_t>(count);
}

// SystemVerilog fixed-size unpacked array (LRM 7.4.2). One C++ container layer
// per declared unpacked dimension; multi-dim composes as
// `UnpackedArray<UnpackedArray<...>>`. Mirrors `PackedArray`'s surface for
// every op that crosses the SV / C++ boundary: `Element` / `Slice` for indexed
// and range access (no `operator[]`), and `operator==` / `CaseEqual` returning
// a 1-bit `PackedArray` so equality on aggregates propagates through the same
// value-type the integral surface uses.
//
// The payload is ordinal-only: it does not carry a declared range. An access
// names an element by its ordinal, counted from the left (LRM 7.6), because
// the declared range is a fact of the static type the select was written
// against and is read there. Whole-array movement is ordinal-wise and
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

  // LRM 10.9.1: `count` replications of `unit`, where a replication stands for
  // an entire dimension. Covers both a fixed array's all-default state (`unit`
  // is one element default, LRM Table 7-1) and an `'{count{...}}` pattern
  // (`unit` is the replicated items), which are the same repeat-and-count shape
  // and so construct through one path. Taking the two separately keeps a
  // uniform array O(unit) to build where an enumerated element list would be
  // O(unit * count); the seeded shield is the out-of-range / discard source.
  UnpackedArray(T element_default, std::span<const T> unit, std::size_t count)
      : shield_(std::move(element_default)) {
    data_.reserve(unit.size() * count);
    for (std::size_t i = 0; i < count; ++i) {
      data_.insert(data_.end(), unit.begin(), unit.end());
    }
  }

  // LRM 5.9 / 21.3.3: a string value assigned to an unpacked array of bytes is
  // left-justified -- the first character lands at the array's left bound and
  // runs toward the right bound, an element past the end of the text keeps the
  // element type's default, and text beyond the array's last element is
  // dropped. The clause admits this form for an array of bytes alone, so the
  // element shape is a packed type; `count` is the destination's element count.
  [[nodiscard]] static auto FromString(
      const String& text, const PackedType& element_type,
      const PackedArray& count) -> UnpackedArray;

  // LRM 5.9: a string literal assigned to an unpacked array of bytes, under the
  // same left justification. A literal is a packed bit-vector constant, not a
  // string value, so its bytes arrive whole -- a NUL among them is a byte like
  // any other, where building a string value would have removed it (LRM 6.16).
  [[nodiscard]] static auto FromPackedArray(
      const PackedArray& bits, const PackedType& element_type,
      const PackedArray& count) -> UnpackedArray;

  // LRM 10.10: adopt an unpacked concatenation's parts, accumulated into a
  // growable array by the concatenation chain, into this fixed-size type. The
  // element counts must agree; a mismatch the front end could not rule out --
  // because a spread part is sized at run time -- is a run-time error.
  // Templated on the accumulator so the fixed-size type needs no dependency on
  // the growable one that feeds it.
  template <typename C>
  [[nodiscard]] static auto ConformSize(const C& parts, std::int64_t count)
      -> UnpackedArray {
    if (static_cast<std::int64_t>(parts.RawSize()) != count) {
      throw SimulationError(
          std::format(
              "unpacked array concatenation yields {} elements but the "
              "fixed-size target has {} (LRM 10.10)",
              parts.RawSize(), count));
    }
    std::vector<T> elements;
    elements.reserve(parts.RawSize());
    for (std::size_t i = 0; i < parts.RawSize(); ++i) {
      elements.push_back(parts.RawAt(i));
    }
    return UnpackedArray(parts.ElementDefault(), std::span<const T>(elements));
  }

  // LRM 7.6: a fixed-size unpacked array assigned an array of another unpacked
  // kind takes its elements in left-to-right order, and how many elements it
  // has is a declared property of the variable being written rather than
  // anything the source decides -- so a source of another size is a run-time
  // error and the assignment does not happen. The count arrives as an operand
  // because the value carries no declared range of its own. Named because the
  // argument list cannot tell it from building over an element list.
  template <OrdinalElements C>
  [[nodiscard]] static auto FromArray(
      const C& source, T element_default, std::int64_t declared)
      -> UnpackedArray {
    if (static_cast<std::int64_t>(source.RawSize()) != declared) {
      throw SimulationError(
          std::format(
              "a fixed-size unpacked array of {} elements cannot be assigned "
              "an array of {} (LRM 7.6)",
              declared, source.RawSize()));
    }
    UnpackedArray result(std::move(element_default));
    result.data_.reserve(source.RawSize());
    for (std::size_t i = 0; i < source.RawSize(); ++i) {
      result.data_.push_back(source.RawAt(i));
    }
    return result;
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

  // Flat-storage element read: `i` is a storage ordinal in [0, RawSize()), with
  // no invalid-index handling. It serves a traversal that already walks storage
  // in ordinal order, where a position the program computed may name no
  // element and is answered the way LRM 7.4.5 requires.
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

  // LRM 11.4.11: the two arms of a conditional operator whose condition is
  // ambiguous, combined element by element -- an element the arms agree on
  // survives, and one they disagree on, or cannot know, takes the element
  // default (Table 7-1). Arms of unequal size put no elements in
  // correspondence, so every element takes that default.
  [[nodiscard]] auto MergeConditional(const UnpackedArray& other) const
      -> UnpackedArray {
    const bool paired = RawSize() == other.RawSize();
    UnpackedArray result = *this;
    for (std::size_t i = 0; i < result.data_.size(); ++i) {
      const bool agree = paired && (data_[i] == other.data_[i]).Truth() ==
                                       Truthiness::kKnownNonzero;
      if (!agree) {
        result.data_[i] = shield_.Default();
      }
    }
    return result;
  }

  // LRM 7.4.5: an invalid-index write lands on the shield's discard target.
  [[nodiscard]] auto ElementRef(const PackedArray& position) -> T& {
    const auto ordinal = ElementOrdinal(position, data_.size());
    if (!ordinal) {
      return shield_.DiscardTarget();
    }
    return data_[*ordinal];
  }

  // LRM 7.4.5: an invalid-index read returns the element default (LRM Table
  // 7-1).
  [[nodiscard]] auto Element(const PackedArray& position) const -> const T& {
    const auto ordinal = ElementOrdinal(position, data_.size());
    if (!ordinal) {
      return shield_.Default();
    }
    return data_[*ordinal];
  }

  // LRM 7.4.5 contiguous-range selector: `count` elements from `start`. An
  // element outside the array reads the canonical default, and a start that
  // names no position reads a wholly-default sub-array. The result is
  // ordinal-only payload.
  [[nodiscard]] auto Slice(const PackedArray& start, std::int64_t count) const
      -> UnpackedArray {
    return UnpackedArray(
        shield_.Default(),
        detail::ArraySliceGather(
            data_, shield_.Default(), ReadPosition(start), SliceCount(count)));
  }

  [[nodiscard]] auto SliceRef(const PackedArray& start, std::int64_t count)
      -> ArraySliceRef<T> {
    return ArraySliceRef<T>{
        data_, shield_.Default(), ReadPosition(start), SliceCount(count)};
  }

  // LRM 11.2.2 + 11.4.5 aggregate equality / case-equality. Slang's binding
  // enforces equivalent operand size, so the loops over `data_` are matched.
  // `==` / `!=` propagate X / Z; `CaseEqual` returns a deterministic 0/1.
  [[nodiscard]] auto operator==(const UnpackedArray& other) const
      -> PackedArray {
    // LRM 11.4.5: the answer carries the state class an element's own equality
    // produces, because that is what a run of them reduces to. Reading the
    // class off the element shape rather than off a first element is what lets
    // the run start at the identity, so no length is a case of its own.
    PackedArray result = PackedArray::FromInt(
        1, 1, false, (shield_.Default() == shield_.Default()).IsFourState());
    for (std::size_t i = 0; i < data_.size(); ++i) {
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

  // Net resolution applied element-wise under the fold `fold` names (LRM 6.6).
  // LRM 6.7.1 defines a net over an unpacked array as one net composed of its
  // elements' bits, so folding two contributions is folding each element pair.
  [[nodiscard]] auto ResolveNet(
      const UnpackedArray& other, NetResolution fold) const -> UnpackedArray {
    UnpackedArray resolved = *this;
    for (std::size_t i = 0; i < resolved.data_.size(); ++i) {
      resolved.data_[i] = resolved.data_[i].ResolveNet(other.data_[i], fold);
    }
    return resolved;
  }

  // What a stronger contribution leaves a weaker one, element by element (LRM
  // 28.12.1).
  [[nodiscard]] auto Dominating(const UnpackedArray& weaker) const
      -> UnpackedArray {
    UnpackedArray resolved = *this;
    for (std::size_t i = 0; i < resolved.data_.size(); ++i) {
      resolved.data_[i] = resolved.data_[i].Dominating(weaker.data_[i]);
    }
    return resolved;
  }

  // `prototype`'s shape with every bit set to `fill`: the element count and
  // each element filled the same way (LRM 6.7.1). Only the prototype's shape is
  // read. The out-of-bounds shield keeps the prototype's element default, which
  // an invalid-index read returns under LRM 7.4.5 whether the array is a net or
  // a variable.
  [[nodiscard]] static auto FilledLike(
      const UnpackedArray& prototype, const PackedArray& fill)
      -> UnpackedArray {
    UnpackedArray filled = prototype;
    for (T& element : filled.data_) {
      element = T::FilledLike(element, fill);
    }
    return filled;
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

  // LRM 6.24.3: the elements' own streams laid end to end, the element at
  // index 0 most significant -- the order a `foreach` traverses them in
  // (LRM 11.4.14.1).
  [[nodiscard]] auto ToBitstream() const -> PackedArray {
    PackedArray stream = data_[0].ToBitstream();
    for (std::size_t i = 1; i < data_.size(); ++i) {
      stream = stream.Concat(data_[i].ToBitstream());
    }
    return stream;
  }

  // The inverse, each element taking its own width off the front of what is
  // left (LRM 11.4.14.3). The prototype states the element count and every
  // element's shape, both of which a sequence of bits carries nothing of.
  [[nodiscard]] static auto FromBitstream(
      const PackedArray& bits, const UnpackedArray& prototype)
      -> UnpackedArray {
    UnpackedArray result = prototype;
    std::uint64_t consumed = 0;
    for (std::size_t i = 0; i < result.data_.size(); ++i) {
      const auto width = static_cast<std::uint64_t>(
          prototype.data_[i].BitstreamWidth().ToInt64());
      result.data_[i] = T::FromBitstream(
          BitstreamSegment(bits, consumed, width), prototype.data_[i]);
      consumed += width;
    }
    return result;
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

  detail::OobShield<T> shield_;
  std::vector<T> data_;

  friend class ArraySliceRef<T>;
};

// LRM 7.6: an assignment to an unpacked slice is a single assignment to the
// entire slice. The proxy aliases the source storage (a non-owning pointer to
// its element vector) plus the window's start and count, so a fixed-size
// unpacked array and a dynamic array share one slice-write surface. A start
// that names no position makes `ToOwned()` a wholly-default sub-array and
// `operator=` a no-op; partial-OOB behaves per-element. The materialized owned
// value is ordinal-only payload (no range). Move-only so the proxy cannot
// outlive what it aliases.
template <typename T>
class ArraySliceRef {
 public:
  ArraySliceRef(
      std::vector<T>& data, T canonical, std::optional<std::int64_t> start,
      std::size_t count)
      : data_(&data),
        canonical_(std::move(canonical)),
        start_(start),
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
        detail::ArraySliceGather(*data_, canonical_, start_, count_));
  }

  auto operator=(const UnpackedArray<T>& value) -> ArraySliceRef& {
    detail::ArraySliceScatter(*data_, start_, count_, value.data_);
    return *this;
  }

 private:
  std::vector<T>* data_;
  T canonical_;
  std::optional<std::int64_t> start_;
  std::size_t count_;
};

// Left-justifies a byte sequence into `count` elements: the first byte lands at
// the array's left bound and runs toward the right, an element past the end of
// the sequence keeps the element type's default, and bytes beyond the last
// element are dropped.
template <typename T>
auto UnpackedArray<T>::FromPackedArray(
    const PackedArray& bits, const PackedType& element_type,
    const PackedArray& count) -> UnpackedArray<T> {
  const std::string bytes = bits.ByteString();
  const auto element_count = static_cast<std::size_t>(count.ToInt64());
  const T element_default{element_type};
  std::vector<T> elements;
  elements.reserve(element_count);
  for (std::size_t i = 0; i < element_count; ++i) {
    elements.push_back(
        i < bytes.size()
            ? PackedArray::FromInt(
                  static_cast<unsigned char>(bytes[i]), element_type)
            : element_default);
  }
  return UnpackedArray<T>{element_default, std::span<const T>{elements}};
}

static_assert(LyraValue<UnpackedArray<PackedArray>>);
static_assert(Sized<UnpackedArray<PackedArray>>);
static_assert(BitstreamSizable<UnpackedArray<PackedArray>>);
static_assert(BitstreamConvertible<UnpackedArray<PackedArray>>);
static_assert(Indexable<UnpackedArray<PackedArray>>);
static_assert(Sliceable<UnpackedArray<PackedArray>>);
static_assert(SliceableRef<UnpackedArray<PackedArray>>);
static_assert(Ownable<UnpackedArray<PackedArray>>);
static_assert(Defaultable<UnpackedArray<PackedArray>>);
static_assert(ConditionallyMergeable<UnpackedArray<PackedArray>>);
static_assert(Sortable<UnpackedArray<PackedArray>>);
static_assert(NetResolvable<UnpackedArray<PackedArray>>);
static_assert(NetResolvable<UnpackedArray<UnpackedArray<PackedArray>>>);
static_assert(OrdinalElements<UnpackedArray<PackedArray>>);

}  // namespace lyra::value

#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <utility>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/basic_dynamic_array.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/queue.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::value {

// SystemVerilog dynamic array (LRM 7.5). Size set at run time via `new[N]` /
// `new[N](other)` constructors; default is the empty array (LRM Table 6-7). The
// element default is what an invalid-index read returns.
template <typename T>
class DynamicArray : public OrdinalArrayMethods<DynamicArray<T>, T> {
 public:
  using ElementType = T;
  template <typename U>
  using Rebound = DynamicArray<U>;

  // Sentinel "uninitialized" form -- empty container with no element default.
  // Used as the declared default state of a `Var<DynamicArray<T>>` field; the
  // first MIR-level assignment overwrites the whole array (LRM 10.5 variable
  // initialization).
  DynamicArray() = default;

  // Empty container with the element default seeded. Used for declarations
  // like `int arr[];` where the array starts empty but the element shape is
  // known at lowering time.
  explicit DynamicArray(T element_default)
      : core_(StaticElem<T>(std::move(element_default))) {
  }

  // LRM 7.5.1 `new[N]`: build `n` elements, each a copy of the element default.
  // `n` is a longint per LRM 7.5.1; the negative-N case throws at construction.
  DynamicArray(std::int64_t n, T element_default)
      : core_(
            StaticElem<T>(std::move(element_default)),
            CountOf(n, "dynamic array new[N]"), nullptr) {
  }

  // LRM 7.5.1 `new[N](other)`: the first `N` of `other`'s elements, padded with
  // the element default where `other` has fewer.
  DynamicArray(std::int64_t n, T element_default, const DynamicArray& src)
      : core_(
            StaticElem<T>(std::move(element_default)),
            CountOf(n, "dynamic array new[N](src)"), &src.core_) {
  }

  // LRM 10.9.1 assignment-pattern construction: the element default seeded,
  // size taken from the pattern's element list. Mirrors `UnpackedArray`'s span
  // ctor so a single emit path produces `std::array<T, N>{...}` as the
  // second argument for either container.
  DynamicArray(T element_default, std::span<const T> init)
      : DynamicArray(std::move(element_default)) {
    core_ = core_.Extended(detail::ReplicatedAddresses(init, 1));
  }

  // LRM 10.9.1 `'{count{...}}` pattern: `count` replications of `unit`.
  // Taking the repeat unit and the count separately keeps the value O(unit)
  // to build rather than O(unit * count).
  DynamicArray(T element_default, std::span<const T> unit, std::size_t count)
      : DynamicArray(std::move(element_default)) {
    core_ = core_.Extended(detail::ReplicatedAddresses(unit, count));
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

  [[nodiscard]] static auto New(std::int64_t n, T element_default)
      -> DynamicArray {
    return DynamicArray(n, std::move(element_default));
  }

  [[nodiscard]] static auto NewCopy(
      std::int64_t n, T element_default, const DynamicArray& src)
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
    result.core_ = result.core_.Extended(detail::OrdinalAddresses(source));
    return result;
  }

  DynamicArray(const DynamicArray&) = default;
  DynamicArray(DynamicArray&&) noexcept = default;
  auto operator=(const DynamicArray&) -> DynamicArray& = default;
  auto operator=(DynamicArray&&) noexcept -> DynamicArray& = default;
  ~DynamicArray() = default;

  // LRM 7.5.1: size() yields an SV int.
  [[nodiscard]] auto Size() const -> Int {
    return Int::FromInt(static_cast<std::int64_t>(RawSize()));
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

  // LRM 7.4.5: an invalid-index write lands where no read reaches, which is no
  // element of the array.
  [[nodiscard]] auto ElementRef(const Position& position, Formation& formed)
      -> T& {
    return *static_cast<T*>(core_.ElementRef(ReadPosition(position), formed));
  }
  [[nodiscard]] auto ElementRef(const Position& position) -> T& {
    return *static_cast<T*>(core_.ExistingAt(ReadPosition(position)));
  }

  // LRM 7.4.5: an invalid-index read returns the element default (LRM Table
  // 7-1).
  [[nodiscard]] auto Element(const Position& position) const -> const T& {
    return *static_cast<const T*>(core_.ElementAt(ReadPosition(position)));
  }

  // LRM 7.4.5 / 7.4.6 contiguous-range selector: a fixed-size unpacked array of
  // `count` elements from `start`. An element outside the array, and every
  // element of a start that names no position, reads the canonical default.
  [[nodiscard]] auto Slice(const Position& start, std::int64_t count) const
      -> UnpackedArray<T> {
    return UnpackedArray<T>::SliceOf(
        core_, ReadPosition(start), SliceCount(count));
  }

  [[nodiscard]] auto SliceRef(const Position& start, std::int64_t count)
      -> ArraySliceRef<T> {
    return ArraySliceRef<T>{core_, ReadPosition(start), SliceCount(count)};
  }

  [[nodiscard]] auto operator==(const DynamicArray& other) const ->
      typename StaticElem<T>::Equality {
    return StaticElem<T>::Equality::Filled(
        detail::SequenceEqual(core_, other.core_));
  }
  [[nodiscard]] auto operator!=(const DynamicArray& other) const ->
      typename StaticElem<T>::Equality {
    return !(*this == other);
  }
  [[nodiscard]] auto CaseEqual(const DynamicArray& other) const -> Bit {
    return detail::SequenceCaseEqual(core_, other.core_);
  }
  [[nodiscard]] auto IsBitIdentical(const DynamicArray& other) const -> bool {
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

  // LRM 7.5.3: empties the array, resulting in a zero-sized array.
  auto Delete() -> void {
    core_.Delete();
  }

  // LRM 10.10 unpacked concatenation, as the two-operand steps a join folds to:
  // this array with one element appended, or with every element of a spread
  // part appended in order. A part is one element unless the program spreads a
  // container. Value-returning so the fold chains them without mutating a
  // shared array, and unbounded, so every appended element is kept.
  [[nodiscard]] auto ConcatElement(const T& item) const -> DynamicArray {
    DynamicArray out;
    const std::array<const void*, 1> appended{&item};
    out.core_ = core_.Extended(appended);
    return out;
  }
  template <typename C>
  [[nodiscard]] auto ConcatSpread(const C& part) const -> DynamicArray {
    DynamicArray out;
    out.core_ = core_.Extended(detail::OrdinalAddresses(part));
    return out;
  }

 private:
  friend class OrdinalArrayMethods<DynamicArray<T>, T>;

  // A run-time element count (LRM 7.5.1), which a negative one has none of.
  [[nodiscard]] static auto CountOf(std::int64_t count, const char* what)
      -> std::size_t {
    if (count < 0) {
      throw SimulationError(
          std::string(what) + ": size operand is negative (LRM 7.5.1)");
    }
    return static_cast<std::size_t>(count);
  }

  BasicDynamicArray<StaticElem<T>> core_;
};

static_assert(LyraValue<DynamicArray<LogicVector<4>>>);
static_assert(Sized<DynamicArray<LogicVector<4>>>);
static_assert(BitstreamSizable<DynamicArray<LogicVector<4>>>);
static_assert(Indexable<DynamicArray<LogicVector<4>>>);
static_assert(Sliceable<DynamicArray<LogicVector<4>>>);
static_assert(SliceableRef<DynamicArray<LogicVector<4>>>);
static_assert(Sortable<DynamicArray<LogicVector<4>>>);
static_assert(OrdinalElements<DynamicArray<LogicVector<4>>>);

}  // namespace lyra::value

#pragma once

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/basic_dynamic_array.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/queue.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::value {

// SystemVerilog dynamic array (LRM 7.5). Size set at run time via `new[N]` /
// `new[N](other)` constructors; default is the empty array (LRM Table 6-7).
// What an invalid-index read returns and a new element starts as is the element
// type's default (LRM Table 7-1), which `T`'s own default construction builds.
template <typename T>
class DynamicArray : public OrdinalArrayMethods<DynamicArray<T>> {
 public:
  using ElementType = T;

  DynamicArray() = default;

  // LRM 7.5.1 `new[N]`: `n` elements, each the element type's default. `n` is
  // a longint per LRM 7.5.1; the negative-N case throws at construction.
  explicit DynamicArray(std::int64_t n)
      : core_(StaticElem<T>{}, CountOf(n), nullptr) {
  }

  // LRM 7.5.1 `new[N](other)`: the first `N` of `other`'s elements, padded with
  // the element type's default where `other` has fewer.
  DynamicArray(std::int64_t n, const DynamicArray& src)
      : core_(StaticElem<T>{}, CountOf(n), &src.core_) {
  }

  // LRM 10.9.1 assignment-pattern construction: the elements of the pattern's
  // list, in order.
  explicit DynamicArray(std::span<const T> init) {
    core_ = core_.Extended(detail::ReplicatedAddresses(init, 1));
  }

  // LRM 10.9.1 `'{count{...}}` pattern: `count` replications of `unit`.
  // Taking the repeat unit and the count separately keeps the value O(unit)
  // to build rather than O(unit * count).
  DynamicArray(std::span<const T> unit, std::size_t count) {
    core_ = core_.Extended(detail::ReplicatedAddresses(unit, count));
  }

  // The LRM 7.5.1 run-time-sized forms, each named. Which one a source
  // construct means is decided where that construct is read, not by the
  // argument list, so a target with no overload resolution of its own has a
  // name to call. The assignment pattern stays the constructor, since a
  // container has one way to be built from an element list.
  [[nodiscard]] static auto New(std::int64_t n) -> DynamicArray {
    return DynamicArray(n);
  }

  [[nodiscard]] static auto NewCopy(std::int64_t n, const DynamicArray& src)
      -> DynamicArray {
    return DynamicArray(n, src);
  }

  // LRM 7.6: a dynamic array assigned an array of any of the three unpacked
  // kinds is resized to the source's element count and takes its elements in
  // left-to-right order. The clause admits the assignment only where the
  // element types are equivalent, so each element crosses as it stands. Named
  // rather than left a constructor because an argument list of one operand
  // does not say which of the run-time-sized forms it is, and a target with no
  // overload resolution has nothing else to read.
  template <OrdinalElements C>
  [[nodiscard]] static auto FromArray(const C& source) -> DynamicArray {
    DynamicArray result;
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

  // LRM 7.4.5: an invalid-index write lands where no read reaches, which is no
  // element of the array.
  [[nodiscard]] auto ElementMut(const Position& position, Formation& formed)
      -> T& {
    return *static_cast<T*>(core_.ElementMut(ReadPosition(position), formed));
  }
  [[nodiscard]] auto ElementMut(const Position& position) -> T& {
    return *static_cast<T*>(core_.ExistingAt(ReadPosition(position)));
  }

  // LRM 7.4.5: an invalid-index read returns the element default (LRM Table
  // 7-1).
  [[nodiscard]] auto Element(const Position& position) const -> const T& {
    return *static_cast<const T*>(core_.ElementAt(ReadPosition(position)));
  }

  // LRM 7.4.5 / 7.4.6 contiguous-range selector: a fixed-size unpacked array of
  // `count` elements from `start`. An element outside the array, and every
  // element of a start that names no position, reads the element default.
  template <typename Result>
  [[nodiscard]] auto Slice(const Position& start, std::int64_t count) const
      -> Result {
    Result::RequireDeclared(count);
    return Result::SliceOf(Elements(), ReadPosition(start));
  }

  // LRM 7.6: `value` written into the `count` elements from `start`, one
  // assignment to the whole window, answering whether any element took a
  // different value (LRM 4.3).
  template <OrdinalElements C>
  auto AssignSlice(const Position& start, std::int64_t count, const C& value)
      -> bool {
    return AssignRun(
        MutableElements(), ReadPosition(start), SliceCount(count), value);
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
  [[nodiscard]] auto MergeConditional(const DynamicArray& other) const
      -> DynamicArray {
    DynamicArray out;
    out.core_ = core_.MergeConditional(other.core_);
    return out;
  }

  // LRM 7.5.3: empties the array, resulting in a zero-sized array.
  auto Delete() -> void {
    core_.Delete();
  }

 private:
  friend class OrdinalArrayMethods<DynamicArray<T>>;

  // A run-time element count (LRM 7.5.1), which a negative one has none of.
  [[nodiscard]] static auto CountOf(std::int64_t count) -> std::size_t {
    if (count < 0) {
      throw SimulationError(
          "the size given to a dynamic array's new[] is negative");
    }
    return static_cast<std::size_t>(count);
  }

  // Puts the value that was at `order[k]` at index `k` (LRM 7.12.2).
  void Permute(std::span<const std::size_t> order) {
    core_.Permute(order);
  }

  // The elements, in index order, where this generation holds them.
  [[nodiscard]] auto Elements() const -> std::span<const T> {
    return {static_cast<const T*>(core_.At(0)), core_.Count()};
  }
  [[nodiscard]] auto MutableElements() -> std::span<T> {
    return {static_cast<T*>(core_.At(0)), core_.Count()};
  }

  BasicDynamicArray<StaticElem<T>> core_;
};

static_assert(LyraValue<DynamicArray<LogicVector<4>>>);
static_assert(Sized<DynamicArray<LogicVector<4>>>);
static_assert(BitstreamSizable<DynamicArray<LogicVector<4>>>);
static_assert(ConditionallyMergeable<DynamicArray<LogicVector<4>>>);
static_assert(Indexable<DynamicArray<LogicVector<4>>>);
static_assert(Sortable<DynamicArray<LogicVector<4>>>);
static_assert(OrdinalElements<DynamicArray<LogicVector<4>>>);

}  // namespace lyra::value

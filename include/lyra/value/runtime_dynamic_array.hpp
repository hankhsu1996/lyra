#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"

namespace lyra::value {

struct RuntimeValue;

// The runtime-owned realization of a SystemVerilog dynamic array (LRM 7.5),
// MIR's `DynamicArrayType`. A run-time-sized homogeneous container that owns
// its elements by value: copy is an element-wise deep copy, destruction is C++
// RAII, so an element never borrows caller storage.
//
// This is the execution backend's type-erased counterpart of the C++ backend's
// monomorphized `DynamicArray<T>`, the aggregate-family peer of `RuntimeTuple`.
// One `RuntimeDynamicArray` holds a vector of type-erased `RuntimeValue`
// elements and an element-default prototype, and composes the value contract by
// visiting them.
//
// Each element is storage of its own, written where it lies, and value
// semantics hold because a copy of the array copies its elements: no two arrays
// share one, so a write through one is never seen through another.
class RuntimeDynamicArray {
 public:
  // The uninitialized sentinel form -- the empty array before its declared
  // element shape is known. It is the declared default state of a
  // `Var<RuntimeDynamicArray>` cell; the cell's first initialization overwrites
  // it with the real element default.
  RuntimeDynamicArray();

  // LRM Table 6-7: the default dynamic array is empty. `element_default` is the
  // shape source for out-of-range reads (LRM 7.4.5) and resize fills; it
  // carries the exact element representation, so a nested struct or packed
  // element keeps its member initializers and width.
  explicit RuntimeDynamicArray(RuntimeValue element_default);

  // LRM 7.5.1 `new[N]`: `n` elements, each a copy of the element default.
  RuntimeDynamicArray(const PackedArray& n, RuntimeValue element_default);

  // LRM 7.5.1 `new[N](src)`: copy `src`, then resize to `n`, truncating when
  // smaller and padding with the element default when larger.
  RuntimeDynamicArray(
      const PackedArray& n, RuntimeValue element_default,
      const RuntimeDynamicArray& src);

  // LRM 10.9.1 assignment-pattern construction: the element list, with the
  // element default seeded for later out-of-range reads.
  RuntimeDynamicArray(
      RuntimeValue element_default, std::vector<RuntimeValue> elements);

  RuntimeDynamicArray(const RuntimeDynamicArray&);
  RuntimeDynamicArray(RuntimeDynamicArray&&) noexcept;
  auto operator=(const RuntimeDynamicArray&) -> RuntimeDynamicArray&;
  auto operator=(RuntimeDynamicArray&&) noexcept -> RuntimeDynamicArray&;
  ~RuntimeDynamicArray();

  // LRM 7.5.1: the current element count as an SV `int`.
  [[nodiscard]] auto Size() const -> PackedArray;

  // The element-default prototype. Its runtime domain is the array's element
  // domain, so a caller boxing an incoming element value into the erased
  // representation reads the target domain from here.
  [[nodiscard]] auto ElementDefault() const -> const RuntimeValue&;

  // LRM 7.4.5 / 7.4.6: reads the element `position` names by reference. A
  // position that names no element here reads the element default.
  [[nodiscard]] auto Element(const PackedArray& position) const
      -> const RuntimeValue&;

  // LRM 7.4.6: the element `position` names, as storage a write lands in. A
  // position that names no element here yields storage nothing reads, so a
  // write there is discarded.
  [[nodiscard]] auto ElementRef(const PackedArray& position) -> RuntimeValue&;

  // The element at storage position `position`, counted from the first in the
  // array's own order -- the coordinate LRM 7.12 walks a container by. A
  // position past the last is a walk defect rather than an out-of-range read.
  [[nodiscard]] auto ElementAt(std::size_t position) const
      -> const RuntimeValue&;

  // LRM 7.5.3 `delete`: empties the array, keeping its element default.
  void Delete();

  // LRM 7.4.6 contiguous-range read: `count` elements from `start`, as a
  // fixed-size unpacked array. An element outside the array, and every element
  // of a start that names no position, reads the element default.
  [[nodiscard]] auto Slice(const PackedArray& start, std::int64_t count) const
      -> RuntimeUnpackedArray;

  // A whole-slice write (LRM 7.6): the window takes `replacement`, element for
  // element, into the elements already there. An element outside the array is
  // skipped and a start that names no position writes no element, matching the
  // invalid-index write contract; assignment compatibility gives the
  // replacement the window's element count.
  void AssignSlice(
      const PackedArray& start, std::int64_t count,
      const RuntimeUnpackedArray& replacement);

  // LRM 10.10 unpacked concatenation, as the two-operand steps a join folds to:
  // this array with one element appended, or with every element of a spread
  // part appended in order. Functional, so the fold chains them without
  // disturbing a shared array, and unbounded, so every appended element is
  // kept.
  [[nodiscard]] auto ConcatElement(RuntimeValue item) const
      -> RuntimeDynamicArray;
  [[nodiscard]] auto ConcatSpread(const RuntimeValue& part) const
      -> RuntimeDynamicArray;

  // LRM 7.6: a dynamic array assigned an array of any of the three unpacked
  // kinds is resized to the source's element count and takes its elements in
  // left-to-right order. The element default is the destination's own, the
  // element shape being a declared property of the variable being written.
  [[nodiscard]] static auto FromArray(
      const RuntimeValue& source, RuntimeValue element_default)
      -> RuntimeDynamicArray;

  // LRM 11.4.5 `==` / `!=` (Any data type): a size check then an element-wise
  // reduction that propagates X / Z through each element's own equality.
  [[nodiscard]] auto operator==(const RuntimeDynamicArray& other) const
      -> PackedArray;
  [[nodiscard]] auto operator!=(const RuntimeDynamicArray& other) const
      -> PackedArray;

  // LRM 11.4.5 `===` / `!==`: element-wise case equality, deterministic in
  // X / Z.
  [[nodiscard]] auto CaseEqual(const RuntimeDynamicArray& other) const
      -> PackedArray;

  // LRM 9.4.2 update-event predicate (engine change-detection hook).
  [[nodiscard]] auto IsBitIdentical(const RuntimeDynamicArray& other) const
      -> bool;

  // LRM 20.9: any element carrying an unknown bit propagates up.
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> PackedArray;

  // LRM 20.6.2 `$bits`: the sum of the elements' own widths, an aggregate's
  // bit stream being its elements' laid end to end.
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray;

  // LRM 20.9 `$countbits`: the sum of the elements' own counts, a container's
  // bit stream being its elements' laid end to end.
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray;

 private:
  // Indirect because `RuntimeValue` closes over this type: a by-value member
  // would need `RuntimeValue` complete here, which it is not.
  std::unique_ptr<RuntimeValue> element_default_;
  std::vector<RuntimeValue> data_;
};

static_assert(LyraValue<RuntimeDynamicArray>);
static_assert(CaseEqualComparable<RuntimeDynamicArray>);
static_assert(Sized<RuntimeDynamicArray>);
static_assert(BitstreamSizable<RuntimeDynamicArray>);
static_assert(EntryWalkable<RuntimeDynamicArray>);
static_assert(Sliceable<RuntimeDynamicArray>);

}  // namespace lyra::value

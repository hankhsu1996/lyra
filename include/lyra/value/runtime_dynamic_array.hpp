#pragma once

#include <memory>
#include <vector>

#include "lyra/value/packed_array.hpp"

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
// Value semantics are preserved by immutability: every apparent mutation is a
// functional operation returning a new array (`WithElement`, `Delete`), never
// an in-place write, so an array whose handle is shared by a copy is never
// disturbed by a write through another copy.
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

  // LRM 7.4.5 / 7.4.6: reads element `index` by reference. An out-of-range or
  // x / z index reads the element default. The caller copies the result out
  // across the opaque-handle boundary rather than aliasing it.
  [[nodiscard]] auto Element(const PackedArray& index) const
      -> const RuntimeValue&;

  // A functional element write: yields a new array equal to this one with
  // element `index` replaced by `value`. LRM 7.4.6: an out-of-range or x / z
  // index leaves the array unchanged (the write is discarded).
  [[nodiscard]] auto WithElement(const PackedArray& index, RuntimeValue value)
      const -> RuntimeDynamicArray;

  // LRM 7.5.3 `delete`: a functional clear -- yields the empty array with the
  // same element default.
  [[nodiscard]] auto Delete() const -> RuntimeDynamicArray;

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

 private:
  // A negative, out-of-range, or x / z index (LRM 7.4.5). A valid index is
  // returned as its storage ordinal.
  [[nodiscard]] auto IsInvalidIndex(const PackedArray& index) const -> bool;

  // Indirect because `RuntimeValue` closes over this type: a by-value member
  // would need `RuntimeValue` complete here, which it is not.
  std::unique_ptr<RuntimeValue> element_default_;
  std::vector<RuntimeValue> data_;
};

}  // namespace lyra::value

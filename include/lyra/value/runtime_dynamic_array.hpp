#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/value/basic_dynamic_array.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// A dynamic array (LRM 7.5) as the library holds one: the dynamic array every
// element type shares, compiled once with its element type's table, so a value
// of a type the library was compiled without is held as its own bytes. Every
// element is handed in and out by its address, which is where it lies in the
// array.
class RuntimeDynamicArray {
 public:
  // The empty array before its declared element type is known: the declared
  // default state of a cell, which the cell's first initialization overwrites.
  RuntimeDynamicArray();

  // LRM 7.5.1: an array of `element` holding `count` elements, the first of
  // them copies of `from`'s first elements where it is given and the rest
  // `element_default` (LRM Table 7-1).
  RuntimeDynamicArray(
      const ValueType& element, const void* element_default, std::size_t count,
      const RuntimeDynamicArray* from);

  RuntimeDynamicArray(const RuntimeDynamicArray&);
  RuntimeDynamicArray(RuntimeDynamicArray&&) noexcept;
  auto operator=(const RuntimeDynamicArray&) -> RuntimeDynamicArray&;
  auto operator=(RuntimeDynamicArray&&) noexcept -> RuntimeDynamicArray&;
  ~RuntimeDynamicArray();

  // LRM 7.6 / 10.9.1: copies of `items`, in order, as an array of `element`.
  [[nodiscard]] static auto FromElements(
      const ValueType& element, const void* element_default,
      std::span<const void* const> items) -> RuntimeDynamicArray;

  [[nodiscard]] auto ElementType() const -> const ValueType&;
  [[nodiscard]] auto ElementDefault() const -> const void*;

  // LRM 7.5.2: the current element count, and as an SV `int`.
  [[nodiscard]] auto Count() const -> std::size_t;
  [[nodiscard]] auto Size() const -> PackedArray;

  // The element at storage position `position` -- the coordinate LRM 7.12
  // walks a container by.
  [[nodiscard]] auto ElementAt(std::size_t position) const -> const void*;
  [[nodiscard]] auto ElementAt(std::size_t position) -> void*;

  // LRM 7.4.5: the element `position` names, the element default where it names
  // none; and the element as storage a write lands in, where no read reaches
  // where it names none.
  [[nodiscard]] auto Element(const PackedArray& position) const -> const void*;
  [[nodiscard]] auto ElementRef(const PackedArray& position, Formation& formed)
      -> void*;

  // LRM 7.5.3: empties the array.
  void Delete();

  // LRM 7.4.5 / 7.4.6: the `count` elements from `start`, each the element
  // default where it lies outside the array, every one of them where `start`
  // names no position.
  [[nodiscard]] auto SliceElements(const PackedArray& start, std::int64_t count)
      const -> std::vector<const void*>;

  // LRM 7.6: the window takes `replacement`, element for element; an element
  // outside the array is skipped and a start naming no position writes
  // nothing. Answers whether any element took a different value (LRM 4.3).
  auto AssignSlice(
      const PackedArray& start, std::int64_t count,
      std::span<const void* const> replacement) -> bool;

  // LRM 10.10: a copy of this array with copies of `items` appended in order.
  [[nodiscard]] auto Concat(std::span<const void* const> items) const
      -> RuntimeDynamicArray;

  // LRM 7.12.2: puts the value that was at `order[k]` at index `k`.
  void Permute(std::span<const std::size_t> order);

  [[nodiscard]] auto operator==(const RuntimeDynamicArray& other) const
      -> PackedArray;
  [[nodiscard]] auto operator!=(const RuntimeDynamicArray& other) const
      -> PackedArray;
  [[nodiscard]] auto CaseEqual(const RuntimeDynamicArray& other) const
      -> PackedArray;
  [[nodiscard]] auto IsBitIdentical(const RuntimeDynamicArray& other) const
      -> bool;
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> PackedArray;
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray;
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray;

 private:
  explicit RuntimeDynamicArray(BasicDynamicArray<WitnessedElem> core);

  void RequireInstalled() const;
  [[nodiscard]] auto Core() const -> const BasicDynamicArray<WitnessedElem>&;
  [[nodiscard]] auto Core() -> BasicDynamicArray<WitnessedElem>&;

  std::optional<BasicDynamicArray<WitnessedElem>> core_;
};

static_assert(LyraValue<RuntimeDynamicArray>);
static_assert(CaseEqualComparable<RuntimeDynamicArray>);
static_assert(Sized<RuntimeDynamicArray>);
static_assert(BitstreamSizable<RuntimeDynamicArray>);

}  // namespace lyra::value

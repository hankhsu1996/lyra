#pragma once

#include <cstddef>
#include <memory>
#include <vector>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

struct RuntimeValue;
class String;

// The runtime-owned realization of a fixed-size unpacked array (LRM 7.4.2),
// MIR's `UnpackedArrayType`. A homogeneous container whose element count is
// fixed at construction and which owns its elements by value: copy is an
// element-wise deep copy, destruction is C++ RAII, so an element never borrows
// caller storage.
//
// This is the execution backend's type-erased counterpart of the C++ backend's
// monomorphized `UnpackedArray<T>`, the fixed-size peer of
// `RuntimeDynamicArray`. A compile-once runtime cannot instantiate a distinct
// C++ type per element type, so one `RuntimeUnpackedArray` holds a vector of
// type-erased `RuntimeValue` elements and an element-default prototype, and
// composes the value contract by visiting them.
//
// The payload is ordinal-only: the declared coordinate range is a fact of the
// receiver's static type and reaches element access as a `[left:right]` operand
// pair, so whole-value movement is range-agnostic and no store relabels a
// coordinate.
//
// Value semantics are preserved by immutability: an element write is a
// functional operation returning a new array (`WithElement`), never an in-place
// write, so an array whose handle is shared by a copy is never disturbed by a
// write through another copy.
class RuntimeUnpackedArray {
 public:
  // The uninitialized sentinel form -- the empty array before its declared
  // element shape is known. It is the declared default state of a
  // `Var<RuntimeUnpackedArray>` cell; the cell's first initialization
  // overwrites it with the real element shape.
  RuntimeUnpackedArray();

  // LRM 10.9.1: `count` replications of `unit`, where a replication stands for
  // an entire dimension. Covers both a fixed array's all-default state (`unit`
  // is one element default, LRM Table 7-1) and an `'{count{...}}` pattern
  // (`unit` is the replicated items), which are the same repeat-and-count shape
  // and so construct through one path, keeping a uniform array O(unit) to build
  // where an enumerated element list would be O(unit * count).
  RuntimeUnpackedArray(
      RuntimeValue element_default, std::vector<RuntimeValue> unit,
      std::size_t count);

  // LRM 5.9 / 21.3.3: a string value assigned to an unpacked array of bytes is
  // left-justified -- the first character lands at the array's left bound and
  // runs toward the right bound, an element past the end of the text keeps the
  // element type's default, and text beyond the array's last element is
  // dropped. The clause admits this form for an array of bytes alone, so the
  // element shape is a packed type rather than a type of any domain, and
  // `count` is the destination's element count.
  [[nodiscard]] static auto FromString(
      const String& text, const PackedType& element_type,
      const PackedArray& count) -> RuntimeUnpackedArray;

  // LRM 5.9: a string literal assigned to an unpacked array of bytes, under the
  // same left justification. A literal is a packed bit-vector constant, not a
  // string value, so its bytes arrive whole -- a NUL among them is a byte like
  // any other, where building a string value would have removed it (LRM 6.16).
  [[nodiscard]] static auto FromPackedArray(
      const PackedArray& bits, const PackedType& element_type,
      const PackedArray& count) -> RuntimeUnpackedArray;

  RuntimeUnpackedArray(const RuntimeUnpackedArray&);
  RuntimeUnpackedArray(RuntimeUnpackedArray&&) noexcept;
  auto operator=(const RuntimeUnpackedArray&) -> RuntimeUnpackedArray&;
  auto operator=(RuntimeUnpackedArray&&) noexcept -> RuntimeUnpackedArray&;
  ~RuntimeUnpackedArray();

  // LRM 21.3.4.3: the array read as a contiguous character sequence in element
  // order, the low byte of each element becoming one character and embedded
  // NULs included -- what a scan takes as its input text. The inverse of the
  // string construction above, under the same clause's byte order.
  [[nodiscard]] auto ToByteString() const -> String;

  // LRM 7.4.2: the element count as an SV `int`. Fixed for the value's life.
  [[nodiscard]] auto Size() const -> PackedArray;

  // The element-default prototype. Its runtime domain is the array's element
  // domain, so a caller boxing an incoming element value into the erased
  // representation reads the target domain from here.
  [[nodiscard]] auto ElementDefault() const -> const RuntimeValue&;

  // LRM 7.4.5: reads the element the source index `sv_index` names, resolved
  // against the declared range `[left:right]` the receiver's static type
  // supplies. An index the range does not name, or an x / z one, reads the
  // element default. The caller copies the result out across the opaque-handle
  // boundary rather than aliasing it.
  [[nodiscard]] auto Element(
      const PackedArray& sv_index, const PackedArray& left,
      const PackedArray& right) const -> const RuntimeValue&;

  // A functional element write: yields a new array equal to this one with the
  // named element replaced by `value`. LRM 7.4.5: an index the declared range
  // does not name, or an x / z one, leaves the array unchanged.
  [[nodiscard]] auto WithElement(
      const PackedArray& sv_index, const PackedArray& left,
      const PackedArray& right, RuntimeValue value) const
      -> RuntimeUnpackedArray;

  // LRM 7.4.5 contiguous-range selector. The raw selector `(a, b, form)` is
  // resolved to the storage-ordinal window against the receiver's declared
  // `[left:right]` range; a partial-out-of-range position yields the element
  // default and an x / z base yields a wholly-default sub-array. The result is
  // ordinal-only payload, so it carries no declared range of its own.
  [[nodiscard]] auto Slice(
      const PackedArray& a, const PackedArray& b, const PackedArray& form,
      const PackedArray& left, const PackedArray& right) const
      -> RuntimeUnpackedArray;

  // LRM 11.4.5 `==` / `!=` (Any data type): an element-wise reduction that
  // propagates X / Z through each element's own equality.
  [[nodiscard]] auto operator==(const RuntimeUnpackedArray& other) const
      -> PackedArray;
  [[nodiscard]] auto operator!=(const RuntimeUnpackedArray& other) const
      -> PackedArray;

  // LRM 11.4.5 `===` / `!==`: element-wise case equality, deterministic in
  // X / Z.
  [[nodiscard]] auto CaseEqual(const RuntimeUnpackedArray& other) const
      -> PackedArray;

  // LRM 11.4.11: the two arms of a conditional operator whose condition is
  // ambiguous, combined element by element -- an element the arms agree on
  // survives, and one they disagree on, or cannot know, takes the element
  // default (Table 7-1). Arms of unequal size put no elements in
  // correspondence, so every element takes that default.
  [[nodiscard]] auto MergeConditional(const RuntimeUnpackedArray& other) const
      -> RuntimeUnpackedArray;

  // LRM 9.4.2 update-event predicate (engine change-detection hook).
  [[nodiscard]] auto IsBitIdentical(const RuntimeUnpackedArray& other) const
      -> bool;

  // LRM 20.9: any element carrying an unknown bit propagates up.
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> PackedArray;

  // LRM 20.6.2 `$bits`: the sum of the elements' own widths, an aggregate's
  // bit stream being its elements' laid end to end.
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray;

  // LRM 20.9 `$countbits`: the element-wise sum of each element's own count,
  // an unpacked array being a bit stream of its elements.
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray;

 private:
  // Indirect because `RuntimeValue` closes over this type: a by-value member
  // would need `RuntimeValue` complete here, which it is not.
  std::unique_ptr<RuntimeValue> element_default_;
  std::vector<RuntimeValue> data_;
};

static_assert(LyraValue<RuntimeUnpackedArray>);
static_assert(CaseEqualComparable<RuntimeUnpackedArray>);
static_assert(ConditionallyMergeable<RuntimeUnpackedArray>);
static_assert(Sized<RuntimeUnpackedArray>);
static_assert(BitstreamSizable<RuntimeUnpackedArray>);
static_assert(RangedSliceable<RuntimeUnpackedArray>);

}  // namespace lyra::value

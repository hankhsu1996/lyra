#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/value/basic_dynamic_array.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

class String;

// A fixed-size unpacked array (LRM 7.4.2) as the library holds one: the
// dynamic array's run of elements, compiled once with its element type's
// table, whose count the array's type fixes rather than the running program.
// Every element is handed in and out by its address, which is where it lies in
// the array.
//
// The payload is ordinal-only: an access names an element by its ordinal,
// counted from the left, because the declared range is a fact of the static
// type the select was written against and is read there. Whole-value movement
// is therefore range-agnostic and no store relabels a coordinate.
class RuntimeUnpackedArray {
 public:
  // The array before its declared element type is known: the declared default
  // state of a cell, which the cell's first initialization overwrites.
  RuntimeUnpackedArray();

  // An array of elements of `element` holding copies of `items`, in order.
  RuntimeUnpackedArray(
      const ValueType& element, const void* element_default,
      std::span<const void* const> items);

  RuntimeUnpackedArray(const RuntimeUnpackedArray&);
  RuntimeUnpackedArray(RuntimeUnpackedArray&&) noexcept;
  auto operator=(const RuntimeUnpackedArray&) -> RuntimeUnpackedArray&;
  auto operator=(RuntimeUnpackedArray&&) noexcept -> RuntimeUnpackedArray&;
  ~RuntimeUnpackedArray();

  // LRM 7.6: a fixed-size unpacked array assigned an array of another unpacked
  // kind takes its elements in left-to-right order, and how many elements it
  // has is a declared property of the variable being written rather than
  // anything the source decides -- so a source of another size is a run-time
  // error and the assignment does not happen.
  [[nodiscard]] static auto FromElements(
      const ValueType& element, const void* element_default,
      std::span<const void* const> items, std::int64_t declared)
      -> RuntimeUnpackedArray;

  // LRM 5.9 / 21.3.3: a string value assigned to an unpacked array of bytes is
  // left-justified -- the first character lands at the array's left bound and
  // runs toward the right bound, an element past the end of the text keeps the
  // element type's default, and text beyond the array's last element is
  // dropped. The clause admits this form for an array of bytes alone, so the
  // element is an integral value of type `element`, and `count` is the
  // destination's element count.
  [[nodiscard]] static auto FromString(
      const String& text, const IntegralValueType& element, std::int64_t count)
      -> RuntimeUnpackedArray;

  // LRM 5.9: a string literal assigned to an unpacked array of bytes, under the
  // same left justification. A literal is a packed bit-vector constant, not a
  // string value, so its bytes arrive whole -- a NUL among them is a byte like
  // any other, where building a string value would have removed it (LRM 6.16).
  [[nodiscard]] static auto FromIntegral(
      const ConstIntegralView& bits, const IntegralValueType& element,
      std::int64_t count) -> RuntimeUnpackedArray;

  // LRM 21.3.4.3: the array read as a contiguous character sequence in element
  // order, the low byte of each element becoming one character and embedded
  // NULs included -- what a scan takes as its input text. The inverse of the
  // string construction above, under the same clause's byte order.
  [[nodiscard]] auto ToByteString() const -> String;

  [[nodiscard]] auto ElementType() const -> const ValueType&;
  [[nodiscard]] auto ElementDefault() const -> const void*;

  // LRM 7.4.2: the element count, and as an SV `int`. Fixed for the value's
  // life.
  [[nodiscard]] auto Count() const -> std::size_t;
  [[nodiscard]] auto Size() const -> Int;

  // The element at storage position `position`, counted from the left -- the
  // coordinate LRM 7.12 walks a container by.
  [[nodiscard]] auto ElementAt(std::size_t position) const -> const void*;
  [[nodiscard]] auto ElementAt(std::size_t position) -> void*;

  // LRM 7.4.5: the element `position` names, the element default where it
  // names none; and the element as storage a write lands in, where no read
  // reaches where it names none.
  [[nodiscard]] auto Element(std::optional<std::int64_t> position) const
      -> const void*;
  [[nodiscard]] auto ElementRef(
      std::optional<std::int64_t> position, Formation& formed) -> void*;

  // LRM 7.4.5 / 7.4.6: the `count` elements from `start`, each the element
  // default where it lies outside the array, every one of them where `start`
  // names no position.
  [[nodiscard]] auto SliceElements(
      std::optional<std::int64_t> start, std::int64_t count) const
      -> std::vector<const void*>;
  [[nodiscard]] auto Slice(
      std::optional<std::int64_t> start, std::int64_t count) const
      -> RuntimeUnpackedArray;

  // LRM 7.6: the window takes `replacement`, element for element; an element
  // outside the array is skipped and a start naming no position writes
  // nothing. Answers whether any element took a different value (LRM 4.3).
  auto AssignSlice(
      std::optional<std::int64_t> start, std::int64_t count,
      std::span<const void* const> replacement) -> bool;

  // LRM 7.12.2: puts the value that was at `order[k]` at position `k`.
  void Permute(std::span<const std::size_t> order);

  [[nodiscard]] auto operator==(const RuntimeUnpackedArray& other) const
      -> FourStateBit;
  [[nodiscard]] auto operator!=(const RuntimeUnpackedArray& other) const
      -> FourStateBit;
  [[nodiscard]] auto CaseEqual(const RuntimeUnpackedArray& other) const -> Bit;

  // LRM 11.4.11: the two arms of a conditional operator whose condition is
  // ambiguous, combined element by element -- an element the arms agree on
  // survives, and one they disagree on, or cannot know, takes the element
  // default (Table 7-1). Arms of unequal size put no elements in
  // correspondence, so every element takes that default.
  [[nodiscard]] auto MergeConditional(const RuntimeUnpackedArray& other) const
      -> RuntimeUnpackedArray;

  // Net resolution applied element by element under each of the three truth
  // tables (LRM 6.6). LRM 6.7.1 admits an unpacked array as a net's data type
  // when its element type is itself valid for a net, and it composes a net out
  // of its elements' bits, so folding two contributions is folding each
  // element pair.
  [[nodiscard]] auto ResolveTriState(const RuntimeUnpackedArray& other) const
      -> RuntimeUnpackedArray;
  [[nodiscard]] auto ResolveWiredAnd(const RuntimeUnpackedArray& other) const
      -> RuntimeUnpackedArray;
  [[nodiscard]] auto ResolveWiredOr(const RuntimeUnpackedArray& other) const
      -> RuntimeUnpackedArray;

  // What a stronger contribution leaves a weaker one, element by element (LRM
  // 28.12.1).
  [[nodiscard]] auto Dominating(const RuntimeUnpackedArray& weaker) const
      -> RuntimeUnpackedArray;

  // `prototype`'s shape with every bit set to `fill`: each element filled the
  // same way (LRM 6.7.1). Only the prototype's shape is read.
  [[nodiscard]] static auto FilledLike(
      const RuntimeUnpackedArray& prototype, const Logic& fill)
      -> RuntimeUnpackedArray;

  // LRM 9.4.2: a size mismatch is a change, which is how the array a fresh
  // cell holds before its declaration is told apart from the first write.
  [[nodiscard]] auto IsBitIdentical(const RuntimeUnpackedArray& other) const
      -> bool;
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> Bit;
  [[nodiscard]] auto BitstreamWidth() const -> Int;
  [[nodiscard]] auto CountBits(const ConstIntegralView& control_bits) const
      -> Int;

  // LRM 6.24.3: the elements' own streams laid end to end into a stream below
  // its `filled` most significant positions, index 0 most significant,
  // answering how many are filled after them; and the inverse, which reads an
  // array of this one's count and element shapes and answers how many
  // positions it took.
  auto WriteToStream(
      Planes stream, std::uint64_t stream_width, std::uint64_t filled) const
      -> std::uint64_t;
  [[nodiscard]] auto ReadFromStream(
      ConstPlanes stream, std::uint64_t stream_width, std::uint64_t taken) const
      -> std::pair<RuntimeUnpackedArray, std::uint64_t>;

 private:
  using Core = BasicDynamicArray<WitnessedElem>;

  explicit RuntimeUnpackedArray(Core core);

  void RequireInstalled() const;
  [[nodiscard]] auto Installed() const -> const Core&;
  [[nodiscard]] auto Installed() -> Core&;

  std::optional<Core> core_;
};

static_assert(LyraValue<RuntimeUnpackedArray>);
static_assert(NetResolvable<RuntimeUnpackedArray>);
static_assert(CaseEqualComparable<RuntimeUnpackedArray>);
static_assert(ConditionallyMergeable<RuntimeUnpackedArray>);
static_assert(Sized<RuntimeUnpackedArray>);
static_assert(BitstreamSizable<RuntimeUnpackedArray, ConstIntegralView>);

}  // namespace lyra::value

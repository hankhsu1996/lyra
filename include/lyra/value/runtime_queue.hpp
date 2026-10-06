#pragma once

#include <cstddef>
#include <optional>
#include <span>

#include "lyra/value/basic_queue.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// A queue (LRM 7.10) as the library holds one: the queue every element type
// shares, compiled once with its element type's table, so a value of a type
// the library was compiled without is held as its own bytes. Every element is
// handed in and out by its address, which is where it lies in the queue.
class RuntimeQueue {
 public:
  // The empty queue before its declared element type is known: the declared
  // default state of a cell, which the cell's first initialization overwrites.
  RuntimeQueue();

  // An empty queue of `element`, whose elements start as `element_default`
  // (LRM Table 7-1) and which holds no element whose index exceeds `bound`, a
  // negative bound being none (LRM 7.10.5).
  RuntimeQueue(
      const ValueType& element, const void* element_default,
      const PackedArray& bound);

  RuntimeQueue(const RuntimeQueue&);
  RuntimeQueue(RuntimeQueue&&) noexcept;
  auto operator=(const RuntimeQueue&) -> RuntimeQueue&;
  auto operator=(RuntimeQueue&&) noexcept -> RuntimeQueue&;
  ~RuntimeQueue();

  [[nodiscard]] auto ElementType() const -> const ValueType&;
  [[nodiscard]] auto ElementDefault() const -> const void*;

  // LRM 7.10.5: the bound belongs to the variable rather than to the value
  // written, so a semantic store brings its right-hand side to the
  // destination's bound and trims what no longer fits.
  [[nodiscard]] auto ConformBound(const PackedArray& bound) const
      -> RuntimeQueue;

  // LRM 7.10.2.1: the current element count, and as an SV `int`.
  [[nodiscard]] auto Count() const -> std::size_t;
  [[nodiscard]] auto Size() const -> PackedArray;

  // The element at storage position `position`, counted from the first -- the
  // coordinate LRM 7.12 walks a container by.
  [[nodiscard]] auto ElementAt(std::size_t position) const -> const void*;
  [[nodiscard]] auto ElementAt(std::size_t position) -> void*;

  // LRM 7.10.1 / 7.4.5: the element `position` names, the element default
  // where it names none; a read never grows the queue.
  [[nodiscard]] auto Element(const PackedArray& position) const -> const void*;

  // LRM 7.10.1: the element `position` names, as storage a write lands in. The
  // position one past the last appends an element there first, trimmed to the
  // bound, and every other position naming none lands where no read reaches.
  // `formed` says which of the three it was.
  [[nodiscard]] auto ElementRef(const PackedArray& position, Formation& formed)
      -> void*;

  // LRM 7.10.1 slice, carrying no bound of its own.
  [[nodiscard]] auto Slice(const PackedArray& lo, const PackedArray& hi) const
      -> RuntimeQueue;

  // LRM 7.10.2.2 / 7.10.2.6 / 7.10.2.7: a copy of `item` added, the queue then
  // held to its bound.
  void PushFront(const void* item);
  void PushBack(const void* item);
  void Insert(const PackedArray& index, const void* item);

  // LRM 10.10: a copy of this queue with copies of `items` appended in order,
  // held to the bound.
  [[nodiscard]] auto Concat(std::span<const void* const> items) const
      -> RuntimeQueue;

  // LRM 7.6: copies of `items`, in order, as a queue of `element` held to
  // `bound`, or as one with no bound.
  [[nodiscard]] static auto FromElements(
      const ValueType& element, const void* element_default,
      const PackedArray& bound, std::span<const void* const> items)
      -> RuntimeQueue;
  [[nodiscard]] static auto FromElements(
      const ValueType& element, const void* element_default,
      std::span<const void* const> items) -> RuntimeQueue;

  // LRM 7.10.2.4 / 7.10.2.5: the first or last element moved into `out` and
  // removed; the element default where there is none.
  void PopFront(void* out);
  void PopBack(void* out);

  // LRM 7.10.2.3.
  void Delete();
  void DeleteIndex(const PackedArray& index);

  // LRM 7.12.2: puts the element that was at `order[k]` at position `k`.
  void Permute(std::span<const std::size_t> order);

  [[nodiscard]] auto operator==(const RuntimeQueue& other) const -> PackedArray;
  [[nodiscard]] auto operator!=(const RuntimeQueue& other) const -> PackedArray;
  [[nodiscard]] auto CaseEqual(const RuntimeQueue& other) const -> PackedArray;
  [[nodiscard]] auto IsBitIdentical(const RuntimeQueue& other) const -> bool;
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> PackedArray;
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray;
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray;

 private:
  explicit RuntimeQueue(BasicQueue<WitnessedElem> core);

  void RequireInstalled() const;
  [[nodiscard]] auto Core() const -> const BasicQueue<WitnessedElem>&;
  [[nodiscard]] auto Core() -> BasicQueue<WitnessedElem>&;

  std::optional<BasicQueue<WitnessedElem>> core_;
};

static_assert(LyraValue<RuntimeQueue>);
static_assert(CaseEqualComparable<RuntimeQueue>);
static_assert(Sized<RuntimeQueue>);
static_assert(BitstreamSizable<RuntimeQueue>);

}  // namespace lyra::value

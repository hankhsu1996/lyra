#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>

#include "lyra/value/basic_queue.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/queue_type.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

// The bound of a queue the library holds, read off the type the queue was
// built at.
class WitnessedBound {
 public:
  explicit WitnessedBound(const QueueType& type) : type_(&type) {
  }

  [[nodiscard]] auto Bound() const -> std::int64_t {
    return type_->Bound();
  }

 private:
  const QueueType* type_;
};

// A queue (LRM 7.10) as the library holds one: the queue every element type
// shares, compiled once with its element type's table, so a value of a type
// the library was compiled without is held as its own bytes. Every element is
// handed in and out by its address, which is where it lies in the queue. A
// position is the number an index names, none where it names no position
// (LRM 7.10.1).
class RuntimeQueue {
 public:
  // The empty queue before its declared type is known: the declared default
  // state of a cell, which the cell's first initialization overwrites.
  RuntimeQueue();

  // An empty queue of `type`.
  explicit RuntimeQueue(const QueueType& type);

  RuntimeQueue(const RuntimeQueue&);
  RuntimeQueue(RuntimeQueue&&) noexcept;
  auto operator=(const RuntimeQueue&) -> RuntimeQueue&;
  auto operator=(RuntimeQueue&&) noexcept -> RuntimeQueue&;
  ~RuntimeQueue();

  // The element type the queue was built with, which an array built out of
  // this one's elements is built with in turn.
  [[nodiscard]] auto ElementType() const -> const value::ElementType&;

  // LRM 7.10.5: this queue's elements as a queue of `type`, which keeps only
  // what its own bound admits.
  [[nodiscard]] auto ConformBound(const QueueType& type) const -> RuntimeQueue;

  // LRM 7.10.2.1: the current element count, and as an SV `int`.
  [[nodiscard]] auto Count() const -> std::size_t;
  [[nodiscard]] auto Size() const -> Int;

  // The element at storage position `position`, counted from the first -- the
  // coordinate LRM 7.12 walks a container by.
  [[nodiscard]] auto ElementAt(std::size_t position) const -> const void*;
  [[nodiscard]] auto ElementAt(std::size_t position) -> void*;

  // LRM 7.10.1 / 7.4.5: the element `position` names, the element default
  // where it names none; a read never grows the queue.
  [[nodiscard]] auto Element(std::optional<std::int64_t> position) const
      -> const void*;

  // LRM 7.10.1: the element `position` names, as storage a write lands in. The
  // position one past the last appends an element there first, trimmed to the
  // bound, and every other position naming none lands where no read reaches.
  // `formed` says which of the three it was.
  [[nodiscard]] auto ElementMut(
      std::optional<std::int64_t> position, Formation& formed) -> void*;

  // LRM 7.10.1 slice, as a queue of `type`, the type the selection answers.
  [[nodiscard]] auto Slice(
      std::optional<std::int64_t> lo, std::optional<std::int64_t> hi,
      const QueueType& type) const -> RuntimeQueue;

  // LRM 7.10.2.2 / 7.10.2.6 / 7.10.2.7: a copy of `item` added, the queue then
  // held to its bound.
  void PushFront(const void* item);
  void PushBack(const void* item);
  void Insert(std::optional<std::int64_t> index, const void* item);

  // LRM 10.10: a copy of this queue with copies of `items` appended in order,
  // held to the bound.
  [[nodiscard]] auto Concat(std::span<const void* const> items) const
      -> RuntimeQueue;

  // LRM 7.6: copies of `items`, in order, as a queue of `type`, held to its
  // bound.
  [[nodiscard]] static auto FromElements(
      const QueueType& type, std::span<const void* const> items)
      -> RuntimeQueue;

  // LRM 7.10.2.4 / 7.10.2.5: the first or last element moved into `out` and
  // removed; the element default where there is none.
  void PopFront(void* out);
  void PopBack(void* out);

  // LRM 7.10.2.3.
  void Delete();
  void DeleteIndex(std::optional<std::int64_t> index);

  // LRM 7.12.2: puts the element that was at `order[k]` at position `k`.
  void Permute(std::span<const std::size_t> order);

  [[nodiscard]] auto operator==(const RuntimeQueue& other) const
      -> FourStateBit;
  [[nodiscard]] auto operator!=(const RuntimeQueue& other) const
      -> FourStateBit;
  [[nodiscard]] auto CaseEqual(const RuntimeQueue& other) const -> Bit;
  // LRM 11.4.11: this queue and `other` as the two arms of a conditional
  // operator whose condition is ambiguous, combined element by element.
  [[nodiscard]] auto MergeConditional(const RuntimeQueue& other) const
      -> RuntimeQueue;
  [[nodiscard]] auto IsBitIdentical(const RuntimeQueue& other) const -> bool;
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> Bit;
  [[nodiscard]] auto BitstreamWidth() const -> Int;
  [[nodiscard]] auto CountBits(const ConstIntegralView& control_bits) const
      -> Int;

 private:
  using Held = BasicQueue<WitnessedElem, WitnessedBound>;

  explicit RuntimeQueue(Held core);

  void RequireInstalled() const;
  [[nodiscard]] auto Core() const -> const Held&;
  [[nodiscard]] auto Core() -> Held&;

  std::optional<Held> core_;
};

static_assert(LyraValue<RuntimeQueue>);
static_assert(CaseEqualComparable<RuntimeQueue>);
static_assert(ConditionallyMergeable<RuntimeQueue>);
static_assert(Sized<RuntimeQueue>);
static_assert(BitstreamSizable<RuntimeQueue, ConstIntegralView>);

}  // namespace lyra::value

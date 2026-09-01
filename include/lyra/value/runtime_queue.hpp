#pragma once

#include <cstdint>
#include <deque>
#include <memory>
#include <optional>
#include <vector>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

struct RuntimeValue;

// The runtime-owned realization of a SystemVerilog queue (LRM 7.10), MIR's
// `QueueType`. A variable-size ordered collection with efficient insertion and
// removal at both ends, so the storage is a deque rather than the vector a
// dynamic array uses. It owns its elements by value: copy is an element-wise
// deep copy, destruction is C++ RAII, so an element never borrows caller
// storage.
//
// This is the execution backend's type-erased counterpart of the C++ backend's
// monomorphized `Queue<T>`. A compile-once runtime cannot instantiate a
// distinct C++ type per element type, so one `RuntimeQueue` holds a deque of
// type-erased `RuntimeValue` elements and an element-default prototype, and
// composes the value contract by visiting them.
//
// Value semantics are preserved by immutability: every apparent mutation is a
// functional operation returning a new queue, never an in-place write, so a
// queue whose handle is shared by a copy is never disturbed by a write through
// another copy.
class RuntimeQueue {
 public:
  // The uninitialized sentinel form -- the empty queue before its declared
  // element shape is known. It is the declared default state of a
  // `Var<RuntimeQueue>` cell; the cell's first initialization overwrites it
  // with the real element default.
  RuntimeQueue();

  // LRM Table 6-7: the default queue is empty. `element_default` is the shape
  // source for out-of-range reads (LRM 7.4.5) and for the slot an append
  // creates, so it carries the exact element representation.
  explicit RuntimeQueue(RuntimeValue element_default);

  // LRM 7.10.5 `int q[$:N]`: the same empty start, holding no element whose
  // index exceeds `max_bound`. A negative bound is no bound at all, which is
  // how a queue with none states it wherever a bound is spelled.
  RuntimeQueue(RuntimeValue element_default, const PackedArray& max_bound);

  // LRM 10.9.1 assignment-pattern construction: the element list, with the
  // element default seeded for later out-of-range reads. Elements past a bound
  // are discarded on entry (LRM 7.10.5).
  RuntimeQueue(
      RuntimeValue element_default, std::vector<RuntimeValue> elements);
  RuntimeQueue(
      RuntimeValue element_default, std::vector<RuntimeValue> elements,
      const PackedArray& max_bound);

  RuntimeQueue(const RuntimeQueue&);
  RuntimeQueue(RuntimeQueue&&) noexcept;
  auto operator=(const RuntimeQueue&) -> RuntimeQueue&;
  auto operator=(RuntimeQueue&&) noexcept -> RuntimeQueue&;
  ~RuntimeQueue();

  // LRM 7.10.5: the bound belongs to the variable rather than to the value
  // written, so a semantic store brings its right-hand side to the
  // destination's bound and trims what no longer fits.
  [[nodiscard]] auto ConformBound(const PackedArray& max_bound) const
      -> RuntimeQueue;

  // LRM 7.10.2.1: the current element count as an SV `int`.
  [[nodiscard]] auto Size() const -> PackedArray;

  // The element-default prototype. Its runtime domain is the queue's element
  // domain, so a caller boxing an incoming element value into the erased
  // representation reads the target domain from here.
  [[nodiscard]] auto ElementDefault() const -> const RuntimeValue&;

  // LRM 7.10.1 / 7.4.5: reads element `index` by reference. An index outside
  // `0..size-1`, or one carrying x or z, reads the element default; a read
  // never grows the queue. The caller copies the result out across the
  // opaque-handle boundary rather than aliasing it.
  [[nodiscard]] auto Element(const PackedArray& index) const
      -> const RuntimeValue&;

  // A functional element write: yields a new queue equal to this one with
  // element `index` replaced. LRM 7.10.1 makes `index == size` an append of one
  // element, and every other invalid index -- negative, past the append
  // position, or carrying x or z -- discards the write.
  [[nodiscard]] auto WithElement(
      const PackedArray& index, RuntimeValue value) const -> RuntimeQueue;

  // LRM 7.10.1 slice. `form` selects the source shape from `(anchor, extent)`:
  // a constant `q[a:b]` is `anchor = a`, `extent = b`; an indexed `q[base+:w]`
  // grows upward from `base`, and `q[base-:w]` downward. The bounds resolve
  // here in the wide x/z-aware domain, never as narrow selector arithmetic; an
  // x or z bound, or an empty window after clamping, yields the empty queue.
  // The result carries no bound of its own: a bound belongs to the variable a
  // value is stored into, and a store is where one is applied.
  [[nodiscard]] auto Slice(
      const PackedArray& anchor, const PackedArray& extent,
      const PackedArray& form) const -> RuntimeQueue;

  // LRM 7.10.2.6 / 7.10.2.7: a copy with one element added at the front or the
  // back, trimmed to the bound.
  [[nodiscard]] auto PushFront(RuntimeValue item) const -> RuntimeQueue;
  [[nodiscard]] auto PushBack(RuntimeValue item) const -> RuntimeQueue;

  // LRM 7.10.2.4 / 7.10.2.5 pop, as its two halves: the element at the front
  // or the back, and the queue left once it is gone. An empty queue has none
  // to remove, so it reads the element default and stays as it is.
  [[nodiscard]] auto Front() const -> const RuntimeValue&;
  [[nodiscard]] auto Back() const -> const RuntimeValue&;
  [[nodiscard]] auto PopFront() const -> RuntimeQueue;
  [[nodiscard]] auto PopBack() const -> RuntimeQueue;

  // LRM 7.10.2.2: a copy with `item` inserted before `index`, where
  // `index == size` appends. An x or z, negative, or beyond-size index leaves
  // the queue unchanged.
  [[nodiscard]] auto Insert(const PackedArray& index, RuntimeValue item) const
      -> RuntimeQueue;

  // LRM 7.10.2.3: a copy emptied, or a copy with the element at `index`
  // removed. An invalid index leaves the queue unchanged.
  [[nodiscard]] auto Delete() const -> RuntimeQueue;
  [[nodiscard]] auto Delete(const PackedArray& index) const -> RuntimeQueue;

  // LRM 11.4.5 `==` / `!=` (Any data type): a size check then an element-wise
  // reduction that propagates X / Z through each element's own equality.
  [[nodiscard]] auto operator==(const RuntimeQueue& other) const -> PackedArray;
  [[nodiscard]] auto operator!=(const RuntimeQueue& other) const -> PackedArray;

  // LRM 11.4.5 `===` / `!==`: element-wise case equality, deterministic in
  // X / Z.
  [[nodiscard]] auto CaseEqual(const RuntimeQueue& other) const -> PackedArray;

  // LRM 9.4.2 update-event predicate (engine change-detection hook).
  [[nodiscard]] auto IsBitIdentical(const RuntimeQueue& other) const -> bool;

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
  // LRM 7.10.5: drops every element whose index exceeds the declared bound.
  void EnforceBound();

  // A negative, out-of-range, or x / z index (LRM 7.10.1). A valid index is
  // this queue's own ordinal, since a queue is declared zero-based.
  [[nodiscard]] auto IsInvalidIndex(const PackedArray& index) const -> bool;

  // Indirect because `RuntimeValue` closes over this type: a by-value member
  // would need `RuntimeValue` complete here, which it is not.
  std::unique_ptr<RuntimeValue> element_default_;
  std::deque<RuntimeValue> data_;
  std::optional<std::uint64_t> max_bound_;
};

static_assert(LyraValue<RuntimeQueue>);
static_assert(CaseEqualComparable<RuntimeQueue>);
static_assert(Sized<RuntimeQueue>);
static_assert(BitstreamSizable<RuntimeQueue>);

}  // namespace lyra::value

#pragma once

#include <cstddef>
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
// Each element is storage of its own, and a method changing the queue changes
// it where it lies. Value semantics hold because a copy of the queue copies its
// elements: no two queues share one, so a write through one is never seen
// through another.
class RuntimeQueue {
 public:
  // The uninitialized sentinel form -- the empty queue before its declared
  // element shape is known. It is the declared default state of a
  // `Var<RuntimeQueue>` cell; the cell's first initialization overwrites it
  // with the real element default.
  RuntimeQueue();

  // An empty queue of a known element shape. `element_default` is the shape
  // source for out-of-range reads (LRM 7.4.5) and for the slot an append
  // creates, so it carries the exact element representation.
  explicit RuntimeQueue(RuntimeValue element_default);

  // The same, holding no element whose index exceeds `max_bound`
  // (LRM 7.10.5).
  RuntimeQueue(RuntimeValue element_default, const PackedArray& max_bound);

  // LRM 10.9.1 assignment-pattern construction: the element list, with the
  // element default seeded for later out-of-range reads. The bounded form
  // discards on entry every element past its bound (LRM 7.10.5); a negative
  // bound is no bound at all, which is how a queue with none states it wherever
  // a bound is spelled.
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

  // LRM 7.10.1 / 7.4.5: reads the element `position` names by reference. A
  // position that names no element here reads the element default; a read
  // never grows the queue.
  [[nodiscard]] auto Element(const PackedArray& position) const
      -> const RuntimeValue&;

  // LRM 7.10.1: the element `position` names, as storage a write lands in. The
  // position one past the last appends an element there first, trimmed to the
  // bound, and every other position naming none -- negative, past the append
  // position, or unknown -- yields storage nothing reads, so a write there is
  // discarded.
  [[nodiscard]] auto ElementRef(const PackedArray& position) -> RuntimeValue&;

  // The element at storage position `position`, counted from the first in the
  // queue's own order -- the coordinate LRM 7.12 walks a container by. A
  // position past the last is a walk defect rather than an out-of-range read.
  [[nodiscard]] auto ElementAt(std::size_t position) const
      -> const RuntimeValue&;

  // LRM 7.10.1 slice: the elements from position `lo` through `hi`. A bound
  // that names no position, or an empty window after clamping, yields the
  // empty queue. The result carries no bound of its own: a bound belongs to
  // the variable a value is stored into, and a store is where one is applied.
  [[nodiscard]] auto Slice(const PackedArray& lo, const PackedArray& hi) const
      -> RuntimeQueue;

  // LRM 7.10.2.6 / 7.10.2.7: one element added at the front or the back, the
  // queue then trimmed to its bound.
  void PushFront(RuntimeValue item);
  void PushBack(RuntimeValue item);

  // LRM 10.10: a copy of this queue with every element of a spread part
  // appended in order, trimmed to the bound. The part crosses erased as any
  // element container, so its own domain is read off the value rather than
  // named here. Only a spread part reaches this; a scalar part appends as one
  // element.
  [[nodiscard]] auto ConcatSpread(const RuntimeValue& part) const
      -> RuntimeQueue;

  // LRM 7.6: a queue assigned an array of any of the three unpacked kinds is
  // resized to the source's element count and takes its elements in
  // left-to-right order. The element default and the LRM 7.10.5 bound are the
  // destination's own declared properties; a bound below zero is the unbounded
  // queue, so one form covers both and the contents are trimmed to it.
  [[nodiscard]] static auto FromArray(
      const RuntimeValue& source, RuntimeValue element_default,
      const PackedArray& max_bound) -> RuntimeQueue;

  // LRM 7.10.2.4 / 7.10.2.5: removes the element at the front or the back and
  // answers with it. An empty queue has none to remove, so it answers with the
  // element default and stays as it is.
  auto PopFront() -> RuntimeValue;
  auto PopBack() -> RuntimeValue;

  // LRM 7.10.2.2: inserts `item` before `index`, where `index == size`
  // appends. An x or z, negative, or beyond-size index leaves the queue
  // unchanged.
  void Insert(const PackedArray& index, RuntimeValue item);

  // LRM 7.10.2.3: empties the queue, or removes the element at `index`. An
  // invalid index leaves the queue unchanged.
  void Delete();
  void DeleteIndex(const PackedArray& index);

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
// A queue's `Slice(lo, hi)` takes its element count from two bounds the
// running program can move (LRM 7.10.1), not the fixed count `Sliceable` names,
// so despite the matching arity it carries its own `Slice` rather than claiming
// that concept.
static_assert(EntryWalkable<RuntimeQueue>);

}  // namespace lyra::value

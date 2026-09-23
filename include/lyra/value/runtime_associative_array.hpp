#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <vector>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

struct RuntimeValue;

// One index and the element stored under it, defined in `runtime_value.hpp`
// where `RuntimeValue` is complete -- it closes over this container, so neither
// can be a by-value member of the other here.
struct RuntimeAssociativeEntry;

// LRM 7.8: the index type is what imposes the order the entries are held in.
// For every declared index type that order is the one the index values already
// carry, so an erased index answers it itself. A wildcard index (LRM 7.8.1) is
// the one it cannot: the clause makes an index self-determined and unsigned
// and admits the same numerical value at any width, so what two indices mean
// to each other is fixed by the declaration and is absent from both of them.
enum class AssociativeIndexOrder : std::uint8_t {
  kIndexValueDomain,
  kWildcardNumeric,
};

// The runtime-owned realization of a SystemVerilog associative array (LRM 7.8),
// MIR's `AssociativeArrayType`. A sparse lookup table allocated entry by entry
// and held in index order, so traversal and formatting follow LRM 7.8.2 /
// 7.8.4 and stay deterministic. It owns its indices and elements by value:
// copy is a deep copy, destruction is C++ RAII.
//
// This is the execution backend's type-erased counterpart of the C++ backend's
// monomorphized `AssociativeArray<K, V>`. A compile-once runtime cannot
// instantiate a distinct C++ type per index and element type, so one
// `RuntimeAssociativeArray` holds type-erased entries and composes the value
// contract by visiting them. Unlike an ordinally indexed container it carries
// no index prototype: an index reaches every operation as a value of its own.
// What it does carry is the order its index type imposes, which the
// monomorphized counterpart reads off its key type parameter.
//
// Value semantics are preserved by immutability: every apparent mutation is a
// functional operation returning a new array, never an in-place write, so an
// array whose handle is shared by a copy is never disturbed by a write through
// another copy.
class RuntimeAssociativeArray {
 public:
  // The uninitialized sentinel form -- the empty array before its declared
  // element shape is known. It is the declared default state of a
  // `Var<RuntimeAssociativeArray>` cell; the cell's first initialization
  // overwrites it with the real element default, and with the order that
  // initializer's own index type imposes.
  RuntimeAssociativeArray();

  // `index_order` is what the declared index type makes of two indices (LRM
  // 7.8); `element_default` carries the element shape a caller boxes an
  // incoming value against; `user_default` is what a read of an index with no
  // entry answers with (LRM 7.8.6) and the value an entry a later write
  // allocates starts from (LRM 7.8.7), which a `default:` clause names (LRM
  // 7.9.11) and which is otherwise the element type's own default.
  RuntimeAssociativeArray(
      AssociativeIndexOrder index_order, RuntimeValue element_default,
      RuntimeValue user_default);

  RuntimeAssociativeArray(const RuntimeAssociativeArray&);
  RuntimeAssociativeArray(RuntimeAssociativeArray&&) noexcept;
  auto operator=(const RuntimeAssociativeArray&) -> RuntimeAssociativeArray&;
  auto operator=(RuntimeAssociativeArray&&) noexcept
      -> RuntimeAssociativeArray&;
  ~RuntimeAssociativeArray();

  // LRM 7.9.2 `num` / `size`: how many entries the array holds, as an SV `int`.
  [[nodiscard]] auto Size() const -> PackedArray;

  // The element-default prototype. Its runtime domain is the array's element
  // domain, so a caller boxing an incoming element value into the erased
  // representation reads the target domain from here. An index has no such
  // prototype, which is why one crosses already erased.
  [[nodiscard]] auto ElementDefault() const -> const RuntimeValue&;

  // What a read of an index with no entry answers with (LRM 7.8.6). It is part
  // of the array's value rather than of its shape, so anything rebuilding an
  // array from another carries it over.
  [[nodiscard]] auto AbsentIndexValue() const -> const RuntimeValue&;

  // The order the declared index type imposes. An operation that projects one
  // array into another keyed the same way (LRM 7.12.5) carries it over, the
  // indices being the receiver's own.
  [[nodiscard]] auto IndexOrder() const -> AssociativeIndexOrder;

  // LRM 7.9.1 `exists`: whether the array holds an entry under `index`, as the
  // SV `int` the method answers with. An index carrying x or z names no entry.
  [[nodiscard]] auto Exists(const RuntimeValue& index) const -> PackedArray;

  // LRM 7.8.6: reads the entry under `index` by reference, or the element
  // default when there is none. A read allocates nothing, so an index with no
  // entry leaves the array's size unchanged.
  [[nodiscard]] auto Element(const RuntimeValue& index) const
      -> const RuntimeValue&;

  // The index and the element at storage position `position`, counted from the
  // first in LRM 7.8 index order -- the coordinate LRM 7.12 walks a container
  // by. An entry of a keyed container reports its own index rather than an
  // ordinal, so the two are read as a pair. A position past the last is a walk
  // defect rather than a read of an index the array has no entry for.
  [[nodiscard]] auto IndexAt(std::size_t position) const -> const RuntimeValue&;
  [[nodiscard]] auto ElementAt(std::size_t position) const
      -> const RuntimeValue&;

  // A functional element write: yields a new array with `value` stored under
  // `index`, allocating the entry if there was none (LRM 7.8.7). An index
  // carrying x or z is invalid whatever it names, so the write is discarded.
  [[nodiscard]] auto WithElement(const RuntimeValue& index, RuntimeValue value)
      const -> RuntimeAssociativeArray;

  // The same writes, all of them, as one operation: `entries` applied in order,
  // so a repeated index keeps the last write and an invalid one is discarded
  // exactly as above. Every apparent mutation here yields a new array, so
  // filling one from a set already in hand is linear in that set only while it
  // is one operation. An array literal, a projection into another keyed array
  // (LRM 7.12.5) and a memory load (LRM 21.4) are each built through it.
  [[nodiscard]] auto WithEntries(std::vector<RuntimeAssociativeEntry> entries)
      const -> RuntimeAssociativeArray;

  // LRM 7.9.3 `delete`: a copy emptied, or a copy without the entry under
  // `index`. An index with no entry leaves the array unchanged.
  [[nodiscard]] auto Delete() const -> RuntimeAssociativeArray;
  [[nodiscard]] auto DeleteIndex(const RuntimeValue& index) const
      -> RuntimeAssociativeArray;

  // LRM 7.9.4 -- 7.9.7 traversal: the smallest and largest indices the array
  // holds, and the neighbours of a probe index. Each is absent when no such
  // index exists, which is what the SV method reports as its return value.
  [[nodiscard]] auto FirstIndex() const -> std::optional<RuntimeValue>;
  [[nodiscard]] auto LastIndex() const -> std::optional<RuntimeValue>;
  [[nodiscard]] auto NextIndex(const RuntimeValue& probe) const
      -> std::optional<RuntimeValue>;
  [[nodiscard]] auto PrevIndex(const RuntimeValue& probe) const
      -> std::optional<RuntimeValue>;

  // LRM 20.7 `$low` / `$high` over an associative dimension: the smallest and
  // largest currently allocated index. With none allocated the dimension has no
  // index to report and the query reads `unallocated` -- the index type's
  // default, which is `'x` for a 4-state index type, as LRM 20.7 requires.
  [[nodiscard]] auto MinIndex(const RuntimeValue& unallocated) const
      -> RuntimeValue;
  [[nodiscard]] auto MaxIndex(const RuntimeValue& unallocated) const
      -> RuntimeValue;

  // LRM 11.4.5 `==` / `!=` (Any data type): equal entry sets under equal
  // indices, with each element's own equality propagating X / Z.
  [[nodiscard]] auto operator==(const RuntimeAssociativeArray& other) const
      -> PackedArray;
  [[nodiscard]] auto operator!=(const RuntimeAssociativeArray& other) const
      -> PackedArray;

  // LRM 11.4.5 `===` / `!==`: the same comparison under case equality,
  // deterministic in X / Z.
  [[nodiscard]] auto CaseEqual(const RuntimeAssociativeArray& other) const
      -> PackedArray;

  // LRM 9.4.2 update-event predicate (engine change-detection hook).
  [[nodiscard]] auto IsBitIdentical(const RuntimeAssociativeArray& other) const
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
  // Where the entry `index` names sits, absent when the array holds none and
  // for an index carrying x or z, which names no entry whatever its value.
  // Every operation over one index asks this, so what counts as the same index
  // is decided in one place.
  [[nodiscard]] auto Find(const RuntimeValue& index) const
      -> std::optional<std::size_t>;

  // Where an entry under `index` would go to keep the entries ordered, which is
  // what an insertion needs and a lookup narrows from.
  [[nodiscard]] auto LowerBound(const RuntimeValue& index) const -> std::size_t;

  // The declared index type's order, applied to two indices. Ordering is the
  // whole of what tells one index from another here, so this is also what
  // makes two indices name one entry: neither ordering before the other.
  [[nodiscard]] auto OrderBefore(
      const RuntimeValue& a, const RuntimeValue& b) const -> bool;
  [[nodiscard]] auto SameIndex(
      const RuntimeValue& a, const RuntimeValue& b) const -> bool;

  // Puts the entries in index order and leaves one per index -- the last of
  // each run, which under a stable order is the most recent write to it. What
  // holds the entries in order is otherwise every insertion doing so itself,
  // which a whole set arriving at once cannot afford.
  void Settle();

  // Indirect because `RuntimeValue` closes over this type: a by-value member
  // would need `RuntimeValue` complete here, which it is not. Neither is ever
  // null.
  std::unique_ptr<RuntimeValue> element_default_;
  std::unique_ptr<RuntimeValue> user_default_;
  std::vector<RuntimeAssociativeEntry> data_;
  AssociativeIndexOrder index_order_ = AssociativeIndexOrder::kIndexValueDomain;
};

static_assert(LyraValue<RuntimeAssociativeArray>);
static_assert(CaseEqualComparable<RuntimeAssociativeArray>);
static_assert(Sized<RuntimeAssociativeArray>);
static_assert(BitstreamSizable<RuntimeAssociativeArray>);
static_assert(KeyedEntryWalkable<RuntimeAssociativeArray>);

}  // namespace lyra::value

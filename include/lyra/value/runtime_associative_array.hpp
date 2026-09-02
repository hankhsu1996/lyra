#pragma once

#include <cstddef>
#include <memory>
#include <optional>
#include <vector>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

struct RuntimeValue;

// One index and the element stored under it. Defined where `RuntimeValue` is
// complete, for the same reason the element default below is held indirectly:
// `RuntimeValue` closes over this container, so neither can be a by-value
// member of the other here.
struct RuntimeAssociativeEntry;

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
// no index prototype: an index reaches every operation as a value of its own,
// and the order two indices sit in is read from the indices themselves.
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
  // overwrites it with the real element default.
  RuntimeAssociativeArray();

  // LRM Table 6-7: the default associative array is empty. `element_default`
  // is what a read of an index with no entry yields (LRM 7.8.6) and the seed
  // an entry a write allocates starts from (LRM 7.8.7).
  explicit RuntimeAssociativeArray(RuntimeValue element_default);

  // LRM 7.9.11 `'{..., default: v}`: the persistent fallback a read of an
  // index with no entry answers with, in place of the element type's own
  // default, and the value an entry a later write allocates starts from.
  RuntimeAssociativeArray(
      RuntimeValue element_default, RuntimeValue user_default);

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

  // LRM 7.9.3 `delete`: a copy emptied, or a copy without the entry under
  // `index`. An index with no entry leaves the array unchanged.
  [[nodiscard]] auto Delete() const -> RuntimeAssociativeArray;
  [[nodiscard]] auto Delete(const RuntimeValue& index) const
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

  // Indirect because `RuntimeValue` closes over this type: a by-value member
  // would need `RuntimeValue` complete here, which it is not. The user default
  // is absent unless the array was written with one, and its absence is what
  // sends a miss to the element type's own default instead.
  std::unique_ptr<RuntimeValue> element_default_;
  std::unique_ptr<RuntimeValue> user_default_;
  std::vector<RuntimeAssociativeEntry> data_;
};

static_assert(LyraValue<RuntimeAssociativeArray>);
static_assert(CaseEqualComparable<RuntimeAssociativeArray>);
static_assert(Sized<RuntimeAssociativeArray>);
static_assert(BitstreamSizable<RuntimeAssociativeArray>);
static_assert(KeyedEntryWalkable<RuntimeAssociativeArray>);

}  // namespace lyra::value

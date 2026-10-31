#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/value/any_value.hpp"
#include "lyra/value/basic_associative_array.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/container_lease.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

// An index the program names, where it lies, with its type: what a lookup is
// handed, which reads the index and keeps nothing of it.
struct IndexView {
  const void* bytes;
  const ValueType* type;
};

// LRM 7.8: the index type imposes the order the entries are held in (LRM
// 7.8.2 -- 7.8.4), which indices name no entry, one carrying x or z (LRM
// 7.8.6). The array is built holding it, since nothing but the array asks it.
// A wildcard index (LRM 7.8.1) has an order of its own too: the clause makes
// an index self-determined and unsigned and admits one numerical value at any
// width, so each index is read with the type it was written in.
struct IndexOrder {
  bool (*before)(IndexView lhs, IndexView rhs);
  bool (*names_no_entry)(IndexView index);
};

// The order of each kind of index the language admits, which generated code
// names by symbol where it builds an array indexed by one.
extern "C" {
extern const IndexOrder lyra_rt_integral_index_order;
extern const IndexOrder lyra_rt_string_index_order;
extern const IndexOrder lyra_rt_chandle_index_order;
extern const IndexOrder lyra_rt_managedref_index_order;
extern const IndexOrder lyra_rt_wildcard_index_index_order;
}

// The index of an associative array the library holds: each key is a value of
// its own, held with its type, in the order the declaration imposes. A lookup
// compares the index it is handed where it lies, and only an insertion copies
// one.
struct WitnessedKey {
  using Stored = AnyValue;
  using Probe = IndexView;

  struct Less {
    using is_transparent = void;

    const IndexOrder* order;
    [[nodiscard]] auto operator()(IndexView a, IndexView b) const -> bool {
      return order->before(a, b);
    }
    [[nodiscard]] auto operator()(const AnyValue& a, const AnyValue& b) const
        -> bool {
      return (*this)(ViewOf(a), ViewOf(b));
    }
    [[nodiscard]] auto operator()(const AnyValue& a, IndexView b) const
        -> bool {
      return (*this)(ViewOf(a), b);
    }
    [[nodiscard]] auto operator()(IndexView a, const AnyValue& b) const
        -> bool {
      return (*this)(a, ViewOf(b));
    }
  };

  const IndexOrder* order = nullptr;

  [[nodiscard]] auto Order() const -> Less {
    return Less{.order = order};
  }
  // LRM 7.8.6: an index carrying x or z names no entry.
  [[nodiscard]] auto Invalid(IndexView key) const -> bool {
    return order->names_no_entry(key);
  }
  [[nodiscard]] static auto Owned(IndexView key) -> AnyValue {
    return AnyValue::CopyOf(*key.type, key.bytes);
  }
  [[nodiscard]] static auto ViewOf(const AnyValue& key) -> IndexView {
    return {.bytes = key.Bytes(), .type = &key.Type()};
  }
};

// An associative array (LRM 7.8) as the library holds one: the associative
// array every index and element type shares, compiled once with their types'
// tables, so values of types the library was compiled without are held as
// their own bytes. An element is handed in and out by its address, which is
// where it lies in the array; a key, by the value it is.
class RuntimeAssociativeArray {
 public:
  // The empty array before its declared element type is known: the declared
  // default state of a cell, which the cell's first initialization overwrites.
  RuntimeAssociativeArray();

  // An empty array of `element`, whose absent keys read the element type's
  // default (LRM 7.8.6).
  RuntimeAssociativeArray(
      const value::IndexOrder& index_order, const value::ElementType& element);

  RuntimeAssociativeArray(const RuntimeAssociativeArray&);
  RuntimeAssociativeArray(RuntimeAssociativeArray&&) noexcept;
  auto operator=(const RuntimeAssociativeArray&) -> RuntimeAssociativeArray&;
  auto operator=(RuntimeAssociativeArray&&) noexcept
      -> RuntimeAssociativeArray&;
  ~RuntimeAssociativeArray();

  // The element type the array was built from, which an array built out of
  // this one's elements is built from in turn.
  [[nodiscard]] auto ElementType() const -> const value::ElementType&;
  // LRM 7.9.11: this array's entries in one whose absent keys read a copy of
  // `stated_default`, and whose entries a later write allocates start as one
  // (LRM 7.8.7), as a literal's `default:` clause states.
  [[nodiscard]] auto WithDefault(const void* stated_default) const
      -> RuntimeAssociativeArray;
  // Whether the array was given a value of its own for an absent index, and a
  // copy of the value such an index reads either way, built in `out` (LRM
  // 7.8.6, 7.9.11).
  [[nodiscard]] auto HasStatedDefault() const -> bool;
  void AbsentValue(void* out) const;

  // LRM 7.9.1: the entry count, and as an SV `int`.
  [[nodiscard]] auto Count() const -> std::size_t;
  [[nodiscard]] auto Size() const -> Int;

  // LRM 7.9.3.
  [[nodiscard]] auto Exists(IndexView index) const -> Int;

  // LRM 7.8.6: the element `index` names, or the array's own value for an
  // absent one, without allocating.
  [[nodiscard]] auto Element(IndexView index) const -> const void*;

  // LRM 7.8.7: the element `index` names, as storage a write lands in,
  // allocated where absent; an index naming no entry lands where no read
  // reaches.
  [[nodiscard]] auto ElementMut(IndexView index, Formation& formed) -> void*;

  // A lease a call takes on `element`, one this array holds, which it then
  // goes on reaching however the array changes (LRM 13.5.2).
  [[nodiscard]] auto Lease(void* element) const -> ContainerLease;

  // LRM 7.9.11: the element `index` names set to a copy of `value`.
  void Store(IndexView index, const void* value);

  // LRM 7.9.2.
  void Delete();
  void DeleteIndex(IndexView index);

  // LRM 7.9.4 -- 7.9.7: the least and greatest indices, and the least after
  // `probe` and greatest before it, each none where there is no such index.
  [[nodiscard]] auto FirstIndex() const -> const AnyValue*;
  [[nodiscard]] auto LastIndex() const -> const AnyValue*;
  [[nodiscard]] auto NextIndex(IndexView probe) const -> const AnyValue*;
  [[nodiscard]] auto PrevIndex(IndexView probe) const -> const AnyValue*;

  // Each entry's index and element, in LRM 7.8 index order -- the coordinate
  // LRM 7.12 walks a container by.
  [[nodiscard]] auto Entries() const
      -> std::vector<std::pair<const AnyValue*, const void*>>;

  // The language's operations on the whole array (LRM 11.4.5, 20.9, 20.6.2,
  // 6.24.3), each comparing or streaming an element by `element`, the
  // operations of the element type.
  [[nodiscard]] auto Equal(
      const RuntimeAssociativeArray& other,
      const ValueOperations& element) const -> FourStateBit;
  [[nodiscard]] auto CaseEqual(
      const RuntimeAssociativeArray& other,
      const ValueOperations& element) const -> Bit;
  [[nodiscard]] auto IsBitIdentical(const RuntimeAssociativeArray& other) const
      -> bool;
  [[nodiscard]] auto HasUnknown(const ValueOperations& element) const -> bool;
  [[nodiscard]] auto BitstreamWidth(const ValueOperations& element) const
      -> Int;
  // LRM 6.24.3: the elements written below the `filled` most significant
  // positions of `stream`, the lowest index first, and as a run of bits of
  // their own.
  [[nodiscard]] auto WriteToStream(
      const IntegralView& stream, std::uint64_t filled,
      const ValueOperations& element) const -> std::uint64_t;
  [[nodiscard]] auto ToBitVec(const ValueOperations& element) const -> BitVec;
  [[nodiscard]] auto CountBits(
      const ConstIntegralView& control_bits,
      const ValueOperations& element) const -> Int;

 private:
  using Core = BasicAssociativeArray<WitnessedKey, WitnessedElem>;

  explicit RuntimeAssociativeArray(Core core);

  void RequireInstalled() const;
  [[nodiscard]] auto Installed() const -> const Core&;
  [[nodiscard]] auto Installed() -> Core&;

  std::optional<Core> core_;
};

static_assert(LyraValue<RuntimeAssociativeArray>);
static_assert(Sized<RuntimeAssociativeArray>);

}  // namespace lyra::value

#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/value/any_value.hpp"
#include "lyra/value/basic_associative_array.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/witnessed_elem.hpp"

namespace lyra::value {

// LRM 7.8: the index type is what imposes the order the entries are held in.
// For every declared index type that order is its type's own. A wildcard index
// (LRM 7.8.1) is the one whose type cannot say it: the clause makes an index
// self-determined and unsigned and admits the same numerical value at any
// width, so what two indices mean to each other is fixed by the declaration
// and is absent from both of them.
enum class AssociativeIndexOrder : std::uint8_t {
  kIndexValueDomain,
  kWildcardNumeric,
};

// An index the program names, where it lies, with its type: what a lookup is
// handed, which reads the index and keeps nothing of it.
struct IndexView {
  const void* bytes;
  const ValueType* type;
};

// The index of an associative array the library holds: each key is a value of
// its own, held with its type, in the order the declaration imposes. A lookup
// compares the index it is handed where it lies, and only an insertion copies
// one.
struct WitnessedKey {
  using Stored = AnyValue;
  using Probe = IndexView;

  struct Less {
    using is_transparent = void;

    AssociativeIndexOrder order;
    [[nodiscard]] auto operator()(IndexView a, IndexView b) const -> bool;
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

  AssociativeIndexOrder order = AssociativeIndexOrder::kIndexValueDomain;

  [[nodiscard]] auto Order() const -> Less {
    return Less{.order = order};
  }
  // LRM 7.8.6: an index carrying x or z names no entry.
  [[nodiscard]] static auto Invalid(IndexView key) -> bool {
    return key.type->HasUnknown(key.bytes);
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

  // An empty array of `element`, whose elements start as `element_default`
  // (LRM Table 7-1) and whose absent keys read `miss` (LRM 7.8.6, 7.9.11).
  RuntimeAssociativeArray(
      AssociativeIndexOrder index_order, const ValueType& element,
      const void* element_default, const void* miss);

  RuntimeAssociativeArray(const RuntimeAssociativeArray&);
  RuntimeAssociativeArray(RuntimeAssociativeArray&&) noexcept;
  auto operator=(const RuntimeAssociativeArray&) -> RuntimeAssociativeArray&;
  auto operator=(RuntimeAssociativeArray&&) noexcept
      -> RuntimeAssociativeArray&;
  ~RuntimeAssociativeArray();

  [[nodiscard]] auto IndexOrder() const -> AssociativeIndexOrder;
  [[nodiscard]] auto ElementType() const -> const ValueType&;
  [[nodiscard]] auto ElementDefault() const -> const void*;
  // What a read of an absent key answers with (LRM 7.8.6, 7.9.11).
  [[nodiscard]] auto Miss() const -> const void*;

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
  [[nodiscard]] auto ElementRef(IndexView index, Formation& formed) -> void*;

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

  [[nodiscard]] auto operator==(const RuntimeAssociativeArray& other) const
      -> FourStateBit;
  [[nodiscard]] auto operator!=(const RuntimeAssociativeArray& other) const
      -> FourStateBit;
  [[nodiscard]] auto CaseEqual(const RuntimeAssociativeArray& other) const
      -> Bit;
  [[nodiscard]] auto IsBitIdentical(const RuntimeAssociativeArray& other) const
      -> bool;
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> Bit;
  [[nodiscard]] auto BitstreamWidth() const -> Int;
  [[nodiscard]] auto CountBits(const ConstIntegralView& control_bits) const
      -> Int;

 private:
  using Core = BasicAssociativeArray<WitnessedKey, WitnessedElem>;

  void RequireInstalled() const;
  [[nodiscard]] auto Installed() const -> const Core&;
  [[nodiscard]] auto Installed() -> Core&;

  std::optional<Core> core_;
};

static_assert(LyraValue<RuntimeAssociativeArray>);
static_assert(CaseEqualComparable<RuntimeAssociativeArray>);
static_assert(Sized<RuntimeAssociativeArray>);
static_assert(BitstreamSizable<RuntimeAssociativeArray, ConstIntegralView>);

}  // namespace lyra::value

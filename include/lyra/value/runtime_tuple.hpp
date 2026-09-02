#pragma once

#include <cstddef>
#include <vector>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

struct RuntimeValue;

// The runtime-owned realization of an unpacked struct (LRM 7.2), MIR's generic
// product type `TupleType`. A product value that owns its components by value:
// copy is a component-wise deep copy, destruction is C++ RAII, so a component
// never borrows caller storage.
//
// This is the execution backend's type-erased counterpart of the C++ backend's
// monomorphized `Tuple<Ts...>`. A compile-once runtime cannot instantiate a
// distinct C++ type per source struct, so one `RuntimeTuple` holds a vector of
// type-erased `RuntimeValue` components and composes the `LyraValue` contract
// by visiting them. A component's runtime domain is the active alternative of
// its `RuntimeValue`; the whole-value operations dispatch on those
// alternatives, so no separate persistent type descriptor is stored --
// construction supplies the component domains, and the alternatives carry them
// thereafter.
class RuntimeTuple {
 public:
  RuntimeTuple();
  explicit RuntimeTuple(std::vector<RuntimeValue> components);
  RuntimeTuple(const RuntimeTuple&);
  RuntimeTuple(RuntimeTuple&&) noexcept;
  auto operator=(const RuntimeTuple&) -> RuntimeTuple&;
  auto operator=(RuntimeTuple&&) noexcept -> RuntimeTuple&;
  ~RuntimeTuple();

  // How many components the product holds, fixed at construction. A product
  // has no SV size query of its own, so this is the host count alone.
  [[nodiscard]] auto RawSize() const -> std::size_t;

  // Reads component `index` by reference. The caller copies it out across the
  // opaque-handle boundary rather than aliasing it.
  [[nodiscard]] auto Component(std::size_t index) const -> const RuntimeValue&;

  // Overwrites component `index` with `value`. The declared domain of a struct
  // field never changes, so the replacement's alternative matches the slot's.
  void SetComponent(std::size_t index, RuntimeValue value);

  // LRM 11.4.5 `==` / `!=` (Any data type): a component-wise reduction that
  // propagates X / Z through each component's own equality.
  [[nodiscard]] auto operator==(const RuntimeTuple& other) const -> PackedArray;
  [[nodiscard]] auto operator!=(const RuntimeTuple& other) const -> PackedArray;

  // LRM 11.4.5 `===` / `!==` (Any data type except real / shortreal):
  // component-wise case equality, deterministic in X / Z. A real component
  // makes the source-level `===` an error, rejected before lowering, so it
  // never reaches this method.
  [[nodiscard]] auto CaseEqual(const RuntimeTuple& other) const -> PackedArray;

  // LRM 6.6.1 Table 6-2 tri-state resolution, applied component by component.
  // LRM 6.7.1 admits an unpacked struct as a net's data type when every member
  // is itself valid for a net, and it composes a net out of its members' bits,
  // so folding two contributions is folding each component pair.
  [[nodiscard]] auto ResolveTriState(const RuntimeTuple& other) const
      -> RuntimeTuple;

  // The all-high-impedance value at `prototype`'s shape: each component's own
  // high-impedance value (LRM 6.6.1). Only the prototype's shape is read.
  [[nodiscard]] static auto HighImpedanceLike(const RuntimeTuple& prototype)
      -> RuntimeTuple;

  // LRM 9.4.2 update-event predicate (engine change-detection hook).
  [[nodiscard]] auto IsBitIdentical(const RuntimeTuple& other) const -> bool;

  // LRM 20.9: any component carrying an unknown bit propagates up.
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> PackedArray;

  // LRM 20.6.2 `$bits`: the sum of the components' own widths, an aggregate's
  // bit stream being its components' laid end to end.
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray;

  // LRM 20.9 `$countbits`: the sum of the components' own counts, a product
  // value's bit stream being its components' laid end to end.
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray;

 private:
  std::vector<RuntimeValue> components_;
};

static_assert(LyraValue<RuntimeTuple>);
static_assert(NetResolvable<RuntimeTuple>);
static_assert(CaseEqualComparable<RuntimeTuple>);
static_assert(BitstreamSizable<RuntimeTuple>);

}  // namespace lyra::value

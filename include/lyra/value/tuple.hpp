#pragma once

#include <cstddef>
#include <tuple>
#include <utility>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// A heterogeneous product value: a positional, fixed list of component value
// types, each reached by its declaration-order index. The value-layer
// realization of MIR's generic product type, not any one SystemVerilog
// construct -- it backs every product the pipeline builds: a task's output
// pack, an associative entry's (key, value) pair, and an SV unpacked struct
// (LRM 7.2), whose member names are erased to positions before this layer. It
// composes the LyraValue contract from its components: member-wise equality
// yielding a 1-bit PackedArray, bit-identity, and unknown detection. A
// component that owns variable-size storage carries its own copy semantics, so
// a Tuple copy is a shallow copy of its components.
template <typename... Ts>
class Tuple {
 public:
  Tuple() = default;

  // Deduced rather than taking `Ts...` directly so that the component list may
  // be empty: a product of no components is a value like any other, and a
  // non-template constructor over an empty pack would collide with the default
  // one instead of simply taking no arguments.
  template <typename... Us>
    requires(sizeof...(Us) == sizeof...(Ts))
  explicit Tuple(Us&&... values) : data_(std::forward<Us>(values)...) {
  }

  // Component access by declaration-order index. The reference qualifier tracks
  // the receiver's value category: a const receiver yields a const reference (a
  // member read), a mutable one a mutable reference (a member write through a
  // Mutate snapshot), and an rvalue one a movable reference.
  template <std::size_t I>
  [[nodiscard]] auto Get() & -> decltype(auto) {
    return std::get<I>(data_);
  }
  template <std::size_t I>
  [[nodiscard]] auto Get() const& -> decltype(auto) {
    return std::get<I>(data_);
  }
  template <std::size_t I>
  [[nodiscard]] auto Get() && -> decltype(auto) {
    return std::get<I>(std::move(data_));
  }

  // LRM 11.4.5 `==` / `!=` (Any data type). Member-wise logical AND, yielding a
  // 1-bit PackedArray; X / Z on any member propagates through the per-member
  // `==`.
  [[nodiscard]] auto operator==(const Tuple& other) const -> PackedArray {
    return [&]<std::size_t... I>(std::index_sequence<I...>) {
      PackedArray result = PackedArray::Bit(true);
      ((result = result && (std::get<I>(data_) == std::get<I>(other.data_))),
       ...);
      return result;
    }(std::index_sequence_for<Ts...>{});
  }
  [[nodiscard]] auto operator!=(const Tuple& other) const -> PackedArray {
    return !(*this == other);
  }

  // LRM 11.4.5 `===` / `!==` (case equality): member-wise bit-for-bit identity,
  // AND-reduced to a 1-bit PackedArray that is always a known 0 or 1. A real /
  // shortreal leaf has no case-equality meaning and is rejected before
  // lowering, so every component itself supplies case equality.
  [[nodiscard]] auto CaseEqual(const Tuple& other) const -> PackedArray {
    return [&]<std::size_t... I>(std::index_sequence<I...>) {
      PackedArray result = PackedArray::Bit(true);
      ((result =
            result && std::get<I>(data_).CaseEqual(std::get<I>(other.data_))),
       ...);
      return result;
    }(std::index_sequence_for<Ts...>{});
  }

  // LRM 9.4.2 update-event predicate (engine change-detection hook): are the
  // two values member-wise bit-identical.
  [[nodiscard]] auto IsBitIdentical(const Tuple& other) const -> bool {
    return [&]<std::size_t... I>(std::index_sequence<I...>) {
      return (
          std::get<I>(data_).IsBitIdentical(std::get<I>(other.data_)) && ...);
    }(std::index_sequence_for<Ts...>{});
  }

  // LRM 20.6.2 `$bits`: a product occupies the sum of its members' bit counts.
  // A member that is itself dynamically sized contributes its current width.
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray {
    return [&]<std::size_t... I>(std::index_sequence<I...>) {
      PackedArray total = PackedArray::Int(0);
      ((total = total + std::get<I>(data_).BitstreamWidth()), ...);
      return total;
    }(std::index_sequence_for<Ts...>{});
  }

  // LRM 20.9 `$isunknown`: any member carrying an X / Z bit propagates up.
  [[nodiscard]] auto HasUnknown() const -> bool {
    return [&]<std::size_t... I>(std::index_sequence<I...>) {
      return (std::get<I>(data_).HasUnknown() || ...);
    }(std::index_sequence_for<Ts...>{});
  }

  [[nodiscard]] auto IsUnknown() const -> PackedArray {
    return PackedArray::Bit(HasUnknown());
  }

  // LRM Table 7-1 unpacked-struct default: member-wise reset, each component to
  // its own Table 6-7 default. In-place rather than reconstruct, so a container
  // can scrub a reused discard slot to canonical before handing out a
  // reference.
  auto ResetToDefault() -> void {
    [&]<std::size_t... I>(std::index_sequence<I...>) {
      (std::get<I>(data_).ResetToDefault(), ...);
    }(std::index_sequence_for<Ts...>{});
  }

 private:
  std::tuple<Ts...> data_;
};

// Every arity is a product, so the contract is asserted at none, one, and many
// components rather than only at the shape that happens to be common.
static_assert(LyraValue<Tuple<>>);
static_assert(CaseEqualComparable<Tuple<>>);
static_assert(BitstreamSizable<Tuple<>>);
static_assert(Defaultable<Tuple<>>);

static_assert(LyraValue<Tuple<PackedArray>>);
static_assert(CaseEqualComparable<Tuple<PackedArray>>);
static_assert(BitstreamSizable<Tuple<PackedArray>>);
static_assert(Defaultable<Tuple<PackedArray>>);

static_assert(LyraValue<Tuple<PackedArray, PackedArray>>);
static_assert(CaseEqualComparable<Tuple<PackedArray, PackedArray>>);
static_assert(BitstreamSizable<Tuple<PackedArray, PackedArray>>);
static_assert(Defaultable<Tuple<PackedArray, PackedArray>>);

}  // namespace lyra::value

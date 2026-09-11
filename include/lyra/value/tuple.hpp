#pragma once

#include <cstddef>
#include <string>
#include <tuple>
#include <utility>

#include "lyra/value/concepts.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// A heterogeneous product value: a positional, fixed list of component value
// types, each reached by its declaration-order index. Not any one SystemVerilog
// construct -- it backs every product the pipeline builds: a task's output
// pack, an associative entry's (key, value) pair, and an SV unpacked struct
// (LRM 7.2), whose members a value reaches by position whatever its type calls
// them, so one realization serves the named and the anonymous alike. It
// composes the LyraValue contract from its components: member-wise equality
// yielding a 1-bit PackedArray, bit-identity, and unknown detection. A
// component that owns variable-size storage carries its own copy semantics, so
// a Tuple copy is a shallow copy of its components.
template <typename... Ts>
class Tuple {
 public:
  Tuple() = default;

  // A product of no components is a value like any other and its only form is
  // the default one, so the constructor that takes components is declared only
  // where there are components to take.
  explicit Tuple(Ts... values)
    requires(sizeof...(Ts) > 0)
      : data_(std::move(values)...) {
  }

  // Component access by declaration-order index. The reference qualifier tracks
  // the receiver's value category: a const receiver yields a const reference (a
  // member read), a mutable one a mutable reference (a member write), and an
  // rvalue one a movable reference.
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

  // Component `I` itself, for a caller that will write it or reach further
  // through it. Every component of a product is live at once, so reaching one
  // settles nothing about the others; a value holding one member at a time
  // answers the same request by settling which member that is, which is why
  // reaching a part is spelled apart from reading its value at all.
  template <std::size_t I>
  [[nodiscard]] auto GetRef() -> decltype(auto) {
    return std::get<I>(data_);
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

  // Net resolution applied member-wise under the fold `fold` names (LRM 6.6).
  // LRM 6.7.1 admits an unpacked struct as a net's data type when every member
  // is itself valid for a net, and it composes a net out of its members' bits,
  // so folding two contributions is folding each member pair.
  [[nodiscard]] auto ResolveNet(const Tuple& other, NetResolution fold) const
      -> Tuple {
    return [&]<std::size_t... I>(std::index_sequence<I...>) {
      return Tuple(
          std::get<I>(data_).ResolveNet(std::get<I>(other.data_), fold)...);
    }(std::index_sequence_for<Ts...>{});
  }

  // The all-high-impedance value at `prototype`'s shape: each member's own
  // high-impedance value (LRM 6.6.1). Only the prototype's shape is read.
  [[nodiscard]] static auto HighImpedanceLike(const Tuple& prototype) -> Tuple {
    return [&]<std::size_t... I>(std::index_sequence<I...>) {
      return Tuple(Ts::HighImpedanceLike(std::get<I>(prototype.data_))...);
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

  // LRM 20.9 `$countbits`: the bit stream this value contributes is its
  // elements' streams laid end to end, so the count over it is the sum of the
  // elements' own counts under the same control bits.
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray {
    return [&]<std::size_t... I>(std::index_sequence<I...>) {
      PackedArray total = PackedArray::Int(0);
      ((total = total + std::get<I>(data_).CountBits(control_bits)), ...);
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

// LRM 21.2.1.6 assignment-pattern format: every component is present at once,
// so every one of them is an element, each deferring to its own type's
// `Formatter`.
template <typename... Ts>
struct Formatter<Tuple<Ts...>> {
  static auto Format(const FormatSpec& spec, const Tuple<Ts...>& value)
      -> std::string {
    PatternWriter pattern;
    [&]<std::size_t... Is>(std::index_sequence<Is...>) {
      (pattern.Add(
           lyra::value::Format(spec, MakeFormatArg(value.template Get<Is>()))),
       ...);
    }(std::index_sequence_for<Ts...>{});
    return std::move(pattern).Finish();
  }
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
static_assert(NetResolvable<Tuple<PackedArray, PackedArray>>);

}  // namespace lyra::value

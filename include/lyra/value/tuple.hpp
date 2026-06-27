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

  explicit Tuple(Ts... values) : data_(std::move(values)...) {
  }

  // Component access by declaration-order index. The explicit object parameter
  // yields a const reference on a const receiver (member read) and a mutable
  // reference on a mutable one (member write through a Mutate snapshot), so one
  // method serves both access sides.
  template <std::size_t I>
  [[nodiscard]] auto Get(this auto&& self) -> auto&& {
    return std::get<I>(std::forward<decltype(self)>(self).data_);
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

  // LRM 20.9 `$isunknown`: any member carrying an X / Z bit propagates up.
  [[nodiscard]] auto HasUnknown() const -> bool {
    return [&]<std::size_t... I>(std::index_sequence<I...>) {
      return (std::get<I>(data_).HasUnknown() || ...);
    }(std::index_sequence_for<Ts...>{});
  }

 private:
  std::tuple<Ts...> data_;
};

static_assert(LyraValue<Tuple<PackedArray, PackedArray>>);
static_assert(CaseEqualComparable<Tuple<PackedArray, PackedArray>>);

}  // namespace lyra::value

#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// A type-checked sum: holds exactly one of its component value types at a time,
// identified by a declaration-order tag index that is part of the value.
// SystemVerilog's tagged union (LRM 7.3.2 / 11.9): construction must specify
// the tag, and every access -- read or write through the dot-notation surface
// -- requires the tag to match the current one; a mismatch is a runtime error,
// not a type-punning loophole. Distinct from `Union<Ts...>`: `Union` erases the
// tag after write (cross-member reads return the component default, a
// deterministic Lyra fallback for undefined SV), whereas here the tag is
// observable and mismatched access throws. Members are reached by index, never
// by type, because a tagged union may declare two members of the same type
// (`union tagged { int A; int B; }`). A `void` member -- allowed only in tagged
// unions (LRM 7.3.2) -- is an ordinary component whose type carries no bits, so
// nothing here treats it apart.
template <typename... Ts>
class TaggedUnion {
 public:
  // LRM 11.9: an uninitialized variable of tagged union type is undefined,
  // including its tag bits. The deterministic stand-in -- the same policy the
  // untagged `Union` applies to its own SV-undefined case -- is tag 0 with
  // the first component's default value, which is what `std::variant`'s
  // default construction already yields. No program may depend on it.
  TaggedUnion() = default;

  // Build a tagged value whose active member is component `I`, carrying
  // `value`. The index form (not a type form) is mandatory because components
  // may repeat.
  template <std::size_t I, typename V>
  [[nodiscard]] static auto Make(V&& value) -> TaggedUnion {
    TaggedUnion u;
    u.data_.template emplace<I>(std::forward<V>(value));
    return u;
  }

  // Asks whether component `I` is the active one, without the runtime error a
  // mismatched read raises. This is the query a caller uses to decide whether
  // a read is legal, so that the error path stays reserved for a program that
  // reads without asking (LRM 11.9).
  template <std::size_t I>
  [[nodiscard]] auto IsTagged() const -> bool {
    return data_.index() == I;
  }

  // Read component `I`. LRM 11.9: reading a member whose type is inconsistent
  // with the current tag results in a run-time error. `Union<Ts...>::Get`
  // returns the component default; this throws instead.
  template <std::size_t I>
  [[nodiscard]] auto Get() const
      -> const std::variant_alternative_t<I, std::variant<Ts...>>& {
    const auto* active = std::get_if<I>(&data_);
    if (active == nullptr) {
      throw SimulationError(
          "read of a tagged union member inconsistent with the current tag "
          "(LRM 11.9)");
    }
    return *active;
  }

  // The writable location of component `I`. LRM 11.9: assigning a member whose
  // type is inconsistent with the current tag is a run-time error. Unlike
  // `Union<Ts...>::GetRef`, which activates the member on write, this throws
  // if `I` is not the current tag -- re-tagging must go through a whole-value
  // `tagged` construction.
  template <std::size_t I>
  [[nodiscard]] auto GetRef()
      -> std::variant_alternative_t<I, std::variant<Ts...>>& {
    auto* active = std::get_if<I>(&data_);
    if (active == nullptr) {
      throw SimulationError(
          "write to a tagged union member inconsistent with the current tag "
          "(LRM 11.9)");
    }
    return *active;
  }

  // LRM 11.4.5 `==` / `!=` (Any data type). Tagged unions are equal only when
  // the same member is active and its values compare equal.
  [[nodiscard]] auto operator==(const TaggedUnion& other) const -> PackedArray {
    if (data_.index() != other.data_.index()) {
      return PackedArray::Bit(false);
    }
    return std::visit(
        [](const auto& a, const auto& b) -> PackedArray {
          if constexpr (std::is_same_v<
                            std::decay_t<decltype(a)>,
                            std::decay_t<decltype(b)>>) {
            return a == b;
          } else {
            return PackedArray::Bit(false);
          }
        },
        data_, other.data_);
  }
  [[nodiscard]] auto operator!=(const TaggedUnion& other) const -> PackedArray {
    return !(*this == other);
  }

  // LRM 11.4.5 `===` / `!==` (case equality): same active member and
  // bit-for-bit identical value.
  [[nodiscard]] auto CaseEqual(const TaggedUnion& other) const -> PackedArray {
    if (data_.index() != other.data_.index()) {
      return PackedArray::Bit(false);
    }
    return std::visit(
        [](const auto& a, const auto& b) -> PackedArray {
          if constexpr (std::is_same_v<
                            std::decay_t<decltype(a)>,
                            std::decay_t<decltype(b)>>) {
            return a.CaseEqual(b);
          } else {
            return PackedArray::Bit(false);
          }
        },
        data_, other.data_);
  }

  // LRM 9.4.2 update-event predicate: changed when the active member changed
  // or the active value's bits changed.
  [[nodiscard]] auto IsBitIdentical(const TaggedUnion& other) const -> bool {
    if (data_.index() != other.data_.index()) {
      return false;
    }
    return std::visit(
        [](const auto& a, const auto& b) -> bool {
          if constexpr (std::is_same_v<
                            std::decay_t<decltype(a)>,
                            std::decay_t<decltype(b)>>) {
            return a.IsBitIdentical(b);
          } else {
            return false;
          }
        },
        data_, other.data_);
  }

  // LRM 20.9 `$isunknown`: the active member's unknown bits propagate up.
  [[nodiscard]] auto HasUnknown() const -> bool {
    return std::visit(
        [](const auto& active) -> bool { return active.HasUnknown(); }, data_);
  }

  [[nodiscard]] auto IsUnknown() const -> PackedArray {
    return PackedArray::Bit(HasUnknown());
  }

 private:
  std::variant<Ts...> data_;
};

static_assert(LyraValue<TaggedUnion<PackedArray, PackedArray>>);
static_assert(LyraValue<TaggedUnion<Empty, PackedArray>>);
static_assert(CaseEqualComparable<TaggedUnion<PackedArray, PackedArray>>);

}  // namespace lyra::value

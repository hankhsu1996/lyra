#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// An active-member value: holds exactly one of its component value types at a
// time, identified by a declaration-order index. The value-layer realization of
// MIR's UnionType -- the runtime form of an SV untagged unpacked union (LRM
// 7.3), whose member names are erased to positions before this layer. Members
// are reached by index, never by type, because a union may declare two members
// of the same type. SystemVerilog gives no reliable semantics to reading a
// member other than the one last written, so the union stores only the active
// member; a read of an inactive member returns that member's default -- a
// deterministic fallback for an operation SV leaves undefined, not a value any
// program may depend on. The LyraValue contract composes from the active
// member: equality is same-active-member-and-equal-value, never a cross-member
// comparison.
template <typename... Ts>
class Union {
 public:
  // LRM Table 7-1: an unpacked union defaults to its first member. The default
  // construction is a placeholder; the lowering emits an explicit first-member
  // default value at each default-init site.
  Union() = default;

  // Build a union whose active member is component `I`, carrying `value`. The
  // index form (not a type form) is mandatory because components may repeat.
  template <std::size_t I, typename V>
  [[nodiscard]] static auto Make(V&& value) -> Union {
    Union u;
    u.data_.template emplace<I>(std::forward<V>(value));
    return u;
  }

  // Read component `I`. Returns the value when `I` is the active member;
  // otherwise returns that component's default, the deterministic stand-in for
  // an SV-undefined cross-member read.
  template <std::size_t I>
  [[nodiscard]] auto Get() const
      -> std::variant_alternative_t<I, std::variant<Ts...>> {
    using Member = std::variant_alternative_t<I, std::variant<Ts...>>;
    if (const auto* active = std::get_if<I>(&data_)) {
      return *active;
    }
    return Member{};
  }

  // LRM 11.4.5 `==` / `!=` (Any data type). Unions are equal only when the same
  // member is active and its values compare equal; a mismatched active member
  // is unequal without any cross-member reinterpretation.
  [[nodiscard]] auto operator==(const Union& other) const -> PackedArray {
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
  [[nodiscard]] auto operator!=(const Union& other) const -> PackedArray {
    return !(*this == other);
  }

  // LRM 11.4.5 `===` / `!==` (case equality): same active member and
  // bit-for-bit identical value. Instantiated only when the source uses `===`;
  // a union with a real / shortreal member never reaches it (lowering rejects
  // case equality on a real leaf, Table 11-1).
  [[nodiscard]] auto CaseEqual(const Union& other) const -> PackedArray {
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

  // LRM 9.4.2 update-event predicate (engine change-detection hook): the cell
  // changed when the active member changed or its value's bits changed.
  [[nodiscard]] auto IsBitIdentical(const Union& other) const -> bool {
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

 private:
  std::variant<Ts...> data_;
};

static_assert(LyraValue<Union<PackedArray, PackedArray>>);
static_assert(CaseEqualComparable<Union<PackedArray, PackedArray>>);

}  // namespace lyra::value

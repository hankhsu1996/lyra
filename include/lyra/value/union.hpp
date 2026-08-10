#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/base/simulation_error.hpp"
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

  // Read component `I` (the read side of member access; `std::get<I>` for a
  // variant). Returns the value when `I` is the active member; otherwise
  // returns that component's default, the deterministic stand-in for an
  // SV-undefined cross-member read.
  template <std::size_t I>
  [[nodiscard]] auto Get() const
      -> std::variant_alternative_t<I, std::variant<Ts...>> {
    using Component = std::variant_alternative_t<I, std::variant<Ts...>>;
    if (const auto* active = std::get_if<I>(&data_)) {
      return *active;
    }
    return Component{};
  }

  // The writable location of component `I` (the write side of member access),
  // the by-reference counterpart of `Get`. Returns a reference to the active
  // member's storage, making `I` active first if it is not -- so a write
  // activates the member it targets. A read goes through `Get` and never
  // activates; only a write takes this reference, so `u.f = v`, `u.f op= v`,
  // and a nested `u.f.g = v` all compose on it the way a struct member's
  // reference does.
  template <std::size_t I>
  [[nodiscard]] auto GetRef()
      -> std::variant_alternative_t<I, std::variant<Ts...>>& {
    if (auto* active = std::get_if<I>(&data_)) {
      return *active;
    }
    return data_.template emplace<I>();
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

  // LRM 6.6.1 Table 6-2 tri-state resolution over the active member. LRM 6.7.1
  // admits an unpacked union as a net's data type when every member is itself
  // valid for a net, so a union net resolves its drivers like any other -- as
  // long as they agree on which member they drive.
  //
  // They need not. Two contributions may nominally carry different members, and
  // one of those cases is ordinary: a contribution that drives nothing is
  // high-impedance, and high-impedance is the fold's identity, so it defers to
  // the other whichever member it nominally carries. That is what makes a
  // single driver of any member exact, since the fold starts from the first
  // member (LRM 7.3) while the driver may carry any.
  //
  // What is left -- two contributions both driving, on different members -- has
  // no answer at all: LRM 7.3 gives an unpacked union no required
  // representation for how its members are stored, and unlike a packed union it
  // does not admit reading back a member written as another, so there is no
  // defined bit space the two overlay in. The design is relying on something
  // the standard declines to define, so this reports that rather than inventing
  // a value.
  [[nodiscard]] auto ResolveTriState(const Union& other) const -> Union {
    if (data_.index() != other.data_.index()) {
      if (IsBitIdentical(HighImpedanceLike(*this))) {
        return other;
      }
      if (other.IsBitIdentical(HighImpedanceLike(other))) {
        return *this;
      }
      throw SimulationError(
          "two drivers of an unpacked-union net are driving different members; "
          "SystemVerilog gives an unpacked union no defined storage overlay, "
          "so "
          "their resolution has no defined value");
    }
    Union resolved;
    [&]<std::size_t... I>(std::index_sequence<I...>) {
      ((std::get_if<I>(&data_) == nullptr
            ? void()
            : void(resolved.data_.template emplace<I>(
                  std::get_if<I>(&data_)->ResolveTriState(
                      *std::get_if<I>(&other.data_))))),
       ...);
    }(std::index_sequence_for<Ts...>{});
    return resolved;
  }

  // The all-high-impedance value at `prototype`'s shape: the prototype's own
  // active member carrying that member's high-impedance value (LRM 6.6.1). A
  // net's prototype is its declared default, which for an unpacked union is its
  // first member (LRM 7.3), so a union net with nothing driving it reads as
  // that member at high impedance.
  [[nodiscard]] static auto HighImpedanceLike(const Union& prototype) -> Union {
    Union floating;
    [&]<std::size_t... I>(std::index_sequence<I...>) {
      ((std::get_if<I>(&prototype.data_) == nullptr
            ? void()
            : void(floating.data_.template emplace<I>(
                  Ts::HighImpedanceLike(*std::get_if<I>(&prototype.data_))))),
       ...);
    }(std::index_sequence_for<Ts...>{});
    return floating;
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

static_assert(LyraValue<Union<PackedArray, PackedArray>>);
static_assert(CaseEqualComparable<Union<PackedArray, PackedArray>>);
static_assert(NetResolvable<Union<PackedArray, PackedArray>>);

}  // namespace lyra::value

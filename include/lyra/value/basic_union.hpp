#pragma once

#include <cstddef>
#include <optional>
#include <utility>
#include <variant>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/net_resolution.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// An unpacked union, tagged or untagged (LRM 7.3, 7.3.2), written once over how
// its live member is held: with the members' C++ types for the C++ backend, and
// in the library with the live member's type. A union holds one member at a
// time, named by its declaration-order index, so each whole-value operation of
// two unions is the live member's own where the same member is live in both.
// What a union lets a program read or write of a member is the union kind's,
// and each kind states it beside this.
//
// `Member` holds the live member. It answers `Index()`; `Visit(f)` with the
// live value; `Paired(a, b, f)` with the live values of two holders of one
// member; and `Rebuilt(a, b, f)` / `Mapped(f)` with a holder of that member
// carrying what `f` answers.
template <typename Derived, typename Member>
class BasicUnion {
 public:
  // LRM 11.4.5 `==` / `!=` (Any data type): equal only when the same member is
  // live and its values compare equal, never a cross-member comparison.
  [[nodiscard]] auto operator==(const Derived& other) const -> PackedArray {
    if (!SameMember(other)) {
      return PackedArray::Bit(false);
    }
    return Member::Paired(
        live_, other.live_,
        [](const auto& a, const auto& b) -> PackedArray { return a == b; });
  }
  [[nodiscard]] auto operator!=(const Derived& other) const -> PackedArray {
    return !(*this == other);
  }

  // LRM 11.4.5 `===` / `!==`: the same member live, with identical bits.
  [[nodiscard]] auto CaseEqual(const Derived& other) const -> PackedArray {
    if (!SameMember(other)) {
      return PackedArray::Bit(false);
    }
    return Member::Paired(
        live_, other.live_, [](const auto& a, const auto& b) -> PackedArray {
          return a.CaseEqual(b);
        });
  }

  // LRM 9.4.2: a union changed when another member became live or the live
  // member's bits changed.
  [[nodiscard]] auto IsBitIdentical(const Derived& other) const -> bool {
    return SameMember(other) &&
           Member::Paired(
               live_, other.live_, [](const auto& a, const auto& b) -> bool {
                 return a.IsBitIdentical(b);
               });
  }

  // LRM 20.9 `$isunknown`: the live member's unknown bits propagate up.
  [[nodiscard]] auto HasUnknown() const -> bool {
    return live_.Visit(
        [](const auto& value) -> bool { return value.HasUnknown(); });
  }
  [[nodiscard]] auto IsUnknown() const -> PackedArray {
    return PackedArray::Bit(HasUnknown());
  }

  // Net resolution over the live member under each truth table (LRM 6.6). LRM
  // 6.7.1 admits an unpacked union as a net's data type when every member is
  // itself valid for a net, so a union net resolves its drivers like any other
  // when they drive the same member.
  [[nodiscard]] auto ResolveTriState(const Derived& other) const -> Derived {
    return FoldedWith(other, NetResolution::kTriState);
  }
  [[nodiscard]] auto ResolveWiredAnd(const Derived& other) const -> Derived {
    return FoldedWith(other, NetResolution::kWiredAnd);
  }
  [[nodiscard]] auto ResolveWiredOr(const Derived& other) const -> Derived {
    return FoldedWith(other, NetResolution::kWiredOr);
  }

  // What a stronger contribution leaves a weaker one (LRM 28.12.1), which for
  // members that differ has an answer only where one of them drives nothing --
  // the same place LRM 7.3 leaves the two without a storage overlay.
  [[nodiscard]] auto Dominating(const Derived& weaker) const -> Derived {
    if (!SameMember(weaker)) {
      return AcrossMembers(Self(), weaker);
    }
    return Derived(
        Member::Rebuilt(live_, weaker.live_, [](const auto& a, const auto& b) {
          return a.Dominating(b);
        }));
  }

  // `prototype`'s shape with every bit set to `fill`: its own live member,
  // filled (LRM 6.7.1). A net's prototype is its declared default, which for an
  // unpacked union is its first member (LRM 7.3), so a union net nothing drives
  // reads as that member at high impedance.
  [[nodiscard]] static auto FilledLike(
      const Derived& prototype, const PackedArray& fill) -> Derived {
    return Derived(prototype.live_.Mapped([&](const auto& value) {
      return std::decay_t<decltype(value)>::FilledLike(value, fill);
    }));
  }

  // LRM 6.24.3 streams a union, which is not carried out yet. A structure with
  // a union member asks these only where the program measures it.
  [[noreturn]] static auto BitstreamWidth() -> PackedArray {
    throw SimulationError(
        "$bits of a union is not yet supported on this backend; please open "
        "an issue asking for support");
  }
  [[noreturn]] static auto ToBitstream() -> PackedArray {
    throw SimulationError(
        "reading this value as a stream of bits is not yet supported on this "
        "backend; please open an issue asking for support");
  }
  // LRM 20.9 counts over the bit stream.
  [[nodiscard]] static auto CountBits(const PackedArray& control_bits)
      -> PackedArray {
    return ToBitstream().CountBits(control_bits);
  }

 protected:
  BasicUnion() = default;
  explicit BasicUnion(Member live) : live_(std::move(live)) {
  }

  [[nodiscard]] auto Live() const -> const Member& {
    return live_;
  }
  [[nodiscard]] auto Live() -> Member& {
    return live_;
  }

 private:
  [[nodiscard]] auto Self() const -> const Derived& {
    return static_cast<const Derived&>(*this);
  }

  [[nodiscard]] auto SameMember(const Derived& other) const -> bool {
    return live_.Index() == other.live_.Index();
  }

  // What two contributions nominally carrying different members resolve to, by
  // either rule that combines contributions. A contribution that drives nothing
  // is high-impedance and defers to the other whichever member it nominally
  // carries, which is what makes a single driver of any member exact while
  // resolution starts from the first member (LRM 7.3).
  //
  // Two contributions both driving different members has no answer: LRM 7.3
  // gives an unpacked union no required storage representation, so there is
  // no defined bit space the two overlay in. That is reported rather than
  // answered with an invented value.
  [[nodiscard]] static auto AcrossMembers(const Derived& a, const Derived& b)
      -> Derived {
    const PackedArray high_impedance = PackedArray::HighImpedanceScalar();
    if (a.IsBitIdentical(FilledLike(a, high_impedance))) {
      return b;
    }
    if (b.IsBitIdentical(FilledLike(b, high_impedance))) {
      return a;
    }
    throw SimulationError(
        "two drivers of an unpacked-union net are driving different members; "
        "SystemVerilog gives an unpacked union no defined storage overlay, so "
        "their resolution has no defined value");
  }

  [[nodiscard]] auto FoldedWith(const Derived& other, NetResolution fold) const
      -> Derived {
    if (!SameMember(other)) {
      return AcrossMembers(Self(), other);
    }
    return Derived(
        Member::Rebuilt(live_, other.live_, [&](const auto& a, const auto& b) {
          return ResolvedUnder(fold, a, b);
        }));
  }

  Member live_;
};

// A read of an untagged union's member other than the live one. LRM 7.3
// defines it where the members are structures sharing a common initial
// sequence, whose part then reads what was written through the live member; a
// union that stores only its live member has no such part to read.
[[noreturn]] inline void RefuseReadOfAnotherMember() {
  throw SimulationError(
      "reading an unpacked-union member other than the one last written is "
      "not yet supported on this backend; please open an issue asking for "
      "support");
}

// The live member of a union whose members are the C++ types `Ts`, held as the
// alternative of the member's index, since two members may share a type.
template <typename... Ts>
class VariantMember {
 public:
  [[nodiscard]] auto Index() const -> std::size_t {
    return alternatives_.index();
  }
  [[nodiscard]] auto Alternatives() const -> const std::variant<Ts...>& {
    return alternatives_;
  }
  [[nodiscard]] auto Alternatives() -> std::variant<Ts...>& {
    return alternatives_;
  }

  template <typename F>
  [[nodiscard]] auto Visit(F f) const {
    return std::visit(f, alternatives_);
  }

  template <typename F>
  [[nodiscard]] static auto Paired(
      const VariantMember& a, const VariantMember& b, F f) {
    using Answer =
        decltype(f(std::get<0>(a.alternatives_), std::get<0>(b.alternatives_)));
    std::optional<Answer> answer;
    a.WithLiveIndex([&]<std::size_t I>() {
      answer.emplace(
          f(std::get<I>(a.alternatives_), std::get<I>(b.alternatives_)));
    });
    return *std::move(answer);
  }

  template <typename F>
  [[nodiscard]] static auto Rebuilt(
      const VariantMember& a, const VariantMember& b, F f) -> VariantMember {
    VariantMember rebuilt;
    a.WithLiveIndex([&]<std::size_t I>() {
      rebuilt.alternatives_.template emplace<I>(
          f(std::get<I>(a.alternatives_), std::get<I>(b.alternatives_)));
    });
    return rebuilt;
  }

  template <typename F>
  [[nodiscard]] auto Mapped(F f) const -> VariantMember {
    VariantMember mapped;
    WithLiveIndex([&]<std::size_t I>() {
      mapped.alternatives_.template emplace<I>(f(std::get<I>(alternatives_)));
    });
    return mapped;
  }

 private:
  // Calls `f` with the live member's index as a constant.
  template <typename F>
  void WithLiveIndex(F f) const {
    [&]<std::size_t... I>(std::index_sequence<I...>) {
      ((alternatives_.index() == I ? f.template operator()<I>() : void()), ...);
    }(std::index_sequence_for<Ts...>{});
  }

  std::variant<Ts...> alternatives_;
};

}  // namespace lyra::value

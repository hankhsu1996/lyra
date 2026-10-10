#pragma once

#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <optional>
#include <span>
#include <string>
#include <type_traits>

#include "lyra/base/fixed_array.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral_fwd.hpp"  // IWYU pragma: export
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

// The bytes one plane of a `width`-bit value occupies: the smallest of 1, 2, 4
// and 8 that holds it up to 64 bits, and a whole number of 64-bit words above
// -- the sizes C's `_BitInt` takes, so a value occupies the bits its type
// declares by at most a constant factor. A value is its value plane followed,
// where it can hold x or z, by its unknown plane, and is aligned as one plane
// up to a word. Every layer that lays a value out or reads one reads this, so
// what one of them built is what another reads.
[[nodiscard]] constexpr auto PlaneBytesFor(std::uint64_t width) -> std::size_t {
  if (width <= 8U) {
    return 1;
  }
  if (width <= 16U) {
    return 2;
  }
  if (width <= 32U) {
    return 4;
  }
  return 8 * WordCountForBits(width);
}

[[nodiscard]] constexpr auto IntegralBytesFor(
    std::uint64_t width, StateDomain domain) -> std::size_t {
  return PlaneBytesFor(width) * (domain == StateDomain::kFourState ? 2 : 1);
}

[[nodiscard]] constexpr auto IntegralAlignFor(std::uint64_t width)
    -> std::size_t {
  return std::min<std::size_t>(PlaneBytesFor(width), 8);
}

namespace detail {

// The machine data one plane of a `kWidth`-bit value is held in: an unsigned
// integer of the plane's size, or the run of words a plane wider than one
// word is.
template <std::uint64_t kWidth>
using PlaneOf = std::conditional_t<
    PlaneBytesFor(kWidth) == 1, std::uint8_t,
    std::conditional_t<
        PlaneBytesFor(kWidth) == 2, std::uint16_t,
        std::conditional_t<
            PlaneBytesFor(kWidth) == 4, std::uint32_t,
            std::conditional_t<
                (kWidth <= 64), std::uint64_t,
                std::array<std::uint64_t, WordCountForBits(kWidth)>>>>>;

// A plane a value does not have: a two-state value's unknown one.
struct NoPlane {
  auto operator==(const NoPlane&) const -> bool = default;
};

[[nodiscard]] constexpr auto CombinedDomain(StateDomain a, StateDomain b)
    -> StateDomain {
  return a == StateDomain::kFourState || b == StateDomain::kFourState
             ? StateDomain::kFourState
             : StateDomain::kTwoState;
}

}  // namespace detail

// An integral value: `kWidth` bits read as `kSignedness`, each of which holds 0
// or 1, or also x or z where `kDomain` is four-state (LRM 6.11). Every
// SystemVerilog integral type is one of these -- `int` is a signed two-state
// 32, `logic [7:0]` an unsigned four-state 8, a packed structure or union the
// width of its members -- because how a declaration divides its bits is the
// declaration's own, and reaches an operation only as the position a select
// names.
//
// The value is its planes and nothing else: the value plane, then the unknown
// plane when four-state, each the machine integer of the plane's size or the
// words a wider plane is, with every position above the width clear. Copying
// it copies those bytes, and nothing about it is looked up while the program
// runs; each operation is the language's operator over its words at this
// width.
template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
class Integral {
  static_assert(kWidthArg >= 1, "an integral value has at least one bit");

 public:
  static constexpr std::uint64_t kWidth = kWidthArg;
  static constexpr Signedness kSignedness = kSignednessArg;
  static constexpr StateDomain kDomain = kDomainArg;
  static constexpr bool kFourState = kDomain == StateDomain::kFourState;
  static constexpr std::size_t kWords = WordCountForBits(kWidth);

  using Plane = detail::PlaneOf<kWidth>;
  using UnknownPlane = std::conditional_t<kFourState, Plane, detail::NoPlane>;

  // The planes worked on as words, every position clear until written. A
  // two-state value has no unknown words.
  struct Words {
    std::array<std::uint64_t, kWords> value{};
    std::array<std::uint64_t, kFourState ? kWords : 0> unknown{};

    [[nodiscard]] LYRA_FOLDED constexpr auto Read() const -> ConstPlanes {
      return ConstPlanes{.value = value, .unknown = unknown};
    }
    [[nodiscard]] LYRA_FOLDED constexpr auto Write() -> Planes {
      return Planes{.value = value, .unknown = unknown};
    }

    // These words as code compiled without the type takes a value of it, which
    // lasts as long as they do.
    [[nodiscard]] constexpr auto View() const -> ConstIntegralView {
      return ConstIntegralView{
          .planes = Read(), .width = kWidth, .signedness = kSignedness};
    }
    [[nodiscard]] constexpr auto MutableView() -> IntegralView {
      return IntegralView{.planes = Write(), .width = kWidth};
    }
  };

  // What a declaration holds before anything writes it (LRM Table 6-7): x in
  // every position of a four-state value, 0 of a two-state one.
  LYRA_FOLDED constexpr Integral() : Integral(AllOf(FourStateBit::kUnknown)) {
  }

  [[nodiscard]] LYRA_FOLDED static constexpr auto FromWords(const Words& words)
      -> Integral {
    return Integral(words);
  }

  // The planes as a constant states them, value then unknown -- the second
  // empty for a two-state value.
  [[nodiscard]] static constexpr auto FromWords(
      const std::array<std::uint64_t, kWords>& value,
      const std::array<std::uint64_t, kFourState ? kWords : 0>& unknown)
      -> Integral {
    return Integral(Words{.value = value, .unknown = unknown});
  }

  // An integer carried into this type, its sign repeated into every position
  // above 64 (LRM 11.6.1).
  [[nodiscard]] LYRA_FOLDED static constexpr auto FromInt(std::int64_t value)
      -> Integral {
    Words words;
    lyra::value::FromInt(words.Write(), kWidth, value);
    return Integral(words);
  }

  [[nodiscard]] LYRA_FOLDED static constexpr auto FromBool(bool value)
      -> Integral {
    return FromInt(value ? 1 : 0);
  }

  // Every position holding one scalar: what a net shows where nothing drives
  // it, and what a driver contributes where it does not drive (LRM 6.6). A
  // two-state value holds an x or z as 0.
  [[nodiscard]] LYRA_FOLDED static constexpr auto Filled(FourStateBit bit)
      -> Integral {
    return Integral(AllOf(bit));
  }

  [[nodiscard]] LYRA_FOLDED constexpr auto Load() const -> Words {
    Words words;
    LoadPlane(value_, words.value);
    if constexpr (kFourState) {
      LoadPlane(unknown_, words.unknown);
    }
    return words;
  }

  // LRM 9.4.2: whether two values are the same bits, x and z included, which
  // is what decides that a write changed a variable. Every position above the
  // width is clear, so the bits are the same exactly where the planes are.
  [[nodiscard]] constexpr auto IsBitIdentical(const Integral& other) const
      -> bool {
    return value_ == other.value_ && unknown_ == other.unknown_;
  }

  [[nodiscard]] LYRA_FOLDED constexpr auto HasUnknown() const -> bool {
    if constexpr (kFourState) {
      return lyra::value::HasUnknown(Load().Read());
    } else {
      return false;
    }
  }

  [[nodiscard]] LYRA_FOLDED constexpr auto Truth() const -> Truthiness {
    return lyra::value::Truth(Load().Read());
  }

  // LRM 12.4: only a definitively-one bit makes a condition true.
  [[nodiscard]] LYRA_FOLDED constexpr auto IsTruthy() const -> bool {
    return Truth() == Truthiness::kKnownNonzero;
  }
  [[nodiscard]] LYRA_FOLDED constexpr explicit operator bool() const {
    return IsTruthy();
  }

  [[nodiscard]] LYRA_FOLDED constexpr auto ToInt64() const -> std::int64_t {
    return lyra::value::ToInt64(Load().Read(), kWidth, kSignedness);
  }

  [[nodiscard]] LYRA_FOLDED constexpr auto Lsb() const -> FourStateBit {
    return LeastSignificantBit(Load().Read());
  }

  // LRM 11.4.3: arithmetic at the operands' own type.
  [[nodiscard]] LYRA_FOLDED constexpr auto operator+(const Integral& b) const
      -> Integral {
    Words out;
    Add(out.Write(), Load().Read(), b.Load().Read(), kWidth);
    return Integral(out);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator-(const Integral& b) const
      -> Integral {
    Words out;
    Subtract(out.Write(), Load().Read(), b.Load().Read(), kWidth);
    return Integral(out);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator*(const Integral& b) const
      -> Integral {
    Words out;
    Multiply(out.Write(), Load().Read(), b.Load().Read(), kWidth);
    return Integral(out);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator/(const Integral& b) const
      -> Integral {
    Words out;
    Divide(out.Write(), Load().Read(), b.Load().Read(), kWidth, kSignedness);
    return Integral(out);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator%(const Integral& b) const
      -> Integral {
    Words out;
    Modulo(out.Write(), Load().Read(), b.Load().Read(), kWidth, kSignedness);
    return Integral(out);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator-() const -> Integral {
    Words out;
    Negate(out.Write(), Load().Read(), kWidth);
    return Integral(out);
  }

  // LRM 11.4.8: the bitwise operators.
  [[nodiscard]] LYRA_FOLDED constexpr auto operator&(const Integral& b) const
      -> Integral {
    Words out;
    BitwiseAnd(out.Write(), Load().Read(), b.Load().Read());
    return Integral(out);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator|(const Integral& b) const
      -> Integral {
    Words out;
    BitwiseOr(out.Write(), Load().Read(), b.Load().Read());
    return Integral(out);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator^(const Integral& b) const
      -> Integral {
    Words out;
    BitwiseXor(out.Write(), Load().Read(), b.Load().Read());
    return Integral(out);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator~() const -> Integral {
    Words out;
    BitwiseNot(out.Write(), Load().Read(), kWidth);
    return Integral(out);
  }

  // LRM 11.4.4, 11.4.5: a comparison answers one bit, which can be x only
  // where its operands can.
  [[nodiscard]] LYRA_FOLDED constexpr auto operator==(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(Equal(Load().Read(), b.Load().Read()));
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator!=(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(NotEqual(Load().Read(), b.Load().Read()));
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator<(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(
        Less(Load().Read(), b.Load().Read(), kWidth, kSignedness));
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator<=(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(
        LessEqual(Load().Read(), b.Load().Read(), kWidth, kSignedness));
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator>(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(
        Greater(Load().Read(), b.Load().Read(), kWidth, kSignedness));
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator>=(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(
        GreaterEqual(Load().Read(), b.Load().Read(), kWidth, kSignedness));
  }

  // LRM 11.4.7: the logical operators, whose answer can be x where either
  // operand can.
  template <IntegralValue B>
  [[nodiscard]] LYRA_FOLDED constexpr auto operator&&(const B& b) const
      -> OneBit<detail::CombinedDomain(kDomain, B::kDomain)> {
    return OneBit<detail::CombinedDomain(kDomain, B::kDomain)>::Filled(
        LogicalAnd(Load().Read(), b.Load().Read()));
  }
  template <IntegralValue B>
  [[nodiscard]] LYRA_FOLDED constexpr auto operator||(const B& b) const
      -> OneBit<detail::CombinedDomain(kDomain, B::kDomain)> {
    return OneBit<detail::CombinedDomain(kDomain, B::kDomain)>::Filled(
        LogicalOr(Load().Read(), b.Load().Read()));
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto operator!() const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(LogicalNot(Load().Read()));
  }

  // LRM 11.4.5 `===`: never unknown, so its answer is a two-state bit. The
  // wildcard equality reads an x or z of its right operand as matching anything
  // (LRM 11.4.6) and can be unknown; the two matches a `casez` and a `casex`
  // make cannot (LRM 12.5.1).
  [[nodiscard]] LYRA_FOLDED constexpr auto CaseEqual(const Integral& b) const
      -> Bit;
  [[nodiscard]] LYRA_FOLDED constexpr auto WildcardEquals(
      const Integral& b) const -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(
        WildcardEqual(Load().Read(), b.Load().Read()));
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto CasezEquals(const Integral& b) const
      -> Bit;
  [[nodiscard]] LYRA_FOLDED constexpr auto CasexEquals(const Integral& b) const
      -> Bit;

  // LRM 11.4.7 `<->`, whose answer can be x where either operand can.
  template <IntegralValue B>
  [[nodiscard]] LYRA_FOLDED constexpr auto LogicalEquivalence(const B& b) const
      -> OneBit<detail::CombinedDomain(kDomain, B::kDomain)> {
    return OneBit<detail::CombinedDomain(kDomain, B::kDomain)>::Filled(
        lyra::value::LogicalEquivalence(Load().Read(), b.Load().Read()));
  }

  // LRM 11.4.8 `~^`.
  [[nodiscard]] LYRA_FOLDED constexpr auto BitwiseXnor(const Integral& b) const
      -> Integral {
    Words out;
    lyra::value::BitwiseXnor(
        out.Write(), Load().Read(), b.Load().Read(), kWidth);
    return Integral(out);
  }

  // LRM 11.4.10: the shifts, by an amount of any integral type.
  template <IntegralValue Amount>
  [[nodiscard]] LYRA_FOLDED constexpr auto ShiftLeft(const Amount& amount) const
      -> Integral {
    Words out;
    lyra::value::ShiftLeft(
        out.Write(), Load().Read(), kWidth, amount.Load().Read());
    return Integral(out);
  }
  template <IntegralValue Amount>
  [[nodiscard]] LYRA_FOLDED constexpr auto LogicalShiftRight(
      const Amount& amount) const -> Integral {
    Words out;
    lyra::value::LogicalShiftRight(
        out.Write(), Load().Read(), kWidth, amount.Load().Read());
    return Integral(out);
  }
  template <IntegralValue Amount>
  [[nodiscard]] LYRA_FOLDED constexpr auto ArithmeticShiftRight(
      const Amount& amount) const -> Integral {
    Words out;
    lyra::value::ArithmeticShiftRight(
        out.Write(), Load().Read(), kWidth, kSignedness, amount.Load().Read());
    return Integral(out);
  }

  // LRM 11.4.9: the reductions, each one bit that is x only where the value
  // can hold one.
  [[nodiscard]] LYRA_FOLDED constexpr auto ReductionAnd() const
      -> OneBit<kDomain> {
    return Reduced(ReductionOp::kAnd);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto ReductionOr() const
      -> OneBit<kDomain> {
    return Reduced(ReductionOp::kOr);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto ReductionXor() const
      -> OneBit<kDomain> {
    return Reduced(ReductionOp::kXor);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto ReductionNand() const
      -> OneBit<kDomain> {
    return Reduced(ReductionOp::kNand);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto ReductionNor() const
      -> OneBit<kDomain> {
    return Reduced(ReductionOp::kNor);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto ReductionXnor() const
      -> OneBit<kDomain> {
    return Reduced(ReductionOp::kXnor);
  }

  // LRM 11.4.12: `{a, b}`, this value in the most significant positions. The
  // answer is unsigned and can hold x or z where either operand can.
  template <IntegralValue B>
  [[nodiscard]] LYRA_FOLDED constexpr auto Concat(const B& b) const -> Integral<
      kWidth + B::kWidth, Signedness::kUnsigned,
      detail::CombinedDomain(kDomain, B::kDomain)> {
    using Result = Integral<
        kWidth + B::kWidth, Signedness::kUnsigned,
        detail::CombinedDomain(kDomain, B::kDomain)>;
    typename Result::Words out;
    lyra::value::Concat(
        out.Write(), Load().Read(), kWidth, b.Load().Read(), B::kWidth);
    return Result::FromWords(out);
  }

  // LRM 11.4.3 power.
  template <IntegralValue Exponent>
  [[nodiscard]] constexpr auto Pow(const Exponent& exponent) const -> Integral {
    Words out;
    Power(
        out.Write(), Load().Read(), kWidth, kSignedness, exponent.Load().Read(),
        Exponent::kWidth, Exponent::kSignedness);
    return Integral(out);
  }

  // LRM 11.4.11: the arms of a conditional whose condition is ambiguous, merged
  // at the arms' own type.
  [[nodiscard]] LYRA_FOLDED constexpr auto MergeConditional(
      const Integral& b) const -> Integral {
    Words out;
    lyra::value::MergeConditional(
        out.Write(), Load().Read(), b.Load().Read(), kWidth);
    return Integral(out);
  }

  // LRM 6.6: two contributions to a net folded under each of the three tables,
  // and a stronger contribution over a weaker one (LRM 28.12.1).
  [[nodiscard]] LYRA_FOLDED constexpr auto ResolveTriState(
      const Integral& b) const -> Integral {
    return Resolved(NetResolution::kTriState, b);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto ResolveWiredAnd(
      const Integral& b) const -> Integral {
    return Resolved(NetResolution::kWiredAnd, b);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto ResolveWiredOr(
      const Integral& b) const -> Integral {
    return Resolved(NetResolution::kWiredOr, b);
  }
  [[nodiscard]] LYRA_FOLDED constexpr auto Dominating(
      const Integral& weaker) const -> Integral {
    Words out;
    Dominate(out.Write(), Load().Read(), weaker.Load().Read());
    return Integral(out);
  }

  // LRM 20.8.1 `$clog2`, answered as an `integer`; LRM 20.9 `$countbits`,
  // answered as an `int`; and LRM 20.9 `$isunknown`.
  [[nodiscard]] constexpr auto Clog2() const -> Integer;
  template <IntegralValue Control>
  [[nodiscard]] constexpr auto CountBits(const Control& control) const -> Int;
  [[nodiscard]] constexpr auto IsUnknown() const -> Bit;

  // LRM 6.24.3: how many bits the value is as a stream, and those bits, which
  // a stream holds unsigned.
  [[nodiscard]] static constexpr auto BitstreamWidth() -> Int;
  [[nodiscard]] constexpr auto ToBitstream() const
      -> Integral<kWidth, Signedness::kUnsigned, kDomain> {
    const Words held = Load();
    return Integral<kWidth, Signedness::kUnsigned, kDomain>::FromWords(
        held.value, held.unknown);
  }

  // LRM 11.4.14.2: the value's blocks of `block_bits` bits in reversed order,
  // the last one whatever the width leaves of it.
  [[nodiscard]] constexpr auto ReverseBlocks(std::int64_t block_bits) const
      -> Integral {
    Words out;
    lyra::value::ReverseBlocks(
        out.Write(), Load().Read(), kWidth,
        static_cast<std::uint64_t>(block_bits));
    return Integral(out);
  }

  // The bits a part of type `Part` names of this value, held where a write
  // lands (LRM 11.5.1).
  template <IntegralValue Part, IntegralValue P>
  [[nodiscard]] constexpr auto SliceRef(const P& position)
      -> BitsRef<Integral, Part> {
    return BitsRef<Integral, Part>{
        *this, PositionNamed(position.Load().Read(), P::kWidth, P::kSignedness),
        BitPositions{.lsb = 0, .width = kWidth}};
  }

 private:
  LYRA_FOLDED explicit constexpr Integral(const Words& words)
      : value_(PlaneFrom(words.value)), unknown_(UnknownFrom(words.unknown)) {
  }

  [[nodiscard]] LYRA_FOLDED constexpr auto Reduced(ReductionOp op) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(Reduce(Load().Read(), kWidth, op));
  }

  [[nodiscard]] LYRA_FOLDED constexpr auto Resolved(
      NetResolution fold, const Integral& b) const -> Integral {
    Words out;
    Resolve(out.Write(), Load().Read(), b.Load().Read(), fold);
    return Integral(out);
  }

  [[nodiscard]] LYRA_FOLDED static constexpr auto AllOf(FourStateBit bit)
      -> Words {
    Words words;
    FillScalar(words.Write(), kWidth, bit);
    return words;
  }

  LYRA_FOLDED static constexpr void LoadPlane(
      const Plane& plane, std::array<std::uint64_t, kWords>& words) {
    if constexpr (kWidth <= 64) {
      words[0] = plane;
    } else {
      words = plane;
    }
  }

  [[nodiscard]] LYRA_FOLDED static constexpr auto PlaneFrom(
      const std::array<std::uint64_t, kWords>& words) -> Plane {
    if constexpr (kWidth <= 64) {
      return static_cast<Plane>(words[0]);
    } else {
      return words;
    }
  }

  [[nodiscard]] LYRA_FOLDED static constexpr auto UnknownFrom(
      const std::array<std::uint64_t, kFourState ? kWords : 0>& words)
      -> UnknownPlane {
    if constexpr (kFourState) {
      return PlaneFrom(words);
    } else {
      return detail::NoPlane{};
    }
  }

  Plane value_;
  [[no_unique_address]] UnknownPlane unknown_;
};

// A value is exactly the bytes its type lays it out in, which is what lets code
// compiled without its type read and write one where it lies.
static_assert(sizeof(Bit) == IntegralBytesFor(1, StateDomain::kTwoState));
static_assert(sizeof(Logic) == IntegralBytesFor(1, StateDomain::kFourState));
static_assert(sizeof(Int) == IntegralBytesFor(32, StateDomain::kTwoState));
static_assert(sizeof(Time) == IntegralBytesFor(64, StateDomain::kFourState));
static_assert(
    sizeof(LogicVector<100>) == IntegralBytesFor(100, StateDomain::kFourState));
static_assert(
    alignof(LogicVector<12>) == IntegralAlignFor(12) &&
    alignof(BitVector<100>) == IntegralAlignFor(100));

// An integral type as code compiled once for every integral type is handed it:
// how many bits a value has, read as which signedness, each holding which
// states.
struct IntegralShape {
  std::uint64_t width = 0;
  Signedness signedness = Signedness::kUnsigned;
  StateDomain domain = StateDomain::kTwoState;

  [[nodiscard]] constexpr auto IsFourState() const -> bool {
    return domain == StateDomain::kFourState;
  }
  [[nodiscard]] constexpr auto Bytes() const -> std::size_t {
    return IntegralBytesFor(width, domain);
  }

  auto operator==(const IntegralShape&) const -> bool = default;
};

template <IntegralValue T>
inline constexpr IntegralShape kShapeOf{
    .width = T::kWidth, .signedness = T::kSignedness, .domain = T::kDomain};

// The planes of one value of a type the code holding it was compiled without,
// loaded out of the bytes the value is laid out in as the words every operation
// takes, and stored back into bytes laid out the same way.
class LoadedWords {
 public:
  // A value of `shape` with every position clear in both planes.
  explicit LoadedWords(IntegralShape shape)
      : shape_(shape),
        words_(
            WordCountForBits(shape.width) * (shape.IsFourState() ? 2U : 1U),
            std::uint64_t{0}) {
  }

  [[nodiscard]] static auto Load(const void* bytes, IntegralShape shape)
      -> LoadedWords {
    static_assert(
        std::endian::native == std::endian::little,
        "a plane narrower than a word is the low bytes of one");
    LoadedWords loaded(shape);
    const std::size_t plane = PlaneBytesFor(shape.width);
    const std::span<const std::byte> from(
        static_cast<const std::byte*>(bytes), shape.Bytes());
    if (shape.width <= 64U) {
      for (std::size_t p = 0; p * plane < from.size(); ++p) {
        std::memcpy(
            &loaded.words_[p], from.subspan(p * plane, plane).data(), plane);
      }
    } else {
      std::memcpy(loaded.words_.data(), from.data(), from.size());
    }
    return loaded;
  }

  void StoreTo(void* bytes) const {
    const std::size_t plane = PlaneBytesFor(shape_.width);
    const std::span<std::byte> to(
        static_cast<std::byte*>(bytes), shape_.Bytes());
    if (shape_.width <= 64U) {
      for (std::size_t p = 0; p * plane < to.size(); ++p) {
        std::memcpy(to.subspan(p * plane, plane).data(), &words_[p], plane);
      }
    } else {
      std::memcpy(to.data(), words_.data(), to.size());
    }
  }

  [[nodiscard]] auto Shape() const -> IntegralShape {
    return shape_;
  }

  [[nodiscard]] auto Read() const -> ConstPlanes {
    const std::span<const std::uint64_t> all(words_.data(), words_.size());
    const std::size_t count = WordCountForBits(shape_.width);
    return ConstPlanes{
        .value = all.first(count), .unknown = all.subspan(count)};
  }
  [[nodiscard]] auto Write() -> Planes {
    const std::span<std::uint64_t> all(words_.data(), words_.size());
    const std::size_t count = WordCountForBits(shape_.width);
    return Planes{.value = all.first(count), .unknown = all.subspan(count)};
  }

  [[nodiscard]] auto View() const -> ConstIntegralView {
    return ConstIntegralView{
        .planes = Read(),
        .width = shape_.width,
        .signedness = shape_.signedness};
  }
  [[nodiscard]] auto MutableView() -> IntegralView {
    return IntegralView{.planes = Write(), .width = shape_.width};
  }

 private:
  IntegralShape shape_;
  // The value plane's words, then the unknown plane's where there is one.
  base::FixedArray<std::uint64_t, 4> words_;
};

// The members that answer at one fixed integral type, which is a type only
// once the definition above is whole.
template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::CaseEqual(
    const Integral& b) const -> Bit {
  return Bit::FromBool(lyra::value::CaseEqual(Load().Read(), b.Load().Read()));
}

template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::CasezEquals(
    const Integral& b) const -> Bit {
  return Bit::FromBool(CasezMatch(Load().Read(), b.Load().Read()));
}

template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::CasexEquals(
    const Integral& b) const -> Bit {
  return Bit::FromBool(CasexMatch(Load().Read(), b.Load().Read()));
}

template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::Clog2() const
    -> Integer {
  return Integer::FromInt(CeilLog2(Load().Read(), kWidth));
}

template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
template <IntegralValue Control>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::CountBits(
    const Control& control) const -> Int {
  return Int::FromInt(
      lyra::value::CountBits(
          Load().Read(), kWidth, control.Load().Read(), Control::kWidth));
}

template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::IsUnknown()
    const -> Bit {
  return Bit::FromBool(HasUnknown());
}

template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::BitstreamWidth()
    -> Int {
  return Int::FromInt(static_cast<std::int64_t>(kWidth));
}

// A value of one integral type as another (LRM 6.24.1), which a two-state type
// holds with each x or z as 0.
template <IntegralValue To, IntegralValue From>
[[nodiscard]] LYRA_FOLDED constexpr auto Convert(const From& from) -> To {
  typename To::Words out;
  lyra::value::Convert(
      out.Write(), To::kWidth, from.Load().Read(), From::kWidth,
      From::kSignedness);
  return To::FromWords(out);
}

// LRM 11.4.12.1: `{n{a}}`, as many copies as the result `R` is wide.
template <IntegralValue R, IntegralValue T>
[[nodiscard]] constexpr auto Replicate(const T& a) -> R {
  static_assert(
      R::kWidth % T::kWidth == 0 && R::kDomain == T::kDomain,
      "a replication is whole copies of its operand");
  typename R::Words out;
  lyra::value::Replicate(out.Write(), R::kWidth, a.Load().Read(), T::kWidth);
  return R::FromWords(out);
}

// The number a position holds, or none where it holds x or z or a magnitude no
// value reaches (LRM 11.5.1, 7.4.5).
template <IntegralValue T>
[[nodiscard]] LYRA_FOLDED constexpr auto ReadPosition(const T& position)
    -> std::optional<std::int64_t> {
  return PositionNamed(position.Load().Read(), T::kWidth, T::kSignedness);
}

// The position an index names, in the type position arithmetic is done in.
template <IntegralValue T>
[[nodiscard]] LYRA_FOLDED constexpr auto ToPosition(const T& index)
    -> Position {
  Position::Words out;
  lyra::value::ToPosition(
      out.Write(), index.Load().Read(), T::kWidth, T::kSignedness);
  return Position::FromWords(out);
}

// LRM 11.5.1: as many bits as the result `R` is wide, from `start`, counted
// from the least significant bit. A position outside the value reads x, and a
// two-state `R` holds each x or z read as 0.
template <IntegralValue R, IntegralValue T>
[[nodiscard]] LYRA_FOLDED constexpr auto ExtractBits(
    const T& a, std::int64_t start) -> R {
  typename R::Words out;
  Extract(out.Write(), R::kWidth, a.Load().Read(), T::kWidth, start);
  return R::FromWords(out);
}

// The same bits from the position a value names, which read as `R`'s default
// where it names none.
template <IntegralValue R, IntegralValue T, IntegralValue P>
[[nodiscard]] LYRA_FOLDED constexpr auto Slice(const T& a, const P& position)
    -> R {
  typename R::Words out;
  lyra::value::Slice(
      out.Write(), R::kWidth, a.Load().Read(), T::kWidth,
      position.Load().Read(), P::kWidth, P::kSignedness);
  return R::FromWords(out);
}

// LRM 11.5.1: writes `bits` at `start`, leaving every other position; a
// position outside the value is not written. Answers the positions written.
template <IntegralValue T, IntegralValue Bits>
LYRA_FOLDED constexpr auto InsertBits(
    T& a, std::int64_t start, const Bits& bits) -> std::optional<BitPositions> {
  typename T::Words words = a.Load();
  const std::optional<BitPositions> written =
      Insert(words.Write(), T::kWidth, bits.Load().Read(), Bits::kWidth, start);
  a = T::FromWords(words);
  return written;
}

// The same write, landing only on the positions `within` names: bits written
// through a part of a part reach nothing outside the outer part. Answers the
// positions written.
template <IntegralValue T, IntegralValue Bits>
LYRA_FOLDED constexpr auto InsertBitsWithin(
    T& a, std::int64_t start, const Bits& bits, BitPositions within)
    -> std::optional<BitPositions> {
  typename T::Words words = a.Load();
  const std::optional<BitPositions> written = InsertWithin(
      words.Write(), T::kWidth, bits.Load().Read(), Bits::kWidth, start,
      within);
  a = T::FromWords(words);
  return written;
}

// Some bits of a value held where a write lands (LRM 11.5.1): the bits of a
// value of `Part` from `start`, counted from the value's least significant bit,
// which lie inside the positions `within` names -- all of the value's, or those
// of the part this one was taken in. `Part` is the type the bits are read and
// written at. A write lands on the bits and leaves every other position, and a
// start that names no position names none to write. A part taken in these
// composes the same way.
template <IntegralValue T, IntegralValue Part>
class BitsRef {
 public:
  constexpr BitsRef(
      T& root, std::optional<std::int64_t> start, BitPositions within)
      : root_(&root), start_(start), within_(within) {
  }

  // Writes `bits`, answering the positions of the value it reached.
  constexpr auto Assign(const Part& bits) -> std::optional<BitPositions> {
    if (!start_) {
      return std::nullopt;
    }
    return InsertBitsWithin(*root_, *start_, bits, within_);
  }

  constexpr auto operator=(const Part& bits) -> BitsRef& {
    Assign(bits);
    return *this;
  }

  // The bits as they stand: `Part`'s default where the start names no
  // position, and x at each position outside the value.
  [[nodiscard]] constexpr auto Read() const -> Part {
    if (!start_) {
      return Part{};
    }
    return ExtractBits<Part>(*root_, *start_);
  }

  // The bits of a value of `Inner` at `position` within these.
  template <IntegralValue Inner, IntegralValue P>
  [[nodiscard]] constexpr auto SliceRef(const P& position) const
      -> BitsRef<T, Inner> {
    const std::optional<std::int64_t> offset = ReadPosition(position);
    const std::optional<std::int64_t> start =
        start_ && offset ? std::optional{*start_ + *offset} : std::nullopt;
    return BitsRef<T, Inner>{*root_, start, Inside()};
  }

  [[nodiscard]] constexpr auto Root() const -> const T& {
    return *root_;
  }

  // The positions of the value a write through these bits reaches, none where
  // they name no position or lie wholly outside it.
  [[nodiscard]] constexpr auto Reached() const -> std::optional<BitPositions> {
    const BitPositions inside = Inside();
    if (inside.width == 0) {
      return std::nullopt;
    }
    return inside;
  }

 private:
  // The value's positions these bits lie in, which bound a part taken in them.
  [[nodiscard]] constexpr auto Inside() const -> BitPositions {
    if (!start_) {
      return BitPositions{};
    }
    const std::int64_t from =
        std::max(*start_, static_cast<std::int64_t>(within_.lsb));
    const std::int64_t to = std::min(
        *start_ + static_cast<std::int64_t>(Part::kWidth),
        static_cast<std::int64_t>(within_.lsb + within_.width));
    if (from >= to) {
      return BitPositions{};
    }
    return BitPositions{
        .lsb = static_cast<std::uint64_t>(from),
        .width = static_cast<std::uint64_t>(to - from)};
  }

  T* root_;
  std::optional<std::int64_t> start_;
  BitPositions within_;
};

// LRM 6.24.3: a value's bits written into a stream of `stream_width` bits
// below the `filled` most significant positions already written, the first
// item of a stream being its most significant. Answers how many are filled
// once these are.
template <IntegralValue T>
constexpr auto WriteToStream(
    const T& a, Planes stream, std::uint64_t stream_width, std::uint64_t filled)
    -> std::uint64_t {
  Insert(
      stream, stream_width, a.Load().Read(), T::kWidth,
      static_cast<std::int64_t>(stream_width - filled - T::kWidth));
  return filled + T::kWidth;
}

// LRM 11.4.14.3: the value of `T` a stream holds below its `taken` most
// significant positions, which a two-state `T` reads with each x or z as 0.
template <IntegralValue T>
[[nodiscard]] constexpr auto ReadFromStream(
    ConstPlanes stream, std::uint64_t stream_width, std::uint64_t taken) -> T {
  typename T::Words bits;
  Extract(
      bits.Write(), T::kWidth, stream, stream_width,
      static_cast<std::int64_t>(stream_width - taken - T::kWidth));
  return T::FromWords(bits);
}

// LRM 5.9: text taken as a value of `R`, its last character the least
// significant byte.
template <IntegralValue R>
[[nodiscard]] constexpr auto FromBytes(std::span<const char> bytes) -> R {
  typename R::Words out;
  lyra::value::FromBytes(out.Write(), R::kWidth, bytes);
  return R::FromWords(out);
}

// A value's bytes, most significant first, the top one holding whatever the
// width leaves of it; a byte with an x or z bit reads 0. This is the order a
// value read as text takes (LRM 6.16, 21.2.1.7).
[[nodiscard]] inline auto BytesOf(const ConstIntegralView& value)
    -> std::string {
  const std::uint64_t count = (value.width + 7U) / 8U;
  std::string out;
  out.reserve(static_cast<std::size_t>(count));
  for (std::uint64_t i = count; i-- > 0;) {
    out.push_back(
        static_cast<char>(ByteAt(value.planes, value.width, i).value_or(0)));
  }
  return out;
}

// What an integral value claims, by one two-state type and one four-state one.
static_assert(LyraValue<Int>);
static_assert(LyraValue<LogicVector<8>>);
static_assert(CaseEqualComparable<Int>);
static_assert(CaseEqualComparable<LogicVector<8>>);
static_assert(WildcardComparable<Int>);
static_assert(WildcardComparable<LogicVector<8>>);
static_assert(Ordered<Int>);
static_assert(Ordered<LogicVector<8>>);
static_assert(BitstreamSizable<Int>);
static_assert(BitstreamSizable<LogicVector<8>>);
static_assert(ConditionallyMergeable<Int>);
static_assert(ConditionallyMergeable<LogicVector<8>>);
static_assert(NetResolvable<LogicVector<8>>);

}  // namespace lyra::value

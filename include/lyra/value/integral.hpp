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
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/base/fixed_array.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/frame_bound.hpp"
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

// The domain a value of an integral type `width` bits wide is held in. Up to a
// word that is the layout the value's bytes have -- the storage unit one plane
// takes and whether an unknown plane follows it -- because what holds such a
// value reads nothing else of its type. A wider one is its planes' words, as
// many as its width asks for, so all its domain says is which planes it has.
[[nodiscard]] constexpr auto IntegralDomainFor(
    std::uint64_t width, StateDomain domain) -> support::ValueDomain {
  using support::ValueDomain;
  const bool four_state = domain == StateDomain::kFourState;
  if (width > 64) {
    return four_state ? ValueDomain::kLogicWide : ValueDomain::kBitWide;
  }
  const std::size_t plane = PlaneBytesFor(width);
  if (plane == 1) {
    return four_state ? ValueDomain::kLogic8 : ValueDomain::kBit8;
  }
  if (plane == 2) {
    return four_state ? ValueDomain::kLogic16 : ValueDomain::kBit16;
  }
  if (plane == 4) {
    return four_state ? ValueDomain::kLogic32 : ValueDomain::kBit32;
  }
  return four_state ? ValueDomain::kLogic64 : ValueDomain::kBit64;
}

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

// How many words a value of `shape` is: those of its value plane and then,
// where it can hold x or z, those of its unknown plane.
[[nodiscard]] constexpr auto WordCountOf(IntegralShape shape) -> std::size_t {
  return WordCountForBits(shape.width) * (shape.IsFourState() ? 2U : 1U);
}

// Whether a value of `shape` is laid out as 64-bit words, which every value
// whose plane takes a whole word or more is. A narrower one is laid out as one
// smaller machine integer per plane.
[[nodiscard]] constexpr auto IsLaidOutAsWords(IntegralShape shape) -> bool {
  return PlaneBytesFor(shape.width) >= sizeof(std::uint64_t);
}

// The value of `shape` whose words lie at `words`, the value plane's and then
// the unknown plane's where there is one, read and written.
[[nodiscard]] inline auto ViewOfWords(
    const std::uint64_t* words, IntegralShape shape) -> ConstIntegralView {
  const std::span<const std::uint64_t> all(words, WordCountOf(shape));
  const std::size_t count = WordCountForBits(shape.width);
  return ConstIntegralView{
      .value = all.first(count),
      .unknown = all.subspan(count),
      .width = shape.width,
      .signedness = shape.signedness};
}
[[nodiscard]] inline auto ViewOfWords(std::uint64_t* words, IntegralShape shape)
    -> IntegralView {
  const std::span<std::uint64_t> all(words, WordCountOf(shape));
  const std::size_t count = WordCountForBits(shape.width);
  return IntegralView{
      .value = all.first(count),
      .unknown = all.subspan(count),
      .width = shape.width};
}

namespace detail {

// The value of `shape` laid out at `bytes` as one `Plane` per plane, read.
template <PlaneWord Plane>
[[nodiscard]] inline auto ViewOfPlanes(const void* bytes, IntegralShape shape)
    -> ConstIntegralViewOf<Plane, 1, std::dynamic_extent> {
  const std::span<const Plane> planes(
      static_cast<const Plane*>(bytes), shape.IsFourState() ? 2U : 1U);
  return {
      .value = planes.template first<1>(),
      .unknown = planes.subspan(1),
      .width = shape.width,
      .signedness = shape.signedness};
}

}  // namespace detail

// Hands `visit` the value of `shape` laid out at `bytes`, read where it lies.
// Which integer a word of it is, only the shape says while the program runs,
// so this is the one place that turns a shape into a view of each kind.
template <typename Visit>
auto VisitLaidOut(const void* bytes, IntegralShape shape, Visit visit)
    -> decltype(auto) {
  const std::size_t plane = PlaneBytesFor(shape.width);
  if (plane == sizeof(std::uint8_t)) {
    return visit(detail::ViewOfPlanes<std::uint8_t>(bytes, shape));
  }
  if (plane == sizeof(std::uint16_t)) {
    return visit(detail::ViewOfPlanes<std::uint16_t>(bytes, shape));
  }
  if (plane == sizeof(std::uint32_t)) {
    return visit(detail::ViewOfPlanes<std::uint32_t>(bytes, shape));
  }
  return visit(ViewOfWords(static_cast<const std::uint64_t*>(bytes), shape));
}

// The words of one value laid out at some bytes, read by code compiled once
// over words. A value laid out as words is read where it lies, and the bytes
// have to outlive this. A narrower one is laid out as smaller machine
// integers, so its words are made of those bytes and held here, and a view of
// them lasts as long as this does.
class LoadedWords {
 public:
  [[nodiscard]] static auto Load(const void* bytes, IntegralShape shape)
      -> LoadedWords {
    if (IsLaidOutAsWords(shape)) {
      return {
          shape, Borrowed{.words = static_cast<const std::uint64_t*>(bytes)}};
    }
    return {
        shape, VisitLaidOut(bytes, shape, [](AnyConstIntegralView auto read) {
          return Owned{
              .words = {WordAt(read.value, 0), WordOrZero(read.unknown, 0)}};
        })};
  }

  [[nodiscard]] auto Shape() const -> IntegralShape {
    return shape_;
  }

  [[nodiscard]] auto View() const -> ConstIntegralView {
    return ViewOfWords(
        std::visit(
            Overloaded{
                [](const Owned& made) { return made.words.data(); },
                [](const Borrowed& lying) { return lying.words; }},
            words_),
        shape_);
  }

 private:
  // The word of each plane of a value narrower than one.
  struct Owned {
    std::array<std::uint64_t, 2> words;
  };
  struct Borrowed {
    const std::uint64_t* words;
  };
  using Words = std::variant<Owned, Borrowed>;

  LoadedWords(IntegralShape shape, Words words) : shape_(shape), words_(words) {
  }

  IntegralShape shape_;
  Words words_;
};

class ScopedWordsWrite;

// The words of one value while code compiled once over words writes it. They
// are held here until the value is laid out somewhere, or, for a value laid
// out as words that is built or changed where it lies, they are the bytes
// there.
class WrittenWords {
 public:
  // A value of `shape` with every position clear in both planes.
  explicit WrittenWords(IntegralShape shape)
      : shape_(shape),
        words_(Owned{.words = {WordCountOf(shape), std::uint64_t{0}}}) {
  }

  // A value of its own that starts as the one of `shape` laid out at `bytes`.
  [[nodiscard]] static auto CopyOf(const void* bytes, IntegralShape shape)
      -> WrittenWords {
    const LoadedWords loaded = LoadedWords::Load(bytes, shape);
    const ConstIntegralView read = loaded.View();
    WrittenWords copy(shape);
    const IntegralView written = copy.MutableView();
    std::ranges::copy(read.value, written.value.begin());
    std::ranges::copy(read.unknown, written.unknown.begin());
    return copy;
  }

  // A value of `shape` with every position clear, built where it will lie: a
  // value laid out as words is written into `bytes` as it is built, and a
  // narrower one is laid out there when it is stored. Nothing the value is
  // made from may lie at `bytes`, which are cleared before any of it is read.
  [[nodiscard]] static auto ZeroedAt(void* bytes, IntegralShape shape)
      -> WrittenWords {
    if (!IsLaidOutAsWords(shape)) {
      return WrittenWords(shape);
    }
    WrittenWords lying{
        shape, Borrowed{.words = static_cast<std::uint64_t*>(bytes)}};
    const IntegralView written = lying.MutableView();
    std::ranges::fill(written.value, std::uint64_t{0});
    std::ranges::fill(written.unknown, std::uint64_t{0});
    return lying;
  }

  // Lays the value out at `bytes`, which does nothing where it was written
  // there.
  void StoreTo(void* bytes) const {
    static_assert(
        std::endian::native == std::endian::little,
        "a plane narrower than a word is the low bytes of one");
    const std::uint64_t* const words = Data();
    const std::span<std::byte> to(
        static_cast<std::byte*>(bytes), shape_.Bytes());
    if (IsLaidOutAsWords(shape_)) {
      if (words != bytes) {
        std::memcpy(to.data(), words, to.size());
      }
      return;
    }
    const std::span<const std::uint64_t> all(words, WordCountOf(shape_));
    const std::size_t plane = PlaneBytesFor(shape_.width);
    for (std::size_t p = 0; p * plane < to.size(); ++p) {
      std::memcpy(to.subspan(p * plane, plane).data(), &all[p], plane);
    }
  }

  [[nodiscard]] auto View() const -> ConstIntegralView {
    return ViewOfWords(Data(), shape_);
  }
  [[nodiscard]] auto MutableView() -> IntegralView {
    return ViewOfWords(
        std::visit(
            Overloaded{
                [](Owned& owned) { return owned.words.data(); },
                [](Borrowed& lying) { return lying.words; }},
            words_),
        shape_);
  }

 private:
  friend class ScopedWordsWrite;

  // The value of `shape` laid out at `bytes`, to be changed: where it lies if
  // it is laid out as words, and otherwise as a copy that storing lays back
  // out, which is why only the scope that stores it back may ask.
  [[nodiscard]] static auto At(void* bytes, IntegralShape shape)
      -> WrittenWords {
    if (IsLaidOutAsWords(shape)) {
      return {shape, Borrowed{.words = static_cast<std::uint64_t*>(bytes)}};
    }
    return CopyOf(bytes, shape);
  }

  struct Owned {
    base::FixedArray<std::uint64_t, 4> words;
  };
  struct Borrowed {
    std::uint64_t* words;
  };
  using Words = std::variant<Owned, Borrowed>;

  WrittenWords(IntegralShape shape, Words words)
      : shape_(shape), words_(std::move(words)) {
  }

  // The value plane's words, then the unknown plane's where there is one.
  [[nodiscard]] auto Data() const -> const std::uint64_t* {
    return std::visit(
        Overloaded{
            [](const Owned& owned) { return owned.words.data(); },
            [](const Borrowed& lying) -> const std::uint64_t* {
              return lying.words;
            }},
        words_);
  }

  IntegralShape shape_;
  Words words_;
};

// A value laid out at some bytes while code compiled once over words writes
// it, laid back out at those bytes when the writing ends.
class ScopedWordsWrite {
 public:
  ScopedWordsWrite(void* bytes, IntegralShape shape)
      : words_(WrittenWords::At(bytes, shape)), bytes_(bytes) {
  }
  ScopedWordsWrite(const ScopedWordsWrite&) = delete;
  ScopedWordsWrite(ScopedWordsWrite&&) = delete;
  auto operator=(const ScopedWordsWrite&) -> ScopedWordsWrite& = delete;
  auto operator=(ScopedWordsWrite&&) -> ScopedWordsWrite& = delete;
  ~ScopedWordsWrite() {
    words_.StoreTo(bytes_);
  }

  [[nodiscard]] auto MutableView() -> IntegralView {
    return words_.MutableView();
  }

 private:
  WrittenWords words_;
  void* bytes_;
};

namespace detail {

// The integer a word of a plane is: the smallest machine integer that holds
// the width up to 64 bits, and a 64-bit word above.
template <std::uint64_t kWidth>
using PlaneOf = std::conditional_t<
    PlaneBytesFor(kWidth) == 1, std::uint8_t,
    std::conditional_t<
        PlaneBytesFor(kWidth) == 2, std::uint16_t,
        std::conditional_t<
            PlaneBytesFor(kWidth) == 4, std::uint32_t, std::uint64_t>>>;

// What a value holds: the words of its value plane and then, where it can hold
// x or z, those of its unknown plane, every position clear until written. A
// value no wider than 64 bits has one word per plane, the smallest machine
// integer that holds its width, and a wider one has 64-bit words.
template <PlaneWord WordArg, std::size_t kWords, bool kFourState>
struct PlaneWords {
  using Word = WordArg;
  static constexpr std::size_t kUnknownWords = kFourState ? kWords : 0;

  std::array<Word, kWords + kUnknownWords> words{};

  [[nodiscard]] constexpr auto Value() const -> std::span<const Word, kWords> {
    return std::span(words).template first<kWords>();
  }
  [[nodiscard]] constexpr auto Value() -> std::span<Word, kWords> {
    return std::span(words).template first<kWords>();
  }
  [[nodiscard]] constexpr auto Unknown() const
      -> std::span<const Word, kUnknownWords> {
    return std::span(words).template last<kUnknownWords>();
  }
  [[nodiscard]] constexpr auto Unknown() -> std::span<Word, kUnknownWords> {
    return std::span(words).template last<kUnknownWords>();
  }
  // Where the value's layout lies.
  [[nodiscard]] auto Bytes() const -> const void* {
    return words.data();
  }
  [[nodiscard]] auto Bytes() -> void* {
    return words.data();
  }

  // Whether two hold the same words, asked a word at a time. Asked of the
  // array as a whole, a host library may compare it as a run of bytes through
  // a call, which every store into a variable would then make.
  constexpr auto operator==(const PlaneWords& other) const -> bool {
    const std::span<const Word> mine(words);
    const std::span<const Word> theirs(other.words);
    bool same = true;
    for (std::size_t i = 0; i < mine.size(); ++i) {
      same = same && mine[i] == theirs[i];
    }
    return same;
  }
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
// The value is its planes and nothing else, with every position above the
// width clear, and nothing about it is looked up while the program runs; each
// operation is the language's operator over its words at this width. Up to a
// word a plane is the machine integer of its size. Above one the planes are
// words, the value plane's and then the unknown plane's when four-state, lying
// in the value.
template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
class Integral {
  static_assert(kWidthArg >= 1, "an integral value has at least one bit");

  // What the value holds, which its type decides and nothing outside reads.
  using Storage = detail::PlaneWords<
      detail::PlaneOf<kWidthArg>, WordCountForBits(kWidthArg),
      kDomainArg == StateDomain::kFourState>;

 public:
  static constexpr std::uint64_t kWidth = kWidthArg;
  static constexpr Signedness kSignedness = kSignednessArg;
  static constexpr StateDomain kDomain = kDomainArg;
  static constexpr bool kFourState = kDomain == StateDomain::kFourState;
  static constexpr std::size_t kWords = WordCountForBits(kWidth);
  static constexpr std::size_t kUnknownWords = kFourState ? kWords : 0;
  // The integer a word of each plane is held in.
  using Word = typename Storage::Word;

  constexpr Integral() {
    FillDefault(MutableView());
  }

  // A value with every position clear in both planes.
  [[nodiscard]] static constexpr auto Zero() -> Integral {
    return Filled(FourStateBit::kZero);
  }

  // The planes as a constant states them, value then unknown -- the second
  // empty for a two-state value.
  [[nodiscard]] static constexpr auto FromWords(
      const std::array<std::uint64_t, kWords>& value,
      const std::array<std::uint64_t, kUnknownWords>& unknown) -> Integral {
    Integral out = Zero();
    const auto written = out.MutableView();
    for (std::size_t i = 0; i < kWords; ++i) {
      SetWord(written.value, i, value[i]);
    }
    for (std::size_t i = 0; i < kUnknownWords; ++i) {
      SetWord(written.unknown, i, unknown[i]);
    }
    return out;
  }

  // An integer carried into this type, its sign repeated into every position
  // above 64 (LRM 11.6.1).
  [[nodiscard]] static constexpr auto FromInt(std::int64_t value) -> Integral {
    Integral out = Zero();
    lyra::value::FromInt(out.MutableView(), value);
    return out;
  }

  [[nodiscard]] static constexpr auto FromBool(bool value) -> Integral {
    return FromInt(value ? 1 : 0);
  }

  // Every position holding one scalar: what a net shows where nothing drives
  // it, and what a driver contributes where it does not drive (LRM 6.6). A
  // two-state value holds an x or z as 0.
  [[nodiscard]] static constexpr auto Filled(FourStateBit bit) -> Integral {
    return Integral{bit};
  }

  // The value where it lies, read and written. A write that reaches part of a
  // value wider than a word touches the words that part is in and no others.
  [[nodiscard]] constexpr auto View() const
      -> ConstIntegralViewOf<Word, kWords, kUnknownWords> {
    return {
        .value = storage_.Value(),
        .unknown = storage_.Unknown(),
        .width = kWidth,
        .signedness = kSignedness};
  }
  [[nodiscard]] constexpr auto MutableView()
      -> IntegralViewOf<Word, kWords, kUnknownWords> {
    return {
        .value = storage_.Value(),
        .unknown = storage_.Unknown(),
        .width = kWidth};
  }

  // The value as code compiled once over 64-bit words reads and writes one:
  // its words where they are that already, and words made of its machine
  // integers where it is narrower, laid back out when the writing ends.
  [[nodiscard]] auto AsWords() const -> LoadedWords {
    return LoadedWords::Load(storage_.Bytes(), kShapeOf<Integral>);
  }
  [[nodiscard]] auto AsMutableWords() -> ScopedWordsWrite {
    return ScopedWordsWrite(storage_.Bytes(), kShapeOf<Integral>);
  }

  // LRM 9.4.2: whether two values are the same bits, x and z included, which
  // is what decides that a write changed a variable. Every position above the
  // width is clear, so the bits are the same exactly where the planes are.
  [[nodiscard]] constexpr auto IsBitIdentical(const Integral& other) const
      -> bool {
    return storage_ == other.storage_;
  }

  [[nodiscard]] constexpr auto HasUnknown() const -> bool {
    if constexpr (kFourState) {
      return lyra::value::HasUnknown(View());
    } else {
      return false;
    }
  }

  [[nodiscard]] constexpr auto Truth() const -> Truthiness {
    return lyra::value::Truth(View());
  }

  // LRM 12.4: only a definitively-one bit makes a condition true.
  [[nodiscard]] constexpr auto IsTruthy() const -> bool {
    return Truth() == Truthiness::kKnownNonzero;
  }
  [[nodiscard]] constexpr explicit operator bool() const {
    return IsTruthy();
  }

  [[nodiscard]] constexpr auto ToInt64() const -> std::int64_t {
    return lyra::value::ToInt64(View());
  }

  [[nodiscard]] constexpr auto Lsb() const -> FourStateBit {
    return LeastSignificantBit(View());
  }

  // LRM 11.4.3: arithmetic at the operands' own type.
  [[nodiscard]] constexpr auto operator+(const Integral& b) const -> Integral {
    Integral out = Zero();
    Add(out.MutableView(), View(), b.View());
    return out;
  }
  [[nodiscard]] constexpr auto operator-(const Integral& b) const -> Integral {
    Integral out = Zero();
    Subtract(out.MutableView(), View(), b.View());
    return out;
  }
  [[nodiscard]] constexpr auto operator*(const Integral& b) const -> Integral {
    Integral out = Zero();
    Multiply(out.MutableView(), View(), b.View());
    return out;
  }
  [[nodiscard]] constexpr auto operator/(const Integral& b) const -> Integral {
    Integral out = Zero();
    Divide(out.MutableView(), View(), b.View());
    return out;
  }
  [[nodiscard]] constexpr auto operator%(const Integral& b) const -> Integral {
    Integral out = Zero();
    Modulo(out.MutableView(), View(), b.View());
    return out;
  }
  [[nodiscard]] constexpr auto operator-() const -> Integral {
    Integral out = Zero();
    Negate(out.MutableView(), View());
    return out;
  }

  // LRM 11.4.8: the bitwise operators.
  [[nodiscard]] constexpr auto operator&(const Integral& b) const -> Integral {
    Integral out = Zero();
    BitwiseAnd(out.MutableView(), View(), b.View());
    return out;
  }
  [[nodiscard]] constexpr auto operator|(const Integral& b) const -> Integral {
    Integral out = Zero();
    BitwiseOr(out.MutableView(), View(), b.View());
    return out;
  }
  [[nodiscard]] constexpr auto operator^(const Integral& b) const -> Integral {
    Integral out = Zero();
    BitwiseXor(out.MutableView(), View(), b.View());
    return out;
  }
  [[nodiscard]] constexpr auto operator~() const -> Integral {
    Integral out = Zero();
    BitwiseNot(out.MutableView(), View());
    return out;
  }

  // LRM 11.4.4, 11.4.5: a comparison answers one bit, which can be x only
  // where its operands can.
  [[nodiscard]] constexpr auto operator==(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(Equal(View(), b.View()));
  }
  [[nodiscard]] constexpr auto operator!=(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(NotEqual(View(), b.View()));
  }
  [[nodiscard]] constexpr auto operator<(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(Less(View(), b.View()));
  }
  [[nodiscard]] constexpr auto operator<=(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(LessEqual(View(), b.View()));
  }
  [[nodiscard]] constexpr auto operator>(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(Greater(View(), b.View()));
  }
  [[nodiscard]] constexpr auto operator>=(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(GreaterEqual(View(), b.View()));
  }

  // LRM 11.4.7: the logical operators, whose answer can be x where either
  // operand can.
  template <IntegralValue B>
  [[nodiscard]] constexpr auto operator&&(const B& b) const
      -> OneBit<detail::CombinedDomain(kDomain, B::kDomain)> {
    return OneBit<detail::CombinedDomain(kDomain, B::kDomain)>::Filled(
        LogicalAnd(View(), b.View()));
  }
  template <IntegralValue B>
  [[nodiscard]] constexpr auto operator||(const B& b) const
      -> OneBit<detail::CombinedDomain(kDomain, B::kDomain)> {
    return OneBit<detail::CombinedDomain(kDomain, B::kDomain)>::Filled(
        LogicalOr(View(), b.View()));
  }
  [[nodiscard]] constexpr auto operator!() const -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(LogicalNot(View()));
  }

  // LRM 11.4.5 `===`: never unknown, so its answer is a two-state bit. The
  // wildcard equality reads an x or z of its right operand as matching anything
  // (LRM 11.4.6) and can be unknown; the two matches a `casez` and a `casex`
  // make cannot (LRM 12.5.1).
  [[nodiscard]] constexpr auto CaseEqual(const Integral& b) const -> Bit;
  [[nodiscard]] constexpr auto WildcardEquals(const Integral& b) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(WildcardEqual(View(), b.View()));
  }
  [[nodiscard]] constexpr auto CasezEquals(const Integral& b) const -> Bit;
  [[nodiscard]] constexpr auto CasexEquals(const Integral& b) const -> Bit;

  // LRM 11.4.7 `<->`, whose answer can be x where either operand can.
  template <IntegralValue B>
  [[nodiscard]] constexpr auto LogicalEquivalence(const B& b) const
      -> OneBit<detail::CombinedDomain(kDomain, B::kDomain)> {
    return OneBit<detail::CombinedDomain(kDomain, B::kDomain)>::Filled(
        lyra::value::LogicalEquivalence(View(), b.View()));
  }

  // LRM 11.4.8 `~^`.
  [[nodiscard]] constexpr auto BitwiseXnor(const Integral& b) const
      -> Integral {
    Integral out = Zero();
    lyra::value::BitwiseXnor(out.MutableView(), View(), b.View());
    return out;
  }

  // LRM 11.4.10: the shifts, by an amount of any integral type.
  template <IntegralValue Amount>
  [[nodiscard]] constexpr auto ShiftLeft(const Amount& amount) const
      -> Integral {
    Integral out = Zero();
    lyra::value::ShiftLeft(out.MutableView(), View(), amount.View());
    return out;
  }
  template <IntegralValue Amount>
  [[nodiscard]] constexpr auto LogicalShiftRight(const Amount& amount) const
      -> Integral {
    Integral out = Zero();
    lyra::value::LogicalShiftRight(out.MutableView(), View(), amount.View());
    return out;
  }
  template <IntegralValue Amount>
  [[nodiscard]] constexpr auto ArithmeticShiftRight(const Amount& amount) const
      -> Integral {
    Integral out = Zero();
    lyra::value::ArithmeticShiftRight(out.MutableView(), View(), amount.View());
    return out;
  }

  // LRM 11.4.9: the reductions, each one bit that is x only where the value
  // can hold one.
  [[nodiscard]] constexpr auto ReductionAnd() const -> OneBit<kDomain> {
    return Reduced(ReductionOp::kAnd);
  }
  [[nodiscard]] constexpr auto ReductionOr() const -> OneBit<kDomain> {
    return Reduced(ReductionOp::kOr);
  }
  [[nodiscard]] constexpr auto ReductionXor() const -> OneBit<kDomain> {
    return Reduced(ReductionOp::kXor);
  }
  [[nodiscard]] constexpr auto ReductionNand() const -> OneBit<kDomain> {
    return Reduced(ReductionOp::kNand);
  }
  [[nodiscard]] constexpr auto ReductionNor() const -> OneBit<kDomain> {
    return Reduced(ReductionOp::kNor);
  }
  [[nodiscard]] constexpr auto ReductionXnor() const -> OneBit<kDomain> {
    return Reduced(ReductionOp::kXnor);
  }

  // LRM 11.4.12: `{a, b}`, this value in the most significant positions. The
  // answer is unsigned and can hold x or z where either operand can.
  template <IntegralValue B>
  [[nodiscard]] constexpr auto Concat(const B& b) const -> Integral<
      kWidth + B::kWidth, Signedness::kUnsigned,
      detail::CombinedDomain(kDomain, B::kDomain)> {
    using Result = Integral<
        kWidth + B::kWidth, Signedness::kUnsigned,
        detail::CombinedDomain(kDomain, B::kDomain)>;
    Result out = Result::Zero();
    lyra::value::Concat(out.MutableView(), View(), b.View());
    return out;
  }

  // LRM 11.4.3 power.
  template <IntegralValue Exponent>
  [[nodiscard]] auto Pow(const Exponent& exponent) const -> Integral {
    Integral out = Zero();
    Power(
        out.AsMutableWords().MutableView(), AsWords().View(),
        exponent.AsWords().View());
    return out;
  }

  // LRM 11.4.11: the arms of a conditional whose condition is ambiguous, merged
  // at the arms' own type.
  [[nodiscard]] constexpr auto MergeConditional(const Integral& b) const
      -> Integral {
    Integral out = Zero();
    lyra::value::MergeConditional(out.MutableView(), View(), b.View());
    return out;
  }

  // LRM 6.6: two contributions to a net folded under each of the three tables,
  // and a stronger contribution over a weaker one (LRM 28.12.1).
  [[nodiscard]] constexpr auto ResolveTriState(const Integral& b) const
      -> Integral {
    return Resolved(NetResolution::kTriState, b);
  }
  [[nodiscard]] constexpr auto ResolveWiredAnd(const Integral& b) const
      -> Integral {
    return Resolved(NetResolution::kWiredAnd, b);
  }
  [[nodiscard]] constexpr auto ResolveWiredOr(const Integral& b) const
      -> Integral {
    return Resolved(NetResolution::kWiredOr, b);
  }
  [[nodiscard]] constexpr auto Dominating(const Integral& weaker) const
      -> Integral {
    Integral out = Zero();
    Dominate(out.MutableView(), View(), weaker.View());
    return out;
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
    using Stream = Integral<kWidth, Signedness::kUnsigned, kDomain>;
    Stream out = Stream::Zero();
    lyra::value::Convert(out.MutableView(), View());
    return out;
  }

  // LRM 11.4.14.2: the value's blocks of `block_bits` bits in reversed order,
  // the last one whatever the width leaves of it.
  [[nodiscard]] auto ReverseBlocks(std::int64_t block_bits) const -> Integral {
    Integral out = Zero();
    lyra::value::ReverseBlocks(
        out.AsMutableWords().MutableView(), AsWords().View(),
        static_cast<std::uint64_t>(block_bits));
    return out;
  }

  // A reference to the bits a part of type `Part` names of this value, which a
  // write lands through where they lie (LRM 11.5.1), as `std::bitset`'s
  // `operator[]` answers a reference to one bit.
  template <IntegralValue Part, IntegralValue P>
  [[nodiscard]] constexpr auto SliceMut(const P& position)
      -> BitsRef<Integral, Part> {
    return BitsRef<Integral, Part>{
        *this, PositionNamed(position.View()),
        BitPositions{.lsb = 0, .width = kWidth}};
  }

 private:
  // Every position holding `fill`, written where the value is kept. The words
  // start clear, so a clear fill has nothing to write.
  explicit constexpr Integral(FourStateBit fill) {
    if (fill != FourStateBit::kZero) {
      FillScalar(MutableView(), fill);
    }
  }

  [[nodiscard]] constexpr auto Reduced(ReductionOp op) const
      -> OneBit<kDomain> {
    return OneBit<kDomain>::Filled(Reduce(View(), op));
  }

  [[nodiscard]] constexpr auto Resolved(
      NetResolution fold, const Integral& b) const -> Integral {
    Integral out = Zero();
    Resolve(out.MutableView(), View(), b.View(), fold);
    return out;
  }

  Storage storage_;
};

// A value is exactly the bytes its type lays it out in, at every width, which
// is what lets code compiled without its type read and write one where it lies.
static_assert(
    sizeof(LogicVector<8 * kMaxInlineBytes>) ==
        IntegralBytesFor(8 * kMaxInlineBytes, StateDomain::kFourState) &&
    sizeof(LogicVector<1U << 24U>) ==
        IntegralBytesFor(1U << 24U, StateDomain::kFourState));
static_assert(sizeof(Bit) == IntegralBytesFor(1, StateDomain::kTwoState));
static_assert(sizeof(Logic) == IntegralBytesFor(1, StateDomain::kFourState));
static_assert(sizeof(Int) == IntegralBytesFor(32, StateDomain::kTwoState));
static_assert(sizeof(Time) == IntegralBytesFor(64, StateDomain::kFourState));
static_assert(
    sizeof(LogicVector<100>) == IntegralBytesFor(100, StateDomain::kFourState));
static_assert(
    alignof(LogicVector<12>) == IntegralAlignFor(12) &&
    alignof(BitVector<100>) == IntegralAlignFor(100));

// The members that answer at one fixed integral type, which is a type only
// once the definition above is whole.
template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::CaseEqual(
    const Integral& b) const -> Bit {
  return Bit::FromBool(lyra::value::CaseEqual(View(), b.View()));
}

template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::CasezEquals(
    const Integral& b) const -> Bit {
  return Bit::FromBool(CasezMatch(View(), b.View()));
}

template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::CasexEquals(
    const Integral& b) const -> Bit {
  return Bit::FromBool(CasexMatch(View(), b.View()));
}

template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::Clog2() const
    -> Integer {
  return Integer::FromInt(CeilLog2(View()));
}

template <
    std::uint64_t kWidthArg, Signedness kSignednessArg, StateDomain kDomainArg>
template <IntegralValue Control>
constexpr auto Integral<kWidthArg, kSignednessArg, kDomainArg>::CountBits(
    const Control& control) const -> Int {
  return Int::FromInt(lyra::value::CountBits(View(), control.View()));
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
[[nodiscard]] constexpr auto Convert(const From& from) -> To {
  To out = To::Zero();
  lyra::value::Convert(out.MutableView(), from.View());
  return out;
}

// LRM 11.4.12.1: `{n{a}}`, as many copies as the result `R` is wide.
template <IntegralValue R, IntegralValue T>
[[nodiscard]] constexpr auto Replicate(const T& a) -> R {
  static_assert(
      R::kWidth % T::kWidth == 0 && R::kDomain == T::kDomain,
      "a replication is whole copies of its operand");
  R out = R::Zero();
  lyra::value::Replicate(out.MutableView(), a.View());
  return out;
}

// The number a position holds, or none where it holds x or z or a magnitude no
// value reaches (LRM 11.5.1, 7.4.5).
template <IntegralValue T>
[[nodiscard]] constexpr auto ReadPosition(const T& position)
    -> std::optional<std::int64_t> {
  return PositionNamed(position.View());
}

// The position an index names, in the type position arithmetic is done in.
template <IntegralValue T>
[[nodiscard]] constexpr auto ToPosition(const T& index) -> Position {
  Position out = Position::Zero();
  lyra::value::ToPosition(out.MutableView(), index.View());
  return out;
}

// LRM 11.5.1: as many bits as the result `R` is wide, from `start`, counted
// from the least significant bit. A position outside the value reads x, and a
// two-state `R` holds each x or z read as 0.
template <IntegralValue R, IntegralValue T>
[[nodiscard]] constexpr auto ExtractBits(const T& a, std::int64_t start) -> R {
  R out = R::Zero();
  Extract(out.MutableView(), a.View(), start);
  return out;
}

// The same bits from the position a value names, which read as `R`'s default
// where it names none.
template <IntegralValue R, IntegralValue T, IntegralValue P>
[[nodiscard]] constexpr auto Slice(const T& a, const P& position) -> R {
  R out = R::Zero();
  lyra::value::Slice(out.MutableView(), a.View(), position.View());
  return out;
}

// `a` with `bits` written at the position a value names, which is `a` where
// it names none (LRM 11.5.1).
template <IntegralValue T, IntegralValue P, IntegralValue Bits>
[[nodiscard]] constexpr auto WithSlice(
    const T& a, const P& position, const Bits& bits) -> T {
  T out = a;
  lyra::value::WithSlice(out.MutableView(), position.View(), bits.View());
  return out;
}

// LRM 11.5.1: writes `bits` at `start`, leaving every other position; a
// position outside the value is not written. Answers the positions written.
template <IntegralValue T, IntegralValue Bits>
constexpr auto InsertBits(T& a, std::int64_t start, const Bits& bits)
    -> std::optional<BitPositions> {
  return Insert(a.MutableView(), bits.View(), start);
}

// The same write, landing only on the positions `within` names: bits written
// through a part of a part reach nothing outside the outer part. Answers the
// positions written.
template <IntegralValue T, IntegralValue Bits>
constexpr auto InsertBitsWithin(
    T& a, std::int64_t start, const Bits& bits, BitPositions within)
    -> std::optional<BitPositions> {
  return InsertWithin(a.MutableView(), bits.View(), start, within);
}

// Some bits of a value held where a write lands (LRM 11.5.1): the bits of a
// value of `Part` from `start`, counted from the value's least significant bit,
// which lie inside the positions `within` names -- all of the value's, or those
// of the part this one was taken in. `Part` is the type the bits are written
// at. A write lands on the bits and leaves every other position, and a
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
  constexpr auto Set(const Part& bits) -> std::optional<BitPositions> {
    if (!start_) {
      return std::nullopt;
    }
    return InsertBitsWithin(*root_, *start_, bits, within_);
  }

  // A reference to the bits of a value of `Inner` at `position` within these.
  template <IntegralValue Inner, IntegralValue P>
  [[nodiscard]] constexpr auto SliceMut(const P& position) const
      -> BitsRef<T, Inner> {
    const std::optional<std::int64_t> offset = ReadPosition(position);
    const std::optional<std::int64_t> start =
        start_ && offset ? std::optional{*start_ + *offset} : std::nullopt;
    return BitsRef<T, Inner>{*root_, start, Reached().value_or(BitPositions{})};
  }

  [[nodiscard]] constexpr auto Root() const -> const T& {
    return *root_;
  }

  // The positions of the value a write through these bits reaches, none
  // where they name no position or lie wholly outside it.
  [[nodiscard]] constexpr auto Reached() const -> std::optional<BitPositions> {
    if (!start_) {
      return std::nullopt;
    }
    return ClampedBitRange(T::kWidth, Part::kWidth, *start_, within_);
  }

 private:
  T* root_;
  std::optional<std::int64_t> start_;
  BitPositions within_;
};

// LRM 6.24.3: a value's bits written into a stream below the `filled` most
// significant positions already written, the first item of a stream being its
// most significant. Answers how many are filled once these are.
template <
    IntegralValue T, PlaneWord WrittenWord, std::size_t kWrittenValueWords,
    std::size_t kWrittenUnknownWords>
constexpr auto WriteToStream(
    const T& a,
    IntegralViewOf<WrittenWord, kWrittenValueWords, kWrittenUnknownWords>
        stream,
    std::uint64_t filled) -> std::uint64_t {
  Insert(
      stream, a.View(),
      static_cast<std::int64_t>(stream.width - filled - T::kWidth));
  return filled + T::kWidth;
}

// LRM 11.4.14.3: the value a stream holds below its `taken` most significant
// positions, read into `out` at its own width, which a two-state `out` holds
// with each x or z as 0. Answers how many positions are taken after it.
template <IntegralValue T>
constexpr auto ReadFromStream(
    AnyConstIntegralView auto stream, std::uint64_t taken, T& out)
    -> std::uint64_t {
  Extract(
      out.MutableView(), stream,
      static_cast<std::int64_t>(stream.width - taken - T::kWidth));
  return taken + T::kWidth;
}

// LRM 5.9: text taken as a value of `R`, its last character the least
// significant byte.
template <IntegralValue R>
[[nodiscard]] auto FromBytes(std::span<const char> bytes) -> R {
  R out = R::Zero();
  lyra::value::FromBytes(out.AsMutableWords().MutableView(), bytes);
  return out;
}

// LRM 21.4: a word of digits read into `out`, which is left as it was where a
// character is no digit of the radix or there is no digit at all. Answers
// whether the word was one.
template <IntegralValue T>
[[nodiscard]] auto FromDigits(T& out, DigitRadix radix, std::string_view digits)
    -> bool {
  return lyra::value::FromDigits(
      out.AsMutableWords().MutableView(), radix, digits);
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
    out.push_back(static_cast<char>(ByteAt(value, i).value_or(0)));
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

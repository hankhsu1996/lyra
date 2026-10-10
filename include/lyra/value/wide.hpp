#pragma once

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <span>
#include <utility>

#include "lyra/base/fixed_array.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

// A value read as the words of its planes at a width it states itself, which
// is what code compiled before any design exists reads of an integral value
// wider than a word.
template <class T>
concept HeldAsWords = requires(const T& bits) {
  { bits.Read() } -> std::same_as<ConstPlanes>;
  { bits.Width() } -> std::same_as<std::uint64_t>;
};

// The planes of an integral value `width` bits wide, wider than a word, where
// its bytes lie: the words of its value plane and then, where it can hold x or
// z, those of its unknown plane (LRM 6.11).
[[nodiscard]] inline auto WidePlanesAt(
    const void* bytes, std::uint64_t width, StateDomain domain) -> ConstPlanes {
  const std::size_t count = WordCountForBits(width);
  const std::span<const std::uint64_t> all(
      static_cast<const std::uint64_t*>(bytes),
      count * (domain == StateDomain::kFourState ? 2U : 1U));
  return ConstPlanes{.value = all.first(count), .unknown = all.subspan(count)};
}

[[nodiscard]] inline auto WidePlanesAt(
    void* bytes, std::uint64_t width, StateDomain domain) -> Planes {
  const std::size_t count = WordCountForBits(width);
  const std::span<std::uint64_t> all(
      static_cast<std::uint64_t*>(bytes),
      count * (domain == StateDomain::kFourState ? 2U : 1U));
  return Planes{.value = all.first(count), .unknown = all.subspan(count)};
}

// An integral value wider than a word where it lies in something that holds
// more than it -- a component of a structure, an element of a container, a
// variable a reference names -- with how wide it is, which those bytes do not
// say. It owns nothing: it reads and writes the bytes it names.
template <StateDomain kDomainArg>
struct WideAt {
  void* bytes;
  std::uint64_t width;

  [[nodiscard]] auto Width() const -> std::uint64_t {
    return width;
  }
  [[nodiscard]] auto ByteSize() const -> std::size_t {
    return IntegralBytesFor(width, kDomainArg);
  }
  [[nodiscard]] auto Read() const -> ConstPlanes {
    return WidePlanesAt(static_cast<const void*>(bytes), width, kDomainArg);
  }
};

// An integral value wider than a word as what holds one keeps it: the words of
// its planes, and how many bits of them the value is. The width is given once,
// where the holder is installed, and every value the holder takes afterwards
// is the bytes of a value that wide, so nothing about the value's type is kept
// or asked. `kDomainArg` says whether an unknown plane follows the value
// plane.
//
// The words are handed out as the value's bytes, which is where generated code
// reads and writes a value of the type it knows; a value written into a holder
// of one as wide is written into the words the holder already has, so those
// bytes go on being what the holder holds (LRM 13.5.2).
template <StateDomain kDomainArg>
class WideVector {
 public:
  static constexpr StateDomain kDomain = kDomainArg;
  static constexpr bool kFourState = kDomain == StateDomain::kFourState;

  // Holds no value: storage before whatever declares it installs one.
  WideVector() = default;

  // A copy of the value `width` bits wide laid out at `bytes`.
  WideVector(std::uint64_t width, const void* bytes) : WideVector(width) {
    std::memcpy(words_.data(), bytes, ByteSize());
  }

  WideVector(const WideVector&) = default;
  WideVector(WideVector&&) noexcept = default;
  auto operator=(const WideVector& other) -> WideVector& {
    if (this == &other) {
      return *this;
    }
    if (words_.size() == other.words_.size()) {
      std::ranges::copy(other.words_, words_.begin());
    } else {
      words_ = other.words_;
    }
    width_ = other.width_;
    return *this;
  }
  auto operator=(WideVector&& other) noexcept -> WideVector& {
    if (this == &other) {
      return *this;
    }
    if (words_.size() == other.words_.size()) {
      std::ranges::copy(other.words_, words_.begin());
    } else {
      words_ = std::move(other.words_);
    }
    width_ = other.width_;
    return *this;
  }
  ~WideVector() = default;

  // A value `width` bits wide with every position holding `bit`, which a
  // two-state value holds as 0 where it is x or z.
  [[nodiscard]] static auto Filled(std::uint64_t width, FourStateBit bit)
      -> WideVector {
    WideVector filled(width);
    FillScalar(filled.Write(), width, bit);
    return filled;
  }

  // A value as wide as this one, of the bytes at `bytes`.
  [[nodiscard]] auto Holding(const void* bytes) const -> WideVector {
    return {width_, bytes};
  }

  [[nodiscard]] auto Width() const -> std::uint64_t {
    return width_;
  }
  [[nodiscard]] auto ByteSize() const -> std::size_t {
    return words_.size() * sizeof(std::uint64_t);
  }
  [[nodiscard]] auto Bytes() const -> const void* {
    return words_.data();
  }
  [[nodiscard]] auto Bytes() -> void* {
    return words_.data();
  }
  [[nodiscard]] auto Read() const -> ConstPlanes {
    return WidePlanesAt(Bytes(), width_, kDomain);
  }
  [[nodiscard]] auto Write() -> Planes {
    return WidePlanesAt(Bytes(), width_, kDomain);
  }
  // The planes of a value as wide as this one laid out at `bytes`.
  [[nodiscard]] auto PlanesAt(const void* bytes) const -> ConstPlanes {
    return WidePlanesAt(bytes, width_, kDomain);
  }

  // A copy of the value, or the value itself, laid out in `out`.
  auto CopyInto(void* out) const -> void* {
    std::memcpy(out, Bytes(), ByteSize());
    return out;
  }
  auto MoveInto(void* out) && -> void* {
    return CopyInto(out);
  }

  // A value whose width is given while the program runs, as a cell holding one
  // reads it: none before a declaration installs one, and two values of one
  // representation where they are as wide.
  [[nodiscard]] auto IsUninitialized() const -> bool {
    return width_ == 0;
  }
  [[nodiscard]] auto SameRepresentation(const WideVector& other) const -> bool {
    return width_ == other.width_;
  }

  [[nodiscard]] auto operator==(const WideVector& other) const -> FourStateBit {
    return Equal(Read(), other.Read());
  }
  [[nodiscard]] auto operator!=(const WideVector& other) const -> FourStateBit {
    return NotEqual(Read(), other.Read());
  }
  // LRM 9.4.2: whether two values are the same bits, x and z included. Every
  // position above the width is clear in both planes, so the bits are the same
  // exactly where the words are.
  [[nodiscard]] auto IsBitIdentical(const WideVector& other) const -> bool {
    return width_ == other.width_ && std::ranges::equal(words_, other.words_);
  }
  [[nodiscard]] auto HasUnknown() const -> bool {
    return kFourState && lyra::value::HasUnknown(Read());
  }

  // The value laid out at `bytes`, as wide as this one, compared with and
  // taken into the words this holds, so a store of one allocates nothing. The
  // bytes may be this value's own.
  [[nodiscard]] auto HoldsBytes(const void* bytes) const -> bool {
    return std::memcmp(words_.data(), bytes, ByteSize()) == 0;
  }
  void TakeBytes(const void* bytes) {
    std::memmove(words_.data(), bytes, ByteSize());
  }

 private:
  // A value `width` bits wide with every position clear in both planes.
  explicit WideVector(std::uint64_t width)
      : width_(width),
        words_(
            WordCountForBits(width) * (kFourState ? 2U : 1U),
            std::uint64_t{0}) {
  }

  std::uint64_t width_ = 0;
  // The value plane's words, then the unknown plane's where there is one.
  base::FixedArray<std::uint64_t, 2> words_;
};

// A held value that compares itself with, and takes, the bytes of a value as
// wide as itself.
template <class T>
concept TakesBytesInPlace =
    requires(T& held, const T& read, const void* bytes) {
      { read.HoldsBytes(bytes) } -> std::same_as<bool>;
      held.TakeBytes(bytes);
    };

using WideBitVector = WideVector<StateDomain::kTwoState>;
using WideLogicVector = WideVector<StateDomain::kFourState>;

static_assert(LyraValue<WideBitVector>);
static_assert(LyraValue<WideLogicVector>);
static_assert(HeldAsWords<WideBitVector>);
static_assert(TakesBytesInPlace<WideLogicVector>);
static_assert(HeldAsWords<WideAt<StateDomain::kFourState>>);

}  // namespace lyra::value

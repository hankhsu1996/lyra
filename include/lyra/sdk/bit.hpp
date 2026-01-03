#pragma once

#include <cstdint>
#include <ostream>
#include <type_traits>

namespace lyra::sdk {

namespace detail {

// Select the smallest unsigned type that can hold Width bits
template <std::size_t Width>
constexpr auto SelectStorage() {
  if constexpr (Width <= 8) {
    return uint8_t{};
  } else if constexpr (Width <= 16) {
    return uint16_t{};
  } else if constexpr (Width <= 32) {
    return uint32_t{};
  } else {
    return uint64_t{};
  }
}

// Select the corresponding signed type for a given unsigned storage type
template <typename T>
struct SignedType;

template <>
struct SignedType<uint8_t> {
  using Type = int8_t;
};
template <>
struct SignedType<uint16_t> {
  using Type = int16_t;
};
template <>
struct SignedType<uint32_t> {
  using Type = int32_t;
};
template <>
struct SignedType<uint64_t> {
  using Type = int64_t;
};

template <std::size_t Width>
using BitStorage = decltype(SelectStorage<Width>());

// Compile-time mask for Width bits
template <std::size_t Width>
constexpr auto kBitMask = static_cast<BitStorage<Width>>(
    Width >= 64 ? ~uint64_t{0} : (uint64_t{1} << Width) - 1);

// Sign-extend a value from Width bits to the full storage type
template <std::size_t Width, typename Storage>
constexpr auto SignExtend(Storage value) -> typename SignedType<Storage>::Type {
  using Signed = typename SignedType<Storage>::Type;
  constexpr std::size_t kStorageBits = sizeof(Storage) * 8;
  constexpr std::size_t kShift = kStorageBits - Width;
  // Shift left then arithmetic shift right to sign-extend
  return static_cast<Signed>(static_cast<Signed>(value << kShift) >> kShift);
}

}  // namespace detail

// Bit<N, Signed> represents an N-bit value with correct SystemVerilog
// semantics.
// - Scalar: Bit<1> (single bit)
// - Vector: Bit<N> where N > 1 (packed array of bits)
// - Signed: Bit<N, true> for signed arithmetic and comparisons
template <std::size_t Width, bool Signed = false>
class Bit {
  static_assert(Width >= 1 && Width <= 64, "Bit width must be 1-64");

 public:
  using Storage = detail::BitStorage<Width>;
  using SignedStorage = typename detail::SignedType<Storage>::Type;
  static constexpr std::size_t kWidth = Width;
  static constexpr bool kSigned = Signed;
  static constexpr Storage kMask = detail::kBitMask<Width>;

  // Constructors
  constexpr Bit() : value_{0} {
  }

  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr Bit(Storage value) : value_{static_cast<Storage>(value & kMask)} {
  }

  // Allow construction from any integral type
  template <typename T>
    requires std::is_integral_v<T>
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr Bit(T value)
      : value_{static_cast<Storage>(static_cast<uint64_t>(value) & kMask)} {
  }

  // Allow conversion from Bit with same width but different signedness
  template <bool OtherSigned>
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr Bit(Bit<Width, OtherSigned> other) : value_{other.Value()} {
  }

  // Allow conversion from Bit with different width
  // Sign extends if source is signed and target is larger, otherwise zero
  // extends
  template <std::size_t OtherWidth, bool OtherSigned>
    requires(OtherWidth != Width)
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr Bit(Bit<OtherWidth, OtherSigned> other)
      : value_{static_cast<Storage>([&]() {
          if constexpr (OtherWidth < Width && OtherSigned) {
            // Sign-extend from smaller signed to larger
            return static_cast<uint64_t>(other.SignedValue()) & kMask;
          } else {
            // Zero-extend or truncate
            return static_cast<uint64_t>(other.Value()) & kMask;
          }
        }())} {
  }

  // Assignment from any integral type
  template <typename T>
    requires std::is_integral_v<T>
  constexpr auto operator=(T value) -> Bit& {
    value_ = static_cast<Storage>(static_cast<uint64_t>(value) & kMask);
    return *this;
  }

  // Access raw value (always unsigned)
  [[nodiscard]] constexpr auto Value() const -> Storage {
    return value_;
  }

  // Access signed value (sign-extended)
  [[nodiscard]] constexpr auto SignedValue() const -> SignedStorage {
    return detail::SignExtend<Width>(value_);
  }

  // Implicit conversion to bool (for use in conditions like if/while)
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr operator bool() const {
    return value_ != 0;
  }

  // Explicit conversion to integral types (for static_cast)
  // Signed types get sign-extended, unsigned types get zero-extended
  template <typename T>
    requires std::is_integral_v<T> && (!std::is_same_v<T, bool>)
  explicit constexpr operator T() const {
    if constexpr (Signed && std::is_signed_v<T>) {
      return static_cast<T>(SignedValue());
    } else {
      return static_cast<T>(value_);
    }
  }

  // Bitwise NOT - the key operation that differs for 1-bit vs multi-bit
  [[nodiscard]] constexpr auto operator~() const -> Bit {
    if constexpr (Width == 1) {
      // For scalar (1-bit), use logical not - efficient and correct
      return Bit{static_cast<Storage>(!value_)};
    } else {
      // For vector, invert and mask to width
      return Bit{static_cast<Storage>(~value_ & kMask)};
    }
  }

  // Logical NOT (returns bool)
  [[nodiscard]] constexpr auto operator!() const -> bool {
    return value_ == 0;
  }

  // Unary plus (no-op)
  [[nodiscard]] constexpr auto operator+() const -> Bit {
    return *this;
  }

  // Unary minus (two's complement negation, masked to width)
  [[nodiscard]] constexpr auto operator-() const -> Bit {
    return Bit{static_cast<Storage>((-value_) & kMask)};
  }

  // Bitwise AND
  [[nodiscard]] constexpr auto operator&(Bit other) const -> Bit {
    return Bit{static_cast<Storage>(value_ & other.value_)};
  }

  // Bitwise OR
  [[nodiscard]] constexpr auto operator|(Bit other) const -> Bit {
    return Bit{static_cast<Storage>(value_ | other.value_)};
  }

  // Bitwise XOR
  [[nodiscard]] constexpr auto operator^(Bit other) const -> Bit {
    return Bit{static_cast<Storage>(value_ ^ other.value_)};
  }

  // Comparison operators - signed types use signed comparison
  [[nodiscard]] constexpr auto operator==(Bit other) const -> bool {
    return value_ == other.value_;
  }

  [[nodiscard]] constexpr auto operator!=(Bit other) const -> bool {
    return value_ != other.value_;
  }

  [[nodiscard]] constexpr auto operator<(Bit other) const -> bool {
    if constexpr (Signed) {
      return SignedValue() < other.SignedValue();
    } else {
      return value_ < other.value_;
    }
  }

  [[nodiscard]] constexpr auto operator<=(Bit other) const -> bool {
    if constexpr (Signed) {
      return SignedValue() <= other.SignedValue();
    } else {
      return value_ <= other.value_;
    }
  }

  [[nodiscard]] constexpr auto operator>(Bit other) const -> bool {
    if constexpr (Signed) {
      return SignedValue() > other.SignedValue();
    } else {
      return value_ > other.value_;
    }
  }

  [[nodiscard]] constexpr auto operator>=(Bit other) const -> bool {
    if constexpr (Signed) {
      return SignedValue() >= other.SignedValue();
    } else {
      return value_ >= other.value_;
    }
  }

  // Arithmetic operators (result is masked to width)
  [[nodiscard]] constexpr auto operator+(Bit other) const -> Bit {
    return Bit{static_cast<Storage>((value_ + other.value_) & kMask)};
  }

  [[nodiscard]] constexpr auto operator-(Bit other) const -> Bit {
    return Bit{static_cast<Storage>((value_ - other.value_) & kMask)};
  }

  [[nodiscard]] constexpr auto operator*(Bit other) const -> Bit {
    return Bit{static_cast<Storage>((value_ * other.value_) & kMask)};
  }

  [[nodiscard]] constexpr auto operator/(Bit other) const -> Bit {
    if constexpr (Signed) {
      // Signed division
      auto result = SignedValue() / other.SignedValue();
      return Bit{static_cast<Storage>(result & kMask)};
    } else {
      return Bit{static_cast<Storage>((value_ / other.value_) & kMask)};
    }
  }

  [[nodiscard]] constexpr auto operator%(Bit other) const -> Bit {
    if constexpr (Signed) {
      // Signed modulo
      auto result = SignedValue() % other.SignedValue();
      return Bit{static_cast<Storage>(result & kMask)};
    } else {
      return Bit{static_cast<Storage>((value_ % other.value_) & kMask)};
    }
  }

  // Shift operators
  // Left shift is the same for signed and unsigned
  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator<<(T amount) const -> Bit {
    return Bit{static_cast<Storage>((value_ << amount) & kMask)};
  }

  // Right shift: arithmetic (sign-extending) for signed, logical for unsigned
  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator>>(T amount) const -> Bit {
    if constexpr (Signed) {
      // Arithmetic right shift (sign-extending)
      auto result = SignedValue() >> amount;
      return Bit{static_cast<Storage>(result & kMask)};
    } else {
      // Logical right shift
      return Bit{static_cast<Storage>(value_ >> amount)};
    }
  }

  // Shift operators with Bit type as shift amount
  template <std::size_t W, bool S>
  [[nodiscard]] constexpr auto operator<<(Bit<W, S> amount) const -> Bit {
    return *this << amount.Value();
  }

  template <std::size_t W, bool S>
  [[nodiscard]] constexpr auto operator>>(Bit<W, S> amount) const -> Bit {
    return *this >> amount.Value();
  }

  // Compound assignment operators
  constexpr auto operator&=(Bit other) -> Bit& {
    value_ &= other.value_;
    return *this;
  }

  constexpr auto operator|=(Bit other) -> Bit& {
    value_ |= other.value_;
    return *this;
  }

  constexpr auto operator^=(Bit other) -> Bit& {
    value_ ^= other.value_;
    return *this;
  }

  constexpr auto operator+=(Bit other) -> Bit& {
    value_ = static_cast<Storage>((value_ + other.value_) & kMask);
    return *this;
  }

  constexpr auto operator-=(Bit other) -> Bit& {
    value_ = static_cast<Storage>((value_ - other.value_) & kMask);
    return *this;
  }

  // Pre/post increment/decrement
  constexpr auto operator++() -> Bit& {
    value_ = static_cast<Storage>((value_ + 1) & kMask);
    return *this;
  }

  constexpr auto operator++(int) -> Bit {
    Bit tmp = *this;
    ++(*this);
    return tmp;
  }

  constexpr auto operator--() -> Bit& {
    value_ = static_cast<Storage>((value_ - 1) & kMask);
    return *this;
  }

  constexpr auto operator--(int) -> Bit {
    Bit tmp = *this;
    --(*this);
    return tmp;
  }

  // Stream output
  friend auto operator<<(std::ostream& os, Bit b) -> std::ostream& {
    if constexpr (Signed) {
      return os << b.SignedValue();
    } else {
      // Cast to largest type to avoid char interpretation for uint8_t
      return os << static_cast<uint64_t>(b.value_);
    }
  }

 private:
  Storage value_;
};

// Logical AND for Bit types (returns bool for C++ compatibility)
template <std::size_t W1, bool S1, std::size_t W2, bool S2>
[[nodiscard]] constexpr auto operator&&(Bit<W1, S1> lhs, Bit<W2, S2> rhs)
    -> bool {
  return static_cast<bool>(lhs) && static_cast<bool>(rhs);
}

// Logical OR for Bit types (returns bool for C++ compatibility)
template <std::size_t W1, bool S1, std::size_t W2, bool S2>
[[nodiscard]] constexpr auto operator||(Bit<W1, S1> lhs, Bit<W2, S2> rhs)
    -> bool {
  return static_cast<bool>(lhs) || static_cast<bool>(rhs);
}

// LRM-aligned type aliases (Section 6.11)
// These match SystemVerilog's predefined integer types

// byte: 2-state, 8-bit signed integer
using Byte = Bit<8, true>;

// shortint: 2-state, 16-bit signed integer
using ShortInt = Bit<16, true>;

// int: 2-state, 32-bit signed integer
using Int = Bit<32, true>;

// longint: 2-state, 64-bit signed integer
using LongInt = Bit<64, true>;

}  // namespace lyra::sdk

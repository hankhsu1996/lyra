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

template <std::size_t Width>
using BitStorage = decltype(SelectStorage<Width>());

// Compile-time mask for Width bits
template <std::size_t Width>
constexpr auto kBitMask = static_cast<BitStorage<Width>>(
    Width >= 64 ? ~uint64_t{0} : (uint64_t{1} << Width) - 1);

}  // namespace detail

// Bit<N> represents an N-bit unsigned value with correct SystemVerilog
// semantics.
// - Scalar: Bit<1> (single bit)
// - Vector: Bit<N> where N > 1 (packed array of bits)
template <std::size_t Width>
class Bit {
  static_assert(Width >= 1 && Width <= 64, "Bit width must be 1-64");

 public:
  using Storage = detail::BitStorage<Width>;
  static constexpr std::size_t kWidth = Width;
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

  // Assignment from any integral type
  template <typename T>
    requires std::is_integral_v<T>
  constexpr auto operator=(T value) -> Bit& {
    value_ = static_cast<Storage>(static_cast<uint64_t>(value) & kMask);
    return *this;
  }

  // Access raw value
  [[nodiscard]] constexpr auto Value() const -> Storage {
    return value_;
  }

  // Implicit conversion to bool (for use in conditions like if/while)
  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr operator bool() const {
    return value_ != 0;
  }

  // Explicit conversion to integral types (for static_cast)
  template <typename T>
    requires std::is_integral_v<T> && (!std::is_same_v<T, bool>)
  explicit constexpr operator T() const {
    return static_cast<T>(value_);
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

  // Comparison operators - return bool for C++ compatibility
  [[nodiscard]] constexpr auto operator==(Bit other) const -> bool {
    return value_ == other.value_;
  }

  [[nodiscard]] constexpr auto operator!=(Bit other) const -> bool {
    return value_ != other.value_;
  }

  [[nodiscard]] constexpr auto operator<(Bit other) const -> bool {
    return value_ < other.value_;
  }

  [[nodiscard]] constexpr auto operator<=(Bit other) const -> bool {
    return value_ <= other.value_;
  }

  [[nodiscard]] constexpr auto operator>(Bit other) const -> bool {
    return value_ > other.value_;
  }

  [[nodiscard]] constexpr auto operator>=(Bit other) const -> bool {
    return value_ >= other.value_;
  }

  // Comparison with integral types
  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator==(T other) const -> bool {
    return value_ == static_cast<Storage>(other);
  }

  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator!=(T other) const -> bool {
    return value_ != static_cast<Storage>(other);
  }

  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator<(T other) const -> bool {
    return value_ < static_cast<Storage>(other);
  }

  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator<=(T other) const -> bool {
    return value_ <= static_cast<Storage>(other);
  }

  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator>(T other) const -> bool {
    return value_ > static_cast<Storage>(other);
  }

  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator>=(T other) const -> bool {
    return value_ >= static_cast<Storage>(other);
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
    return Bit{static_cast<Storage>((value_ / other.value_) & kMask)};
  }

  [[nodiscard]] constexpr auto operator%(Bit other) const -> Bit {
    return Bit{static_cast<Storage>((value_ % other.value_) & kMask)};
  }

  // Arithmetic with integral types
  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator+(T other) const -> Bit {
    return Bit{static_cast<Storage>((value_ + other) & kMask)};
  }

  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator-(T other) const -> Bit {
    return Bit{static_cast<Storage>((value_ - other) & kMask)};
  }

  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator*(T other) const -> Bit {
    return Bit{static_cast<Storage>((value_ * other) & kMask)};
  }

  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator/(T other) const -> Bit {
    return Bit{static_cast<Storage>((value_ / other) & kMask)};
  }

  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator%(T other) const -> Bit {
    return Bit{static_cast<Storage>((value_ % other) & kMask)};
  }

  // Shift operators
  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator<<(T amount) const -> Bit {
    return Bit{static_cast<Storage>((value_ << amount) & kMask)};
  }

  template <typename T>
    requires std::is_integral_v<T>
  [[nodiscard]] constexpr auto operator>>(T amount) const -> Bit {
    return Bit{static_cast<Storage>(value_ >> amount)};
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

  template <typename T>
    requires std::is_integral_v<T>
  constexpr auto operator+=(T other) -> Bit& {
    value_ = static_cast<Storage>((value_ + other) & kMask);
    return *this;
  }

  template <typename T>
    requires std::is_integral_v<T>
  constexpr auto operator-=(T other) -> Bit& {
    value_ = static_cast<Storage>((value_ - other) & kMask);
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
    // Cast to largest type to avoid char interpretation for uint8_t
    return os << static_cast<uint64_t>(b.value_);
  }

 private:
  Storage value_;
};

// Logical AND for Bit types (returns bool for C++ compatibility)
template <std::size_t W1, std::size_t W2>
[[nodiscard]] constexpr auto operator&&(Bit<W1> lhs, Bit<W2> rhs) -> bool {
  return static_cast<bool>(lhs) && static_cast<bool>(rhs);
}

// Logical OR for Bit types (returns bool for C++ compatibility)
template <std::size_t W1, std::size_t W2>
[[nodiscard]] constexpr auto operator||(Bit<W1> lhs, Bit<W2> rhs) -> bool {
  return static_cast<bool>(lhs) || static_cast<bool>(rhs);
}

// Reverse comparison operators (int == Bit)
template <std::size_t Width, typename T>
  requires std::is_integral_v<T>
[[nodiscard]] constexpr auto operator==(T lhs, Bit<Width> rhs) -> bool {
  return rhs == lhs;
}

template <std::size_t Width, typename T>
  requires std::is_integral_v<T>
[[nodiscard]] constexpr auto operator!=(T lhs, Bit<Width> rhs) -> bool {
  return rhs != lhs;
}

}  // namespace lyra::sdk

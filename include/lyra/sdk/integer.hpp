#pragma once

#include <cstdint>

namespace lyra::sdk {

namespace detail {

template <std::size_t Width>
struct IntegerStorage;

template <>
struct IntegerStorage<1> {
  using Type = uint8_t;
};

template <>
struct IntegerStorage<8> {
  using Type = uint8_t;
};

template <>
struct IntegerStorage<16> {
  using Type = uint16_t;
};

template <>
struct IntegerStorage<32> {
  using Type = uint32_t;
};

template <>
struct IntegerStorage<64> {
  using Type = uint64_t;
};

}  // namespace detail

template <std::size_t Width>
class Integer {
 public:
  using StorageType = typename detail::IntegerStorage<Width>::Type;

  Integer() : value_(0) {}
  explicit Integer(StorageType value) : value_(value) {}

  explicit operator StorageType() const { return value_; }

  auto operator=(StorageType value) -> Integer& {
    value_ = value;
    return *this;
  }

  auto operator++() -> Integer& {
    ++value_;
    return *this;
  }

  auto operator++(int) -> Integer {
    Integer tmp = *this;
    ++value_;
    return tmp;
  }

  auto operator--() -> Integer& {
    --value_;
    return *this;
  }

  auto operator--(int) -> Integer {
    Integer tmp = *this;
    --value_;
    return tmp;
  }

 private:
  StorageType value_;
};

}  // namespace lyra::sdk

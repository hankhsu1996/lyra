#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <span>

namespace lyra::value {

// A borrowed view of the entries a unit holds as a constant array, laid out as
// the library reads it: where the first entry is and how many there are, which
// is what a table is in a C program, a `std::span` is in C++, and a slice is in
// Rust. The array is what owns the entries; a view of all of it is made from
// it, so the count is the array's own and no view of none points anywhere.
template <typename T>
struct ConstantSpan {
  const T* data = nullptr;
  std::uint64_t size = 0;

  constexpr ConstantSpan() = default;

  template <std::size_t N>
  constexpr explicit ConstantSpan(const std::array<T, N>* entries) noexcept
      : data(N == 0 ? nullptr : entries->data()), size(N) {
  }

  [[nodiscard]] constexpr auto Entries() const -> std::span<const T> {
    return {data, size};
  }
};

// Whether `T` is a view the library lays out as `ConstantSpan`: the one
// structure of the library whose meaning is a view of entries rather than the
// members it declares.
template <typename T>
inline constexpr bool kIsConstantSpan = false;

template <typename T>
inline constexpr bool kIsConstantSpan<ConstantSpan<T>> = true;

}  // namespace lyra::value

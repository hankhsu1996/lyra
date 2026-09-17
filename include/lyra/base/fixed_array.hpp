#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <iterator>
#include <memory>
#include <type_traits>

namespace lyra::base {

// A sequence whose length is chosen when it is built and does not change while
// it lives. Up to N elements live in the sequence itself and any more live in a
// buffer of their own, so a sequence short enough to fit pays for no buffer and
// reaches its elements without one. Restricting T to trivially-copyable keeps
// both cases plain memory operations, with no element constructors or
// destructors to run.
//
// What the short case is worth comes from the compiler knowing, wherever
// elements are written, both how much room there is and that the length cannot
// exceed it. Reaching that room through a plain pointer and a run-time length
// gives both away, and measurably costs more than the room itself saves. So
// every path that writes elements settles which side it is on first, and the
// short side then names the room rather than a pointer to it.
template <class T, std::size_t N>
  requires std::is_trivially_copyable_v<T>
class FixedArray {
  static_assert(N >= 1, "FixedArray inline capacity must be at least 1");

 public:
  FixedArray() = default;

  explicit FixedArray(std::size_t count) : FixedArray(count, T{}) {
  }

  FixedArray(std::size_t count, const T& value) : length_(count) {
    if (count > N) {
      elsewhere_ = std::allocator<T>{}.allocate(count);
      std::fill_n(elsewhere_, count, value);
      return;
    }
    room_.fill(value);
  }

  // Constrained so that a pair of integers reaches the length-and-value
  // constructor rather than deducing itself into an iterator range.
  template <std::input_iterator InputIt>
  FixedArray(InputIt first, InputIt last)
      : length_(static_cast<std::size_t>(std::distance(first, last))) {
    if (length_ > N) {
      elsewhere_ = std::allocator<T>{}.allocate(length_);
      std::copy(first, last, elsewhere_);
      return;
    }
    std::copy(first, last, room_.data());
  }

  FixedArray(const FixedArray& other)
      : room_(other.room_), length_(other.length_) {
    TakeWhatDidNotFit(other);
  }

  FixedArray(FixedArray&& other) noexcept
      : elsewhere_(other.elsewhere_),
        room_(other.room_),
        length_(other.length_) {
    other.elsewhere_ = nullptr;
    other.length_ = 0;
  }

  auto operator=(const FixedArray& other) -> FixedArray& {
    if (this != &other) {
      Release();
      room_ = other.room_;
      length_ = other.length_;
      TakeWhatDidNotFit(other);
    }
    return *this;
  }

  auto operator=(FixedArray&& other) noexcept -> FixedArray& {
    if (this != &other) {
      Release();
      elsewhere_ = other.elsewhere_;
      room_ = other.room_;
      length_ = other.length_;
      other.elsewhere_ = nullptr;
      other.length_ = 0;
    }
    return *this;
  }

  ~FixedArray() {
    Release();
  }

  [[nodiscard]] auto data() -> T* {
    return elsewhere_ != nullptr ? elsewhere_ : room_.data();
  }
  [[nodiscard]] auto data() const -> const T* {
    return elsewhere_ != nullptr ? elsewhere_ : room_.data();
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return length_;
  }
  [[nodiscard]] auto empty() const -> bool {
    return length_ == 0;
  }

  [[nodiscard]] auto operator[](std::size_t i) -> T& {
    return data()[i];
  }
  [[nodiscard]] auto operator[](std::size_t i) const -> const T& {
    return data()[i];
  }

  [[nodiscard]] auto front() -> T& {
    return data()[0];
  }
  [[nodiscard]] auto front() const -> const T& {
    return data()[0];
  }

  [[nodiscard]] auto back() -> T& {
    return data()[length_ - 1];
  }
  [[nodiscard]] auto back() const -> const T& {
    return data()[length_ - 1];
  }

  [[nodiscard]] auto begin() -> T* {
    return data();
  }
  [[nodiscard]] auto end() -> T* {
    return data() + length_;
  }
  [[nodiscard]] auto begin() const -> const T* {
    return data();
  }
  [[nodiscard]] auto end() const -> const T* {
    return data() + length_;
  }

  [[nodiscard]] auto operator==(const FixedArray& other) const -> bool {
    return std::equal(begin(), end(), other.begin(), other.end());
  }

 private:
  auto TakeWhatDidNotFit(const FixedArray& other) -> void {
    if (other.elsewhere_ == nullptr) {
      return;
    }
    elsewhere_ = std::allocator<T>{}.allocate(other.length_);
    std::copy_n(other.elsewhere_, other.length_, elsewhere_);
  }

  // Reads the length it is releasing, so it runs before a new one is settled.
  auto Release() noexcept -> void {
    if (elsewhere_ != nullptr) {
      std::allocator<T>{}.deallocate(elsewhere_, length_);
      elsewhere_ = nullptr;
    }
  }

  T* elsewhere_ = nullptr;
  std::array<T, N> room_{};
  std::size_t length_ = 0;
};

}  // namespace lyra::base

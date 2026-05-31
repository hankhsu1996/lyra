#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <iterator>
#include <type_traits>
#include <utility>
#include <vector>

namespace lyra::base {

// A contiguous sequence that stores up to N elements inline and spills to a
// heap buffer when constructed with more. Capacity is fixed at construction:
// the sequence can shrink via pop_back but does not grow. Restricting T to
// trivially-copyable keeps copy and move as plain memory operations with no
// element constructors or destructors to run.
//
// The element data lives inline while the spill buffer is empty and in that
// buffer otherwise; `heap_.empty()` is the inline-vs-spilled predicate.
template <class T, std::size_t N>
  requires std::is_trivially_copyable_v<T>
class InlinedVector {
  static_assert(N >= 1, "InlinedVector inline capacity must be at least 1");

 public:
  InlinedVector() = default;

  explicit InlinedVector(std::size_t count) : InlinedVector(count, T{}) {
  }

  InlinedVector(std::size_t count, const T& value) : size_(count) {
    if (count > N) {
      heap_.assign(count, value);
    } else {
      std::fill_n(inline_.data(), count, value);
    }
  }

  template <class InputIt>
  InlinedVector(InputIt first, InputIt last)
      : size_(static_cast<std::size_t>(std::distance(first, last))) {
    if (size_ > N) {
      heap_.assign(first, last);
    } else {
      std::copy(first, last, inline_.data());
    }
  }

  InlinedVector(const InlinedVector&) = default;
  auto operator=(const InlinedVector&) -> InlinedVector& = default;

  InlinedVector(InlinedVector&& other) noexcept
      : inline_(other.inline_),
        heap_(std::move(other.heap_)),
        size_(other.size_) {
    other.size_ = 0;
  }

  auto operator=(InlinedVector&& other) noexcept -> InlinedVector& {
    if (this != &other) {
      inline_ = other.inline_;
      heap_ = std::move(other.heap_);
      size_ = other.size_;
      other.size_ = 0;
    }
    return *this;
  }

  ~InlinedVector() = default;

  [[nodiscard]] auto data() -> T* {
    return heap_.empty() ? inline_.data() : heap_.data();
  }
  [[nodiscard]] auto data() const -> const T* {
    return heap_.empty() ? inline_.data() : heap_.data();
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return size_;
  }
  [[nodiscard]] auto empty() const -> bool {
    return size_ == 0;
  }

  [[nodiscard]] auto operator[](std::size_t i) -> T& {
    return data()[i];
  }
  [[nodiscard]] auto operator[](std::size_t i) const -> const T& {
    return data()[i];
  }

  [[nodiscard]] auto back() -> T& {
    return data()[size_ - 1];
  }
  [[nodiscard]] auto back() const -> const T& {
    return data()[size_ - 1];
  }

  [[nodiscard]] auto begin() -> T* {
    return data();
  }
  [[nodiscard]] auto end() -> T* {
    return data() + size_;
  }
  [[nodiscard]] auto begin() const -> const T* {
    return data();
  }
  [[nodiscard]] auto end() const -> const T* {
    return data() + size_;
  }

  auto pop_back() -> void {
    --size_;
  }

 private:
  std::array<T, N> inline_{};
  std::vector<T> heap_;
  std::size_t size_ = 0;
};

}  // namespace lyra::base

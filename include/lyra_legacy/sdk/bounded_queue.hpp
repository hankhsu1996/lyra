#pragma once

#include <cstddef>
#include <deque>
#include <initializer_list>
#include <utility>

namespace lyra::sdk {

/// BoundedQueue - A std::deque wrapper that enforces a maximum size.
///
/// When the queue is full (size >= max_bound + 1), new insertions are ignored.
/// This matches SystemVerilog bounded queue semantics per IEEE 1800-2023.
///
/// Template parameters:
///   T - Element type
///
/// Constructor takes max_bound at runtime (0 = unbounded).
///
/// Note: Method names use lowercase to match std::deque API (generated code
/// calls these methods directly). NOLINT suppresses naming style warnings.
// NOLINTBEGIN(readability-identifier-naming)
template <typename T>
class BoundedQueue {
  std::deque<T> data_;
  std::size_t max_bound_;  // 0 = unbounded

  [[nodiscard]] auto is_full() const -> bool {
    if (max_bound_ == 0) {
      return false;  // Unbounded
    }
    return data_.size() >= max_bound_ + 1;
  }

 public:
  explicit BoundedQueue(std::size_t max_bound = 0) : max_bound_(max_bound) {
  }

  BoundedQueue(std::size_t max_bound, std::initializer_list<T> init)
      : data_(init), max_bound_(max_bound) {
    // Truncate if initializer exceeds bound
    if (max_bound_ > 0) {
      while (data_.size() > max_bound_ + 1) {
        data_.pop_back();
      }
    }
  }

  [[nodiscard]] auto max_bound() const -> std::size_t {
    return max_bound_;
  }

  // Mutating operations - check bound before inserting
  void push_back(const T& v) {
    if (!is_full()) {
      data_.push_back(v);
    }
  }

  void push_back(T&& v) {
    if (!is_full()) {
      data_.push_back(std::move(v));
    }
  }

  void push_front(const T& v) {
    if (!is_full()) {
      data_.push_front(v);
    }
  }

  void push_front(T&& v) {
    if (!is_full()) {
      data_.push_front(std::move(v));
    }
  }

  // Insert by iterator (for codegen compatibility)
  auto insert(typename std::deque<T>::iterator pos, const T& v) ->
      typename std::deque<T>::iterator {
    if (!is_full()) {
      return data_.insert(pos, v);
    }
    return pos;
  }

  // Insert by index (for interpreter)
  void insert(std::size_t index, const T& v) {
    if (!is_full() && index <= data_.size()) {
      data_.insert(data_.begin() + static_cast<std::ptrdiff_t>(index), v);
    }
  }

  // Non-mutating operations pass through
  [[nodiscard]] auto size() const -> std::size_t {
    return data_.size();
  }
  [[nodiscard]] auto empty() const -> bool {
    return data_.empty();
  }

  auto operator[](std::size_t i) -> T& {
    return data_[i];
  }
  auto operator[](std::size_t i) const -> const T& {
    return data_[i];
  }

  auto front() -> T& {
    return data_.front();
  }
  auto front() const -> const T& {
    return data_.front();
  }
  auto back() -> T& {
    return data_.back();
  }
  auto back() const -> const T& {
    return data_.back();
  }

  // Pop operations don't need bound checking
  void pop_front() {
    data_.pop_front();
  }
  void pop_back() {
    data_.pop_back();
  }
  void clear() {
    data_.clear();
  }

  // Erase by iterator (for codegen compatibility)
  auto erase(typename std::deque<T>::iterator pos) {
    return data_.erase(pos);
  }

  // Erase by index (for interpreter)
  void erase(std::size_t index) {
    if (index < data_.size()) {
      data_.erase(data_.begin() + static_cast<std::ptrdiff_t>(index));
    }
  }

  auto begin() {
    return data_.begin();
  }
  auto end() {
    return data_.end();
  }
  auto begin() const {
    return data_.begin();
  }
  auto end() const {
    return data_.end();
  }

  // Equality comparison - compares elements only (not max_bound)
  [[nodiscard]] auto operator==(const BoundedQueue& other) const -> bool {
    return data_ == other.data_;
  }
};
// NOLINTEND(readability-identifier-naming)

}  // namespace lyra::sdk

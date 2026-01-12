#pragma once

#include <cstddef>
#include <deque>
#include <initializer_list>

namespace lyra::sdk {

/// BoundedQueue - A std::deque wrapper that enforces a maximum size.
///
/// When the queue is full (size == MaxBound + 1), new insertions are ignored.
/// This matches SystemVerilog bounded queue semantics per IEEE 1800-2023.
///
/// Template parameters:
///   T - Element type
///   MaxBound - Maximum index (max size = MaxBound + 1)
///
/// Note: Method names use lowercase to match std::deque API (generated code
/// calls these methods directly). NOLINT suppresses naming style warnings.
// NOLINTBEGIN(readability-identifier-naming)
template <typename T, std::size_t MaxBound>
class BoundedQueue {
  std::deque<T> data_;

  static constexpr std::size_t kMaxSize = MaxBound + 1;

  [[nodiscard]] auto is_full() const -> bool {
    return data_.size() >= kMaxSize;
  }

 public:
  BoundedQueue() = default;

  BoundedQueue(std::initializer_list<T> init) : data_(init) {
    // Truncate if initializer exceeds bound
    while (data_.size() > kMaxSize) {
      data_.pop_back();
    }
  }

  // Mutating operations - check bound before inserting
  void push_back(const T& v) {
    if (!is_full()) {
      data_.push_back(v);
    }
  }

  void push_front(const T& v) {
    if (!is_full()) {
      data_.push_front(v);
    }
  }

  auto insert(typename std::deque<T>::iterator pos, const T& v) ->
      typename std::deque<T>::iterator {
    if (!is_full()) {
      return data_.insert(pos, v);
    }
    return pos;
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

  auto erase(typename std::deque<T>::iterator pos) {
    return data_.erase(pos);
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
};
// NOLINTEND(readability-identifier-naming)

}  // namespace lyra::sdk

#include "lyra/runtime/activation_trace.hpp"

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

ActivationTrace::ActivationTrace(uint32_t capacity)
    : buffer_(capacity), capacity_(capacity) {
}

void ActivationTrace::Append(const ActivationEvent& event) {
  buffer_[head_] = event;
  head_ = (head_ + 1) % capacity_;
  if (count_ < capacity_) {
    ++count_;
  }
}

auto ActivationTrace::Size() const -> uint32_t {
  return count_;
}

auto ActivationTrace::Capacity() const -> uint32_t {
  return capacity_;
}

auto ActivationTrace::operator[](uint32_t index) const
    -> const ActivationEvent& {
  if (index >= count_) {
    throw common::InternalError(
        "ActivationTrace::operator[]", "index out of range");
  }
  // Oldest event is at (head_ - count_) mod capacity_.
  uint32_t start = (head_ + capacity_ - count_) % capacity_;
  return buffer_[(start + index) % capacity_];
}

}  // namespace lyra::runtime

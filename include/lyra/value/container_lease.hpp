#pragma once

#include <cstdint>
#include <utility>

namespace lyra::value {

// How many leases are taken on storage a container keeps -- an element of a
// queue or of an associative array, or the elements of a dynamic array --
// through a reference bound to a part of it (LRM 13.5.2), and whether the
// container has detached it: let it go while a lease was still taken on it.
struct LeaseCount {
  std::uint32_t held = 0;
  bool detached = false;
};

// Ends storage its container detached: its value or values, then the storage
// itself. `element` is the element type, as its policy's opaque value.
using EndDetached = void (*)(void* storage, const void* element);

// A lease a call holds on storage a container keeps, taken where the call's
// binding reaches a part of it and let go of no sooner than the call returns.
// An element removed from its array, or an array resized, while a call holds it
// "shall continue to exist within the scope of the called subroutines until
// they complete" (LRM 13.5.2): so a container that removes or replaces leased
// storage detaches it instead of ending it, and the last lease to let go ends
// it. A copy of a lease is one more.
class ContainerLease {
 public:
  ContainerLease(
      LeaseCount& count, void* storage, EndDetached end, const void* element)
      : count_(&count), storage_(storage), end_(end), element_(element) {
    ++count_->held;
  }
  ContainerLease(const ContainerLease& other)
      : count_(other.count_),
        storage_(other.storage_),
        end_(other.end_),
        element_(other.element_) {
    ++count_->held;
  }
  ContainerLease(ContainerLease&& other) noexcept
      : count_(std::exchange(other.count_, nullptr)),
        storage_(other.storage_),
        end_(other.end_),
        element_(other.element_) {
  }
  auto operator=(const ContainerLease& other) -> ContainerLease& {
    ContainerLease copy(other);
    Swap(copy);
    return *this;
  }
  auto operator=(ContainerLease&& other) noexcept -> ContainerLease& {
    ContainerLease taken(std::move(other));
    Swap(taken);
    return *this;
  }
  ~ContainerLease() {
    if (count_ != nullptr && --count_->held == 0 && count_->detached) {
      end_(storage_, element_);
    }
  }

 private:
  void Swap(ContainerLease& other) noexcept {
    std::swap(count_, other.count_);
    std::swap(storage_, other.storage_);
    std::swap(end_, other.end_);
    std::swap(element_, other.element_);
  }

  LeaseCount* count_;
  void* storage_;
  EndDetached end_;
  const void* element_;
};

}  // namespace lyra::value

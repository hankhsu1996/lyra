#pragma once

#include <cstddef>
#include <memory>
#include <utility>

namespace lyra::runtime {

// A managed reference to a class object (LRM 8.3): a handle that refers to a
// heap object whose lifetime the simulator owns. Null is a legal value, copies
// are shallow (two handles refer to the same object), and identity is compared
// by object address.
//
// Realized as a shared owner: the last handle to drop releases the object. A
// cycle of handles that becomes unreachable is not reclaimed.
template <typename T>
class GcRef {
 public:
  GcRef() = default;
  GcRef(std::nullptr_t) {  // NOLINT(google-explicit-constructor)
  }
  explicit GcRef(std::shared_ptr<T> ptr) : ptr_(std::move(ptr)) {
  }

  auto operator->() const -> T* {
    return ptr_.get();
  }
  auto operator*() const -> T& {
    return *ptr_;
  }
  [[nodiscard]] auto Get() const -> T* {
    return ptr_.get();
  }

  friend auto operator==(const GcRef& a, const GcRef& b) -> bool {
    return a.ptr_.get() == b.ptr_.get();
  }
  friend auto operator!=(const GcRef& a, const GcRef& b) -> bool {
    return a.ptr_.get() != b.ptr_.get();
  }

 private:
  std::shared_ptr<T> ptr_;
};

template <typename T, typename... Args>
auto GcNew(Args&&... args) -> GcRef<T> {
  return GcRef<T>(std::make_shared<T>(std::forward<Args>(args)...));
}

}  // namespace lyra::runtime

#pragma once

#include <cstddef>
#include <memory>
#include <utility>
#include <vector>

namespace lyra::runtime {

// A managed reference to a class object (LRM 8.3): a handle that refers to a
// heap object whose lifetime the simulator owns. Null is a legal value, copies
// are shallow (two handles refer to the same object), and identity is compared
// by object address.
template <typename T>
class GcRef {
 public:
  GcRef() = default;
  // `null` (LRM 8.4) flows in as a null pointer; the implicit conversion lets a
  // handle be assigned, initialized, and compared against `null` directly.
  GcRef(std::nullptr_t) {  // NOLINT(google-explicit-constructor)
  }
  explicit GcRef(T* object) : object_(object) {
  }

  auto operator->() const -> T* {
    return object_;
  }
  auto operator*() const -> T& {
    return *object_;
  }
  [[nodiscard]] auto Get() const -> T* {
    return object_;
  }

  friend auto operator==(const GcRef& a, const GcRef& b) -> bool {
    return a.object_ == b.object_;
  }
  friend auto operator!=(const GcRef& a, const GcRef& b) -> bool {
    return a.object_ != b.object_;
  }

 private:
  T* object_ = nullptr;
};

// The managed heap. Every allocation is retained for the life of the run and
// reclaimed at shutdown, so a handle never dangles.
namespace detail {
inline auto ManagedHeap() -> std::vector<std::shared_ptr<void>>& {
  static std::vector<std::shared_ptr<void>> heap;
  return heap;
}
}  // namespace detail

// Allocates a class object on the managed heap and returns a handle to it. The
// object is retained by the heap, so the returned handle never dangles.
template <typename T, typename... Args>
auto GcNew(Args&&... args) -> GcRef<T> {
  auto object = std::make_shared<T>(std::forward<Args>(args)...);
  T* raw = object.get();
  detail::ManagedHeap().push_back(std::move(object));
  return GcRef<T>(raw);
}

}  // namespace lyra::runtime

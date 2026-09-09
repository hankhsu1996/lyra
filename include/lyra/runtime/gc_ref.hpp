#pragma once

#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>

namespace lyra::runtime {

// A managed reference to a class object (LRM 8.3): a handle that refers to a
// heap object whose lifetime the simulator owns. Null is a legal value, copies
// are shallow (two handles refer to the same object), and identity is compared
// by object address.
//
// A handle assigns from a handle to a subclass -- LRM 8.14 permits assigning a
// derived-class handle to a base-class variable -- so the type carries a
// converting constructor when the pointee is a compatible-pointer subclass.
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

  // By value, so the caller's own copy or move fills the parameter and the
  // conversion itself always moves.
  template <typename U>
    requires(std::is_convertible_v<U*, T*> && !std::is_same_v<U, T>)
  GcRef(GcRef<U> other)  // NOLINT(google-explicit-constructor)
      : ptr_(std::move(other.ptr_)) {
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
  template <typename U>
  friend class GcRef;

  std::shared_ptr<T> ptr_;
};

template <typename T, typename... Args>
auto GcNew(Args&&... args) -> GcRef<T> {
  return GcRef<T>(std::make_shared<T>(std::forward<Args>(args)...));
}

// The base an object carries so it can name the handle referring to it. A body
// reaches its own object through a borrowed pointer, which is all a member
// access needs; LRM 8.11 `this` asks for a handle instead, and going from the
// one to the other is possible only where the object records which owner refers
// to it. That record is a cost of realizing the handle as a shared owner, not a
// property of the handle itself, so it lives here beside that realization. It
// adds no virtual destructor: what releases an object is the deleter its
// allocation fixed, so an object whose class declares no virtual method keeps
// no table.
class GcObject : public std::enable_shared_from_this<GcObject> {};

// The handle referring to the object the running subroutine was invoked on
// (LRM 8.11). Deducing the object's own type from the receiver keeps the result
// a handle to that class rather than to the base holding the record, which is
// what a derived class's `this` must be; the downcast is sound because the
// receiver is the body's own object and so is of that class already.
template <typename T>
auto SelfHandle(T* self) -> GcRef<T> {
  return GcRef<T>(std::static_pointer_cast<T>(self->shared_from_this()));
}

}  // namespace lyra::runtime

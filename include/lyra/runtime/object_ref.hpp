#pragma once

#include <concepts>
#include <memory>
#include <utility>

#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

// The base an object carries so a body running on it can name the reference
// that refers to it. A body reaches its own object through a borrowed pointer,
// which serves every member access; LRM 8.11 `this` asks for a reference
// instead, and the borrowed pointer does not answer which object it belongs to
// -- it names a subobject, which for a class implementing an interface is not
// the object's own address. So the object records the identity it was created
// with, and `this` reads it.
//
// Recovering a reference from an object is what a shared-owner realization
// needs; where reachability retains an object a body already holds its receiver
// as a root, and this record goes with the realization rather than into it. It
// adds no virtual destructor: what releases an object is the deleter its
// allocation fixed, so an object whose class declares no virtual method keeps
// no table.
class GcObject : public std::enable_shared_from_this<GcObject> {
 public:
  // Called once, by the allocation, with the address the allocation produced.
  void AdoptIdentity(void* address) {
    identity_ = address;
  }

  [[nodiscard]] auto IdentityAddress() const -> void* {
    return identity_;
  }

 private:
  void* identity_ = nullptr;
};

// A reference to an object whose typed owner is already in hand. The share
// names the complete object, so the address it stores is the object's identity
// -- which is what separates this from building a reference out of a pointer to
// some base, where the address is a subobject's and answers a different
// question.
template <typename T>
auto RefToObject(std::shared_ptr<T> owned) -> value::ObjectRef {
  T* view = owned.get();
  return value::ObjectRef(
      value::ManagedRef(std::shared_ptr<void>(std::move(owned))), view);
}

// Brings an object into existence and fixes its identity as the address the
// allocation produced. An object no reference is recovered from records
// nothing, which is why the record is written here rather than by a base every
// such object would have to gain.
template <typename T, typename... Args>
auto GcNew(Args&&... args) -> value::ObjectRef {
  std::shared_ptr<T> owned = std::make_shared<T>(std::forward<Args>(args)...);
  if constexpr (std::derived_from<T, GcObject>) {
    owned->AdoptIdentity(owned.get());
  }
  return RefToObject(std::move(owned));
}

// The same object seen as `To`, where the program point holding it saw it as
// `From` (LRM 8.14, 8.26.5). The identity passes through untouched; only the
// view is formed, and it is formed by the target language's conversion rules
// because forming a pointer is what those rules are for. Whether the conversion
// is permitted was settled before lowering and is not asked here.
template <typename From, typename To>
auto ViewAs(const value::ObjectRef& ref) -> value::ObjectRef {
  return value::ObjectRef(ref.Handle(), static_cast<To*>(ref.View<From>()));
}

// The reference referring to the object the running subroutine was invoked on
// (LRM 8.11). The identity is the one the object recorded when it was created;
// the share comes from the object, aliased onto that identity so the reference
// owns what every other reference to the object owns while naming what they
// name. The view is the receiver itself, which is the class whose body is
// running -- what a derived class's `this` must be.
template <typename T>
auto SelfHandle(T* self) -> value::ObjectRef {
  return value::ObjectRef(
      value::ManagedRef(
          std::shared_ptr<void>(
              self->shared_from_this(), self->IdentityAddress())),
      self);
}

}  // namespace lyra::runtime

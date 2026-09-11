#pragma once

#include <concepts>
#include <cstddef>
#include <memory>
#include <utility>

#include "lyra/base/simulation_error.hpp"

namespace lyra::runtime {

// LRM 8.4: reaching a non-static member or a virtual method through a null
// object handle is illegal, and the result is indeterminate. Which handle a
// variable holds is a value the design computed, so the access is the design's
// own failure and answers to the run rather than to the host.
[[noreturn]] inline void RaiseNullObjectHandleAccess() {
  throw SimulationError(
      "a member reached through a null object handle "
      "(LRM 8.4)");
}

// Which object a reference names. Fixed when the object is created and carried
// unchanged by every reference to it, so two references naming one object agree
// whatever each was declared as. It is never computed from a reference's view:
// which subobject sits at an object's own address depends on whether the design
// declared a virtual method, so nothing derived from a view answers this for
// every class.
//
// It carries the share of the object's lifetime as well as the address, because
// a reference keeps its object alive while the lifetime is realized by sharing.
// The two separate when reachability is what retains an object.
class ObjectIdentity {
 public:
  ObjectIdentity() = default;

  explicit ObjectIdentity(std::shared_ptr<void> share)
      : share_(std::move(share)) {
  }

  // The share itself, which a boundary that keeps an object alive without
  // naming its class carries instead of the address.
  [[nodiscard]] auto Share() const -> const std::shared_ptr<void>& {
    return share_;
  }

  friend auto operator==(const ObjectIdentity& a, const ObjectIdentity& b)
      -> bool {
    return a.share_.get() == b.share_.get();
  }

 private:
  std::shared_ptr<void> share_;
};

// A reference to an object the simulator owns (LRM 8.3), realizing the managed
// reference of the object model. Null is a legal value, copies are shallow, and
// equality is which object.
//
// One shape serves every static view. A program point's static view says what
// may be done through a reference, never what the reference is: two units hold
// one cell under different views wherever a name resolves at elaboration, so a
// representation that followed the view would put two of them on one storage
// and could be read only by arranging for the two to agree.
//
// So the view travels as the pointer the reference was formed with, and a point
// that names a class reads its own pointer back out exactly -- not a
// conversion, since that is the pointer that was stored. A point with no class
// to name reads the identity and leaves the view alone.
class ObjectRef {
 public:
  ObjectRef() = default;

  ObjectRef(std::nullptr_t) {  // NOLINT(google-explicit-constructor)
  }

  ObjectRef(ObjectIdentity identity, void* view)
      : identity_(std::move(identity)), view_(view) {
  }

  // The object, as the class this program point assumes. A type argument
  // stands where a reference to a cell needs none, because a cell is the one
  // thing it holds while this states which object it names and leaves what may
  // be read through it to the point holding it. Reaching through a reference
  // that names no object is the design's own failure (LRM 8.4).
  template <typename T>
  [[nodiscard]] auto Deref() const -> T& {
    if (view_ == nullptr) {
      RaiseNullObjectHandleAccess();
    }
    return *static_cast<T*>(view_);
  }

  // The same pointer where nothing is reached through it -- forming another
  // view of the object, which is defined on a reference naming none (LRM 8.4
  // admits null as a value and forbids only reaching through it).
  template <typename T>
  [[nodiscard]] auto View() const -> T* {
    return static_cast<T*>(view_);
  }

  [[nodiscard]] auto Identity() const -> const ObjectIdentity& {
    return identity_;
  }

  friend auto operator==(const ObjectRef& a, const ObjectRef& b) -> bool {
    return a.identity_ == b.identity_;
  }

 private:
  ObjectIdentity identity_;
  void* view_ = nullptr;
};

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
auto RefToObject(std::shared_ptr<T> owned) -> ObjectRef {
  T* view = owned.get();
  return ObjectRef(
      ObjectIdentity(std::shared_ptr<void>(std::move(owned))), view);
}

// Brings an object into existence and fixes its identity as the address the
// allocation produced. An object no reference is recovered from records
// nothing, which is why the record is written here rather than by a base every
// such object would have to gain.
template <typename T, typename... Args>
auto GcNew(Args&&... args) -> ObjectRef {
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
auto ViewAs(const ObjectRef& ref) -> ObjectRef {
  return ObjectRef(ref.Identity(), static_cast<To*>(ref.View<From>()));
}

// The reference referring to the object the running subroutine was invoked on
// (LRM 8.11). The identity is the one the object recorded when it was created;
// the share comes from the object, aliased onto that identity so the reference
// owns what every other reference to the object owns while naming what they
// name. The view is the receiver itself, which is the class whose body is
// running -- what a derived class's `this` must be.
template <typename T>
auto SelfHandle(T* self) -> ObjectRef {
  return ObjectRef(
      ObjectIdentity(
          std::shared_ptr<void>(
              self->shared_from_this(), self->IdentityAddress())),
      self);
}

}  // namespace lyra::runtime

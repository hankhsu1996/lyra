#pragma once

#include <concepts>
#include <memory>
#include <utility>

#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

struct ObjectDefinition;

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
// as a root, and this record goes with the realization rather than into it.
//
// The destructor is virtual here and nowhere below. A value of a class is
// released through a pointer to this base -- a scope by the tree that owns it,
// an object by the share that holds it -- so the kind has to be recovered at
// that pointer. Declaring it here rather than at whichever kind first needs it
// is what keeps this base at the address every entry is handed: a kind
// introducing the table pointer itself would take offset zero for it and push
// this off the front, and an entry taking an untyped address to be one of these
// would then read that table pointer as the first field.
class GcObject : public std::enable_shared_from_this<GcObject> {
 public:
  GcObject() = default;
  virtual ~GcObject() = default;
  GcObject(const GcObject&) = default;
  auto operator=(const GcObject&) -> GcObject& = default;
  GcObject(GcObject&&) = delete;
  auto operator=(GcObject&&) -> GcObject& = delete;

  // Called once, by the allocation, with the address the allocation produced.
  void AdoptIdentity(void* address) {
    identity_ = address;
  }

  [[nodiscard]] auto IdentityAddress() const -> void* {
    return identity_;
  }

  // What a class of the source language states about its own objects, which a
  // generated class redeclares with its own record. It is how an object answers
  // where its own properties live and which body answers a behavior -- the
  // questions a referrer with no name for the class cannot answer for itself.
  // A class whose objects the runtime lays out states none here and takes its
  // record where it is built, because there the record is what the object was
  // built from rather than something the class alone knows.
  static constexpr const ObjectDefinition* kClassRecord = nullptr;

  // Called once, as the object comes into existence, with what every object of
  // its class shares.
  void AdoptClass(const ObjectDefinition* of) {
    class_ = of;
  }

  [[nodiscard]] auto Class() const -> const ObjectDefinition* {
    return class_;
  }

 private:
  void* identity_ = nullptr;
  const ObjectDefinition* class_ = nullptr;
};

// A reference to an object whose typed owner is already in hand. The share
// names the complete object, so the address it stores is the object's identity
// -- which is what separates this from building a reference out of a pointer to
// some base, where the address is a subobject's and answers a different
// question.
template <typename T>
auto RefToObject(std::shared_ptr<T> owned) -> value::ObjectRef {
  T* view = owned.get();
  // What the share points at is the object itself, which for an object of a
  // source-language class is the one thing every such object is -- so an entry
  // reaching the object reads the share and needs no name for the class. The
  // conversion is the target language's own and happens here, where the
  // concrete type is in hand; deriving it from the share afterwards is what
  // nothing can do. The handle a process is named by (LRM 9.7) answers for no
  // class and keeps the object as it stands.
  if constexpr (std::derived_from<T, GcObject>) {
    std::shared_ptr<GcObject> object = std::move(owned);
    return value::ObjectRef(
        value::ManagedRef(std::shared_ptr<void>(std::move(object))), view);
  } else {
    return value::ObjectRef(
        value::ManagedRef(std::shared_ptr<void>(std::move(owned))), view);
  }
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
    if constexpr (T::kClassRecord != nullptr) {
      owned->AdoptClass(T::kClassRecord);
    }
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
// (LRM 8.11). The share comes from the object and names the object itself, so
// this reference owns what every other reference to the object owns and names
// what they name. The view is the receiver itself, which is the class whose
// body is running -- what a derived class's `this` must be.
template <typename T>
auto SelfHandle(T* self) -> value::ObjectRef {
  return value::ObjectRef(
      value::ManagedRef(std::shared_ptr<void>(self->shared_from_this())), self);
}

}  // namespace lyra::runtime

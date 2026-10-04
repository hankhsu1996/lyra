#pragma once

#include <concepts>
#include <memory>
#include <utility>

#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

class Observable;

// The root every object the runtime holds is derived from. Whoever holds an
// object holds it as this class, without knowing which class it is of, so the
// destructor is virtual. A body running on an object reaches it through a
// borrowed pointer, which serves every member access; LRM 8.11 `this` asks for
// a reference instead, which this part recovers from the share it holds.
//
// Recovering a reference from an object is what a shared-owner realization
// needs; where reachability retains an object a body already holds its receiver
// as a root, and this part goes with the realization rather than into it.
//
// Every member is defined in this class's own source file, because a unit
// building an object of a source-language class reaches each of them, and a
// definition written here would be compiled again by every such unit. The
// destructor defined there is also what emits this class's table once.
class GcObject : public std::enable_shared_from_this<GcObject> {
 public:
  GcObject();
  virtual ~GcObject();
  GcObject(const GcObject&);
  auto operator=(const GcObject&) -> GcObject&;
  GcObject(GcObject&&) = delete;
  auto operator=(GcObject&&) -> GcObject& = delete;

  // The one event source every property of this object shares (LRM 9.4.2): a
  // write to any of them reevaluates every expression that reached the object,
  // and what that expression is worth decides whether it was an event. It is
  // made when a wait first reaches the object, since most objects are never
  // waited on. A copy of an object starts with none: what waits on the one it
  // was copied from waits on that one.
  [[nodiscard]] auto EventSource() -> Observable&;

  // Whether a write to one of this object's properties has anyone to tell.
  [[nodiscard]] auto Watched() const -> bool;

  // A write to one of this object's properties is over.
  void PublishChange();

 private:
  std::unique_ptr<Observable> event_source_;
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

template <typename T, typename... Args>
auto GcNew(Args&&... args) -> value::ObjectRef {
  return RefToObject(std::make_shared<T>(std::forward<Args>(args)...));
}

// The same object seen as `To`, where the program point holding it saw it as
// `From` (LRM 8.14, 8.26.5). The identity passes through untouched; only the
// view is formed, and it is formed by the target language's conversion rules
// because forming a pointer is what those rules are for.
//
// The conversion is the checked one because a `$cast` forms the view to ask
// whether the object is one (LRM 8.16): toward a subclass or an interface class
// the object may not be, and then the answer refers to no object, which is
// what the cast reads. Toward a class `From` extends the conversion is known to
// hold and costs nothing to check.
template <typename From, typename To>
auto ViewAs(const value::ObjectRef& ref) -> value::ObjectRef {
  To* view = dynamic_cast<To*>(ref.View<From>());
  return view == nullptr ? value::ObjectRef{}
                         : value::ObjectRef(ref.Handle(), view);
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

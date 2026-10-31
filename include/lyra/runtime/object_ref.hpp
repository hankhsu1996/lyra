#pragma once

#include <concepts>
#include <memory>
#include <utility>

#include "lyra/runtime/class_definition.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

class RandomizationModes;
class Observable;
struct WaitMembership;
template <class Node>
class IntrusiveList;

// The root every object the runtime holds is derived from. Whoever holds an
// object holds it as this class, without knowing which class it is of, and
// reaches what its class does -- ending it included -- through the definition
// the object starts with. A body running on an object reaches its members
// through a borrowed pointer; a reference to it -- LRM 8.11 `this`, and the
// share a method of a managed class holds while it runs (LRM 8.4) -- this part
// recovers from the weak reference it keeps to the object.
//
// Recovering a reference from an object is what a shared-owner realization
// needs; where reachability retains an object a body already holds its receiver
// as a root, and this part goes with the realization rather than into it.
//
// Every member is defined in this class's own source file, because a unit
// building an object of a source-language class reaches each of them, and a
// definition written here would be compiled again by every such unit.
class GcObject : public ObjectHeader,
                 public std::enable_shared_from_this<GcObject> {
 public:
  // An object whose class is written by whoever creates it whole, before
  // anything of the design runs on it.
  GcObject();
  // An object of a class of this library, which states its own definition.
  explicit GcObject(const abi::ObjectDefinition& definition);
  ~GcObject();
  GcObject(const GcObject&);
  auto operator=(const GcObject&) -> GcObject&;
  GcObject(GcObject&&) = delete;
  auto operator=(GcObject&&) -> GcObject& = delete;

  // The one event source every property of this object shares (LRM 9.4.2): a
  // write to any of them reevaluates every expression that reached the object,
  // and what that expression is worth decides whether it was an event. A wait
  // watching it does not refer to the object (LRM 8.29), and the object's end
  // tells it nothing: nothing can write an object that is gone, so nothing a
  // wait read of it changes. A copy of an object starts with none: what waits
  // on the one it was copied from waits on that one.
  [[nodiscard]] auto EventSource() -> Observable&;

  // The waits that reached this object through a weak reference (LRM 8.30.3),
  // which its expiry does change: what the weak reference answers. They are
  // told once it has expired, never while it is being destroyed.
  [[nodiscard]] auto Expiry() -> IntrusiveList<WaitMembership>&;

  // Whether a write to one of this object's properties has anyone to tell.
  [[nodiscard]] auto Watched() const -> bool;

  // Which of this object's random variables and constraint blocks are on (LRM
  // 18.8, 18.9), made the first time it is asked for, since most objects are
  // never asked. A copy starts with every one on, as a new object does.
  [[nodiscard]] auto Modes() -> RandomizationModes&;

  // A write to one of this object's properties is over.
  void PublishChange();

  // The reference the code creating this object holds, taken over by the
  // `new` that created it as soon as the object exists whole and before its
  // constructor runs, so the constructor runs on an object already referred to
  // and may hand `this` on (LRM 8.11), as an object starts with one reference
  // its creator owns (Swift's `swift_allocObject`, CPython's `tp_new`). An
  // object one owner holds alone -- an instance of the design hierarchy, which
  // its parent owns -- keeps it to the end, where it ends nothing.
  [[nodiscard]] auto TakeCreatorsShare() -> std::shared_ptr<GcObject>;

 private:
  // What waits on this object, made when a wait first reaches it, since most
  // objects are never waited on.
  struct Watchers;

  [[nodiscard]] auto WatchersOf() -> Watchers&;

  std::unique_ptr<Watchers> watchers_;
  std::unique_ptr<RandomizationModes> modes_;
  std::shared_ptr<GcObject> creators_share_;
};

// A reference to an object whose owner is already in hand. The share names the
// object itself, so the address it stores is the object's identity.
template <typename T>
auto RefToObject(std::shared_ptr<T> owned) -> value::ObjectRef {
  return value::ObjectRef(
      value::ManagedRef(std::shared_ptr<void>(std::move(owned))));
}

// A handle owning `object`, created whole and not yet constructed. It is the
// reference its creator holds while the constructor runs, which the program's
// `new` answers (LRM 8.3), and it ends the object through its definition when
// the last reference to it goes.
[[nodiscard]] auto AdoptObject(GcObject* object) -> value::ObjectRef;

// A SystemVerilog `new` of an object of class `T` (LRM 8.7): the object is
// created whole -- `T`'s C++ constructor builds every member of every class it
// extends, and runs none of the design's code, and the object is given its
// class, `definition` -- then held, then constructed by `construct`, the
// class's constructor, with `args`. The holder is made before the constructor
// runs, so a construction left partway lets go of the object and leaves it to
// whatever else refers to it (LRM 8.29), and a virtual call the constructor
// makes answers for `T` (LRM 8.17).
template <std::derived_from<GcObject> T, typename... Params, typename... Args>
auto CreateObject(
    const abi::ObjectDefinition* definition, void (T::*construct)(Params...),
    Args&&... args) -> value::ObjectRef {
  auto created = std::make_unique<T>();
  created->definition = definition;
  value::ObjectRef held = AdoptObject(created.get());
  // The object ends through the reference from here, which every holder of it
  // shares.
  T* object = created.release();
  (object->*construct)(std::forward<Args>(args)...);
  return held;
}

// A call dispatching on a behavior a class introduced (LRM 8.20): the body the
// class of `self` gives it, read off that class's table as `Table`, the table
// type of the class introducing the behavior, at its member `Slot`, entered on
// `self` with `args`. A table of a class extending the introducer begins with
// the introducer's, so the member lies where it does in the introducer's.
template <typename Table, auto Slot, typename... Args>
auto Dispatch(GcObject& self, Args&&... args) -> decltype(auto) {
  const auto* table = static_cast<const Table*>(self.definition->methods);
  return (table->*Slot)(&self, std::forward<Args>(args)...);
}

// A call through a handle of the interface class whose definition is
// `Interface` (LRM 8.26.5): the same, read off the table the class of `self`
// gives that interface class.
template <
    typename Table, auto Slot, const abi::ObjectDefinition* Interface,
    typename... Args>
auto InterfaceDispatch(GcObject& self, Args&&... args) -> decltype(auto) {
  const auto* table =
      static_cast<const Table*>(InterfaceTableOf(self.definition, Interface));
  return (table->*Slot)(&self, std::forward<Args>(args)...);
}

// Ends the object of exactly the class `T` whose storage starts at `object`:
// how the definition of `T` states its objects end, where `T` is a class C++
// knows whole.
template <std::derived_from<GcObject> T>
void EndObjectOf(void* object) noexcept {
  std::default_delete<T>{}(static_cast<T*>(object));
}

// The definition of a class of this library whose objects the library itself
// creates: one extending nothing a design names, whose objects end as `T`'s.
template <std::derived_from<GcObject> T>
auto LibraryObjectDefinition() -> abi::ObjectDefinition {
  return abi::ObjectDefinition{
      .base = nullptr,
      .scope = nullptr,
      .class_object = nullptr,
      .end = &EndObjectOf<T>,
      .interfaces = {},
      .methods = nullptr};
}

// Ends an object one owner holds alone, through its class's definition, so the
// owner may hold it as any class it is an object of.
struct EndOwnedObject {
  void operator()(GcObject* object) const noexcept;
};

// An object one owner holds alone: an instance of the design hierarchy, which
// its parent owns.
template <typename T>
using OwnedObject = std::unique_ptr<T, EndOwnedObject>;

// The same as `CreateObject` for an object one owner holds alone.
template <std::derived_from<GcObject> T, typename... Params, typename... Args>
auto CreateOwned(
    const abi::ObjectDefinition* definition, void (T::*construct)(Params...),
    Args&&... args) -> OwnedObject<T> {
  auto created = std::make_unique<T>();
  created->definition = definition;
  OwnedObject<T> held(created.release());
  (held.get()->*construct)(std::forward<Args>(args)...);
  return held;
}

// The reference referring to the object the running subroutine was invoked on
// (LRM 8.11). The share comes from the object and names the object itself, so
// this reference owns what every other reference to the object owns and names
// what they name.
[[nodiscard]] auto SelfHandle(GcObject* self) -> value::ObjectRef;

}  // namespace lyra::runtime

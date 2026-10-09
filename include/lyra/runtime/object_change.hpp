#pragma once

#include "lyra/runtime/object_ref.hpp"
#include "lyra/value/managed_ref.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

class Observable;

// The object a handle names, as the root every object shares, which is the
// address the entries below take.
[[nodiscard]] auto ObjectRootOf(const value::ManagedRef& handle) -> GcObject*;

[[nodiscard]] inline auto ObjectRootOf(const value::ObjectRef& ref)
    -> GcObject* {
  return ObjectRootOf(ref.Handle());
}

// The event source of an object, which a wait reaching the object enrols on
// (LRM 9.4.2), given whatever reaches it: a handle, or the running method's
// own object. Reaching a member through a handle naming no object is the
// design's own failure (LRM 8.4).
[[nodiscard]] auto EventSourceOf(GcObject* object) -> Observable*;

[[nodiscard]] inline auto EventSourceOf(const value::ObjectRef& ref)
    -> Observable* {
  return EventSourceOf(ObjectRootOf(ref));
}

// A write in progress into a property of an object (LRM 8.4), held by whoever
// writes for as long as the write lasts. Ending it tells the object that one of
// its properties was written, which reevaluates every expression waiting on it
// (LRM 9.4.2); the expression decides whether that was an event, so the write
// keeps nothing from before it.
//
// It is opened on the object alone, and the writer reaches the property through
// it, so the write lasts as long as the full-expression that writes there does
// and ends after the value has landed. Reaching a property through a handle
// naming no object is the design's own failure (LRM 8.4), so opening one there
// fails.
class ErasedObjectWrite {
 public:
  explicit ErasedObjectWrite(GcObject* object);
  ErasedObjectWrite(const ErasedObjectWrite&) = delete;
  auto operator=(const ErasedObjectWrite&) -> ErasedObjectWrite& = delete;
  ErasedObjectWrite(ErasedObjectWrite&&) = delete;
  auto operator=(ErasedObjectWrite&&) -> ErasedObjectWrite& = delete;
  ~ErasedObjectWrite();

  [[nodiscard]] auto Object() const -> GcObject*;

 private:
  GcObject* object_;
};

// The same write, opened on whatever reaches the object -- a handle, or the
// running method's own object -- and dereferenced as the class `C` the writer
// reaches the property through, the way a guard is dereferenced to what it
// guards. Which class that is, is the writer's to know; the write needs only
// the object, so what the library compiles once is the write above.
template <class C>
class ObjectWrite : public ErasedObjectWrite {
 public:
  explicit ObjectWrite(const value::ObjectRef& handle)
      : ErasedObjectWrite(ObjectRootOf(handle)), object_(handle.View<C>()) {
  }

  explicit ObjectWrite(C* object) : ErasedObjectWrite(object), object_(object) {
  }

  [[nodiscard]] auto operator*() const -> C& {
    return *object_;
  }

  [[nodiscard]] auto operator->() const -> C* {
    return object_;
  }

 private:
  C* object_;
};

}  // namespace lyra::runtime

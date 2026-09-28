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

// The event source of the object `object` points at, which a wait reaching the
// object subscribes to (LRM 9.4.2). Reaching a member through a handle naming
// no object is the design's own failure (LRM 8.4).
[[nodiscard]] auto EventSourceOf(GcObject* object) -> Observable*;

// A write in progress into a property of an object (LRM 8.4), held by whoever
// writes for as long as the write lasts. Ending it tells the object that one of
// its properties was written, which reevaluates every expression waiting on it
// (LRM 9.4.2); the expression decides whether that was an event, so the write
// keeps nothing from before it.
//
// It is opened on the object for the place written, and the writer reaches the
// place through it, so the write lasts as long as the full-expression that
// writes there does and ends after the value has landed.
class ObjectWrite {
 public:
  ObjectWrite(GcObject* object, const void* place);
  ObjectWrite(const ObjectWrite&) = delete;
  auto operator=(const ObjectWrite&) -> ObjectWrite& = delete;
  ObjectWrite(ObjectWrite&&) = delete;
  auto operator=(ObjectWrite&&) -> ObjectWrite& = delete;
  ~ObjectWrite();

  // The place the write was opened for.
  [[nodiscard]] auto Place() const -> const void*;

 private:
  GcObject* object_;
  const void* place_;
};

}  // namespace lyra::runtime

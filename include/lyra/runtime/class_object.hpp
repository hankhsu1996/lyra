#pragma once

#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// The part every object of a class starts with (LRM 8.3), whether the source
// or this library defines the class: what the language gives every class
// object whatever its class declares, which is its own generator (LRM 18.14.1,
// 18.14.3). An object `new` makes has its generator split from whatever is
// running there -- a process, or a static initialization, whose generator its
// container's initialization RNG gave -- so its draws follow from where it was
// made and from nothing else. `randomize()` draws from it (LRM 18.15).
//
// A copy made by `new` (LRM 8.12) is a new object, so it is seeded as any new
// object is rather than continuing the stream of the one it copies.
//
// What `randomize()` asks of the object (LRM 18.6) its class's definition
// states, and the functions below enter it there, on the object as the root
// every object is.
//
// Every member is defined in the library, as the root's are.
class ClassObject : public GcObject {
 public:
  ClassObject();
  // An object of a class of this library, which states its own definition and
  // is handed the generator it starts with rather than splitting one.
  ClassObject(const abi::ObjectDefinition& definition, DrawRng generator);
  ~ClassObject();
  ClassObject(const ClassObject& other);
  auto operator=(const ClassObject& other) -> ClassObject&;
  ClassObject(ClassObject&&) = delete;
  auto operator=(ClassObject&&) -> ClassObject& = delete;

  [[nodiscard]] auto Generator() -> DrawRng& {
    return generator_;
  }

 private:
  DrawRng generator_;
};

// What `randomize()` asks of `object` (LRM 18.6), each the body its class's
// definition states, or nothing where it states none. `problem` is the call's
// problem, a handle to it, through which a class states what it adds and
// hands nested objects on.
void ObjectRandVariables(GcObject& object, const value::ObjectRef& problem);
void ObjectConstrain(GcObject& object, const value::ObjectRef& problem);
void ObjectRandObjects(GcObject& object, const value::ObjectRef& problem);

// The built-in `pre_randomize()` and `post_randomize()` (LRM 18.6.2) on the
// object reached, where no class of its lineage writes its own: the part every
// object starts with gives them, and a call naming them reaches that part's
// own rather than dispatching, as a call naming any method a class inherits
// does.
void BuiltinPreRandomize(GcObject* object);
void BuiltinPostRandomize(GcObject* object);
inline void BuiltinPreRandomize(const value::ObjectRef& handle) {
  BuiltinPreRandomize(ObjectRootOf(handle));
}
inline void BuiltinPostRandomize(const value::ObjectRef& handle) {
  BuiltinPostRandomize(ObjectRootOf(handle));
}

// The hooks the object's own class runs, as `randomize()` runs them. A call
// naming one through an interface class's handle runs these: LRM 8.26.9 calls
// an interface class's two hooks virtual while 18.6.2 calls a class's not, so
// the standard leaves which body that is open, and the object's own is the one
// hook of each kind it has whatever handle reaches it.
void ObjectPreRandomize(GcObject* object);
void ObjectPostRandomize(GcObject* object);
inline void ObjectPreRandomize(const value::ObjectRef& handle) {
  ObjectPreRandomize(ObjectRootOf(handle));
}
inline void ObjectPostRandomize(const value::ObjectRef& handle) {
  ObjectPostRandomize(ObjectRootOf(handle));
}

// LRM 18.13.3 -- 18.13.5, on the generator of the object reached: through a
// handle, or as the running method's own object. Reaching one through a
// handle naming no object is the design's own failure (LRM 8.4).
void ObjectSrandom(ClassObject* object, const value::ConstIntegralView& seed);

template <value::IntegralValue T>
void ObjectSrandom(ClassObject* object, const T& seed) {
  ObjectSrandom(object, seed.AsWords().View());
}
template <value::IntegralValue T>
void ObjectSrandom(const value::ObjectRef& handle, const T& seed) {
  ObjectSrandom(&handle.Deref<ClassObject>(), seed.AsWords().View());
}

[[nodiscard]] auto ObjectGetRandstate(ClassObject* object) -> value::String;
[[nodiscard]] inline auto ObjectGetRandstate(const value::ObjectRef& handle)
    -> value::String {
  return ObjectGetRandstate(&handle.Deref<ClassObject>());
}

// A state no `get_randstate` answered is undefined (LRM 18.13.5), and it is a
// value the design computed, so setting one is the design's error and leaves
// the generator as it was.
void ObjectSetRandstate(ClassObject* object, const value::String& state);
inline void ObjectSetRandstate(
    const value::ObjectRef& handle, const value::String& state) {
  ObjectSetRandstate(&handle.Deref<ClassObject>(), state);
}

}  // namespace lyra::runtime

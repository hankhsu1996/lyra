#pragma once

#include <memory>

#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/randomization.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// The part every object of a source class starts with (LRM 8.3): what the
// language gives every class object whatever its class declares, which is its
// own generator (LRM 18.14.1, 18.14.3). An object's generator is split from
// whatever is running where `new` makes it -- a process, or a static
// initialization, whose generator its container's initialization RNG gave --
// so an object's draws follow from where it was made and from nothing else.
// `randomize()` draws from it and nothing else does (LRM 18.15).
//
// A copy made by `new` (LRM 8.12) is a new object, so it is seeded as any new
// object is rather than continuing the stream of the one it copies.
//
// It also declares what `randomize()` asks of every class (LRM 18.6), as five
// virtual functions: declaring its random variables (LRM 18.4) and stating its
// constraints (LRM 18.5) to a problem, naming the random objects its handles
// reach (LRM 18.4), and the two hooks around a call (LRM 18.6.2). Here each
// does nothing, which is what a class declaring nothing random states and what
// the built-in hooks do. A class extending another does what the one it extends
// does and then what it adds.
//
// Every member is defined in the library, as the root's are.
class ClassObject : public GcObject {
 public:
  ClassObject();
  ~ClassObject() override;
  ClassObject(const ClassObject& other);
  auto operator=(const ClassObject& other) -> ClassObject&;
  ClassObject(ClassObject&&) = delete;
  auto operator=(ClassObject&&) -> ClassObject& = delete;

  [[nodiscard]] auto Generator() -> DrawRng& {
    return generator_;
  }

  // What this object keeps of its randomization between calls, made the
  // first time it is asked for.
  [[nodiscard]] auto Modes() -> RandomizationModes&;

  // `problem` is the call's problem, a handle to it, through which a class
  // states what it adds and hands nested objects on.
  virtual void sv_rand_variables(const value::ObjectRef& problem);
  virtual void sv_constrain(const value::ObjectRef& problem);
  virtual void sv_rand_objects(const value::ObjectRef& problem);
  virtual void sv_pre_randomize();
  virtual void sv_post_randomize();

 private:
  DrawRng generator_;
  std::unique_ptr<RandomizationModes> modes_;
};

// The object a class handle or a method's own object reaches, as the class
// object it is. Every object a handle of a source class names is one.
[[nodiscard]] auto AsClassObject(GcObject* object) -> ClassObject&;

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

// The hooks the object's own class runs, as `randomize()` runs them: the two an
// interface class contains are virtual (LRM 8.26.9), so a call naming them
// through its handle dispatches on the object.
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
void ObjectSrandom(GcObject* object, const value::ConstIntegralView& seed);

template <value::IntegralValue T>
void ObjectSrandom(GcObject* object, const T& seed) {
  ObjectSrandom(object, seed.AsWords().View());
}
template <value::IntegralValue T>
void ObjectSrandom(const value::ObjectRef& handle, const T& seed) {
  ObjectSrandom(ObjectRootOf(handle), seed.AsWords().View());
}

[[nodiscard]] auto ObjectGetRandstate(GcObject* object) -> value::String;
[[nodiscard]] inline auto ObjectGetRandstate(const value::ObjectRef& handle)
    -> value::String {
  return ObjectGetRandstate(ObjectRootOf(handle));
}

// A state no `get_randstate` answered is undefined (LRM 18.13.5), and it is a
// value the design computed, so setting one is the design's error and leaves
// the generator as it was.
void ObjectSetRandstate(GcObject* object, const value::String& state);
inline void ObjectSetRandstate(
    const value::ObjectRef& handle, const value::String& state) {
  ObjectSetRandstate(ObjectRootOf(handle), state);
}

}  // namespace lyra::runtime

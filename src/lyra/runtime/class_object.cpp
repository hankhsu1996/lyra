#include "lyra/runtime/class_object.hpp"

#include <cstdint>
#include <optional>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

ClassObject::ClassObject()
    : generator_(current_runtime().Running().ObjectGenerator()) {
}

ClassObject::ClassObject(
    const abi::ObjectDefinition& definition, DrawRng generator)
    : GcObject(definition), generator_(generator) {
}

ClassObject::~ClassObject() = default;

// A copy's generator is its own, as a new object's is.
ClassObject::ClassObject(const ClassObject& other)
    : GcObject(other),
      generator_(current_runtime().Running().ObjectGenerator()) {
}

// Assigning changes what an object holds, and its generator is not among
// what the source can assign.
auto ClassObject::operator=(const ClassObject& other) -> ClassObject& {
  if (this == &other) {
    return *this;
  }
  GcObject::operator=(other);
  return *this;
}

namespace {

// The body `entry` of the object's class's randomization, entered on the
// object with `args` after it, or nothing where the class states none.
template <typename... Args>
void EnterRandomization(
    GcObject& object,
    void (*abi::ClassObjectInfo::*entry)(GcObject*, const Args&...),
    const Args&... args) {
  const abi::ClassObjectInfo* info = object.definition->class_object;
  if (info == nullptr) {
    throw InternalError(
        "ClassObject: a class object has a definition that states "
        "nothing of its randomization -- please report this as a bug");
  }
  if (const auto body = info->*entry; body != nullptr) {
    body(&object, args...);
  }
}

}  // namespace

void ObjectRandVariables(GcObject& object, const value::ObjectRef& problem) {
  EnterRandomization(object, &abi::ClassObjectInfo::rand_variables, problem);
}

void ObjectConstrain(GcObject& object, const value::ObjectRef& problem) {
  EnterRandomization(object, &abi::ClassObjectInfo::constrain, problem);
}

void ObjectRandObjects(GcObject& object, const value::ObjectRef& problem) {
  EnterRandomization(object, &abi::ClassObjectInfo::rand_objects, problem);
}

void BuiltinPreRandomize() {
}

void BuiltinPostRandomize() {
}

void ObjectPreRandomize(GcObject* object) {
  EnterRandomization(*object, &abi::ClassObjectInfo::pre_randomize);
}

void ObjectPostRandomize(GcObject* object) {
  EnterRandomization(*object, &abi::ClassObjectInfo::post_randomize);
}

void ObjectSrandom(ClassObject* object, const value::ConstIntegralView& seed) {
  object->Generator().Reseed(
      RandomSeed{static_cast<std::uint32_t>(value::ToInt64(seed))});
}

auto ObjectGetRandstate(ClassObject* object) -> value::String {
  return value::String{object->Generator().StateText()};
}

void ObjectSetRandstate(
    ClassObject* object, const value::String& state, base::SourceLocation at) {
  const std::optional<DrawRng> restored = DrawRng::FromStateText(state.View());
  if (!restored.has_value()) {
    const base::SourceLocationScope bound(at);
    throw SimulationError(
        "'set_randstate' was given a state that no 'get_randstate' returned");
  }
  object->Generator() = *restored;
}

}  // namespace lyra::runtime

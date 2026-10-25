#include "lyra/runtime/class_object.hpp"

#include <cstdint>
#include <memory>
#include <optional>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/rng.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

// The generator of an object made now (LRM 18.14.1): the next split of
// whatever is running, a process or a static initialization.
auto SplitFromRunning() -> DrawRng {
  return current_runtime().Running().rng.Split();
}

}  // namespace

ClassObject::ClassObject() : generator_(SplitFromRunning()) {
}

ClassObject::~ClassObject() = default;

// The modes and cycles a copy starts with are its own, all on, as a new
// object's are.
ClassObject::ClassObject(const ClassObject& other)
    : GcObject(other), generator_(SplitFromRunning()) {
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

auto ClassObject::Modes() -> RandomizationModes& {
  if (modes_ == nullptr) {
    modes_ = std::make_unique<RandomizationModes>();
  }
  return *modes_;
}

void ClassObject::sv_rand_variables(const value::ObjectRef&) {
}

void ClassObject::sv_constrain(const value::ObjectRef&) {
}

void ClassObject::sv_rand_objects(const value::ObjectRef&) {
}

void ClassObject::sv_pre_randomize() {
}

void ClassObject::sv_post_randomize() {
}

void BuiltinPreRandomize(GcObject* object) {
  AsClassObject(object).ClassObject::sv_pre_randomize();
}

void BuiltinPostRandomize(GcObject* object) {
  AsClassObject(object).ClassObject::sv_post_randomize();
}

void ObjectPreRandomize(GcObject* object) {
  AsClassObject(object).sv_pre_randomize();
}

void ObjectPostRandomize(GcObject* object) {
  AsClassObject(object).sv_post_randomize();
}

auto AsClassObject(GcObject* object) -> ClassObject& {
  if (object == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
  auto* class_object = dynamic_cast<ClassObject*>(object);
  if (class_object == nullptr) {
    throw InternalError(
        "AsClassObject: an object of no source class reached a class "
        "object's method -- please report this as a bug");
  }
  return *class_object;
}

void ObjectSrandom(GcObject* object, const value::ConstIntegralView& seed) {
  AsClassObject(object).Generator().Reseed(
      RandomSeed{static_cast<std::uint32_t>(value::ToInt64(seed))});
}

auto ObjectGetRandstate(GcObject* object) -> value::String {
  return value::String{AsClassObject(object).Generator().StateText()};
}

void ObjectSetRandstate(GcObject* object, const value::String& state) {
  const std::optional<DrawRng> restored = DrawRng::FromStateText(state.View());
  if (!restored.has_value()) {
    throw SimulationError(
        "'set_randstate' was given a state that no 'get_randstate' returned");
  }
  AsClassObject(object).Generator() = *restored;
}

}  // namespace lyra::runtime

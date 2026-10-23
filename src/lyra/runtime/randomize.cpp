#include "lyra/runtime/randomize.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/class_object.hpp"
#include "lyra/runtime/constraint_cycle.hpp"
#include "lyra/runtime/constraint_problem.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/randomization.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::runtime {

namespace {

auto ProblemOf(const value::ObjectRef& handle) -> ConstraintProblem& {
  auto* problem = dynamic_cast<ConstraintProblem*>(ObjectRootOf(handle));
  if (problem == nullptr) {
    throw InternalError(
        "randomize: a call's problem names no problem -- please report this "
        "as a bug");
  }
  return *problem;
}

// The modes of whatever owns the storage `variable` names: the object for a
// property, and the run for a static property or anything else that lives as
// long as the run.
auto ModesOf(const ErasedReference& variable) -> RandomizationModes& {
  return std::visit(
      Overloaded{
          [](GcObject* object) -> RandomizationModes& {
            return AsClassObject(object).Modes();
          },
          [](std::monostate) -> RandomizationModes& {
            return StaticRandomizationModes();
          },
          [](VariableCell*) -> RandomizationModes& {
            return StaticRandomizationModes();
          },
          [](const HeldByNet&) -> RandomizationModes& {
            return StaticRandomizationModes();
          }},
      variable.holder);
}

auto Begin(GcObject* object, bool listed, RuntimeEffects& runtime)
    -> value::ObjectRef {
  ClassObject& randomized = AsClassObject(object);
  auto made = std::make_unique<ConstraintProblem>(
      runtime, randomized.Generator(), SelfHandle(object), listed);
  ConstraintProblem* problem = made.get();
  value::ObjectRef handle = AdoptObject(made.release());
  // `pre_randomize()` runs on the object and then on every random object it
  // reaches (LRM 18.6.2), each asked after its own hook for the ones it
  // reaches, since the hook may make them. The list grows as it is walked, so
  // it is walked by position. A call listing what it chooses makes every
  // handle it does not list a state variable (LRM 18.11), so the object
  // reaches none.
  randomized.sv_pre_randomize();
  if (!listed) {
    randomized.sv_rand_objects(handle);
  }
  for (std::size_t position = 0; position < problem->Nested().size();
       ++position) {
    ClassObject& nested =
        AsClassObject(ObjectRootOf(problem->Nested()[position]));
    nested.sv_pre_randomize();
    nested.sv_rand_objects(handle);
  }
  return handle;
}

}  // namespace

auto RandomizeObject(GcObject* object, RuntimeEffects& runtime)
    -> value::ObjectRef {
  return Begin(object, false, runtime);
}

auto RandomizeObjectListed(GcObject* object, RuntimeEffects& runtime)
    -> value::ObjectRef {
  return Begin(object, true, runtime);
}

auto RandomizeScope(RuntimeEffects& runtime) -> value::ObjectRef {
  auto made = std::make_unique<ConstraintProblem>(
      runtime, runtime.Running().rng, value::ObjectRef{}, true);
  return AdoptObject(made.release());
}

void RandomizeConstrain(const value::ObjectRef& problem) {
  ConstraintProblem& p = ProblemOf(problem);
  GcObject* object = ObjectRootOf(p.Object());
  if (object == nullptr) {
    return;
  }
  // Every object declares its random variables before any states a
  // constraint, so a constraint naming one, of its own object or of another
  // the call reaches (LRM 18.5.8), names the variable that was declared.
  AsClassObject(object).sv_rand_variables(problem);
  for (const value::ObjectRef& nested : p.Nested()) {
    AsClassObject(ObjectRootOf(nested)).sv_rand_variables(problem);
  }
  AsClassObject(object).sv_constrain(problem);
  for (const value::ObjectRef& nested : p.Nested()) {
    AsClassObject(ObjectRootOf(nested)).sv_constrain(problem);
  }
}

auto RandomizeFinish(const value::ObjectRef& problem) -> value::Int {
  ConstraintProblem& p = ProblemOf(problem);
  if (p.Failed() || !p.Solve()) {
    p.Abandon();
    return value::Int::FromInt(0);
  }
  p.Commit();
  if (GcObject* object = ObjectRootOf(p.Object()); object != nullptr) {
    AsClassObject(object).sv_post_randomize();
    for (const value::ObjectRef& nested : p.Nested()) {
      AsClassObject(ObjectRootOf(nested)).sv_post_randomize();
    }
  }
  return value::Int::FromInt(1);
}

// How the array the storage `whole` names, of the library's own type `type`,
// is resized: kept as far as it reaches and grown by elements at their
// default (LRM 7.5.1), written as an assignment to it is, and put back by
// writing what it held. Nothing for a type that is no array of a size the
// program chooses.
auto ResizeOf(const ErasedReference& whole, const value::ValueType& type)
    -> std::optional<ArrayResizer> {
  if (&type == &value::lyra_rt_dynarray_value_type) {
    return [whole, &type](std::size_t count) -> std::function<void()> {
      const auto& current =
          *static_cast<const value::RuntimeDynamicArray*>(whole.storage);
      auto before = std::make_shared<value::RuntimeDynamicArray>(current);
      const value::RuntimeDynamicArray resized(
          current.ElementType(), count, &current);
      WriteLaidOutThrough(whole, type, &resized);
      return [whole, &type, before] {
        WriteLaidOutThrough(whole, type, before.get());
      };
    };
  }
  if (&type == &value::lyra_rt_queue_value_type) {
    return [whole, &type](std::size_t count) -> std::function<void()> {
      const auto& current =
          *static_cast<const value::RuntimeQueue*>(whole.storage);
      auto before = std::make_shared<value::RuntimeQueue>(current);
      value::RuntimeQueue resized = current;
      while (resized.Count() < count) {
        resized.PushBack(resized.ElementType().Default());
      }
      while (resized.Count() > count) {
        resized.DeleteIndex(static_cast<std::int64_t>(resized.Count() - 1));
      }
      WriteLaidOutThrough(whole, type, &resized);
      return [whole, &type, before] {
        WriteLaidOutThrough(whole, type, before.get());
      };
    };
  }
  return std::nullopt;
}

namespace {

// The cycle a `randc` variable runs through (LRM 18.4.2): every value of its
// width, or the positions of its enumeration's members where it is one.
auto CycleOf(
    RandomizationModes& modes, const DeclaredPart& part, std::uint64_t width)
    -> ConstraintCycle& {
  if (part.members == nullptr) {
    return modes.CycleOf(part.path, width, std::nullopt);
  }
  const std::uint64_t count = part.members->members;
  return modes.CycleOf(
      part.path, std::bit_width(count == 0 ? 0 : count - 1), count);
}

}  // namespace

// Each integral part of the storage `variable` names, holding `value` now, of
// type `type`, declared as `declared` says: the storage itself where it is
// integral, and every element of an array, at any depth, each by its own
// storage (LRM 18.4), within the innermost array whose size the program
// chooses. An element is chosen where the array's mode and its own are both
// on.
void DeclareParts(
    ConstraintProblem& problem, RandomizationModes& modes,
    const ErasedReference& variable, const void* value,
    const value::ValueType& type, RandomQualifier declared,
    const DeclaredPart& part) {
  const bool chosen = part.enabled && modes.RandEnabled(part.path);
  if (const value::IntegralValueType* integral = type.AsIntegral()) {
    ConstraintCycle* cycle =
        declared == RandomQualifier::kRandc
            ? &CycleOf(modes, part, integral->Shape().width)
            : nullptr;
    problem.Declare(
        variable, value, type, declared, chosen, cycle, part.members,
        part.within);
    return;
  }
  if (!type.HasParts()) {
    throw SimulationError(
        "randomizing a random variable of this type is not yet supported");
  }
  std::optional<std::int64_t> array = part.within;
  if (std::optional<ArrayResizer> resize = ResizeOf(variable, type)) {
    array = problem.DeclareArray(variable, *std::move(resize), chosen);
  }
  const value::ValueType& element = type.PartType(value);
  const std::size_t count = type.PartCount(value);
  for (std::size_t position = 0; position < count; ++position) {
    DeclareParts(
        problem, modes,
        ErasedReference{
            .holder = variable.holder,
            .storage = type.PartAt(variable.storage, position)},
        type.PartAt(value, position), element, declared,
        part.Into(position, chosen, array));
  }
}

void ConstraintDeclareErased(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const void* value, const value::ValueType& type, RandomQualifier declared,
    const DeclaredPart& part) {
  DeclareParts(
      ProblemOf(problem), ModesOf(variable), variable, value, type, declared,
      part);
}

auto DeclaredWhole(
    const value::ObjectRef& problem, const ErasedReference& variable)
    -> DeclaredPart {
  return DeclaredPart{
      .path = RandomVariablePath{.declared = variable.storage, .positions = {}},
      .enabled = true,
      .members = ProblemOf(problem).MembersOf(variable),
      .within = std::nullopt};
}

void ConstraintMembersOf(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const value::Enumeration& members) {
  ProblemOf(problem).Members(variable, members);
}

auto NamedMembers(
    const value::ObjectRef& problem, const ErasedReference& variable)
    -> const value::Enumeration* {
  return ProblemOf(problem).MembersOf(variable);
}

void RandomizeListErased(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const void* value, const value::ValueType& type,
    const value::Enumeration* members, std::optional<std::int64_t> within) {
  ConstraintProblem& p = ProblemOf(problem);
  if (type.AsIntegral() != nullptr) {
    p.List(variable, value, type, members, within);
    return;
  }
  if (!type.HasParts()) {
    throw SimulationError(
        "randomizing a listed variable of this type is not yet supported");
  }
  std::optional<std::int64_t> array = within;
  if (std::optional<ArrayResizer> resize = ResizeOf(variable, type)) {
    array = p.ListArray(variable, *std::move(resize));
  }
  const value::ValueType& part = type.PartType(value);
  const std::size_t count = type.PartCount(value);
  for (std::size_t position = 0; position < count; ++position) {
    RandomizeListErased(
        problem,
        ErasedReference{
            .holder = variable.holder,
            .storage = type.PartAt(variable.storage, position)},
        type.PartAt(value, position), part, members, array);
  }
}

auto RandomizeListArrayErased(
    const value::ObjectRef& problem, const ErasedReference& array,
    ArrayResizer resize) -> std::int64_t {
  return ProblemOf(problem).ListArray(array, std::move(resize));
}

auto ConstraintDeclareArrayErased(
    const value::ObjectRef& problem, const ErasedReference& array,
    ArrayResizer resize, bool enabled) -> std::int64_t {
  return ProblemOf(problem).DeclareArray(array, std::move(resize), enabled);
}

auto ConstraintArraySizeErased(
    const value::ObjectRef& problem, const ErasedReference& array,
    std::size_t count) -> std::int64_t {
  return ProblemOf(problem).ArraySize(array, count);
}

auto RandomizeSettleFirst(const value::ObjectRef& problem) -> bool {
  return ProblemOf(problem).SettleFirst();
}

void ConstraintWant(const value::ObjectRef& p, std::int64_t variable) {
  ProblemOf(p).Want(variable);
}

auto RandEnabledErased(
    const ErasedReference& variable, const RandomVariablePath& path) -> bool {
  return ModesOf(variable).RandEnabled(path);
}

auto ConstraintVariableErased(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const void* value, const value::ValueType& type) -> std::int64_t {
  return ProblemOf(problem).Named(variable, value, type);
}

auto ConstraintConstantErased(
    const value::ObjectRef& problem, const void* value,
    const value::ValueType& type) -> std::int64_t {
  return ProblemOf(problem).Constant(value, type);
}

// Each object the handles in the storage `handle` names reach, holding
// `value` now: the handle itself, or every element of an array of them, at any
// depth (LRM 18.4).
void NestedParts(
    ConstraintProblem& problem, RandomizationModes& modes,
    const ErasedReference& handle, const void* value,
    const value::ValueType& type, const RandomVariablePath& path,
    bool enabled) {
  if (!enabled || !modes.RandEnabled(path)) {
    return;
  }
  if (!type.HasParts()) {
    const auto& reached = *static_cast<const value::ObjectRef*>(value);
    if (ObjectRootOf(reached) != nullptr) {
      problem.AddNested(reached);
    }
    return;
  }
  const value::ValueType& part = type.PartType(value);
  const std::size_t count = type.PartCount(value);
  for (std::size_t position = 0; position < count; ++position) {
    NestedParts(
        problem, modes,
        ErasedReference{
            .holder = handle.holder,
            .storage = type.PartAt(handle.storage, position)},
        type.PartAt(value, position), part, path.Into(position), true);
  }
}

void RandomizeNestedErased(
    const value::ObjectRef& problem, const ErasedReference& handle,
    const void* value, const value::ValueType& type,
    const RandomVariablePath& path, bool enabled) {
  NestedParts(
      ProblemOf(problem), ModesOf(handle), handle, value, type, path, enabled);
}

auto ConstraintBlockEnabled(GcObject* self, const value::String& name) -> bool {
  return AsClassObject(self).Modes().ConstraintEnabled(
      std::string{name.View()});
}

auto StaticConstraintBlockEnabled(const value::String& key) -> bool {
  return StaticRandomizationModes().ConstraintEnabled(std::string{key.View()});
}

auto ConstraintNegate(const value::ObjectRef& p, std::int64_t a)
    -> std::int64_t {
  return ProblemOf(p).Negate(a);
}
auto ConstraintBitwiseNot(const value::ObjectRef& p, std::int64_t a)
    -> std::int64_t {
  return ProblemOf(p).BitwiseNot(a);
}
auto ConstraintReduceAnd(const value::ObjectRef& p, std::int64_t a)
    -> std::int64_t {
  return ProblemOf(p).ReduceAnd(a);
}
auto ConstraintReduceOr(const value::ObjectRef& p, std::int64_t a)
    -> std::int64_t {
  return ProblemOf(p).ReduceOr(a);
}
auto ConstraintReduceXor(const value::ObjectRef& p, std::int64_t a)
    -> std::int64_t {
  return ProblemOf(p).ReduceXor(a);
}
auto ConstraintLogicalNot(const value::ObjectRef& p, std::int64_t a)
    -> std::int64_t {
  return ProblemOf(p).LogicalNot(a);
}
auto ConstraintAdd(const value::ObjectRef& p, std::int64_t a, std::int64_t b)
    -> std::int64_t {
  return ProblemOf(p).Add(a, b);
}
auto ConstraintSubtract(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t {
  return ProblemOf(p).Subtract(a, b);
}
auto ConstraintMultiply(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t {
  return ProblemOf(p).Multiply(a, b);
}
auto ConstraintDivide(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b, bool is_signed)
    -> std::int64_t {
  return ProblemOf(p).Divide(a, b, is_signed);
}
auto ConstraintRemainder(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b, bool is_signed)
    -> std::int64_t {
  return ProblemOf(p).Remainder(a, b, is_signed);
}
auto ConstraintBitwiseAnd(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t {
  return ProblemOf(p).BitwiseAnd(a, b);
}
auto ConstraintBitwiseOr(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t {
  return ProblemOf(p).BitwiseOr(a, b);
}
auto ConstraintBitwiseXor(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t {
  return ProblemOf(p).BitwiseXor(a, b);
}
auto ConstraintShiftLeft(
    const value::ObjectRef& p, std::int64_t a, std::int64_t amount)
    -> std::int64_t {
  return ProblemOf(p).ShiftLeft(a, amount);
}
auto ConstraintShiftRight(
    const value::ObjectRef& p, std::int64_t a, std::int64_t amount,
    bool arithmetic) -> std::int64_t {
  return ProblemOf(p).ShiftRight(a, amount, arithmetic);
}
auto ConstraintEqual(const value::ObjectRef& p, std::int64_t a, std::int64_t b)
    -> std::int64_t {
  return ProblemOf(p).Equal(a, b);
}
auto ConstraintLess(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b, bool is_signed)
    -> std::int64_t {
  return ProblemOf(p).Less(a, b, is_signed);
}
auto ConstraintLogicalAnd(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t {
  return ProblemOf(p).LogicalAnd(a, b);
}
auto ConstraintLogicalOr(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t {
  return ProblemOf(p).LogicalOr(a, b);
}
auto ConstraintConcatenate(
    const value::ObjectRef& p, std::int64_t high, std::int64_t low)
    -> std::int64_t {
  return ProblemOf(p).Concatenate(high, low);
}
auto ConstraintSelect(
    const value::ObjectRef& p, std::int64_t condition, std::int64_t then_term,
    std::int64_t else_term) -> std::int64_t {
  return ProblemOf(p).Select(condition, then_term, else_term);
}
auto ConstraintResize(
    const value::ObjectRef& p, std::int64_t a, std::int64_t width,
    bool sign_extend) -> std::int64_t {
  return ProblemOf(p).Resize(a, width, sign_extend);
}
auto ConstraintSlice(
    const value::ObjectRef& p, std::int64_t a, std::int64_t offset,
    std::int64_t width) -> std::int64_t {
  return ProblemOf(p).Slice(a, offset, width);
}

void ConstraintRequire(const value::ObjectRef& p, std::int64_t condition) {
  ProblemOf(p).Require(condition);
}
void ConstraintPrefer(const value::ObjectRef& p, std::int64_t condition) {
  ProblemOf(p).Prefer(condition);
}
void ConstraintDisableSoft(const value::ObjectRef& p, std::int64_t variable) {
  ProblemOf(p).DisableSoft(variable);
}
void ConstraintSolveBefore(
    const value::ObjectRef& p, std::int64_t before, std::int64_t after) {
  ProblemOf(p).SolveBefore(before, after);
}
auto ConstraintDistribution(
    const value::ObjectRef& p, std::int64_t subject, std::int64_t guard,
    bool is_signed, bool soft) -> std::int64_t {
  return ProblemOf(p).Distribution(subject, guard, is_signed, soft);
}
void ConstraintDistributionItem(
    const value::ObjectRef& p, std::int64_t distribution, std::int64_t low,
    std::int64_t high, std::int64_t weight, bool per_value) {
  ProblemOf(p).DistributionItem(distribution, low, high, weight, per_value);
}
void ConstraintDistributionDefault(
    const value::ObjectRef& p, std::int64_t distribution, std::int64_t weight) {
  ProblemOf(p).DistributionDefault(distribution, weight);
}
auto ConstraintUniqueSet(const value::ObjectRef& p) -> std::int64_t {
  return ProblemOf(p).UniqueSet();
}
void ConstraintUniqueMember(
    const value::ObjectRef& p, std::int64_t set, std::int64_t term) {
  ProblemOf(p).UniqueMember(set, term);
}
void ConstraintUniqueRequire(
    const value::ObjectRef& p, std::int64_t set, std::int64_t guard) {
  ProblemOf(p).UniqueRequire(set, guard);
}

void ConstraintUniqueArrayErased(
    const value::ObjectRef& p, std::int64_t set, const ErasedReference& array,
    const void* value, const value::ValueType& type) {
  ConstraintProblem& problem = ProblemOf(p);
  if (type.AsIntegral() != nullptr) {
    problem.UniqueMember(set, problem.Named(array, value, type));
    return;
  }
  if (!type.HasParts()) {
    throw SimulationError(
        "a uniqueness constraint over values of this type is not yet "
        "supported");
  }
  const value::ValueType& part = type.PartType(value);
  const std::size_t count = type.PartCount(value);
  for (std::size_t position = 0; position < count; ++position) {
    ConstraintUniqueArrayErased(
        p, set,
        ErasedReference{
            .holder = array.holder,
            .storage = type.PartAt(array.storage, position)},
        type.PartAt(value, position), part);
  }
}

namespace {

// The positions of the part `part` names within storage `declared`, holding
// `value` now, of type `type`.
auto ErasedPositionsOf(
    const void* value, void* declared, const value::ValueType& type,
    const void* part, std::vector<std::size_t>& out) -> bool {
  if (declared == part) {
    return true;
  }
  if (!type.HasParts()) {
    return false;
  }
  const value::ValueType& element = type.PartType(value);
  const std::size_t count = type.PartCount(value);
  for (std::size_t position = 0; position < count; ++position) {
    out.push_back(position);
    if (ErasedPositionsOf(
            type.PartAt(value, position), type.PartAt(declared, position),
            element, part, out)) {
      return true;
    }
    out.pop_back();
  }
  return false;
}

auto ErasedPathOf(
    const ErasedReference& declared, const void* value,
    const value::ValueType& type, const ErasedReference& part)
    -> RandomVariablePath {
  RandomVariablePath path{.declared = declared.storage, .positions = {}};
  if (!ErasedPositionsOf(
          value, declared.storage, type, part.storage, path.positions)) {
    throw InternalError(
        "rand_mode: the part named lies outside the variable declared");
  }
  return path;
}

}  // namespace

void RandModeSetAt(
    const ErasedReference& declared, const RandomVariablePath& path, bool on) {
  ModesOf(declared).SetRand(path, on);
}

auto RandModeGetAt(
    const ErasedReference& declared, const RandomVariablePath& path)
    -> value::Int {
  return value::Int::FromInt(ModesOf(declared).RandEnabled(path) ? 1 : 0);
}

void RandModeSetErased(
    const ErasedReference& declared, const void* value,
    const value::ValueType& type, const ErasedReference& part, bool on) {
  RandModeSetAt(declared, ErasedPathOf(declared, value, type, part), on);
}

auto RandModeGetErased(
    const ErasedReference& declared, const void* value,
    const value::ValueType& type, const ErasedReference& part) -> value::Int {
  return RandModeGetAt(declared, ErasedPathOf(declared, value, type, part));
}

void ObjectRandModeSet(GcObject* object, bool on) {
  AsClassObject(object).Modes().SetAllRand(on);
}

void ConstraintModeSet(GcObject* object, const value::String& name, bool on) {
  AsClassObject(object).Modes().SetConstraint(std::string{name.View()}, on);
}

auto ConstraintModeGet(GcObject* object, const value::String& name)
    -> value::Int {
  return value::Int::FromInt(
      AsClassObject(object).Modes().ConstraintEnabled(std::string{name.View()})
          ? 1
          : 0);
}

void ObjectConstraintModeSet(GcObject* object, bool on) {
  AsClassObject(object).Modes().SetAllConstraints(on);
}

void StaticConstraintModeSet(const value::String& key, bool on) {
  StaticRandomizationModes().SetConstraint(std::string{key.View()}, on);
}

auto StaticConstraintModeGet(const value::String& key) -> value::Int {
  return value::Int::FromInt(
      StaticRandomizationModes().ConstraintEnabled(std::string{key.View()})
          ? 1
          : 0);
}

}  // namespace lyra::runtime

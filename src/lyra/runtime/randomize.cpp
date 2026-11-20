#include "lyra/runtime/randomize.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/class_object.hpp"
#include "lyra/runtime/constraint_cycle.hpp"
#include "lyra/runtime/constraint_problem.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/randomization.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/random_qualifier.hpp"
#include "lyra/value/dynamic_array_type.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/queue_type.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::runtime {

namespace {

auto ProblemOf(const value::ObjectRef& handle) -> ConstraintProblem& {
  auto* problem = handle.Get<ConstraintProblem>();
  if (problem == nullptr ||
      problem->definition != &ConstraintProblem::kDefinition) {
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
            return object->Modes();
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

auto Begin(
    ClassObject* object, bool listed, RuntimeEffects& runtime,
    base::SourceLocation at) -> value::ObjectRef {
  auto made = std::make_unique<ConstraintProblem>(
      runtime, object->Generator(), SelfHandle(object), listed, at);
  ConstraintProblem* problem = made.get();
  value::ObjectRef handle = AdoptObject(made.release());
  // `pre_randomize()` runs on the object and then on every random object it
  // reaches (LRM 18.6.2), each asked after its own hook for the ones it
  // reaches, since the hook may make them. The list grows as it is walked, so
  // it is walked by position. A call listing what it chooses makes every
  // handle it does not list a state variable (LRM 18.11), so the object
  // reaches none.
  ObjectPreRandomize(object);
  if (!listed) {
    ObjectRandObjects(*object, handle, at);
  }
  std::size_t position = 0;
  while (position < problem->Nested().size()) {
    GcObject& nested = *ObjectRootOf(problem->Nested()[position]);
    ObjectPreRandomize(&nested);
    ObjectRandObjects(nested, handle, at);
    ++position;
  }
  return handle;
}

}  // namespace

auto RandomizeObject(
    ClassObject* object, RuntimeEffects& runtime, base::SourceLocation at)
    -> value::ObjectRef {
  return Begin(object, false, runtime, at);
}

auto RandomizeObjectListed(
    ClassObject* object, RuntimeEffects& runtime, base::SourceLocation at)
    -> value::ObjectRef {
  return Begin(object, true, runtime, at);
}

auto RandomizeScope(RuntimeEffects& runtime, base::SourceLocation at)
    -> value::ObjectRef {
  auto made = std::make_unique<ConstraintProblem>(
      runtime, *runtime.Running().generator, value::ObjectRef{}, true, at);
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
  ObjectRandVariables(*object, problem, p.WrittenAt());
  for (const value::ObjectRef& nested : p.Nested()) {
    ObjectRandVariables(*ObjectRootOf(nested), problem, p.WrittenAt());
  }
  ObjectConstrain(*object, problem);
  for (const value::ObjectRef& nested : p.Nested()) {
    ObjectConstrain(*ObjectRootOf(nested), problem);
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
    ObjectPostRandomize(object);
    for (const value::ObjectRef& nested : p.Nested()) {
      ObjectPostRandomize(ObjectRootOf(nested));
    }
  }
  return value::Int::FromInt(1);
}

auto ResizeOf(const ErasedReference& whole, const value::ValueType& type)
    -> ArrayResizer {
  const auto resizer = [&](auto resized_into) -> ArrayResizer {
    return [whole, &type,
            resized_into](std::size_t count) -> std::function<void()> {
      auto before = std::make_shared<value::AnyValue>(
          value::AnyValue::CopyOf(type, whole.storage));
      const value::AnyValue resized = value::AnyValue::Built(
          type, [&](void* out) { resized_into(whole.storage, count, out); });
      WriteLaidOutThrough(whole, type, resized.Bytes());
      return [whole, &type, before] {
        WriteLaidOutThrough(whole, type, before->Bytes());
      };
    };
  };
  switch (type.Kind()) {
    case value::ValueTypeKind::kDynamicArray:
      return resizer([dynamic = type.AsDynamicArray()](
                         const void* array, std::size_t count, void* out) {
        dynamic->Resized(array, count, out);
      });
    case value::ValueTypeKind::kQueue:
      return resizer([queue = type.AsQueue()](
                         const void* array, std::size_t count, void* out) {
        queue->Resized(array, count, out);
      });
    case value::ValueTypeKind::kAssociativeArray:
    case value::ValueTypeKind::kIntegral:
    case value::ValueTypeKind::kFixedArray:
    case value::ValueTypeKind::kOther:
      break;
  }
  throw InternalError(
      "randomize: an array whose size the program chooses is told a type "
      "that is no dynamic array or queue -- please report this as a bug");
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
  const std::uint64_t count = part.members->Count();
  return modes.CycleOf(
      part.path, std::bit_width(count == 0 ? 0 : count - 1), count);
}

}  // namespace

namespace {

// The walk the problem `problem` names begun on the whole of the variable
// `whole` names, for `purpose`: the whole is the walk's first part, chosen
// where its mode is on, of the members its enumeration declares where it is
// one.
void BeginWalk(
    const value::ObjectRef& problem, const ErasedReference& whole,
    WalkPurpose purpose) {
  ConstraintProblem& p = ProblemOf(problem);
  p.Walk() = RandomWalk{
      .purpose = purpose,
      .whole = whole,
      .parts = {WalkedPart{
          .part =
              DeclaredPart{
                  .path =
                      RandomVariablePath{
                          .declared = whole.storage, .positions = {}},
                  .enabled = true,
                  .members = p.MembersOf(whole),
                  .within = std::nullopt},
          .elements_chosen = false,
          .elements_within = std::nullopt}}};
}

// The part of the walk the type's code is at.
auto PartAt(RandomWalk& walk) -> WalkedPart& {
  if (walk.parts.empty()) {
    throw InternalError(
        "randomize: a part of a random variable is walked outside any walk -- "
        "please report this as a bug");
  }
  return walk.parts.back();
}

}  // namespace

void RandomWalkDeclare(
    const value::ObjectRef& problem, const ErasedReference& whole,
    support::RandomQualifier declared) {
  BeginWalk(problem, whole, DeclaringWalk{.declared = declared});
}
void RandomWalkList(
    const value::ObjectRef& problem, const ErasedReference& whole) {
  BeginWalk(problem, whole, ListingWalk{});
}
void RandomWalkNested(
    const value::ObjectRef& problem, const ErasedReference& whole) {
  BeginWalk(problem, whole, NestingWalk{});
}
void RandomWalkUnique(
    const value::ObjectRef& problem, std::int64_t set,
    const ErasedReference& whole) {
  BeginWalk(problem, whole, UniqueWalk{.set = set});
}

namespace {

// The array the walk is at (LRM 18.4): its elements are chosen where its mode
// and the array's own are both on, and lie within the arrays around it.
auto EnterArrayPart(RandomWalk& walk) -> WalkedPart& {
  WalkedPart& at = PartAt(walk);
  at.elements_chosen =
      at.part.enabled && ModesOf(walk.whole).RandEnabled(at.part.path);
  at.elements_within = at.part.within;
  return at;
}

}  // namespace

void RandomWalkFixedArray(const value::ObjectRef& problem) {
  EnterArrayPart(ProblemOf(problem).Walk());
}

// One whose size the program chooses is declared, or listed, before its
// elements, as the array they lie within, and made as long as the solve
// chooses by `resize`.
void RandomWalkResizableArray(
    const value::ObjectRef& problem, const ErasedReference& array,
    const ArrayResizer& resize) {
  ConstraintProblem& p = ProblemOf(problem);
  RandomWalk& walk = p.Walk();
  WalkedPart& at = EnterArrayPart(walk);
  std::visit(
      Overloaded{
          [&](const DeclaringWalk&) {
            at.elements_within =
                p.DeclareArray(array, resize, at.elements_chosen);
          },
          [&](const ListingWalk&) {
            at.elements_within = p.ListArray(array, resize);
          },
          [](const NestingWalk&) {}, [](const UniqueWalk&) {}},
      walk.purpose);
}

void RandomWalkEnter(const value::ObjectRef& problem, std::int64_t position) {
  RandomWalk& walk = ProblemOf(problem).Walk();
  const WalkedPart& at = PartAt(walk);
  walk.parts.push_back(
      WalkedPart{
          .part = at.part.Into(
              static_cast<std::size_t>(position), at.elements_chosen,
              at.elements_within),
          .elements_chosen = false,
          .elements_within = std::nullopt});
}

void RandomWalkLeave(const value::ObjectRef& problem) {
  RandomWalk& walk = ProblemOf(problem).Walk();
  PartAt(walk);
  walk.parts.pop_back();
}

// A value the walk reaches that is no array: declared random, listed, the
// object a handle names joined to the call, or entered into a uniqueness set,
// as the walk is for.
void RandomWalkLeaf(
    const value::ObjectRef& problem, const ErasedReference& lent,
    const value::ValueType* type) {
  ConstraintProblem& p = ProblemOf(problem);
  RandomWalk& walk = p.Walk();
  const DeclaredPart& part = PartAt(walk).part;
  void* const storage = lent.storage;
  RandomizationModes& modes = ModesOf(walk.whole);
  const value::IntegralValueType* integral = type->AsIntegral();
  std::visit(
      Overloaded{
          [&](const DeclaringWalk& declaring) {
            if (integral == nullptr) {
              const base::SourceLocationScope bound(p.WrittenAt());
              throw SimulationError(
                  "randomizing a random variable of this type is not yet "
                  "supported");
            }
            const bool chosen = part.enabled && modes.RandEnabled(part.path);
            ConstraintCycle* cycle =
                declaring.declared == support::RandomQualifier::kRandc
                    ? &CycleOf(modes, part, integral->Shape().width)
                    : nullptr;
            p.Declare(
                lent, storage, *type, declaring.declared, chosen, cycle,
                part.members, part.within);
          },
          [&](const ListingWalk&) {
            if (integral == nullptr) {
              const base::SourceLocationScope bound(p.WrittenAt());
              throw SimulationError(
                  "randomizing a listed variable of this type is not yet "
                  "supported");
            }
            p.List(lent, storage, *type, part.members, part.within);
          },
          [&](const NestingWalk&) {
            if (!part.enabled || !modes.RandEnabled(part.path)) {
              return;
            }
            const auto& reached =
                *static_cast<const value::ObjectRef*>(storage);
            if (ObjectRootOf(reached) != nullptr) {
              p.AddNested(reached);
            }
          },
          [&](const UniqueWalk& unique) {
            if (integral == nullptr) {
              const base::SourceLocationScope bound(p.WrittenAt());
              throw SimulationError(
                  "a uniqueness constraint over values of this type is not "
                  "yet supported");
            }
            p.UniqueMember(unique.set, p.Named(lent, storage, *type));
          }},
      walk.purpose);
}

void RandomWalkEnd(const value::ObjectRef& problem) {
  ProblemOf(problem).Walk().parts.clear();
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

auto ConstraintArraySize(
    const value::ObjectRef& problem, const ErasedReference& array,
    std::int64_t count) -> std::int64_t {
  return ProblemOf(problem).ArraySize(array, static_cast<std::size_t>(count));
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

auto ConstraintBlockEnabled(GcObject* self, const value::String& name) -> bool {
  return self->Modes().ConstraintEnabled(std::string{name.View()});
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

namespace {

// The random variable the storage `declared` names is the root of, at
// `positions` within it.
auto PathAt(
    const ErasedReference& declared, std::span<const std::int64_t> positions)
    -> RandomVariablePath {
  RandomVariablePath path{.declared = declared.storage, .positions = {}};
  path.positions.reserve(positions.size());
  for (const std::int64_t position : positions) {
    path.positions.push_back(static_cast<std::size_t>(position));
  }
  return path;
}

}  // namespace

void RandModeSet(
    const ErasedReference& declared, std::span<const std::int64_t> positions,
    bool on) {
  ModesOf(declared).SetRand(PathAt(declared, positions), on);
}

auto RandModeGet(
    const ErasedReference& declared, std::span<const std::int64_t> positions)
    -> value::Int {
  return value::Int::FromInt(
      ModesOf(declared).RandEnabled(PathAt(declared, positions)) ? 1 : 0);
}

void ObjectRandModeSet(GcObject* object, bool on) {
  object->Modes().SetAllRand(on);
}

void ConstraintModeSet(GcObject* object, const value::String& name, bool on) {
  object->Modes().SetConstraint(std::string{name.View()}, on);
}

auto ConstraintModeGet(GcObject* object, const value::String& name)
    -> value::Int {
  return value::Int::FromInt(
      object->Modes().ConstraintEnabled(std::string{name.View()}) ? 1 : 0);
}

void ObjectConstraintModeSet(GcObject* object, bool on) {
  object->Modes().SetAllConstraints(on);
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

#include "lyra/lir/value_step.hpp"

#include <algorithm>
#include <format>
#include <optional>
#include <span>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/support/layout.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/value/scalar.hpp"

namespace lyra::lir {

namespace {

// Whether ending an object of the library's does anything, as the storage it
// is stated to take says, which states it once for every class built there.
auto LibraryNeedsEnd(const support::RuntimeObject& object) -> bool {
  const std::optional<support::ObjectLayout> stated =
      support::StatedStorageOf(object);
  if (!stated.has_value()) {
    throw InternalError(
        "lir: a value held as its bits is asked as an object something holds "
        "-- please report this as a bug");
  }
  return stated->needs_end;
}

// How `step` of a value the library holds as `object` is carried out: by the
// library's own entry, but an end that does nothing, which is trivial.
auto StepOfLibraryObject(
    const support::RuntimeObject& object, support::ValueStep step) -> StepKind {
  if (step == support::ValueStep::kEnd && !LibraryNeedsEnd(object)) {
    return TrivialStep{};
  }
  return LibraryStep{.object = object};
}

// The parts a value laid out by its type is made of: a product's components,
// a tagged union's members, an array's element.
auto PartsOf(const CompilationUnit& unit, TypeId type)
    -> std::span<const TypeId> {
  const Type& stated = unit.types.Get(type);
  if (const auto* array = stated.As<UnpackedArrayType>()) {
    return {&array->element_type, 1};
  }
  const std::optional<std::span<const TypeId>> components =
      ComponentTypes(unit, type);
  if (!components.has_value()) {
    throw InternalError(
        "lir: the parts of a value asked of a type not laid out by its type "
        "-- please report this as a bug");
  }
  return *components;
}

// A value laid out by its type that has nothing to end is trivial in every
// step. One that has is ended by its glue, part by part, and relocates
// trivially where every part does, since a relocation ends nothing of what it
// took, and by its glue otherwise. It is copied and written over by its glue,
// except where a part has no copy -- an owning pointer -- when both are
// deleted for the whole.
auto LaidOutStep(
    const CompilationUnit& unit, TypeId type, support::ValueStep step)
    -> StepKind {
  const auto every_part = [&](support::ValueStep asked, auto&& holds) {
    return std::ranges::all_of(PartsOf(unit, type), [&](TypeId part) {
      return holds(StepOf(unit, part, asked));
    });
  };
  // A container's core is its pointers and counts, which relocate as their
  // bytes; every other step of one does something, and each is the body its
  // type has for it.
  if (unit.types.Get(type).IsLaidOutContainer()) {
    switch (step) {
      case support::ValueStep::kRelocate:
        return TrivialStep{};
      case support::ValueStep::kCopy:
      case support::ValueStep::kAssign:
      case support::ValueStep::kEnd:
        return GlueStep{};
    }
    throw InternalError("lir: unknown value step");
  }
  if (!NeedsEnd(unit, type)) {
    return TrivialStep{};
  }
  switch (step) {
    case support::ValueStep::kEnd:
      return GlueStep{};
    case support::ValueStep::kRelocate:
      if (every_part(step, [](const StepKind& realized) {
            return std::holds_alternative<TrivialStep>(realized);
          })) {
        return TrivialStep{};
      }
      return GlueStep{};
    case support::ValueStep::kCopy:
    case support::ValueStep::kAssign:
      if (every_part(support::ValueStep::kCopy, [](const StepKind& realized) {
            return !std::holds_alternative<DeletedStep>(realized);
          })) {
        return GlueStep{};
      }
      return DeletedStep{};
  }
  throw InternalError("lir: unknown value step");
}

}  // namespace

auto IsScalar(const Type& type) -> bool {
  const std::optional<Holding> held = type.HeldAs();
  return held.has_value() && std::holds_alternative<value::Scalar>(*held);
}

auto IsLaidOutByItsType(const Type& type) -> bool {
  const std::optional<Holding> held = type.HeldAs();
  return held.has_value() && std::holds_alternative<LaidOutByType>(*held);
}

auto NeedsEnd(const CompilationUnit& unit, TypeId type) -> bool {
  const std::optional<Holding> held = unit.types.Get(type).HeldAs();
  if (!held.has_value()) {
    return false;
  }
  return std::visit(
      Overloaded{
          [](const value::Scalar&) { return false; },
          // A value laid out by its type ends as its parts do, where any has
          // anything to end.
          [&](const LaidOutByType&) {
            if (unit.types.Get(type).IsLaidOutContainer()) {
              return true;
            }
            return std::ranges::any_of(PartsOf(unit, type), [&](TypeId part) {
              return NeedsEnd(unit, part);
            });
          },
          [](const OwnedStorageAddress&) { return true; },
          [](const ClosureStorage&) { return true; },
          [](const support::RuntimeObject& object) {
            return LibraryNeedsEnd(object);
          }},
      *held);
}

auto StepFunctionOf(
    const CompilationUnit& unit, TypeId type, support::ValueStep step)
    -> FunctionId {
  const auto found = unit.type_steps.find(std::pair{type, step});
  if (found == unit.type_steps.end()) {
    throw InternalError(
        std::format(
            "lir: the unit states no function to {} a value of a type that "
            "has one -- please report this as a bug",
            support::ValueStepName(step)));
  }
  return found->second;
}

auto StepOf(const CompilationUnit& unit, TypeId type, support::ValueStep step)
    -> StepKind {
  const Type& stated = unit.types.Get(type);
  const std::optional<Holding> held = stated.HeldAs();
  // A value held as itself -- a machine scalar, a borrowed address -- is its
  // bytes.
  if (!held.has_value()) {
    return TrivialStep{};
  }
  return std::visit(
      Overloaded{
          [](const value::Scalar&) -> StepKind { return TrivialStep{}; },
          [&](const LaidOutByType&) -> StepKind {
            return LaidOutStep(unit, type, step);
          },
          // An owning pointer is the address it holds, which relocates as its
          // bytes and is never copied; ending it ends what it points at and
          // gives the storage back. What it points at is ended through the
          // definition that value opens with where only that definition knows
          // it -- an object of whatever class it was created as, a closure of
          // no known type -- and otherwise as its type says, where the pointer
          // ends.
          [&](const OwnedStorageAddress&) -> StepKind {
            switch (step) {
              case support::ValueStep::kRelocate:
                return TrivialStep{};
              case support::ValueStep::kCopy:
              case support::ValueStep::kAssign:
                return DeletedStep{};
              case support::ValueStep::kEnd:
                break;
            }
            const Type& pointee = unit.types.Get(*stated.Pointee());
            if (pointee.Is<ObjectType>() ||
                pointee.RuntimeClassOf().has_value() ||
                pointee.Is<ErasedCallableType>()) {
              return VirtualStep{};
            }
            return BoxStep{};
          },
          // A closure is built where it lies and ended there, by the body
          // its declaration's captures make.
          [&](const ClosureStorage&) -> StepKind {
            if (step != support::ValueStep::kEnd) {
              return DeletedStep{};
            }
            return GlueStep{};
          },
          [&](const support::RuntimeObject& object) -> StepKind {
            return StepOfLibraryObject(object, step);
          }},
      *held);
}

}  // namespace lyra::lir

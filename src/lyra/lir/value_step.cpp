#include "lyra/lir/value_step.hpp"

#include <algorithm>
#include <format>
#include <optional>
#include <span>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::lir {

namespace {

using support::LibraryObject;
using support::ValueDomain;

// Whether ending a value of the library's own does anything, as the storage
// it is stated to take says.
auto LibraryNeedsEnd(const support::RuntimeObject& object) -> bool {
  const std::optional<support::ObjectLayout> stated =
      support::StatedStorageOf(object);
  if (!stated.has_value()) {
    throw InternalError(
        "lir: a value held as its bits or its bytes, or an execution, is "
        "asked as an object something holds -- please report this as a bug");
  }
  return !stated->ends_with_nothing_to_do;
}

// The parts a value laid out by its type is made of: a product's components,
// a union's members, an array's element.
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

// A value laid out by its type that has nothing to end is its bytes. One that
// has is stepped by its function, part by part, except where a part has no
// copy -- an owning pointer. Then the whole has no copy, nor a write over it,
// which copies; and no move either, since a move leaves the value it took for
// its holder to end, and ending that would give back what the pointer moved
// into the new value now owns. Only its end remains.
auto LaidOutStep(const CompilationUnit& unit, TypeId type, ValueStep step)
    -> StepRealization {
  if (!NeedsEnd(unit, type)) {
    return StepIsBytes{};
  }
  switch (step) {
    case ValueStep::kEnd:
      return StepByFunction{};
    case ValueStep::kCopy:
    case ValueStep::kMove:
    case ValueStep::kAssign:
      break;
  }
  const bool every_part_copies =
      std::ranges::none_of(PartsOf(unit, type), [&](TypeId part) {
        return std::holds_alternative<NoSuchStep>(
            StepOf(unit, part, ValueStep::kCopy));
      });
  if (!every_part_copies) {
    return NoSuchStep{};
  }
  return StepByFunction{};
}

}  // namespace

auto IsLaidOutByItsType(const Type& type) -> bool {
  const std::optional<Holding> held = type.HeldAs();
  if (!held.has_value()) {
    return false;
  }
  const auto* domain = std::get_if<ValueDomain>(&*held);
  return domain != nullptr && support::IsLaidOutByItsType(*domain);
}

auto ValueStepName(ValueStep step) -> std::string_view {
  switch (step) {
    case ValueStep::kCopy:
      return "copy";
    case ValueStep::kMove:
      return "move";
    case ValueStep::kAssign:
      return "assign";
    case ValueStep::kEnd:
      return "end";
  }
  throw InternalError("lir: unknown value step");
}

auto NeedsEnd(const CompilationUnit& unit, TypeId type) -> bool {
  const std::optional<Holding> held = unit.types.Get(type).HeldAs();
  if (!held.has_value()) {
    return false;
  }
  return std::visit(
      Overloaded{
          [](const IntegralBits&) { return false; },
          [](const OwnedStorageAddress&) { return true; },
          [](const ClosureStorage&) { return true; },
          [&](ValueDomain domain) {
            if (!support::IsLaidOutByItsType(domain)) {
              return LibraryNeedsEnd(domain);
            }
            // An untagged union's members are only bits another member may
            // have written, so none of them is held to end.
            if (const std::optional<RecordKind> kind =
                    RecordKindOf(unit, type)) {
              switch (*kind) {
                case RecordKind::kUnion:
                  return false;
                case RecordKind::kStruct:
                case RecordKind::kTaggedUnion:
                  break;
              }
            }
            return std::ranges::any_of(PartsOf(unit, type), [&](TypeId part) {
              return NeedsEnd(unit, part);
            });
          },
          [](LibraryObject library) { return LibraryNeedsEnd(library); }},
      *held);
}

auto StepFunctionOf(const CompilationUnit& unit, TypeId type, ValueStep step)
    -> FunctionId {
  const auto found = unit.type_steps.find(std::pair{type, step});
  if (found == unit.type_steps.end()) {
    throw InternalError(
        std::format(
            "lir: the unit states no function to {} a value of a type that "
            "has one -- please report this as a bug",
            ValueStepName(step)));
  }
  return found->second;
}

auto StepOf(const CompilationUnit& unit, TypeId type, ValueStep step)
    -> StepRealization {
  const Type& stated = unit.types.Get(type);
  const std::optional<Holding> held = stated.HeldAs();
  // A value held as itself -- a machine scalar, a borrowed address -- is its
  // bytes.
  if (!held.has_value()) {
    return StepIsBytes{};
  }
  return std::visit(
      Overloaded{
          [](const IntegralBits&) -> StepRealization { return StepIsBytes{}; },
          // An owning pointer is the address it holds, which moves as its
          // bytes and is never copied; ending it ends what it points at and
          // gives the storage back. What it points at is ended through the
          // definition that value opens with where only that definition knows
          // it -- an object of whatever class it was created as, a closure of
          // no known type -- and otherwise by its type.
          [&](const OwnedStorageAddress&) -> StepRealization {
            switch (step) {
              case ValueStep::kMove:
                return StepIsBytes{};
              case ValueStep::kCopy:
              case ValueStep::kAssign:
                return NoSuchStep{};
              case ValueStep::kEnd:
                break;
            }
            const Type& pointee = unit.types.Get(*stated.Pointee());
            if (pointee.Is<ObjectType>() || pointee.Is<RuntimeClassType>() ||
                pointee.Is<ErasedCallableType>()) {
              return StepThroughDefinition{};
            }
            return StepByFunction{};
          },
          // A closure is built where it lies and ended there, by the body
          // its declaration's captures make.
          [&](const ClosureStorage&) -> StepRealization {
            if (step != ValueStep::kEnd) {
              return NoSuchStep{};
            }
            return StepByFunction{};
          },
          [&](ValueDomain domain) -> StepRealization {
            if (support::IsLaidOutByItsType(domain)) {
              return LaidOutStep(unit, type, step);
            }
            if (step == ValueStep::kEnd && !LibraryNeedsEnd(domain)) {
              return StepIsBytes{};
            }
            return StepByLibrary{.object = domain};
          },
          [&](LibraryObject library) -> StepRealization {
            if (step == ValueStep::kEnd && !LibraryNeedsEnd(library)) {
              return StepIsBytes{};
            }
            return StepByLibrary{.object = library};
          }},
      *held);
}

}  // namespace lyra::lir

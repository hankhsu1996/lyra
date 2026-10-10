#include "lyra/lowering/hir_to_mir/access_path.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/object_change.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// What a path is taken for at one of its steps: the part's value, the part as
// a place a write lands in, the part designated within a write in progress, or
// a reference to the part.
enum class StepUse : std::uint8_t { kRead, kWrite, kDesignate, kLend };

// Only an element and a component may be passed by reference (LRM 13.5.2), so
// a step to anything else taken on a reference is a producer that lent what the
// language does not.
[[noreturn]] void RefuseReference() {
  throw InternalError(
      "access path: only an element and a component may be passed by "
      "reference (LRM 13.5.2), and this lent target steps into neither");
}

// The library entry that takes a step to each kind of part for each thing a
// path is taken for.
auto EntryToBits(StepUse use) -> support::BuiltinFn {
  switch (use) {
    case StepUse::kRead:
      return support::BuiltinFn::kSlice;
    case StepUse::kWrite:
      return support::BuiltinFn::kSliceRef;
    case StepUse::kDesignate:
      return support::BuiltinFn::kDesignateSlice;
    case StepUse::kLend:
      RefuseReference();
  }
  throw InternalError("access path: unknown step use");
}

auto EntryToElementRun(StepUse use) -> support::BuiltinFn {
  switch (use) {
    case StepUse::kRead:
      return support::BuiltinFn::kElementSlice;
    case StepUse::kWrite:
      return support::BuiltinFn::kElementSliceRef;
    case StepUse::kDesignate:
      return support::BuiltinFn::kDesignateElementSlice;
    case StepUse::kLend:
      RefuseReference();
  }
  throw InternalError("access path: unknown step use");
}

auto EntryToElement(StepUse use) -> support::BuiltinFn {
  switch (use) {
    case StepUse::kRead:
      return support::BuiltinFn::kElement;
    case StepUse::kWrite:
      return support::BuiltinFn::kElementRef;
    case StepUse::kDesignate:
      return support::BuiltinFn::kDesignateElement;
    case StepUse::kLend:
      return support::BuiltinFn::kReferElement;
  }
  throw InternalError("access path: unknown step use");
}

auto EntryToAssocElement(StepUse use) -> support::BuiltinFn {
  switch (use) {
    case StepUse::kRead:
      return support::BuiltinFn::kAssocElement;
    case StepUse::kWrite:
      return support::BuiltinFn::kAssocElementRef;
    case StepUse::kDesignate:
      return support::BuiltinFn::kAssocDesignateElement;
    case StepUse::kLend:
      return support::BuiltinFn::kAssocReferElement;
  }
  throw InternalError("access path: unknown step use");
}

// A queue's run of elements is a queue of its own (LRM 7.10.1), so a producer
// that put one on the way to a write or a reference named a destination the
// language does not have.
auto EntryToQueueRun(StepUse use) -> support::BuiltinFn {
  switch (use) {
    case StepUse::kRead:
      return support::BuiltinFn::kQueueSlice;
    case StepUse::kWrite:
    case StepUse::kDesignate:
    case StepUse::kLend:
      throw InternalError(
          "access path: a write or a reference reaches a part of what it is "
          "taken into, and this descent takes a step that builds a value "
          "instead");
  }
  throw InternalError("access path: unknown step use");
}

auto EntryToComponent(StepUse use) -> support::BuiltinFn {
  switch (use) {
    case StepUse::kRead:
      return support::BuiltinFn::kComponent;
    case StepUse::kWrite:
      return support::BuiltinFn::kComponentRef;
    case StepUse::kDesignate:
      return support::BuiltinFn::kDesignateComponent;
    case StepUse::kLend:
      return support::BuiltinFn::kReferComponent;
  }
  throw InternalError("access path: unknown step use");
}

// The call that takes one step on a receiver: the entry, what its callee
// states beside the receiver, and what it is handed beside it.
struct StepCall {
  support::BuiltinFn entry;
  std::optional<mir::CallPart> part = std::nullopt;
  std::optional<mir::TypeId> type_argument = std::nullopt;
  std::vector<mir::ExprId> arguments;
};

// A step to bits is called at the type the bits are read or written at, which
// no operand states; a step to a component names it on the callee, the
// component having a type of its own; a run of elements is handed how many it
// takes as a machine count.
auto StepCallFor(
    const mir::CompilationUnit& unit, mir::Block& block,
    const DescentStep& step, StepUse use) -> StepCall {
  return std::visit(
      Overloaded{
          [&](const StepToBits& bits) {
            return StepCall{
                .entry = EntryToBits(use),
                .type_argument = step.part_type,
                .arguments = {bits.start}};
          },
          [&](const StepToElementRun& run) {
            return StepCall{
                .entry = EntryToElementRun(use),
                .arguments = {
                    run.start,
                    BuildMachineIntLiteral(
                        unit, block, static_cast<std::int64_t>(run.count))}};
          },
          [&](const StepToElement& element) {
            return StepCall{
                .entry = EntryToElement(use), .arguments = {element.position}};
          },
          [&](const StepToAssocElement& element) {
            return StepCall{
                .entry = EntryToAssocElement(use), .arguments = {element.key}};
          },
          [&](const StepToQueueRun& run) {
            return StepCall{
                .entry = EntryToQueueRun(use),
                .arguments = {run.lowest, run.highest}};
          },
          [&](const StepToComponent& component) {
            return StepCall{
                .entry = EntryToComponent(use),
                .part = mir::CallPart{component.position},
                .arguments = {}};
          }},
      step.to);
}

// Whether a write lands where a step designated within it leads: bits, or
// several elements in a row, are parts rather than one place, so nothing steps
// further within the write from them.
auto EndsDesignation(const DescentStep& step) -> bool {
  return std::visit(
      Overloaded{
          [](const StepToBits&) { return true; },
          [](const StepToElementRun&) { return true; },
          [](const StepToQueueRun&) { return true; },
          [](const StepToElement&) { return false; },
          [](const StepToAssocElement&) { return false; },
          [](const StepToComponent&) { return false; }},
      step.to);
}

// One step taken on `receiver` for `use`, answering `type`.
auto TakeStep(
    const mir::CompilationUnit& unit, mir::Block& block,
    const DescentStep& step, StepUse use, mir::ExprId receiver,
    mir::TypeId type) -> mir::ExprId {
  StepCall call = StepCallFor(unit, block, step, use);
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = call.entry,
                          .receiver = receiver,
                          .part = call.part,
                          .type_argument = call.type_argument},
                  .arguments = std::move(call.arguments)},
          .type = type});
}

// A net's resolved cell is readable and observable, but no value gets into it
// this way at all: a net takes one only through a driver (LRM 6.5), and a net
// is not a variable, so it is neither a store destination nor a legal `ref`
// actual (LRM 13.5.2). A producer hands this the driver in the cell's place;
// arriving with the cell means it did not.
void RefuseNetCell(const mir::Type& place_ty) {
  if (place_ty.Is<mir::ResolvedType>()) {
    throw InternalError(
        "access path: a net's cell takes no value a write may put there; the "
        "destination is one of its drivers");
  }
}

// The owner, where it is a capability wrapper whose whole contents a store may
// replace by acting on the wrapper.
auto StoredWrapper(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const PathOwner& owner) -> std::optional<mir::ExprId> {
  return std::visit(
      Overloaded{
          [&](mir::ExprId place) -> std::optional<mir::ExprId> {
            const mir::Type& type = unit.types.Get(block.exprs.Get(place).type);
            if (!type.IsCapabilityWrapper() || type.Is<mir::ResolvedType>()) {
              return std::nullopt;
            }
            return place;
          },
          [](const ObjectProperty&) -> std::optional<mir::ExprId> {
            return std::nullopt;
          }},
      owner);
}

// The node `id` of `from`, named again in `to`. Only a node that evaluates
// nothing can be: a name, a constant, or a place formed over those means the
// same wherever it is written, while a computation written again would run
// again.
auto NamedAgain(const mir::Block& from, mir::Block& to, mir::ExprId id)
    -> mir::ExprId {
  const mir::Expr& node = from.exprs.Get(id);
  const auto again = [&](mir::ExprId operand) {
    return NamedAgain(from, to, operand);
  };
  const auto computes = []() -> mir::ExprData {
    throw InternalError(
        "access path: a settled path evaluates nothing, and this one holds a "
        "node that computes");
  };
  mir::ExprData data = std::visit(
      Overloaded{
          [](const mir::StringLiteral& e) -> mir::ExprData { return e; },
          [](const mir::NullLiteral& e) -> mir::ExprData { return e; },
          [](const mir::MachineBoolLiteral& e) -> mir::ExprData { return e; },
          [](const mir::MachineIntLiteral& e) -> mir::ExprData { return e; },
          [](const mir::MachineFloatLiteral& e) -> mir::ExprData { return e; },
          [](const mir::ReferenceExpr& e) -> mir::ExprData { return e; },
          [&](const mir::DerefExpr& e) -> mir::ExprData {
            return mir::DerefExpr{.pointer = again(e.pointer)};
          },
          [&](const mir::AddressOfExpr& e) -> mir::ExprData {
            return mir::AddressOfExpr{.operand = again(e.operand)};
          },
          [&](const mir::FieldAccessExpr& e) -> mir::ExprData {
            return mir::FieldAccessExpr{
                .receiver = again(e.receiver), .field = e.field};
          },
          [&](const mir::UnaryExpr&) { return computes(); },
          [&](const mir::BinaryExpr&) { return computes(); },
          [&](const mir::CastExpr&) { return computes(); },
          [&](const mir::DynamicCastExpr&) { return computes(); },
          [&](const mir::ConditionalExpr&) { return computes(); },
          [&](const mir::BlockExpr&) { return computes(); },
          [&](const mir::AssignExpr&) { return computes(); },
          [&](const mir::CallExpr&) { return computes(); },
          [&](const mir::MoveExpr&) { return computes(); },
          [&](const mir::ClosureExpr&) { return computes(); },
          [&](const mir::CompositeExpr&) { return computes(); },
          [&](const mir::AwaitExpr&) { return computes(); },
          [&](const mir::WaitExpr&) { return computes(); },
          [&](const mir::VectorGetExpr&) { return computes(); }},
      node.data);
  return to.exprs.Add(mir::Expr{.data = std::move(data), .type = node.type});
}

// The type of the value the last step of `path`, which takes at least one,
// selects within. Every step into a packed value names bits of that one vector
// (LRM 7.2.1), so where the steps at the end of the descent each enter a packed
// value, what the last of them selects within is the outermost of those.
auto ValueSelectedWithin(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const AccessPath& path) -> mir::TypeId {
  const auto stepped_into = [&](std::size_t step) {
    return step == 0 ? OwnerValueType(unit, block, path.owner)
                     : path.descent[step - 1].part_type;
  };
  std::size_t first = path.descent.size() - 1;
  while (first > 0 && unit.types.Get(stepped_into(first - 1)).IsIntegral()) {
    --first;
  }
  return stepped_into(first);
}

}  // namespace

auto DescendInto(AccessPath base, DescentStep step) -> AccessPath {
  base.descent.push_back(std::move(step));
  return base;
}

auto OwnerPlace(const AccessPath& path) -> mir::ExprId {
  return std::visit(
      Overloaded{
          [](mir::ExprId place) { return place; },
          [](const ObjectProperty&) -> mir::ExprId {
            throw InternalError(
                "access path: this construct's owner is a place, and a "
                "property of an object reached it");
          }},
      path.owner);
}

auto OwnerValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const PathOwner& owner) -> mir::TypeId {
  return std::visit(
      Overloaded{
          [&](mir::ExprId place) {
            const mir::TypeId owner_type = block.exprs.Get(place).type;
            const mir::Type& owner_ty = unit.types.Get(owner_type);
            return owner_ty.IsCapabilityWrapper() ? owner_ty.WrappedValueType()
                                                  : owner_type;
          },
          [](const ObjectProperty& property) { return property.type; }},
      owner);
}

auto PathValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const AccessPath& path) -> mir::TypeId {
  if (!path.descent.empty()) {
    return path.descent.back().part_type;
  }
  return OwnerValueType(unit, block, path.owner);
}

auto PathPlace(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId {
  auto step = path.descent.begin();
  const auto from_place = [&](mir::ExprId owner) -> mir::ExprId {
    const mir::Type& owner_ty = unit.types.Get(block.exprs.Get(owner).type);
    if (!owner_ty.IsCapabilityWrapper()) {
      return owner;
    }
    RefuseNetCell(owner_ty);
    // The write is opened on the wrapper, the whole of what the wrapper holds
    // is designated within it, and each step into a part that is storage of its
    // own designates that part within the same write, so the write knows what
    // forming each one did. Bits of a packed value are designated the same
    // way, so the write knows which bits it reached. Where the value
    // stepped into has no such parts -- or the step reached a slice, several
    // elements or bits rather than one place -- the write lands, and any step
    // left reaches into the value landed on.
    mir::TypeId value = owner_ty.WrappedValueType();
    const auto designated = [&](mir::TypeId part) {
      return unit.types.Intern(mir::Type{mir::DesignationType{.value = part}});
    };
    const mir::ExprId write = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kOpenForWrite,
                            .receiver = owner},
                    .arguments = {}},
            .type = unit.types.Intern(
                mir::Type{mir::OpenWriteType{.value = value}})});
    mir::ExprId designation = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kDesignateWhole,
                            .receiver = write},
                    .arguments = {}},
            .type = designated(value)});
    while (step != path.descent.end() &&
           (unit.types.Get(value).PartsAreStorage() ||
            unit.types.Get(value).BitsAreWrittenInPlace())) {
      const DescentStep& taken = *step++;
      value = taken.part_type;
      designation = TakeStep(
          unit, block, taken, StepUse::kDesignate, designation,
          designated(value));
      if (EndsDesignation(taken)) {
        break;
      }
    }
    return block.exprs.Add(mir::MakeDerefExpr(designation, value));
  };
  // A property is written through a write opened on its object alone, which
  // the object hears when the write ends; the property is reached through the
  // write, as a member is through a guard.
  mir::ExprId reached = std::visit(
      Overloaded{
          from_place,
          [&](const ObjectProperty& owner) {
            return block.exprs.Add(PropertyStorage(
                unit, block, OpenObjectWrite(unit, block, owner.object),
                owner.property, owner.type));
          }},
      path.owner);
  for (; step != path.descent.end(); ++step) {
    reached =
        TakeStep(unit, block, *step, StepUse::kWrite, reached, step->part_type);
  }
  return reached;
}

auto PathReference(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId {
  mir::ExprId reference = std::visit(
      Overloaded{
          [&](mir::ExprId place) {
            return BuildReferenceArg(
                unit, block, place, block.exprs.Get(place).type);
          },
          [&](const ObjectProperty& owner) {
            return PropertyReference(
                unit, block, owner.object, owner.property, owner.type);
          }},
      path.owner);
  mir::TypeId value = OwnerValueType(unit, block, path.owner);
  for (const DescentStep& step : path.descent) {
    if (!unit.types.Get(value).PartsAreStorage()) {
      throw InternalError(
          "access path: a part lent by reference is storage of its own (LRM "
          "13.5.2), and this descent reaches into a value whose parts are not");
    }
    value = step.part_type;
    reference = TakeStep(
        unit, block, step, StepUse::kLend, reference,
        unit.types.Intern(
            mir::Type{mir::RefType{
                .pointee = value, .mutability = mir::Mutability::kMutable}}));
  }
  return reference;
}

auto PathValue(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId {
  mir::ExprId reached = std::visit(
      Overloaded{
          [&](mir::ExprId place) {
            const mir::Type& owner_ty =
                unit.types.Get(block.exprs.Get(place).type);
            if (!owner_ty.IsCapabilityWrapper()) {
              return place;
            }
            return block.exprs.Add(
                mir::MakeCellLoadCallExpr(place, owner_ty.WrappedValueType()));
          },
          [&](const ObjectProperty& owner) {
            return block.exprs.Add(PropertyStorage(
                unit, block, owner.object, owner.property, owner.type));
          }},
      path.owner);
  for (const DescentStep& step : path.descent) {
    reached = block.exprs.Add(StepRead(unit, block, step, reached));
  }
  return reached;
}

auto StepRead(
    const mir::CompilationUnit& unit, mir::Block& block,
    const DescentStep& step, mir::ExprId receiver) -> mir::Expr {
  StepCall call = StepCallFor(unit, block, step, StepUse::kRead);
  return MakeBuiltinCall(
      unit, block, call.entry, receiver, call.part, call.type_argument,
      std::move(call.arguments), step.part_type);
}

auto PartSelectNaturalType(
    mir::CompilationUnit& unit, mir::TypeId source_type, mir::TypeId part_type)
    -> mir::TypeId {
  const auto& source = unit.types.Get(source_type);
  const auto& part = unit.types.Get(part_type);
  if (!source.IsIntegral() || !part.IsIntegral()) {
    return part_type;
  }
  mir::IntegralType natural = part.Integral();
  natural.signedness = mir::Signedness::kUnsigned;
  natural.state_kind = source.Integral().state_kind;
  return unit.types.Intern(mir::Type{natural});
}

auto PathValueAsDeclared(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId {
  // A path that descends nowhere reads the whole of what its owner holds,
  // which is a value of its own.
  if (path.descent.empty()) {
    return PathValue(unit, block, path);
  }
  AccessPath into = path;
  DescentStep last = std::move(into.descent.back());
  into.descent.pop_back();
  const mir::TypeId declared = last.part_type;
  last.part_type = PartSelectNaturalType(
      unit, ValueSelectedWithin(unit, block, path), declared);
  const mir::ExprId receiver = PathValue(unit, block, into);
  const mir::ExprId read =
      block.exprs.Add(StepRead(unit, block, last, receiver));
  return ConvertToType(unit, block, read, declared);
}

auto SettledPlace(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId place)
    -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  if (mir::EvaluatesNothing(block, place)) {
    return place;
  }
  // By value: forming the settled place appends to the pool this views.
  const mir::Expr node = block.exprs.Get(place);
  // A pointer a computation yields is a value, and holding it in a local names
  // the same object. Any other value a computation yields names no storage a
  // place could be formed over again, and no path is rooted at one.
  const auto computed = [&]() -> mir::ExprId {
    if (unit_lowerer.Unit().types.Get(node.type).Is<mir::PointerType>()) {
      return EvaluatedOnce(frame, place);
    }
    throw InternalError(
        "access path: a place named at more than one point is a name, or a "
        "field or a dereference over what it is reached through, and this one "
        "is a value a computation yields");
  };
  return std::visit(
      Overloaded{
          [&](const mir::FieldAccessExpr& field) -> mir::ExprId {
            return block.exprs.Add(
                mir::MakeFieldAccessExpr(
                    SettledPlace(unit_lowerer, frame, field.receiver),
                    field.field, node.type));
          },
          [&](const mir::DerefExpr& deref) -> mir::ExprId {
            const mir::TypeId reached_through =
                block.exprs.Get(deref.pointer).type;
            // A capability wrapper is storage, so what it is reached through
            // is settled and the wrapper stays where it is; a pointer or a
            // handle is a value, and holding it in a local names the same
            // object.
            if (unit_lowerer.Unit()
                    .types.Get(reached_through)
                    .IsCapabilityWrapper()) {
              return block.exprs.Add(
                  mir::MakeDerefExpr(
                      SettledPlace(unit_lowerer, frame, deref.pointer),
                      node.type));
            }
            return block.exprs.Add(
                mir::MakeDerefExpr(
                    EvaluatedOnce(frame, deref.pointer), node.type));
          },
          [&](const mir::StringLiteral&) { return computed(); },
          [&](const mir::NullLiteral&) { return computed(); },
          [&](const mir::MachineBoolLiteral&) { return computed(); },
          [&](const mir::MachineIntLiteral&) { return computed(); },
          [&](const mir::MachineFloatLiteral&) { return computed(); },
          [&](const mir::ReferenceExpr&) { return computed(); },
          [&](const mir::AddressOfExpr&) { return computed(); },
          [&](const mir::UnaryExpr&) { return computed(); },
          [&](const mir::BinaryExpr&) { return computed(); },
          [&](const mir::CastExpr&) { return computed(); },
          [&](const mir::DynamicCastExpr&) { return computed(); },
          [&](const mir::ConditionalExpr&) { return computed(); },
          [&](const mir::BlockExpr&) { return computed(); },
          [&](const mir::AssignExpr&) { return computed(); },
          [&](const mir::CallExpr&) { return computed(); },
          [&](const mir::MoveExpr&) { return computed(); },
          [&](const mir::ClosureExpr&) { return computed(); },
          [&](const mir::CompositeExpr&) { return computed(); },
          [&](const mir::AwaitExpr&) { return computed(); },
          [&](const mir::WaitExpr&) { return computed(); },
          [&](const mir::VectorGetExpr&) { return computed(); }},
      node.data);
}

auto SettledOwner(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const PathOwner& owner) -> PathOwner {
  return std::visit(
      Overloaded{
          [&](mir::ExprId place) -> PathOwner {
            return SettledPlace(unit_lowerer, frame, place);
          },
          [&](const ObjectProperty& property) -> PathOwner {
            return ObjectProperty{
                .object = EvaluatedOnce(frame, property.object),
                .property = property.property,
                .type = property.type};
          }},
      owner);
}

auto Settled(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path)
    -> AccessPath {
  path.owner = SettledOwner(unit_lowerer, frame, path.owner);
  for (DescentStep& step : path.descent) {
    ForEachOperand(step, [&](mir::ExprId& operand) {
      operand = EvaluatedOnce(frame, operand);
    });
  }
  return path;
}

auto SettledForRead(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path)
    -> SettledPath {
  AccessPath settled = Settled(unit_lowerer, frame, std::move(path));
  return SettledPath{
      .named_in = frame.current_block,
      .owner = settled.owner,
      .descent = std::move(settled.descent)};
}

auto NamedIn(const SettledPath& settled, mir::Block& to) -> AccessPath {
  const mir::Block& from = *settled.named_in;
  AccessPath path{.owner = settled.owner, .descent = settled.descent};
  // Naming a node again reads it out of one pool while adding to the other, so
  // the two are different pools; a path already named in `to` is the answer.
  if (&from == &to) {
    return path;
  }
  const auto again = [&](mir::ExprId id) { return NamedAgain(from, to, id); };
  path.owner = std::visit(
      Overloaded{
          [&](mir::ExprId place) -> PathOwner { return again(place); },
          [&](const ObjectProperty& property) -> PathOwner {
            return ObjectProperty{
                .object = again(property.object),
                .property = property.property,
                .type = property.type};
          }},
      path.owner);
  for (DescentStep& step : path.descent) {
    ForEachOperand(step, [&](mir::ExprId& operand) {
      operand = NamedAgain(from, to, operand);
    });
  }
  return path;
}

auto ReadThenWrite(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath place)
    -> ReadThenWritten {
  AccessPath settled = Settled(unit_lowerer, frame, std::move(place));
  const mir::ExprId incoming =
      PathValueAsDeclared(unit_lowerer.Unit(), *frame.current_block, settled);
  return ReadThenWritten{.place = std::move(settled), .incoming = incoming};
}

auto BitsWithinOwner(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> PathBits {
  mir::ExprId first = BuildConstantPosition(unit, block, 0);
  for (const DescentStep& step : path.descent) {
    const auto* bits = std::get_if<StepToBits>(&step.to);
    if (bits == nullptr) {
      throw InternalError(
          "access path: a part of a packed value is reached by steps naming "
          "bits from one start, and this descent takes a step that is not one");
    }
    first = BuildPositionSum(unit, block, first, bits->start);
  }
  const mir::TypeId part = PathValueType(unit, block, path);
  return PathBits{
      .first = first, .width = unit.types.Get(part).Integral().bit_width};
}

auto BuildStoreExpr(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path,
    mir::ExprId rhs_id) -> mir::Expr {
  // A store carries the right-hand side to the destination's declared
  // representation (LRM 10.6.1).
  rhs_id = ConvertToType(unit, block, rhs_id, PathValueType(unit, block, path));
  // Replacing the whole of what a capability wrapper holds acts on the wrapper
  // -- the value lands in its storage and it reports the change to whatever is
  // watching -- so it is a call taking the wrapper as its destination. A store
  // that descends writes a part, which reaches storage the way a read does and
  // assigns through what it reaches.
  const std::optional<mir::ExprId> wrapper =
      StoredWrapper(unit, block, path.owner);
  if (path.descent.empty() && wrapper.has_value()) {
    // The operands are the destination and the value, and nothing else: the
    // engine the wrapper reports through is the ambient one, which has the
    // standing of a stack pointer rather than of program data.
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kStore,
                        .receiver = wrapper},
                .arguments = {rhs_id}},
        .type = unit.builtins.void_type};
  }
  return mir::MakeAssignExpr(
      unit.builtins, PathPlace(unit, block, path), rhs_id);
}

}  // namespace lyra::lowering::hir_to_mir

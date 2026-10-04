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
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto CallEntry(
    mir::Block& block, support::BuiltinFn fn,
    std::optional<base::ComponentIndex> position, mir::ExprId receiver,
    std::vector<mir::ExprId> operands, mir::TypeId type) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = fn,
                          .receiver = receiver,
                          .position = position},
                  .arguments = std::move(operands)},
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

// A step of a write's descent taken within the write in progress: the entry
// that takes it, and whether the write lands where it leads -- a slice is
// several elements rather than one place, so nothing steps further within the
// write from one.
struct DesignatingStep {
  support::BuiltinFn entry;
  bool lands;
};

auto DesignatingStepOf(const DescentStep& step) -> DesignatingStep {
  const std::optional<support::PartSelection> selects =
      support::RuntimeEntryOf(step.part_entry).selects;
  if (!selects.has_value()) {
    throw InternalError(
        "access path: a step of a write's descent reaches a part, and this one "
        "names an entry that reaches none");
  }
  switch (*selects) {
    case support::PartSelection::kElement:
      return {.entry = support::BuiltinFn::kDesignateElement, .lands = false};
    case support::PartSelection::kComponent:
      return {.entry = support::BuiltinFn::kDesignateComponent, .lands = false};
    case support::PartSelection::kSlice:
      return {.entry = support::BuiltinFn::kDesignateSlice, .lands = true};
  }
  throw InternalError("access path: unknown part selection");
}

// A step of a target's descent taken on a reference. Only an element and a
// component may be passed by reference (LRM 13.5.2), so a slice reaching here
// is a producer that lent what the language does not.
auto ReferringStepOf(const DescentStep& step) -> support::BuiltinFn {
  const std::optional<support::PartSelection> selects =
      support::RuntimeEntryOf(step.part_entry).selects;
  if (!selects.has_value()) {
    throw InternalError(
        "access path: a step of a lent target's descent reaches a part, and "
        "this "
        "one names an entry that reaches none");
  }
  switch (*selects) {
    case support::PartSelection::kElement:
      return support::BuiltinFn::kReferElement;
    case support::PartSelection::kComponent:
      return support::BuiltinFn::kReferComponent;
    case support::PartSelection::kSlice:
      throw InternalError(
          "access path: a slice may not be passed by reference (LRM 13.5.2), "
          "so "
          "no lent target reaches one");
  }
  throw InternalError("access path: unknown part selection");
}

// The owner as a write reaches it: through a write opened on the object it is a
// property of, where it is one, so that ending the write tells the object.
auto WrittenOwner(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId {
  if (!path.object.has_value()) {
    return path.owner;
  }
  return PropertyWrittenThrough(unit, block, *path.object, path.owner);
}

// `lhs op= rhs`, at an operator whose two forms are the whole of what an
// assignment may apply. An operator the target applies to two values of one
// type rides the store, which reaches the place once; one it does not is
// applied by the entry that performs it, against the value the place holds,
// which reaches it once for the same reason (LRM 11.4.1). Both are ordinary MIR
// nodes with nothing left to decide.
auto BuildCompoundExpr(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path,
    mir::ExprId rhs_id, CompoundOperation op, mir::TypeId result_type)
    -> mir::Expr {
  const mir::ExprId place = PathPlace(unit, block, path);
  return std::visit(
      Overloaded{
          [&](mir::BinaryOp applied) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::AssignExpr{
                        .target = place,
                        .compound_op = applied,
                        .value = rhs_id},
                .type = result_type};
          },
          [&](support::BuiltinFn entry) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee =
                            mir::Direct{.target = entry, .receiver = place},
                        .arguments = {rhs_id}},
                .type = unit.builtins.void_type};
          }},
      op);
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
          [&](const mir::MachineArrayDataExpr& e) -> mir::ExprData {
            return mir::MachineArrayDataExpr{.array = again(e.array)};
          },
          [&](const mir::FieldAccessExpr& e) -> mir::ExprData {
            return mir::FieldAccessExpr{
                .receiver = again(e.receiver), .field = e.field};
          },
          [&](const mir::UnaryExpr&) { return computes(); },
          [&](const mir::BinaryExpr&) { return computes(); },
          [&](const mir::CastExpr&) { return computes(); },
          [&](const mir::ConditionalExpr&) { return computes(); },
          [&](const mir::BlockExpr&) { return computes(); },
          [&](const mir::AssignExpr&) { return computes(); },
          [&](const mir::IncDecExpr&) { return computes(); },
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
// selects within. Every step into a packed value is a run of that one vector
// (LRM 7.2.1), so where the steps at the end of the descent each enter a packed
// value, what the last of them selects within is the outermost of those.
auto ValueSelectedWithin(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const AccessPath& path) -> mir::TypeId {
  const auto stepped_into = [&](std::size_t step) {
    return step == 0 ? PathValueType(
                           unit, block, {.owner = path.owner, .descent = {}})
                     : path.descent[step - 1].part_type;
  };
  std::size_t first = path.descent.size() - 1;
  while (first > 0 &&
         unit.types.Get(stepped_into(first - 1)).IsIntegralPacked()) {
    --first;
  }
  return stepped_into(first);
}

}  // namespace

auto StepArguments(
    const mir::CompilationUnit& unit, mir::Block& block,
    const DescentStep& step) -> std::vector<mir::ExprId> {
  std::vector<mir::ExprId> arguments = step.operands;
  if (step.count.has_value()) {
    arguments.push_back(BuildMachineIntLiteral(
        unit, block, static_cast<std::int64_t>(*step.count)));
  }
  return arguments;
}

auto DescendInto(AccessPath base, DescentStep step) -> AccessPath {
  base.descent.push_back(std::move(step));
  return base;
}

auto PathValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const AccessPath& path) -> mir::TypeId {
  if (!path.descent.empty()) {
    return path.descent.back().part_type;
  }
  const mir::TypeId owner_type = block.exprs.Get(path.owner).type;
  const mir::Type& owner_ty = unit.types.Get(owner_type);
  return owner_ty.IsCapabilityWrapper() ? owner_ty.WrappedValueType()
                                        : owner_type;
}

auto PathPlace(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId {
  const auto reach = [&](support::BuiltinFn entry, mir::ExprId from,
                         const DescentStep& step, mir::TypeId type) {
    return CallEntry(
        block, entry, step.position, from, StepArguments(unit, block, step),
        type);
  };
  auto step = path.descent.begin();
  const mir::ExprId owner = WrittenOwner(unit, block, path);
  mir::ExprId reached = owner;
  const mir::Type& owner_ty = unit.types.Get(block.exprs.Get(path.owner).type);
  if (owner_ty.IsCapabilityWrapper()) {
    RefuseNetCell(owner_ty);
    // The write is opened on the wrapper, the whole of what the wrapper holds
    // is designated within it, and each step into a part that is storage of its
    // own designates that part within the same write, so the write knows what
    // forming each one did. Where the value stepped into has no such parts --
    // or the step reached a slice, several elements rather than one place --
    // the write lands, and any step left reaches into the value landed on.
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
    mir::ExprId designation = CallEntry(
        block, support::BuiltinFn::kDesignateWhole, std::nullopt, write, {},
        designated(value));
    while (step != path.descent.end() &&
           unit.types.Get(value).PartsAreStorage()) {
      const DescentStep& taken = *step++;
      const DesignatingStep designating = DesignatingStepOf(taken);
      value = taken.part_type;
      designation =
          reach(designating.entry, designation, taken, designated(value));
      if (designating.lands) {
        break;
      }
    }
    reached = block.exprs.Add(mir::MakeDerefExpr(designation, value));
  }
  for (; step != path.descent.end(); ++step) {
    reached = reach(step->part_entry, reached, *step, step->part_type);
  }
  return reached;
}

auto PathReference(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId {
  mir::ExprId reference =
      path.object.has_value()
          ? PropertyReferred(unit, block, *path.object, path.owner)
          : BuildReferenceArg(
                unit, block, path.owner, block.exprs.Get(path.owner).type);
  mir::TypeId value =
      PathValueType(unit, block, {.owner = path.owner, .descent = {}});
  for (const DescentStep& step : path.descent) {
    if (!unit.types.Get(value).PartsAreStorage()) {
      throw InternalError(
          "access path: a part lent by reference is storage of its own (LRM "
          "13.5.2), and this descent reaches into a value whose parts are not");
    }
    value = step.part_type;
    reference = CallEntry(
        block, ReferringStepOf(step), step.position, reference,
        StepArguments(unit, block, step),
        unit.types.Intern(
            mir::Type{mir::RefType{
                .pointee = value, .mutability = mir::Mutability::kMutable}}));
  }
  return reference;
}

auto PathValue(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId {
  const mir::Type& owner_ty = unit.types.Get(block.exprs.Get(path.owner).type);
  mir::ExprId reached = path.owner;
  if (owner_ty.IsCapabilityWrapper()) {
    reached = block.exprs.Add(
        mir::MakeCellLoadCallExpr(path.owner, owner_ty.WrappedValueType()));
  }
  for (const DescentStep& step : path.descent) {
    reached = block.exprs.Add(StepRead(unit, block, step, reached));
  }
  return reached;
}

auto StepRead(
    const mir::CompilationUnit& unit, mir::Block& block,
    const DescentStep& step, mir::ExprId receiver) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = step.value_entry,
                      .receiver = receiver,
                      .position = step.position},
              .arguments = StepArguments(unit, block, step)},
      .type = step.part_type};
}

auto OwnedValue(
    const mir::CompilationUnit& unit, mir::Block& block, mir::Expr read)
    -> mir::Expr {
  const mir::TypeId type = read.type;
  if (!unit.types.Get(type).IsIntegralPacked()) {
    return read;
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kToOwned,
                      .receiver = block.exprs.Add(std::move(read))},
              .arguments = {}},
      .type = type};
}

auto PartSelectNaturalType(
    mir::CompilationUnit& unit, mir::TypeId source_type, mir::TypeId part_type)
    -> mir::TypeId {
  const auto& source = unit.types.Get(source_type);
  const auto& part = unit.types.Get(part_type);
  if (!source.IsIntegralPacked() || !part.IsIntegralPacked()) {
    return part_type;
  }
  mir::PackedArrayType natural = part.PackedShape();
  natural.signedness = mir::Signedness::kUnsigned;
  natural.state_kind = source.PackedShape().state_kind;
  return unit.types.Intern(mir::Type{std::move(natural)});
}

auto PathOwnedValue(
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
  const mir::ExprId owned = block.exprs.Add(
      OwnedValue(unit, block, StepRead(unit, block, last, receiver)));
  return ConvertToType(unit, block, owned, declared);
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
          [&](const mir::MachineArrayDataExpr&) { return computed(); },
          [&](const mir::UnaryExpr&) { return computed(); },
          [&](const mir::BinaryExpr&) { return computed(); },
          [&](const mir::CastExpr&) { return computed(); },
          [&](const mir::ConditionalExpr&) { return computed(); },
          [&](const mir::BlockExpr&) { return computed(); },
          [&](const mir::AssignExpr&) { return computed(); },
          [&](const mir::IncDecExpr&) { return computed(); },
          [&](const mir::CallExpr&) { return computed(); },
          [&](const mir::MoveExpr&) { return computed(); },
          [&](const mir::ClosureExpr&) { return computed(); },
          [&](const mir::CompositeExpr&) { return computed(); },
          [&](const mir::AwaitExpr&) { return computed(); },
          [&](const mir::WaitExpr&) { return computed(); },
          [&](const mir::VectorGetExpr&) { return computed(); }},
      node.data);
}

auto Settled(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path)
    -> AccessPath {
  path.owner = SettledPlace(unit_lowerer, frame, path.owner);
  if (path.object.has_value()) {
    path.object = EvaluatedOnce(frame, *path.object);
  }
  for (DescentStep& step : path.descent) {
    for (mir::ExprId& operand : step.operands) {
      operand = EvaluatedOnce(frame, operand);
    }
  }
  return path;
}

auto SettledForRead(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path)
    -> SettledPath {
  path.object = std::nullopt;
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
  path.owner = NamedAgain(from, to, path.owner);
  for (DescentStep& step : path.descent) {
    for (mir::ExprId& operand : step.operands) {
      operand = NamedAgain(from, to, operand);
    }
  }
  return path;
}

auto ReadThenWrite(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath place)
    -> ReadThenWritten {
  AccessPath settled = Settled(unit_lowerer, frame, std::move(place));
  const mir::ExprId incoming =
      PathOwnedValue(unit_lowerer.Unit(), *frame.current_block, settled);
  return ReadThenWritten{.place = std::move(settled), .incoming = incoming};
}

auto RunWithinOwner(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> PathRun {
  mir::ExprId first = BuildConstantPosition(unit, block, 0);
  for (const DescentStep& step : path.descent) {
    // A step that reaches a run is the one that states a count, and its one
    // operand is where the run starts.
    if (!step.count.has_value()) {
      throw InternalError(
          "access path: a part of a packed value is reached by runs of a fixed "
          "count from one start, and this descent takes a step that is not "
          "one");
    }
    first = BuildPositionSum(unit, block, first, step.operands.front());
  }
  const mir::TypeId part = PathValueType(unit, block, path);
  return PathRun{
      .first = first, .width = unit.types.Get(part).PackedShape().BitWidth()};
}

auto BuildStoreExpr(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path,
    mir::ExprId rhs_id, std::optional<CompoundOperation> compound_op,
    mir::TypeId result_type) -> mir::Expr {
  // A compound store computes its value through the operator, which already
  // yields the destination's shape, so only a plain store carries the
  // right-hand side to the destination's declared representation (LRM 10.6.1).
  // The front end already converts width, signedness, and state domain; the
  // dimension stack -- and, for a container, the element representation and
  // bound -- is the axis it leaves to assignment.
  if (compound_op.has_value()) {
    return BuildCompoundExpr(
        unit, block, path, rhs_id, *compound_op, result_type);
  }
  rhs_id = ConvertToType(unit, block, rhs_id, PathValueType(unit, block, path));
  // Replacing the whole of what a capability wrapper holds acts on the wrapper
  // -- the value lands in its storage and it reports the change to whatever is
  // watching -- so it is a call taking the wrapper as its destination. A store
  // that descends writes a part, which reaches storage the way a read does and
  // assigns through what it reaches.
  const mir::Type& owner_ty = unit.types.Get(block.exprs.Get(path.owner).type);
  if (path.descent.empty() && owner_ty.IsCapabilityWrapper() &&
      !owner_ty.Is<mir::ResolvedType>()) {
    // The operands are the destination and the value, and nothing else: the
    // engine the wrapper reports through is the ambient one, which has the
    // standing of a stack pointer rather than of program data.
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kStore,
                        .receiver = WrittenOwner(unit, block, path)},
                .arguments = {rhs_id}},
        .type = unit.builtins.void_type};
  }
  return mir::Expr{
      .data =
          mir::AssignExpr{
              .target = PathPlace(unit, block, path), .value = rhs_id},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir

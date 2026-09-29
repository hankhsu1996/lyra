#include "lyra/lowering/hir_to_mir/lhs_store.hpp"

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
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
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
        "lhs_store: a net's cell takes no value a write may put there; the "
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
        "lhs_store: a step of a write's descent reaches a part, and this one "
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
  throw InternalError("lhs_store: unknown part selection");
}

// A step of a target's descent taken on a reference. Only an element and a
// component may be passed by reference (LRM 13.5.2), so a slice reaching here
// is a producer that lent what the language does not.
auto ReferringStepOf(const DescentStep& step) -> support::BuiltinFn {
  const std::optional<support::PartSelection> selects =
      support::RuntimeEntryOf(step.part_entry).selects;
  if (!selects.has_value()) {
    throw InternalError(
        "lhs_store: a step of a lent target's descent reaches a part, and this "
        "one names an entry that reaches none");
  }
  switch (*selects) {
    case support::PartSelection::kElement:
      return support::BuiltinFn::kReferElement;
    case support::PartSelection::kComponent:
      return support::BuiltinFn::kReferComponent;
    case support::PartSelection::kSlice:
      throw InternalError(
          "lhs_store: a slice may not be passed by reference (LRM 13.5.2), so "
          "no lent target reaches one");
  }
  throw InternalError("lhs_store: unknown part selection");
}

// The owner as a write reaches it: through a write opened on the object it is a
// property of, where it is one, so that ending the write tells the object.
auto WrittenOwner(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target)
    -> mir::ExprId {
  if (!target.object.has_value()) {
    return target.owner;
  }
  return PropertyWrittenThrough(unit, block, *target.object, target.owner);
}

// `lhs op= rhs`, at an operator whose two forms are the whole of what an
// assignment may apply. An operator the target applies to two values of one
// type rides the store, which reaches the place once; one it does not is
// applied by the entry that performs it, against the value the place holds,
// which reaches it once for the same reason (LRM 11.4.1). Both are ordinary MIR
// nodes with nothing left to decide.
auto BuildCompoundExpr(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target,
    mir::ExprId rhs_id, CompoundOperation op, mir::TypeId result_type)
    -> mir::Expr {
  const mir::ExprId place = TargetPlace(unit, block, target);
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

auto DescendInto(WriteTarget base, DescentStep step) -> WriteTarget {
  base.descent.push_back(std::move(step));
  return base;
}

auto TargetValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const WriteTarget& target) -> mir::TypeId {
  if (!target.descent.empty()) {
    return target.descent.back().part_type;
  }
  const mir::TypeId owner_type = block.exprs.Get(target.owner).type;
  const mir::Type& owner_ty = unit.types.Get(owner_type);
  return owner_ty.IsCapabilityWrapper() ? owner_ty.WrappedValueType()
                                        : owner_type;
}

auto TargetPlace(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target)
    -> mir::ExprId {
  const auto reach = [&](support::BuiltinFn entry, mir::ExprId from,
                         const DescentStep& step, mir::TypeId type) {
    return CallEntry(
        block, entry, step.position, from, StepArguments(unit, block, step),
        type);
  };
  const mir::Type& owner_ty =
      unit.types.Get(block.exprs.Get(target.owner).type);
  auto step = target.descent.begin();
  const mir::ExprId owner = WrittenOwner(unit, block, target);
  mir::ExprId reached = owner;
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
    while (step != target.descent.end() &&
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
  for (; step != target.descent.end(); ++step) {
    reached = reach(step->part_entry, reached, *step, step->part_type);
  }
  return reached;
}

auto TargetReference(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target)
    -> mir::ExprId {
  mir::ExprId reference =
      target.object.has_value()
          ? PropertyReferred(unit, block, *target.object, target.owner)
          : BuildReferenceArg(
                unit, block, target.owner, block.exprs.Get(target.owner).type);
  mir::TypeId value =
      TargetValueType(unit, block, {.owner = target.owner, .descent = {}});
  for (const DescentStep& step : target.descent) {
    if (!unit.types.Get(value).PartsAreStorage()) {
      throw InternalError(
          "lhs_store: a part lent by reference is storage of its own (LRM "
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

auto ReadTargetValue(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target)
    -> mir::ExprId {
  const mir::Type& owner_ty =
      unit.types.Get(block.exprs.Get(target.owner).type);
  mir::ExprId reached = target.owner;
  if (owner_ty.IsCapabilityWrapper()) {
    reached = block.exprs.Add(
        mir::MakeCellLoadCallExpr(target.owner, owner_ty.WrappedValueType()));
  }
  for (const DescentStep& step : target.descent) {
    reached = CallEntry(
        block, step.value_entry, step.position, reached,
        StepArguments(unit, block, step), step.part_type);
  }
  return reached;
}

auto BuildStoreExpr(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target,
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
        unit, block, target, rhs_id, *compound_op, result_type);
  }
  rhs_id =
      ConvertToType(unit, block, rhs_id, TargetValueType(unit, block, target));
  // Replacing the whole of what a capability wrapper holds acts on the wrapper
  // -- the value lands in its storage and it reports the change to whatever is
  // watching -- so it is a call taking the wrapper as its destination. A store
  // that descends writes a part, which reaches storage the way a read does and
  // assigns through what it reaches.
  const mir::Type& owner_ty =
      unit.types.Get(block.exprs.Get(target.owner).type);
  if (target.descent.empty() && owner_ty.IsCapabilityWrapper() &&
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
                        .receiver = WrittenOwner(unit, block, target)},
                .arguments = {rhs_id}},
        .type = unit.builtins.void_type};
  }
  return mir::Expr{
      .data =
          mir::AssignExpr{
              .target = TargetPlace(unit, block, target), .value = rhs_id},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir

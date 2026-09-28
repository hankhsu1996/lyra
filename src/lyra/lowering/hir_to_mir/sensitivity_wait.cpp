#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"

#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/endpoint.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/object_change.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Where a routed leaf's cell is, reached from the scope the route is counted
// from: a scope's own lowering is that scope, and a procedural body's is the
// scope enclosing the body. Only a route asks.
auto BindRouted(
    const StructuralScopeLowerer& scope, const WalkFrame& frame,
    const hir::RoutedValueRef& reference) -> BoundEndpoint {
  return BindEndpoint(scope, frame, reference);
}
auto BindRouted(
    const ProcessLowerer& process, const WalkFrame& frame,
    const hir::RoutedValueRef& reference) -> BoundEndpoint {
  return BindEndpoint(process.EnclosingScopeLowerer(), frame, reference);
}

// The cell a virtual interface's member is, in the instance the handle holds
// as the wait collects its leaves: a pointer to it, the form a registration
// takes. The
// handle was lowered once already, as part of the event expression the wait
// evaluates, so lowering it here can fail only if the two disagree.
template <typename Lowerer>
auto HeldCellPointer(
    Lowerer& lowerer, const WalkFrame& frame, mir::Block& block,
    const hir::InterfaceMemberAccessExpr& held) -> mir::ExprId {
  auto pointer = HeldInterfaceMember(lowerer, frame.WithBlock(&block), held);
  if (!pointer) {
    throw InternalError(
        "HeldCellPointer: a virtual interface lowered as part of the event "
        "expression failed to lower again as the cell it watches");
  }
  return *pointer;
}

// The event source of the object a leaf reached, found as the wait collects its
// leaves: whichever object the handle names then, or the method's own object.
// The handle was lowered once already, as part of the event expression the wait
// evaluates, so lowering it here can fail only if the two disagree.
template <typename Lowerer>
auto ObjectSourcePointer(
    Lowerer& lowerer, const WalkFrame& frame, mir::Block& block,
    const hir::ObjectEventSource& source) -> mir::ExprId {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::ExprId receiver = std::visit(
      Overloaded{
          [&](hir::ExprId handle) -> mir::ExprId {
            auto lowered = lowerer.LowerExpr(
                lowerer.HirExprs().Get(handle), frame.WithBlock(&block));
            if (!lowered) {
              throw InternalError(
                  "ObjectSourcePointer: a handle lowered as part of the event "
                  "expression failed to lower again as the object it reaches");
            }
            return block.exprs.Add(*std::move(lowered));
          },
          [&](const hir::ReceiverObject&) -> mir::ExprId {
            return block.exprs.Add(
                MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
          }},
      source.object);
  return ObjectEventSourceOf(unit, block, ObjectRootOf(unit, block, receiver));
}

}  // namespace

template <typename Lowerer>
auto BuildObservableCellExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::SensitivityEntry& entry) -> mir::ExprId {
  return std::visit(
      Overloaded{
          [&](const hir::RoutedValueRef& reference) -> mir::ExprId {
            return block.exprs.Add(EndpointCellExpr(
                frame, unit, BindRouted(lowerer, frame, reference)));
          },
          [&](const hir::ExternalUnitValueRef& pkg) -> mir::ExprId {
            return block.exprs.Add(
                LowerExternalUnitValueRefExpr(lowerer.Owner(), pkg));
          },
          [&](const hir::StaticPropertyRef& property) -> mir::ExprId {
            return block.exprs.Add(
                LowerStaticPropertyRefExpr(lowerer.Owner(), frame, property));
          },
          [&](const hir::InterfaceMemberAccessExpr& held) -> mir::ExprId {
            const mir::ExprId pointer =
                HeldCellPointer(lowerer, frame, block, held);
            return block.exprs.Add(
                mir::Expr{
                    .data = mir::DerefExpr{.pointer = pointer},
                    .type = unit.types.Get(block.exprs.Get(pointer).type)
                                .Get<mir::PointerType>()
                                .pointee});
          },
          // An object's event source is subscribed to and holds no value, so
          // there is no cell of it for anything to arm or read.
          [&](const hir::ObjectEventSource&) -> mir::ExprId {
            throw InternalError(
                "BuildObservableCellExpr: an object's event source is reached "
                "only as what a wait subscribes to, and holds no value");
          },
      },
      entry.ref);
}

namespace {

// The same storage as a borrowed pointer, which is the form a registration
// hands the runtime. A route answers for this form itself, because an endpoint
// that reached out of the unit already holds a pointer and composing one from
// the cell would send that case through a dereference and back. Every other
// form is reached as the cell itself, so its pointer is that cell's address.
template <typename Lowerer>
auto BuildObservablePtrExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::SensitivityEntry& entry) -> mir::ExprId {
  const auto address_of_cell = [&]() -> mir::ExprId {
    const mir::ExprId cell =
        BuildObservableCellExpr(block, frame, unit, lowerer, entry);
    const mir::TypeId ptr_type = unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = block.exprs.Get(cell).type,
            .ownership = mir::PointerOwnership::kBorrowed,
            .mutability = mir::Mutability::kMutable}});
    return block.exprs.Add(mir::MakeAddressOfExpr(cell, ptr_type));
  };
  return std::visit(
      Overloaded{
          [&](const hir::RoutedValueRef& reference) -> mir::ExprId {
            return EndpointObservablePtr(
                block, frame, unit, BindRouted(lowerer, frame, reference));
          },
          [&](const hir::ExternalUnitValueRef&) -> mir::ExprId {
            return address_of_cell();
          },
          [&](const hir::StaticPropertyRef&) -> mir::ExprId {
            return address_of_cell();
          },
          // The instance answers with the member's address, which is already
          // the pointer a registration takes.
          [&](const hir::InterfaceMemberAccessExpr& held) -> mir::ExprId {
            return HeldCellPointer(lowerer, frame, block, held);
          },
          [&](const hir::ObjectEventSource& source) -> mir::ExprId {
            return ObjectSourcePointer(lowerer, frame, block, source);
          },
      },
      entry.ref);
}

// One leaf of the wait: the place it watches, what decides whether what happens
// there is an event, and which bits of that place's packed encoding it reads.
// LRM 9.4.2 / 9.4.2.2 / 9.4.3: a bit-addressed footprint becomes
// `(lsb, hi - lsb + 1)`; a read of the whole of it (no footprint) is width 0,
// which is also what a named event carries, having no bits at all.
template <typename Lowerer>
auto BuildTriggerExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::SensitivityEntry& entry,
    mir::LocalId observation) -> mir::ExprId {
  const mir::ExprId observable_ptr =
      BuildObservablePtrExpr(block, frame, unit, lowerer, entry);
  const std::int64_t lsb_bit_offset =
      entry.footprint.has_value()
          ? static_cast<std::int64_t>(entry.footprint->first)
          : 0;
  const std::int64_t bit_width =
      entry.footprint.has_value()
          ? static_cast<std::int64_t>(
                entry.footprint->second - entry.footprint->first + 1)
          : 0;
  const auto int_literal = [&](std::int64_t value) {
    return BuildIntLiteral(unit, block, value);
  };
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments =
                      {observable_ptr,
                       block.exprs.Add(
                           mir::MakeLocalRefExpr(
                               observation, unit.builtins.observation)),
                       int_literal(lsb_bit_offset), int_literal(bit_width)}},
          .type = unit.builtins.trigger});
}

}  // namespace

auto IsFoundThroughAHandle(const hir::SensitivityTarget& target) -> bool {
  return std::visit(
      Overloaded{
          [](const hir::RoutedValueRef&) { return false; },
          [](const hir::ExternalUnitValueRef&) { return false; },
          [](const hir::StaticPropertyRef&) { return false; },
          [](const hir::InterfaceMemberAccessExpr&) { return true; },
          [](const hir::ObjectEventSource&) { return true; },
      },
      target);
}

auto DeclareObservation(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::Block& block,
    support::BuiltinFn entry, std::vector<mir::ExprId> arguments)
    -> mir::LocalId {
  const mir::ExprId observe_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = entry},
                  .arguments = std::move(arguments)},
          .type = unit.builtins.observation});
  const mir::LocalId local =
      frame.bindings->DeclareAnonymous(unit.builtins.observation);
  block.AppendStmt(mir::LocalDeclStmt{.target = local, .init = observe_id});
  return local;
}

template <typename Lowerer>
auto BuildWaitStmt(
    mir::Block& target_block, const WalkFrame& frame, Lowerer& lowerer,
    std::span<const ObservedLeaf> leaves, support::BuiltinFn entry)
    -> mir::Stmt {
  auto& unit = lowerer.Owner().Unit();
  std::vector<mir::ExprId> triggers;
  triggers.reserve(leaves.size());
  for (const ObservedLeaf& leaf : leaves) {
    triggers.push_back(BuildTriggerExpr(
        target_block, frame, unit, lowerer, leaf.entry, leaf.observation));
  }
  const mir::TypeId triggers_type =
      mir::MachineArrayOf(unit.types, unit.builtins.trigger, triggers.size());
  const mir::ExprId triggers_id = target_block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(triggers)},
          .type = triggers_type});

  const mir::ExprId runtime_id =
      target_block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner()));
  const mir::ExprId call_id = target_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = entry},
                  .arguments = {runtime_id, triggers_id}},
          .type = unit.builtins.machine_bool});

  return BuildWaitStmt(lowerer.Owner(), target_block, call_id);
}

template <typename Lowerer>
auto BuildValueChangeWaitStmt(
    mir::Block& target_block, const WalkFrame& frame, Lowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list,
    support::BuiltinFn entry) -> mir::Stmt {
  const mir::LocalId observation = DeclareObservation(
      lowerer.Owner().Unit(), frame, target_block,
      support::BuiltinFn::kObservationOnReaching, {});
  std::vector<ObservedLeaf> leaves;
  leaves.reserve(sensitivity_list.size());
  for (const hir::SensitivityEntry& read : sensitivity_list) {
    leaves.push_back(ObservedLeaf{.entry = read, .observation = observation});
  }
  return BuildWaitStmt(target_block, frame, lowerer, leaves, entry);
}

// One instantiation per lowering a wait is built in.
template auto BuildObservableCellExpr(
    mir::Block&, const WalkFrame&, mir::CompilationUnit&, ProcessLowerer&,
    const hir::SensitivityEntry&) -> mir::ExprId;
template auto BuildObservableCellExpr(
    mir::Block&, const WalkFrame&, mir::CompilationUnit&,
    const StructuralScopeLowerer&, const hir::SensitivityEntry&) -> mir::ExprId;
template auto BuildWaitStmt(
    mir::Block&, const WalkFrame&, ProcessLowerer&,
    std::span<const ObservedLeaf>, support::BuiltinFn) -> mir::Stmt;
template auto BuildWaitStmt(
    mir::Block&, const WalkFrame&, const StructuralScopeLowerer&,
    std::span<const ObservedLeaf>, support::BuiltinFn) -> mir::Stmt;
template auto BuildValueChangeWaitStmt(
    mir::Block&, const WalkFrame&, ProcessLowerer&,
    const std::vector<hir::SensitivityEntry>&, support::BuiltinFn) -> mir::Stmt;
template auto BuildValueChangeWaitStmt(
    mir::Block&, const WalkFrame&, const StructuralScopeLowerer&,
    const std::vector<hir::SensitivityEntry>&, support::BuiltinFn) -> mir::Stmt;

}  // namespace lyra::lowering::hir_to_mir

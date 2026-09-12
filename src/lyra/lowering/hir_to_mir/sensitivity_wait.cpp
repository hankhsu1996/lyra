#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"

#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/endpoint.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildObservableCellExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    const StructuralScopeLowerer& lowerer, const hir::SensitivityEntry& entry)
    -> mir::ExprId {
  return std::visit(
      Overloaded{
          [&](const hir::ReferenceRoute& route) -> mir::ExprId {
            return block.exprs.Add(EndpointCellExpr(
                frame, unit, BindEndpoint(lowerer, frame, route)));
          },
          [&](const hir::ExternalUnitValueRef& pkg) -> mir::ExprId {
            return block.exprs.Add(
                LowerExternalUnitValueRefExpr(lowerer.Owner(), pkg));
          },
          [&](const hir::StaticPropertyRef& property) -> mir::ExprId {
            return block.exprs.Add(
                LowerStaticPropertyRefExpr(lowerer.Owner(), frame, property));
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
auto BuildObservablePtrExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    const StructuralScopeLowerer& lowerer, const hir::SensitivityEntry& entry)
    -> mir::ExprId {
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
          [&](const hir::ReferenceRoute& route) -> mir::ExprId {
            return EndpointObservablePtr(
                block, frame, unit, BindEndpoint(lowerer, frame, route));
          },
          [&](const hir::ExternalUnitValueRef&) -> mir::ExprId {
            return address_of_cell();
          },
          [&](const hir::StaticPropertyRef&) -> mir::ExprId {
            return address_of_cell();
          },
      },
      entry.ref);
}

// One leaf of the wait: the place it watches, what decides whether what happens
// there is an event, and which bits of that place's packed encoding it reads.
// LRM 9.4.2 / 9.4.2.2 / 9.4.3: a bit-addressed footprint becomes
// `(lsb, hi - lsb + 1)`; a read of the whole of it (no footprint) is width 0,
// which is also what a named event carries, having no bits at all.
auto BuildTriggerExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    const StructuralScopeLowerer& lowerer, const hir::SensitivityEntry& entry,
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

auto BuildWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer, std::span<const ObservedLeaf> leaves)
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
                  .callee = mir::Direct{.target = support::BuiltinFn::kWaitAny},
                  .arguments = {runtime_id, triggers_id}},
          .type = unit.builtins.void_type});
  const mir::ExprId await_id = target_block.exprs.Add(
      mir::Expr{
          .data = mir::AwaitExpr{.awaitable = call_id},
          .type = unit.builtins.void_type});

  return mir::Stmt{
      .label = std::nullopt, .data = mir::ExprStmt{.expr = await_id}};
}

auto BuildValueChangeWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list) -> mir::Stmt {
  const mir::LocalId observation = DeclareObservation(
      lowerer.Owner().Unit(), frame, target_block,
      support::BuiltinFn::kObservationOnReaching, {});
  std::vector<ObservedLeaf> leaves;
  leaves.reserve(sensitivity_list.size());
  for (const hir::SensitivityEntry& entry : sensitivity_list) {
    leaves.push_back(ObservedLeaf{.entry = entry, .observation = observation});
  }
  return BuildWaitStmt(target_block, frame, lowerer, leaves);
}

}  // namespace lyra::lowering::hir_to_mir

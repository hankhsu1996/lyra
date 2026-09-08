#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"

#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/endpoint.hpp"
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

namespace {

// One leaf of the wait: the place it watches, which bits of that place's packed
// encoding it reads, and what decides whether what happens there is an event.
// LRM 9.4.2 / 9.4.2.2 / 9.4.3: a bit-addressed footprint becomes
// `(lsb, hi - lsb + 1)`; a read of the whole of it (no footprint) is width 0,
// which is also what a named event carries, having no bits at all. A leaf with
// no observation decides by being reached -- an implicit sensitivity, or an
// unqualified named-event wait (LRM 15.5.1).
auto BuildTriggerExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    const StructuralScopeLowerer& lowerer, const hir::SensitivityEntry& entry,
    std::optional<mir::LocalId> observation) -> mir::ExprId {
  // The leaf watches either an intra-unit cell reached through its route, or a
  // package variable's one program-global cell reached by name (LRM 26.2). Both
  // resolve to a borrowed pointer to the place the runtime registers the wait
  // on; only the way that place is reached differs.
  const mir::ExprId observable_ptr = std::visit(
      Overloaded{
          [&](const hir::ReferenceRoute& route) -> mir::ExprId {
            return EndpointObservablePtr(
                block, frame, unit, BindEndpoint(lowerer, frame, route));
          },
          [&](const hir::ExternalUnitValueRef& pkg) -> mir::ExprId {
            unit.AddExternalReferencedUnit(pkg.unit_name);
            const mir::TypeId cell_type = mir::ObservableCellOf(
                unit.types, lowerer.Owner().TranslateType(pkg.value_type));
            const mir::ExprId cell = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::ExternalUnitVariableRef{
                            .unit_name = pkg.unit_name,
                            .variable_name = pkg.variable_name},
                    .type = cell_type});
            const mir::TypeId ptr_type = unit.types.Intern(
                mir::Type{mir::PointerType{
                    .pointee = cell_type,
                    .ownership = mir::PointerOwnership::kBorrowed,
                    .mutability = mir::Mutability::kMutable}});
            return block.exprs.Add(mir::MakeAddressOfExpr(cell, ptr_type));
          },
      },
      entry.ref);
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
  std::vector<mir::ExprId> arguments{observable_ptr};
  if (observation.has_value()) {
    arguments.push_back(block.exprs.Add(
        mir::MakeLocalRefExpr(*observation, unit.builtins.observation)));
  }
  arguments.push_back(int_literal(lsb_bit_offset));
  arguments.push_back(int_literal(bit_width));
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = std::move(arguments)},
          .type = unit.builtins.trigger});
}

auto BuildWaitStmt(
    mir::Block& target_block, const StructuralScopeLowerer& lowerer,
    std::vector<mir::ExprId> triggers) -> mir::Stmt {
  auto& unit = lowerer.Owner().Unit();
  const mir::TypeId triggers_type =
      mir::MachineArrayOf(unit.types, unit.builtins.trigger, triggers.size());
  const mir::ExprId triggers_id = target_block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(triggers)},
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

}  // namespace

auto BuildValueChangeWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list) -> mir::Stmt {
  auto& unit = lowerer.Owner().Unit();

  std::vector<mir::ExprId> triggers;
  triggers.reserve(sensitivity_list.size());
  for (const auto& entry : sensitivity_list) {
    triggers.push_back(BuildTriggerExpr(
        target_block, frame, unit, lowerer, entry, std::nullopt));
  }
  return BuildWaitStmt(target_block, lowerer, std::move(triggers));
}

auto BuildEventControlWaitStmt(
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
  return BuildWaitStmt(target_block, lowerer, std::move(triggers));
}

}  // namespace lyra::lowering::hir_to_mir

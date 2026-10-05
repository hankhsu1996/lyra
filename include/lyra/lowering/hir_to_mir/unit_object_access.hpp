#pragma once

#include <cstdint>
#include <expected>
#include <span>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/external_scope_ref.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

// The storage behind a member another unit published, reached on the object
// `object` points at, of the scope class `scope_class` records: the field this
// unit's record of that class laid the member out at. The answer is a borrowed
// pointer to that storage.
auto ReadPublishedMember(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId object,
    hir::ExternalScopeClassId scope_class, hir::PublishedMemberId member)
    -> mir::ExprId;

// Descends one path element (LRM 23.6) from the object `object` points at,
// onto what its unit published under `names`: the member at the position that
// unit's signature gave it, then one index per select. An instance (LRM 25.3,
// 25.10) is reached as the pointer type its member holds; a generate block
// (LRM 27) is viewed as the class the element says it was published as.
auto StepThroughPublished(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId object,
    const hir::ExternalScopeRef& names, std::span<const std::uint32_t> selects)
    -> mir::ExprId;

// What a `disable` of a block or task another unit published terminates (LRM
// 9.6.2), on the object `object` points at, as a borrowed pointer.
auto ReachPublishedDisableTarget(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId object,
    const hir::ExternalDisableTargetLeaf& leaf) -> mir::ExprId;

// The rest of reaching the instance a virtual interface holds, given the handle
// already lowered into `steps`: the handle evaluated once, and yielded as
// `object_pointer` only when it holds an instance.
auto GuardHeldInterface(
    mir::CompilationUnit& unit, BlockBuilder& steps, mir::Expr handle,
    mir::TypeId object_pointer) -> mir::Expr;

// An interface instance reached through a virtual interface, as a pointer to
// its unit's object: the instance the handle holds, taken as the object of the
// unit the access was resolved against -- which the handle's own value does not
// carry -- then each step of the descent. Using a handle that holds null is a
// fatal run-time error (LRM 25.9), so the instance is guarded by that test, and
// the handle is evaluated once whatever the access does with the answer --
// which is why it is lowered here, into the steps that bind it, rather than
// handed in lowered.
template <ExprLowerer Lowerer>
auto HeldInterfaceObject(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::InterfaceInstanceAccessExpr& access)
    -> diag::Result<mir::ExprId> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::TypeId object_pointer = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = lowerer.Owner().UnitObjectType(access.scope_class),
          .ownership = mir::PointerOwnership::kBorrowed}});
  BlockBuilder guard(frame);
  auto handle_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(access.handle), guard.Frame());
  if (!handle_or) return std::unexpected(std::move(handle_or.error()));
  mir::Block& block = *frame.current_block;
  mir::ExprId reached = block.exprs.Add(
      GuardHeldInterface(unit, guard, *std::move(handle_or), object_pointer));
  for (const hir::ExternalStep& step : access.steps) {
    reached = StepThroughPublished(
        lowerer.Owner(), block, reached, step.names, step.selects);
  }
  return reached;
}

// The storage of a member reached through a virtual interface, as a pointer to
// it: the instance the access descends to, then the member at the position
// that instance's unit published.
template <ExprLowerer Lowerer>
auto HeldInterfaceMember(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::InterfaceMemberAccessExpr& access) -> diag::Result<mir::ExprId> {
  auto reached = HeldInterfaceObject(lowerer, frame, access.instance);
  if (!reached) return std::unexpected(std::move(reached.error()));
  return ReadPublishedMember(
      lowerer.Owner(), *frame.current_block, *reached, access.scope_class,
      access.member);
}

// An interface instance named as a value (LRM 25.9), given a pointer to its
// object: the value a virtual interface holds, built from that address -- which
// instance it is, and nothing about what reaching into it needs.
auto InterfaceValueOf(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object,
    mir::TypeId type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir

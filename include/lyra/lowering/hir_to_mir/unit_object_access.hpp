#pragma once

#include <expected>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/signature_member_step.hpp"
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
// `object` points at: the behavior of that unit's promise which answers with
// the member's storage, dispatched on the object. Which behavior it is is
// counted out of the order the promise published its members in, and the
// storage it answers with is the only thing this side learns about the object.
// The answer is a borrowed pointer to that storage.
auto ReadPublishedMember(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object,
    hir::PublishedMemberId member) -> mir::ExprId;

// Descends from the object `object` points at onto an instance its unit
// declared and published (LRM 25.3, 25.10): the member at the position that
// unit's signature gave it, then one index per coordinate the step names. The
// answer is the reached object, as a value of the pointer type the member
// holds.
auto StepThroughPublishedMember(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId object,
    const hir::SignatureMemberStep& step) -> mir::ExprId;

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
          .pointee = unit.types.Intern(
              mir::Type{mir::ExternalUnitObjectType{
                  .object = lowerer.Owner().TranslateExternalUnitObject(
                      access.object)}}),
          .ownership = mir::PointerOwnership::kBorrowed}});
  BlockBuilder guard(frame);
  auto handle_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(access.handle), guard.Frame());
  if (!handle_or) return std::unexpected(std::move(handle_or.error()));
  mir::Block& block = *frame.current_block;
  mir::ExprId reached = block.exprs.Add(
      GuardHeldInterface(unit, guard, *std::move(handle_or), object_pointer));
  for (const hir::SignatureMemberStep& step : access.steps) {
    reached = StepThroughPublishedMember(lowerer.Owner(), block, reached, step);
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
      lowerer.Owner().Unit(), *frame.current_block, *reached, access.member);
}

// An interface instance named as a value (LRM 25.9), given a pointer to its
// object: the value a virtual interface holds, built from that address -- which
// instance it is, and nothing about what reaching into it needs.
auto InterfaceValueOf(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object,
    mir::TypeId type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir

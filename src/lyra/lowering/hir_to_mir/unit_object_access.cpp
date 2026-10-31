#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"

#include <cstdint>
#include <span>
#include <utility>
#include <variant>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/published_scope_ref.hpp"
#include "lyra/hir/scope_class.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A borrowed pointer to an object of the scope class `scope_class` records.
auto ObjectPointerOf(UnitLowerer& unit_lowerer, hir::ScopeClassId scope_class)
    -> mir::TypeId {
  return unit_lowerer.Unit().types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit_lowerer.UnitObjectType(scope_class),
          .ownership = mir::PointerOwnership::kBorrowed}});
}

// The block of a loop generate a hierarchical name reaches, asked of
// `blocks`, the member holding the loop's blocks, by `position`, the machine
// integer the name's select evaluates to (LRM 23.6, 27.4): as a scope of the
// design hierarchy, which the caller views as the class it knows the block to
// be. Which block that is was settled by the object running the loop.
auto ReachLoopBlock(
    const UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId blocks,
    mir::ExprId position) -> mir::ExprId {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  return block.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kLoopBlocksAt,
                          .receiver = blocks},
                  .arguments = {position}},
          .type = unit.builtins.scope_ptr});
}

// The field of the scope class `scope_class` records, on the object `object`
// points at, at the place `place_of` reads out of where this unit's record of
// the class laid out what its scope published. The access names the class, so
// the object is reached as one of it: a set whose objects are of several kinds
// holds each as the scope every one of them is, and which kind this one is,
// is what the route stated by naming the class here.
template <typename PlaceOf>
auto AccessPublishedSlot(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId object,
    hir::ScopeClassId scope_class, PlaceOf place_of) -> mir::ExprId {
  const ScopeClassLayout& record = unit_lowerer.ScopeClassLayoutOf(scope_class);
  const mir::FieldId slot = place_of(record.published);
  const mir::ExprId of_class =
      ObjectAs(block, object, ObjectPointerOf(unit_lowerer, scope_class));
  return block.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(unit_lowerer.Unit(), block, of_class),
          mir::ClassFieldTarget{.owner = record.cls, .slot = slot},
          record.field_types[slot.value]));
}

// The same field, addressed.
template <typename PlaceOf>
auto AddressPublishedSlot(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId object,
    hir::ScopeClassId scope_class, PlaceOf place_of) -> mir::ExprId {
  const mir::ExprId access =
      AccessPublishedSlot(unit_lowerer, block, object, scope_class, place_of);
  return block.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = access},
          .type = unit_lowerer.Unit().types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = block.Get(access).type,
                  .ownership = mir::PointerOwnership::kBorrowed}})});
}

}  // namespace

auto ReadPublishedMember(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId object,
    hir::ScopeClassId scope_class, hir::PublishedMemberId member)
    -> mir::ExprId {
  return AddressPublishedSlot(
      unit_lowerer, block, object, scope_class,
      [&](const PublishedScopeLayout& layout) {
        return layout.members.Get(member);
      });
}

auto ReachPublishedDisableTarget(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId object,
    const hir::PublishedDisableTargetLeaf& leaf) -> mir::ExprId {
  return AddressPublishedSlot(
      unit_lowerer, block, object, leaf.scope_class,
      [&](const PublishedScopeLayout& layout) {
        return layout.disable_targets.Get(leaf.target);
      });
}

auto StepThroughPublished(
    UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId object,
    const hir::PublishedScopeRef& names, std::span<const std::uint32_t> selects,
    const SelectEvaluator& evaluate_select) -> diag::Result<mir::ExprId> {
  const auto selected = [&](mir::ExprId held) {
    return ApplyInstanceSelects(
               unit_lowerer, block,
               ReachedObject{.expr = held, .type = block.Get(held).type},
               selects)
        .expr;
  };
  return std::visit(
      Overloaded{
          [&](const hir::PublishedMemberRef& member)
              -> diag::Result<mir::ExprId> {
            return selected(AccessPublishedSlot(
                unit_lowerer, block, object, member.scope_class,
                [&](const PublishedScopeLayout& layout) {
                  return layout.members.Get(member.member);
                }));
          },
          // A loop's block is asked of the loop's member by the value the
          // select evaluates to, since which block that is was settled by the
          // object running the loop, and is viewed as the class it was
          // published as.
          [&](const hir::PublishedGenerateLoopRef& loop)
              -> diag::Result<mir::ExprId> {
            const mir::ExprId blocks = AccessPublishedSlot(
                unit_lowerer, block, object, loop.scope_class,
                [&](const PublishedScopeLayout& layout) {
                  return layout.loops.Get(loop.loop);
                });
            auto position = evaluate_select(loop.position);
            if (!position) return std::unexpected(std::move(position.error()));
            const mir::ExprId reached =
                ReachLoopBlock(unit_lowerer, block, blocks, *position);
            return ObjectAs(
                block, reached,
                ObjectPointerOf(unit_lowerer, loop.result_class));
          },
          // What a construct building at most one block built holds the base
          // every block of it extends, so the block reached is viewed as the
          // class it was published as.
          [&](const hir::PublishedGenerateBlockRef& chosen)
              -> diag::Result<mir::ExprId> {
            const mir::ExprId held = AccessPublishedSlot(
                unit_lowerer, block, object, chosen.scope_class,
                [&](const PublishedScopeLayout& layout) {
                  return layout.choices.Get(chosen.choice);
                });
            return ObjectAs(
                block, held,
                ObjectPointerOf(unit_lowerer, chosen.result_class));
          }},
      names);
}

auto InterfaceValueOf(
    mir::CompilationUnit& unit, mir::Builder block, mir::ExprId object,
    mir::TypeId type) -> mir::Expr {
  const mir::ExprId address = block.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = block.AsValue(unit.types, object)},
          .type = mir::ErasedPointer(unit.types)});
  return mir::Expr{
      .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {address}},
      .type = type};
}

auto ReachHeldInterface(
    mir::CompilationUnit& unit, BlockBuilder& steps, mir::Expr handle,
    mir::TypeId object_pointer) -> mir::Expr {
  mir::Builder body = steps.Body();
  // The handle is tested and then reached through, and the source wrote it
  // once.
  const mir::ExprId held =
      EvaluatedOnce(steps.Frame(), body.Add(std::move(handle)));
  const mir::ExprId present = body.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = held},
          .type = unit.builtins.machine_bool});
  mir::AppendRunTimeCheck(
      unit, body, present,
      base::DiagMessage("use of a null virtual interface").TakeText());
  // The handle is a value holding the instance's address, so reaching the
  // instance reads that address out and states which unit's object it is.
  const mir::ExprId address = body.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kChandlePtr,
                          .receiver = held},
                  .arguments = {}},
          .type = mir::ErasedPointer(unit.types)});
  return steps.Build(body.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = address}, .type = object_pointer}));
}

}  // namespace lyra::lowering::hir_to_mir

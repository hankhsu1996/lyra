#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"

#include <cstdint>
#include <span>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/external_scope_class.hpp"
#include "lyra/hir/external_scope_ref.hpp"
#include "lyra/hir/published_member.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The field of the scope class `scope_class` records, on the object `object`
// points at, at the place `place_of` reads out of where this unit's record of
// the class laid out what its scope published.
template <typename PlaceOf>
auto AccessPublishedSlot(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId object,
    hir::ExternalScopeClassId scope_class, PlaceOf place_of) -> mir::ExprId {
  const ExternalScopeLayout& record =
      unit_lowerer.ExternalScopeLayoutOf(scope_class);
  const mir::FieldId slot = place_of(record.published);
  return block.exprs.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(unit_lowerer.Unit(), block, object),
          mir::CrossUnitClassFieldTarget{
              .unit_name = record.cls.unit_name,
              .class_name = record.cls.class_name,
              .slot = slot},
          record.field_types[slot.value]));
}

// The same field, addressed.
template <typename PlaceOf>
auto AddressPublishedSlot(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId object,
    hir::ExternalScopeClassId scope_class, PlaceOf place_of) -> mir::ExprId {
  const mir::ExprId access =
      AccessPublishedSlot(unit_lowerer, block, object, scope_class, place_of);
  return block.exprs.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = access},
          .type = unit_lowerer.Unit().types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = block.exprs.Get(access).type,
                  .ownership = mir::PointerOwnership::kBorrowed}})});
}

}  // namespace

auto ReadPublishedMember(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId object,
    hir::ExternalScopeClassId scope_class, hir::PublishedMemberId member)
    -> mir::ExprId {
  return AddressPublishedSlot(
      unit_lowerer, block, object, scope_class,
      [&](const PublishedScopeLayout& layout) {
        return layout.members.Get(member);
      });
}

auto ReachPublishedDisableTarget(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId object,
    const hir::ExternalDisableTargetLeaf& leaf) -> mir::ExprId {
  return AddressPublishedSlot(
      unit_lowerer, block, object, leaf.scope_class,
      [&](const PublishedScopeLayout& layout) {
        return layout.disable_targets.Get(leaf.target);
      });
}

auto StepThroughPublished(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId object,
    const hir::ExternalScopeRef& names, std::span<const std::uint32_t> selects)
    -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const auto selected = [&](mir::ExprId held) {
    return ApplyInstanceSelects(
               unit_lowerer, block,
               ReachedObject{.expr = held, .type = block.exprs.Get(held).type},
               selects)
        .expr;
  };
  return std::visit(
      Overloaded{
          // A member's objects are held by their class where they are all of
          // one, and by the scope every one of them is where something written
          // elsewhere made them differ; the object the selects picked out is
          // then viewed as the class of its position. Selects that leave a
          // dimension open reach several objects and pick none out.
          [&](const hir::ExternalMemberRef& member) {
            const mir::ExprId reached = selected(AccessPublishedSlot(
                unit_lowerer, block, object, member.scope_class,
                [&](const PublishedScopeLayout& layout) {
                  return layout.members.Get(member.member);
                }));
            const mir::TypeId held = block.exprs.Get(reached).type;
            const mir::TypeId viewed = unit.types.Intern(
                mir::Type{mir::PointerType{
                    .pointee = unit_lowerer.UnitObjectType(member.result_class),
                    .ownership = mir::PointerOwnership::kBorrowed}});
            if (held == viewed ||
                unit.types.Get(held).As<mir::PointerType>() == nullptr) {
              return reached;
            }
            return block.exprs.Add(
                mir::Expr{
                    .data = mir::CastExpr{.operand = reached}, .type = viewed});
          },
          // What a generate construct built holds the base every block of it
          // extends, so the block reached is viewed as the class it was
          // published as.
          [&](const hir::ExternalGenerateRef& generate) {
            const mir::ExprId held = AccessPublishedSlot(
                unit_lowerer, block, object, generate.scope_class,
                [&](const PublishedScopeLayout& layout) {
                  return layout.generates.Get(generate.generate);
                });
            return block.exprs.Add(
                mir::Expr{
                    .data = mir::CastExpr{.operand = selected(held)},
                    .type = unit.types.Intern(
                        mir::Type{mir::PointerType{
                            .pointee = unit_lowerer.UnitObjectType(
                                generate.result_class),
                            .ownership = mir::PointerOwnership::kBorrowed}})});
          }},
      names);
}

auto InterfaceValueOf(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object,
    mir::TypeId type) -> mir::Expr {
  const mir::ExprId address = block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = object},
          .type = mir::ErasedPointer(unit.types)});
  return mir::Expr{
      .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {address}},
      .type = type};
}

auto GuardHeldInterface(
    mir::CompilationUnit& unit, BlockBuilder& steps, mir::Expr handle,
    mir::TypeId object_pointer) -> mir::Expr {
  mir::Block& body = steps.Body();
  const mir::TypeId type = handle.type;
  // The handle is tested and then reached through, and the source wrote it
  // once.
  const mir::ExprId held =
      EvaluatedOnce(steps.Frame(), body.exprs.Add(std::move(handle)));
  const mir::ExprId present = body.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = held},
          .type = unit.builtins.machine_bool});
  const mir::ExprId test = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kFromBool},
                  .arguments = {present}},
          .type = unit.builtins.bit1});
  const mir::ExprId message = body.exprs.Add(
      mir::MakeStringLiteral(
          unit.builtins.string, "use of a null virtual interface (LRM 25.9)"));
  const mir::ExprId guarded = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kRequire,
                          .receiver = held},
                  .arguments = {test, message}},
          .type = type});
  // The handle is a value holding the instance's address, so reaching the
  // instance reads that address out and states which unit's object it is.
  const mir::ExprId address = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kChandlePtr,
                          .receiver = guarded},
                  .arguments = {}},
          .type = mir::ErasedPointer(unit.types)});
  return steps.Build(body.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = address}, .type = object_pointer}));
}

}  // namespace lyra::lowering::hir_to_mir

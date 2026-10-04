#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"

#include <utility>

#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/behavior_ordinal.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto ReadPublishedMember(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object,
    hir::PublishedMemberId member) -> mir::ExprId {
  const mir::TypeId pointee = unit.types.Get(block.exprs.Get(object).type)
                                  .Get<mir::PointerType>()
                                  .pointee;
  const mir::ExternalUnitObject& promised = unit.external_unit_objects.Get(
      unit.types.Get(pointee).Get<mir::ExternalUnitObjectType>().object);
  const mir::FieldId slot = UnitLowerer::TranslatePublishedMember(member);
  const mir::PromisedField& field = promised.fields.Get(slot);
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Virtual{
                          .receiver = BuildObjectDeref(unit, block, object),
                          .slot =
                              mir::ExternalVirtualSlot{
                                  .unit_name = promised.unit_name,
                                  .class_name = promised.class_name,
                                  .ordinal = mir::BehaviorOrdinal{slot.value}}},
                  .arguments = {}},
          .type = unit.types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = field.type,
                  .ownership = mir::PointerOwnership::kBorrowed}})});
}

auto StepThroughPublishedMember(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId object,
    const hir::SignatureMemberStep& step) -> mir::ExprId {
  const mir::ExprId storage =
      ReadPublishedMember(unit_lowerer.Unit(), block, object, step.member);
  const mir::TypeId reached = unit_lowerer.Unit()
                                  .types.Get(block.exprs.Get(storage).type)
                                  .Get<mir::PointerType>()
                                  .pointee;
  return IndexCoordinates(
             unit_lowerer, block,
             ReachedObject{
                 .expr = block.exprs.Add(
                     mir::Expr{
                         .data = mir::DerefExpr{.pointer = storage},
                         .type = reached}),
                 .type = reached},
             step.indices)
      .expr;
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

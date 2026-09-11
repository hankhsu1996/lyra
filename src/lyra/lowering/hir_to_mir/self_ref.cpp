#include "lyra/lowering/hir_to_mir/self_ref.hpp"

#include <cstdint>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto MakeSelfRefExpr(const WalkFrame& frame, mir::TypeId self_ptr_type)
    -> mir::Expr {
  // The receiver is an ordinary binding: resolve it as the body-local carrier
  // for the receiver origin (a parameter in a directly-invoked body, a captured
  // field in a closure), forwarded across closure boundaries by the resolver,
  // then read through the uniform binding-read path. The read is typed as the
  // enclosing object's borrowed pointer.
  const BodyBindingRef self =
      frame.bindings->EnsureCarrier(BindingOriginId::Receiver());
  mir::Expr read = frame.bindings->MakeReadExpr(self, *frame.current_block);
  read.type = self_ptr_type;
  return read;
}

auto BuildEnclosingScopeReceiver(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  // Where the structural scope this body counts from sits: the body itself is
  // that scope, or it was handed the instance its class belongs to.
  mir::ExprId nav = std::visit(
      Overloaded{
          [&](const ScopeIsSelf&) {
            return block.exprs.Add(
                MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
          },
          [&](const ScopeThroughMember& through) {
            const mir::ExprId self = block.exprs.Add(
                MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
            return block.exprs.Add(
                mir::MakeFieldAccessExpr(
                    self,
                    mir::ClassFieldTarget{
                        .owner = frame.current_class_id,
                        .slot = through.member},
                    frame.EnclosingClassAtHops(mir::EnclosingHops{0})
                        .cls->self_pointer_type));
          },
          [&](const ScopeThroughParameter& through) {
            return block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::ReferenceExpr{
                            .target = mir::LocalRef{.var = through.local}},
                    .type = frame.EnclosingClassAtHops(mir::EnclosingHops{0})
                                .cls->self_pointer_type});
          }},
      frame.structural_base);
  if (hops.value == 0) {
    return nav;
  }
  for (std::uint32_t step = 0; step < hops.value; ++step) {
    nav = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kParent,
                            .receiver = nav},
                    .arguments = {}},
            .type = unit.builtins.scope_ptr});
  }
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = nav},
          .type = frame.EnclosingClassAtHops(hops).cls->self_pointer_type});
}

auto BuildStructuralFieldAccessExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops, mir::FieldId var) -> mir::Expr {
  const EnclosingClass owner = frame.EnclosingClassAtHops(hops);
  const mir::TypeId field_type = owner.cls->fields.Get(var).type;
  const mir::ExprId receiver = BuildEnclosingScopeReceiver(frame, unit, hops);
  return mir::MakeFieldAccessExpr(
      receiver, mir::ClassFieldTarget{.owner = owner.id, .slot = var},
      field_type);
}

auto BuildReferenceArg(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId cell,
    mir::TypeId pointee) -> mir::ExprId {
  const auto& pointee_ty = unit.types.Get(pointee);
  // Referencing a value that is itself a reference seals to the same final
  // cell: a `Ref<T>` taken over a `Ref<T>` aliases what that reference aliases,
  // never nesting into `Ref<Ref<T>>` (LRM 23.3.3.2). The argument is the
  // existing reference, copied to share its pointee.
  if (pointee_ty.Is<mir::RefType>()) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{.callee = mir::Construct{}, .arguments = {cell}},
            .type = pointee});
  }
  // The reference aliases the cell's value, not its storage wrapper: a `Ref<T>`
  // over an observable cell binds the underlying `Var<T>`, so the pointee is
  // the value type, not the `ObservableType`.
  if (pointee_ty.Is<mir::ObservableType>()) {
    pointee = pointee_ty.Get<mir::ObservableType>().value;
  }
  const mir::TypeId ref_type = unit.types.Intern(
      mir::Type{mir::RefType{
          .pointee = pointee, .mutability = mir::Mutability::kMutable}});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{.callee = mir::Construct{}, .arguments = {cell}},
          .type = ref_type});
}

auto BindReferenceSlot(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId ref_lvalue,
    mir::ExprId source_cell) -> mir::ExprId {
  const mir::TypeId ref_type = block.exprs.Get(ref_lvalue).type;
  const mir::ExprId ref_value = BuildReferenceArg(
      unit, block, source_cell, block.exprs.Get(source_cell).type);
  return block.exprs.Add(
      mir::Expr{
          .data = mir::AssignExpr{.target = ref_lvalue, .value = ref_value},
          .type = ref_type});
}

}  // namespace lyra::lowering::hir_to_mir

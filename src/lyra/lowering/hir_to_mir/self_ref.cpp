#include "lyra/lowering/hir_to_mir/self_ref.hpp"

#include <cstdint>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/object_lifetime.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto MakeSelfRefExpr(const WalkFrame& frame) -> mir::Expr {
  // The receiver is an ordinary binding: resolve it as the body-local carrier
  // for the receiver origin -- a parameter in a directly-invoked body; in a
  // closure, a captured field, or, for an object that lives while referenced,
  // a local holding the address taken from the share of it the closure
  // captures -- then named through the uniform binding path, which types it
  // as the binding was declared: the enclosing object's borrowed pointer.
  const BodyBindingRef self = frame.bindings->EnsureCarrier(ReceiverOrigin{});
  return frame.bindings->MakeBindingPlace(self, *frame.current_block);
}

auto BuildObjectDeref(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId reaches)
    -> mir::ExprId {
  // `*&x` is `x`: an object whose address was just taken to reach it is that
  // object, so the round trip is not stated.
  if (const auto* address =
          std::get_if<mir::AddressOfExpr>(&block.exprs.Get(reaches).data)) {
    return address->operand;
  }
  const mir::TypeId object =
      mir::ObjectReachedThrough(unit.types, block.exprs.Get(reaches).type);
  return block.Deref(unit.types, reaches, object);
}

auto BuildObjectPointer(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId reaches)
    -> mir::ExprId {
  const mir::ExprId object = BuildObjectDeref(unit, block, reaches);
  return block.exprs.Add(
      mir::MakeAddressOfExpr(
          object, unit.types.Intern(
                      mir::Type{mir::PointerType{
                          .pointee = block.exprs.Get(object).type,
                          .ownership = mir::PointerOwnership::kBorrowed,
                          .mutability = mir::Mutability::kMutable}})));
}

auto ObjectAs(mir::Block& block, mir::ExprId object, mir::TypeId pointer)
    -> mir::ExprId {
  // A pointer named where it is held is read: what is cast is its value.
  if (mir::IsPlace(block.exprs.Get(object).data)) {
    object = block.Read(object);
  }
  if (block.exprs.Get(object).type == pointer) return object;
  return block.exprs.Add(
      mir::Expr{.data = mir::CastExpr{.operand = object}, .type = pointer});
}

namespace {

// Declares a share of the object the body runs on, made where the body begins,
// which keeps the object alive for as long as the body runs. Whoever called a
// method may let go of the object before the method is done -- a task can
// wait, and a function can drop the last handle itself -- yet the object stays
// while a body runs on it (LRM 8.29). A constructor's object is held by the
// process creating it from its first moment (LRM 8.29), so the share is one a
// constructor can take too.
void HoldReceiverShare(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::LocalId receiver, mir::TypeId self_pointer_type) {
  mir::Block& body = *frame.current_block;
  const mir::TypeId share_type = unit.types.Intern(
      mir::Type{mir::ManagedRefType{
          .pointee =
              mir::ObjectReachedThrough(unit.types, self_pointer_type)}});
  const mir::LocalId share =
      frame.bindings->Declare(ReceiverShareOrigin{}, share_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = share,
          .init = body.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target = support::BuiltinFn::kSelfHandle},
                          .arguments = {body.ReadLocal(
                              receiver, self_pointer_type)}},
                  .type = share_type})});
}

}  // namespace

auto BindImplicitParameters(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const ClassShape& owner, CallableForm form) -> BoundImplicitParameters {
  struct Takes {
    bool object;
    bool instance;
  };
  const Takes takes = [&] {
    switch (form) {
      case CallableForm::kInstanceMember:
        return Takes{.object = true, .instance = false};
      case CallableForm::kTypeAssociated:
        return Takes{.object = false, .instance = true};
      case CallableForm::kConstructor:
        return Takes{.object = true, .instance = true};
    }
    throw InternalError("BindImplicitParameters: unknown CallableForm");
  }();
  CallableBindings& bindings = *frame.bindings;
  BoundImplicitParameters bound{.params = {}, .frame = frame};
  if (takes.object) {
    const mir::LocalId receiver =
        bindings.Declare(ReceiverOrigin{}, owner.self_pointer_type);
    bound.params.push_back(receiver);
    const mir::ObjectLifetime lifetime = mir::LifetimeOfLineage(
        mir::ClassRef{frame.current_class.id},
        [&](mir::ClassId cls) { return unit_lowerer.BaseOf(cls); });
    switch (lifetime) {
      case mir::ObjectLifetime::kTheRun:
        break;
      case mir::ObjectLifetime::kWhileReferenced:
        HoldReceiverShare(
            unit_lowerer.Unit(), frame, receiver, owner.self_pointer_type);
        break;
    }
  }
  if (takes.instance && owner.declaring_instance.has_value()) {
    bound.params.push_back(bindings.Declare(
        DeclaringInstanceOrigin{}, owner.declaring_instance->type));
    bound.frame = bound.frame.WithStructuralBase(ScopeThroughParameter{});
  }
  return bound;
}

auto BuildEnclosingScopeReceiver(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  // Where the structural scope this body counts from sits: the body itself is
  // that scope, or it was handed the instance its class belongs to.
  mir::ExprId nav = std::visit(
      Overloaded{
          [](const NoScope&) -> mir::ExprId {
            throw InternalError(
                "BuildEnclosingScopeReceiver: a body of a namespace unit "
                "belongs to no instance, so nothing it names is reached "
                "through one");
          },
          [&](const ScopeIsSelf&) {
            return block.Read(block.exprs.Add(MakeSelfRefExpr(frame)));
          },
          // The instance is held as the base every scope extends, and the
          // body viewing it is one of that scope's unit, which views it as the
          // scope's own class.
          [&](const ScopeThroughMember& through) {
            const mir::ExprId self = block.exprs.Add(MakeSelfRefExpr(frame));
            return block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CastExpr{
                            .operand = block.Read(block.exprs.Add(
                                mir::MakeFieldAccessExpr(
                                    BuildObjectDeref(unit, block, self),
                                    mir::ClassFieldTarget{
                                        .owner = frame.current_class.id,
                                        .slot = through.member},
                                    unit.builtins.scope_ptr)))},
                    .type = frame.EnclosingClassAtHops(mir::EnclosingHops{0})
                                .cls->self_pointer_type});
          },
          [&](const ScopeThroughParameter&) {
            const BodyBindingRef instance =
                frame.bindings->EnsureCarrier(DeclaringInstanceOrigin{});
            return block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CastExpr{
                            .operand = block.Read(block.exprs.Add(
                                frame.bindings->MakeBindingPlace(
                                    instance, block)))},
                    .type = frame.EnclosingClassAtHops(mir::EnclosingHops{0})
                                .cls->self_pointer_type});
          }},
      frame.structural_base);
  for (std::uint32_t step = 0; step < hops.value; ++step) {
    const EnclosingClass at =
        frame.EnclosingClassAtHops(mir::EnclosingHops{step});
    if (!at.enclosing.has_value()) {
      throw InternalError(
          "BuildEnclosingScopeReceiver: a reference climbs out of a scope no "
          "scope of its unit encloses -- please report this as a bug");
    }
    nav = block.Read(block.exprs.Add(
        mir::MakeFieldAccessExpr(
            BuildObjectDeref(unit, block, nav),
            mir::ClassFieldTarget{.owner = at.id, .slot = *at.enclosing},
            at.cls->fields.Get(*at.enclosing).type)));
  }
  return nav;
}

auto BuildStructuralFieldAccessExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops, mir::FieldId var) -> mir::Expr {
  const EnclosingClass owner = frame.EnclosingClassAtHops(hops);
  return BuildStructuralFieldAccessExpr(
      frame, unit, hops, mir::ClassFieldTarget{.owner = owner.id, .slot = var},
      owner.cls->fields.Get(var).type);
}

auto BuildStructuralFieldAccessExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops, const mir::ClassFieldTarget& field,
    mir::TypeId field_type) -> mir::Expr {
  const mir::ExprId receiver = BuildObjectDeref(
      unit, *frame.current_block,
      BuildEnclosingScopeReceiver(frame, unit, hops));
  return mir::MakeFieldAccessExpr(receiver, field, field_type);
}

auto BuildReferenceArg(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId cell,
    mir::TypeId pointee) -> mir::ExprId {
  const auto& pointee_ty = unit.types.Get(pointee);
  // Referencing a value that is itself a reference seals to the same final
  // cell: a `VariableRef<T>` taken over a `VariableRef<T>` aliases what that
  // reference aliases, never nesting into `VariableRef<VariableRef<T>>`
  // (LRM 23.3.3.2). The argument is the existing reference itself, as a C++
  // reference initialized from a reference binds what that one binds.
  // A reference held as a member is lent the same way, itself, so a `ref`
  // port bound from it is bound to it rather than to what it names now.
  if (pointee_ty.IsVariableRef()) {
    return block.AsValue(unit.types, cell);
  }
  // The reference aliases the cell's value, not its storage wrapper: a
  // `VariableRef<T>` over an observable cell binds the underlying `Var<T>`, so
  // the pointee is the value type, not the `ObservableType`.
  if (pointee_ty.Is<mir::ObservableType>()) {
    pointee = pointee_ty.Get<mir::ObservableType>().value;
  }
  const mir::TypeId ref_type = unit.types.Intern(
      mir::Type{mir::VariableRefType{
          .pointee = pointee, .mutability = mir::Mutability::kMutable}});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{.callee = mir::Construct{}, .arguments = {cell}},
          .type = ref_type});
}

auto BuildThisHandle(const WalkFrame& frame) -> mir::Expr {
  const BodyBindingRef share =
      frame.bindings->EnsureCarrier(ReceiverShareOrigin{});
  return mir::MakeCopyExpr(
      frame.current_block->exprs.Add(
          frame.bindings->MakeBindingPlace(share, *frame.current_block)),
      frame.bindings->TypeOf(share));
}

}  // namespace lyra::lowering::hir_to_mir

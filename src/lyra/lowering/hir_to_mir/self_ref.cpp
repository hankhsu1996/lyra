#include "lyra/lowering/hir_to_mir/self_ref.hpp"

#include <cstdint>
#include <variant>

#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto MakeSelfRefExpr(const WalkFrame& frame, mir::TypeId self_ptr_type)
    -> mir::Expr {
  return mir::MakeLocalRefExpr(
      frame.block_depth - frame.self_decl_depth, *frame.self_binding,
      self_ptr_type);
}

auto BuildEnclosingScopeReceiver(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops) -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  mir::ExprId nav = block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  if (hops.value == 0) {
    return nav;
  }
  for (std::uint32_t step = 0; step < hops.value; ++step) {
    nav = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kParent},
                    .arguments = {nav}},
            .type = unit.builtins.scope_ptr});
  }
  return block.exprs.Add(
      mir::Expr{
          .data = mir::PointerCastExpr{.operand = nav},
          .type = frame.EnclosingClassAtHops(hops).self_pointer_type});
}

auto BuildStructuralMemberAccessExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops, mir::MemberId var) -> mir::Expr {
  const mir::TypeId field_type =
      frame.EnclosingClassAtHops(hops).members.Get(var).type;
  const mir::ExprId receiver = BuildEnclosingScopeReceiver(frame, unit, hops);
  return mir::MakeMemberAccessExpr(
      receiver, mir::MemberRef{.var = var}, field_type);
}

auto BuildStructuralParamAccessExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops, mir::ParamId param) -> mir::Expr {
  const mir::TypeId field_type =
      frame.EnclosingClassAtHops(hops).params.Get(param).type;
  const mir::ExprId receiver = BuildEnclosingScopeReceiver(frame, unit, hops);
  return mir::Expr{
      .data = mir::ParamRef{.receiver = receiver, .param = param},
      .type = field_type};
}

auto BuildReferenceArg(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId cell,
    mir::TypeId pointee) -> mir::ExprId {
  const auto& pointee_ty = unit.types.Get(pointee);
  // Referencing a value that is itself a reference seals to the same final
  // cell: a `Ref<T>` taken over a `Ref<T>` aliases what that reference aliases,
  // never nesting into `Ref<Ref<T>>` (LRM 23.3.3.2). The argument is the
  // existing reference, copied to share its pointee.
  if (std::holds_alternative<mir::RefType>(pointee_ty.data)) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{.callee = mir::Construct{}, .arguments = {cell}},
            .type = pointee});
  }
  // The reference aliases the cell's value, not its storage wrapper: a `Ref<T>`
  // over an observable cell binds the underlying `Var<T>`, so the pointee is
  // the value type, not the `ObservableType`.
  if (std::holds_alternative<mir::ObservableType>(pointee_ty.data)) {
    pointee = std::get<mir::ObservableType>(pointee_ty.data).value;
  }
  const mir::TypeId ref_type =
      unit.types.Intern(mir::RefType{.pointee = pointee, .is_const = false});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{.callee = mir::Construct{}, .arguments = {cell}},
          .type = ref_type});
}

}  // namespace lyra::lowering::hir_to_mir

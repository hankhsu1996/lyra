#include "lyra/lowering/hir_to_mir/self_ref.hpp"

#include <variant>

#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto MakeSelfRefExpr(const WalkFrame& frame, mir::TypeId self_ptr_type)
    -> mir::Expr {
  return mir::MakeLocalRefExpr(
      frame.block_depth - frame.self_decl_depth, *frame.self_binding,
      self_ptr_type);
}

auto BuildStructuralMemberAccessExpr(
    const WalkFrame& frame, const mir::MemberRef& member) -> mir::Expr {
  const mir::TypeId field_type =
      frame.EnclosingClassAtHops(member.hops).members.Get(member.var).type;
  const mir::ExprId receiver = frame.current_block->exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  return mir::MakeMemberAccessExpr(receiver, member, field_type);
}

auto BuildReferenceArg(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId cell,
    mir::TypeId pointee) -> mir::ExprId {
  const auto& pointee_ty = unit.GetType(pointee);
  // Referencing a value that is itself a reference seals to the same final
  // cell: a `Ref<T>` taken over a `Ref<T>` aliases what that reference aliases,
  // never nesting into `Ref<Ref<T>>` (LRM 23.3.3.2). The argument is the
  // existing reference, copied to share its pointee.
  if (std::holds_alternative<mir::RefType>(pointee_ty.data)) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::ConstructorCallee{}, .arguments = {cell}},
            .type = pointee});
  }
  // The reference aliases the cell's value, not its storage wrapper: a `Ref<T>`
  // over an observable cell binds the underlying `Var<T>`, so the pointee is
  // the value type, not the `ObservableType`.
  if (std::holds_alternative<mir::ObservableType>(pointee_ty.data)) {
    pointee = std::get<mir::ObservableType>(pointee_ty.data).value;
  }
  const mir::TypeId ref_type = unit.AddType(
      mir::TypeData{mir::RefType{.pointee = pointee, .is_const = false}});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::ConstructorCallee{}, .arguments = {cell}},
          .type = ref_type});
}

}  // namespace lyra::lowering::hir_to_mir

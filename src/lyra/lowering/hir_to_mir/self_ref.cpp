#include "lyra/lowering/hir_to_mir/self_ref.hpp"

#include <variant>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildSelfRefExpr(const WalkFrame& frame, mir::TypeId self_ptr_type)
    -> mir::Expr {
  return mir::MakeProceduralVarRefExpr(
      frame.procedural_depth - frame.self_decl_depth, *frame.self_binding,
      self_ptr_type);
}

auto BuildStructuralMemberAccessExpr(
    const WalkFrame& frame, const mir::StructuralVarRef& member) -> mir::Expr {
  const mir::TypeId field_type = frame.StructuralScopeAtHops(member.hops)
                                     .GetStructuralVar(member.var)
                                     .type;
  const mir::ExprId receiver = frame.current_procedural_scope->AddExpr(
      BuildSelfRefExpr(
          frame, frame.current_structural_scope->self_pointer_type));
  return mir::MakeMemberAccessExpr(receiver, member, field_type);
}

auto BuildReferenceArg(
    mir::CompilationUnit& unit, mir::ProceduralScope& scope, mir::ExprId cell,
    mir::TypeId pointee) -> mir::ExprId {
  // The reference aliases the cell's value, not its storage wrapper: a `Ref<T>`
  // over an observable cell binds the underlying `Var<T>`, so the pointee is
  // the value type, not the `ObservableType`.
  const auto& pointee_ty = unit.GetType(pointee);
  if (std::holds_alternative<mir::ObservableType>(pointee_ty.data)) {
    pointee = std::get<mir::ObservableType>(pointee_ty.data).value;
  }
  const mir::TypeId ref_type = unit.AddType(
      mir::TypeData{mir::RefType{.pointee = pointee, .is_const = false}});
  return scope.AddExpr(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::ConstructorCallee{}, .arguments = {cell}},
          .type = ref_type});
}

}  // namespace lyra::lowering::hir_to_mir

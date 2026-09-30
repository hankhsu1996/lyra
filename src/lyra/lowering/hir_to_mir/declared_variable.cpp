#include "lyra/lowering/hir_to_mir/declared_variable.hpp"

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto DeclareVariable(
    mir::CompilationUnit& unit, CallableBindings& bindings, mir::Block& block,
    BindingOriginId origin, const std::optional<std::string>& name,
    mir::TypeId value_type, bool body_can_wait) -> DeclaredVariable {
  if (!body_can_wait) {
    return DeclaredVariable{
        .local = bindings.DeclareProcedural(origin, name, value_type),
        .type = value_type};
  }
  const mir::TypeId cell_type = mir::ObservableCellOf(unit.types, value_type);
  const mir::LocalId local =
      bindings.DeclareProcedural(origin, name, cell_type);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = local,
          .init = block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee = mir::Construct{}, .arguments = {}},
                  .type = cell_type})});
  return DeclaredVariable{.local = local, .type = cell_type};
}

auto InitializeVariable(
    mir::CompilationUnit& unit, mir::Block& block,
    const DeclaredVariable& variable, mir::ExprId value) -> mir::Stmt {
  if (!unit.types.Get(variable.type).Is<mir::ObservableType>()) {
    return mir::Stmt{
        .label = std::nullopt,
        .data = mir::LocalDeclStmt{.target = variable.local, .init = value}};
  }
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeCapabilityInstallCallExpr(
                  block.exprs.Add(
                      mir::MakeLocalRefExpr(variable.local, variable.type)),
                  value, support::BuiltinFn::kInitialize,
                  unit.builtins.void_type))}};
}

auto ReadVariable(
    mir::CompilationUnit& unit, mir::Block& block,
    const DeclaredVariable& variable) -> mir::ExprId {
  const mir::ExprId local =
      block.exprs.Add(mir::MakeLocalRefExpr(variable.local, variable.type));
  const auto* cell = unit.types.Get(variable.type).As<mir::ObservableType>();
  if (cell == nullptr) {
    return local;
  }
  return block.exprs.Add(mir::MakeCellLoadCallExpr(local, cell->value));
}

}  // namespace lyra::lowering::hir_to_mir

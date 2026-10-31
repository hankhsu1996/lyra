#include "lyra/lowering/hir_to_mir/declared_variable.hpp"

#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto DeclareVariable(
    mir::CompilationUnit& unit, CallableBindings& bindings, mir::Builder block,
    BindingOriginId origin, const std::optional<std::string>& name,
    mir::TypeId value_type, bool runs_as_function_call) -> DeclaredVariable {
  const mir::TypeId held_as =
      runs_as_function_call ? value_type
                            : mir::ObservableCellOf(unit.types, value_type);
  const mir::LocalId local = bindings.DeclareProcedural(origin, name, held_as);
  if (!unit.types.Get(held_as).Is<mir::ObservableType>()) {
    return DeclaredVariable{.local = local, .type = held_as};
  }
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = local,
          .init = block.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee = mir::Construct{}, .arguments = {}},
                  .type = held_as})});
  return DeclaredVariable{.local = local, .type = held_as};
}

auto InitializeVariable(
    mir::CompilationUnit& unit, mir::Builder block,
    const DeclaredVariable& variable, mir::ExprId value) -> mir::Stmt {
  if (!unit.types.Get(variable.type).Is<mir::ObservableType>()) {
    return mir::Stmt{
        .label = std::nullopt,
        .data = mir::LocalDeclStmt{.target = variable.local, .init = value}};
  }
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.Add(
              mir::MakeCapabilityInstallCallExpr(
                  block.LocalPlace(variable.local, variable.type), value,
                  support::BuiltinFn::kInitialize, unit.builtins.void_type))}};
}

}  // namespace lyra::lowering::hir_to_mir

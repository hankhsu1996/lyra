#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"

#include <expected>
#include <optional>
#include <utility>

#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto IntegrateStaticInitializer(
    ProcessLowerer& process, const hir::ProceduralBody& body,
    const StorageBringUp& bring_up, const StaticVarBinding& binding)
    -> diag::Result<void> {
  const WalkFrame& install_frame = bring_up.install;
  const WalkFrame& value_frame = bring_up.value;
  auto& value_block = *value_frame.current_block;
  mir::CompilationUnit& unit = process.Owner().Unit();
  const hir::ProceduralVarDecl& decl = body.procedural_vars.Get(binding.var);
  const mir::TypeId storage_type = process.Owner().TranslateType(decl.type);

  // An observable cell installs its declared representation and default
  // contents once (LRM 10.5); a later user initializer stores through the cell,
  // which verifies the value against the installed representation. The
  // default-only case is fully expressed by that installation and needs no
  // store.
  if (unit.types.Get(binding.cell_type).IsCapabilityWrapper()) {
    auto& install_block = *install_frame.current_block;
    const mir::ExprId installed =
        install_block.exprs.Add(BuildStaticStorageAccess(
            unit, install_frame, binding.home, binding.cell_type,
            mir::EnclosingHops{}));
    const mir::ExprId prototype = install_block.exprs.Add(
        BuildDefaultValueFromHir(process.Owner(), install_block, decl.type));
    install_block.AppendStmt(
        mir::ExprStmt{
            .expr = install_block.exprs.Add(
                mir::MakeCapabilityInstallCallExpr(
                    installed, prototype, support::BuiltinFn::kInitialize,
                    unit.builtins.void_type))});
    if (!decl.init.has_value()) {
      return {};
    }
  }

  const mir::ExprId target = value_block.exprs.Add(BuildStaticStorageAccess(
      unit, value_frame, binding.home, binding.cell_type,
      mir::EnclosingHops{}));
  mir::ExprId init_value{};
  if (decl.init.has_value()) {
    auto init_or = process.LowerExpr(body.exprs.Get(*decl.init), value_frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_value = value_block.exprs.Add(*std::move(init_or));
  } else {
    init_value = value_block.exprs.Add(
        BuildDefaultValueFromHir(process.Owner(), value_block, decl.type));
  }

  const mir::Expr assign_expr = BuildStoreExpr(
      unit, value_block, WriteTarget{.owner = target, .descent = {}},
      init_value, std::nullopt, storage_type);
  value_block.AppendStmt(
      mir::ExprStmt{.expr = value_block.exprs.Add(assign_expr)});
  return {};
}

}  // namespace lyra::lowering::hir_to_mir

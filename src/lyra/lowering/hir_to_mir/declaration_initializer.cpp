#include "lyra/lowering/hir_to_mir/declaration_initializer.hpp"

#include <expected>
#include <optional>
#include <utility>

#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto IntegrateStaticInitializer(
    ProcessLowerer& process, const hir::ProceduralBody& body,
    const WalkFrame& init_frame, const StaticVarBinding& binding)
    -> diag::Result<void> {
  auto& init_block = *init_frame.current_block;
  const mir::CompilationUnit& unit = process.Owner().Unit();
  const hir::ProceduralVarDecl& decl = body.procedural_vars.Get(binding.var);
  const mir::TypeId storage_type = process.Owner().TranslateType(decl.type);

  const mir::ExprId target =
      init_block.exprs.Add(BuildStructuralFieldAccessExpr(
          init_frame, unit, mir::EnclosingHops{}, binding.field));
  const bool target_is_observable_cell =
      unit.types.Get(init_block.exprs.Get(target).type).IsCapabilityWrapper();

  // An observable cell installs its declared representation and default
  // contents once at construction (LRM 10.5); a later user initializer stores
  // through the cell, which verifies the value against the installed
  // representation. The default-only case is fully expressed by that
  // installation and needs no store.
  if (target_is_observable_cell) {
    const mir::ExprId prototype = init_block.exprs.Add(
        BuildDefaultValueFromHir(process.Owner(), init_frame, decl.type));
    init_block.AppendStmt(
        mir::ExprStmt{
            .expr = init_block.exprs.Add(
                mir::MakeCapabilityInitializeCallExpr(
                    target, prototype, unit.builtins.void_type))});
    if (!decl.init.has_value()) {
      return {};
    }
  }

  mir::ExprId init_value{};
  if (decl.init.has_value()) {
    auto init_or = process.LowerExpr(body.exprs.Get(*decl.init), init_frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_value = init_block.exprs.Add(*std::move(init_or));
  } else {
    init_value = init_block.exprs.Add(
        BuildDefaultValueFromHir(process.Owner(), init_frame, decl.type));
  }

  const mir::Expr assign_expr = BuildStoreExpr(
      unit, init_block, target, init_value, std::nullopt, storage_type);
  init_block.AppendStmt(
      mir::ExprStmt{.expr = init_block.exprs.Add(assign_expr)});
  return {};
}

}  // namespace lyra::lowering::hir_to_mir

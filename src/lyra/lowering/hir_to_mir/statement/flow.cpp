#include "lyra/lowering/hir_to_mir/statement/flow.hpp"

#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>

#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/procedural_depth.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_hops.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/structural_var.hpp"
#include "lyra/mir/value_ref.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 13.3.1: a static-lifetime body local has per-instance storage that
// outlives every activation of the body. Realize it as a structural var on
// the callable's owner scope; the init AssignExpr lands in that owner's
// constructor_scope (LRM Table 6-7 variable-initialization). The body
// declaration itself emits nothing. The mangled name carries
// `<callable>__<source>_<hir_var_id>` so sibling callables sharing a source
// name (`static int x;` in two processes of the same module) and nested
// blocks repeating an identifier do not collide on the owner's
// structural-var arena (`docs/decisions/variable-lifetime-storage.md`).
auto LowerStaticVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v, const hir::ProceduralVarDecl& hir_local,
    mir::TypeId type) -> diag::Result<mir::Stmt> {
  auto* owner_scope = frame.current_structural_scope;
  const auto& ctor_frame = process.OwnerCtorFrame();
  auto& ctor_scope = *ctor_frame.current_procedural_scope;
  const mir::TypeId self_ptr_type = owner_scope->self_pointer_type;

  const std::string mangled = std::format(
      "{}__{}_{}", process.CallableName(), hir_local.name, v.var.value);
  const mir::StructuralVarId static_var = owner_scope->AddStructuralVar(
      mir::StructuralVarDecl{.name = mangled, .type = type});
  process.MapProceduralVar(v.var, StaticVarBinding{.var = static_var});

  mir::ExprId init_value{};
  if (v.init.has_value()) {
    auto init_or = process.LowerExpr(
        process.HirBody().exprs.at(v.init->value), ctor_frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_value = ctor_scope.AddExpr(*std::move(init_or));
  } else {
    init_value = ctor_scope.AddExpr(
        BuildDefaultValueExpr(process.Module(), ctor_frame, type));
  }

  const mir::ExprId ctor_self_read =
      ctor_scope.AddExpr(BuildSelfRefExpr(ctor_frame, self_ptr_type));
  const mir::ExprId target = ctor_scope.AddExpr(
      mir::MakeMemberAccessExpr(
          ctor_self_read,
          mir::StructuralVarRef{
              .hops = mir::StructuralHops{.value = 0}, .var = static_var},
          type));
  // A static-lifetime local lives as a structural var on the owner; if its
  // declared type is an observable cell wrapper the init must route through
  // `Var<T>::Set` so subscribers fire on its initial value (LRM 13.3.1 +
  // `docs/decisions/value-type-concepts.md`).
  const mir::ExprId services_id = ctor_scope.AddExpr(
      mir::MakeServicesCallExpr(
          ctor_self_read, process.Module().Unit().builtins.services));
  const mir::Expr assign_expr = BuildObservableAssignExpr(
      process.Module().Unit(), ctor_scope, services_id, target, init_value,
      std::nullopt, type, process.Module().Unit().builtins.void_type);
  const mir::ExprId assign = ctor_scope.AddExpr(assign_expr);
  ctor_scope.AppendStmt(
      mir::Stmt{.label = std::nullopt, .data = mir::ExprStmt{.expr = assign}});
  return mir::Stmt{.label = std::move(label), .data = mir::EmptyStmt{}};
}

auto LowerAutomaticVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v, const hir::ProceduralVarDecl& hir_local,
    mir::TypeId type) -> diag::Result<mir::Stmt> {
  auto& proc_scope = *frame.current_procedural_scope;
  const mir::ProceduralVarId local_id = proc_scope.AddProceduralVar(
      mir::ProceduralVarDecl{.name = hir_local.name, .type = type});
  process.MapProceduralVar(
      v.var, AutomaticVarBinding{
                 .declaration_procedural_depth = frame.procedural_depth,
                 .var = local_id});

  mir::ExprId init_value{};
  if (v.init.has_value()) {
    auto init_or =
        process.LowerExpr(process.HirBody().exprs.at(v.init->value), frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_value = proc_scope.AddExpr(*std::move(init_or));
  } else {
    init_value = proc_scope.AddExpr(
        BuildDefaultValueExpr(process.Module(), frame, type));
  }

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ProceduralVarDeclStmt{
          .target =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = local_id},
          .init = init_value}};
}

}  // namespace

auto LowerVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt> {
  const auto& hir_local = process.HirBody().procedural_vars.at(v.var.value);
  const mir::TypeId type = process.Module().TranslateType(hir_local.type);
  if (hir_local.lifetime == hir::VariableLifetime::kStatic) {
    return LowerStaticVarDeclStmt(
        process, frame, std::move(label), v, hir_local, type);
  }
  return LowerAutomaticVarDeclStmt(
      process, frame, std::move(label), v, hir_local, type);
}

auto LowerReturnStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ReturnStmt& r) -> diag::Result<mir::Stmt> {
  auto& proc_scope = *frame.current_procedural_scope;
  std::optional<mir::ExprId> value;
  if (r.value.has_value()) {
    auto value_or =
        process.LowerExpr(process.HirBody().exprs.at(r.value->value), frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    value = proc_scope.AddExpr(*std::move(value_or));
  }
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ReturnStmt{
          .value = value, .is_coroutine_return = frame.is_coroutine_body}};
}

auto LowerBreakStmt(
    std::optional<std::string> label, std::optional<hir::LoopLabelId> target)
    -> diag::Result<mir::Stmt> {
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BreakStmt{
          .target = target.has_value()
                        ? std::optional{mir::LoopLabelId{target->value}}
                        : std::nullopt}};
}

auto LowerContinueStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt> {
  return mir::Stmt{.label = std::move(label), .data = mir::ContinueStmt{}};
}

}  // namespace lyra::lowering::hir_to_mir

#include "lyra/lowering/hir_to_mir/statement/flow.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/procedural_depth.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  const auto& hir_local = hir_proc.procedural_vars.at(v.var.value);
  const mir::TypeId type = process.Module().TranslateType(hir_local.type);

  // LRM 13.3.1: a static body local does not live in the activation. Its
  // storage and init go into the subroutine's frame scope (the root
  // procedural scope), so a body reference reaches it by hops and the init
  // is evaluated once when the instance is built. The body declaration
  // itself emits nothing.
  const bool is_static = hir_local.lifetime == hir::VariableLifetime::kStatic;
  if (is_static && frame.static_frame_scope != nullptr) {
    auto& frame_scope = *frame.static_frame_scope;
    const mir::ProceduralVarId static_id = frame_scope.AddProceduralVar(
        mir::ProceduralVarDecl{
            .name = hir_local.name,
            .type = type,
            .lifetime = mir::VariableLifetime::kStatic});
    process.MapProceduralVar(
        v.var, ProceduralVarBinding{
                   .declaration_procedural_depth = ProceduralDepth{.value = 0},
                   .var = static_id});
    mir::ExprId static_init{};
    if (v.init.has_value()) {
      auto init_or = process.LowerExpr(
          hir_proc.exprs.at(v.init->value),
          frame.WithProceduralScope(&frame_scope));
      if (!init_or) return std::unexpected(std::move(init_or.error()));
      static_init = frame_scope.AddExpr(*std::move(init_or));
    } else {
      static_init = SynthesizeDefaultValueExpr(
          process.Module(), frame.WithProceduralScope(&frame_scope), type);
    }
    process.AddStaticLocal(
        mir::StaticLocal{.var = static_id, .init = static_init});
    return mir::Stmt{.label = std::move(label), .data = mir::EmptyStmt{}};
  }

  auto& proc_scope = *frame.current_procedural_scope;
  const mir::ProceduralVarId local_id = proc_scope.AddProceduralVar(
      mir::ProceduralVarDecl{.name = hir_local.name, .type = type});
  process.MapProceduralVar(
      v.var, ProceduralVarBinding{
                 .declaration_procedural_depth = frame.procedural_depth,
                 .var = local_id});
  mir::ExprId init_id{};
  if (v.init.has_value()) {
    auto init_or = process.LowerExpr(hir_proc.exprs.at(v.init->value), frame);
    if (!init_or) {
      return std::unexpected(std::move(init_or.error()));
    }
    init_id = proc_scope.AddExpr(*std::move(init_or));
  } else {
    init_id = SynthesizeDefaultValueExpr(process.Module(), frame, type);
  }
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ProceduralVarDeclStmt{
          .target =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = local_id},
          .init = init_id}};
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

auto LowerBreakStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt> {
  return mir::Stmt{.label = std::move(label), .data = mir::BreakStmt{}};
}

auto LowerContinueStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt> {
  return mir::Stmt{.label = std::move(label), .data = mir::ContinueStmt{}};
}

}  // namespace lyra::lowering::hir_to_mir

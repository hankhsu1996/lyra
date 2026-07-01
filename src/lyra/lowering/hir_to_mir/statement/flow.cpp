#include "lyra/lowering/hir_to_mir/statement/flow.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>

#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callable_storage_plan.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/value_ref.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 13.3.1: a static-lifetime body local has per-instance storage that
// outlives every activation of the body. Body lowering records the
// initializer as a pending request (var + HIR init expression + placement);
// the dedicated initializer lowering path runs in the Initialize phase and
// produces the AssignExpr to the placement. The body's declaration
// statement itself emits no executable text.
auto LowerStaticVarDeclStmt(
    ProcessLowerer& process, std::optional<std::string> label,
    const hir::VarDeclStmt& v, const hir::ProceduralVarDecl& hir_local,
    mir::TypeId type) -> diag::Result<mir::Stmt> {
  process.RecordPendingStaticInitializer(
      PendingStaticInitializer{
          .var = v.var,
          .hir_type = hir_local.type,
          .init_expr = v.init,
          .placement = process.LookupStaticPlacement(v.var),
          .storage_type = type});
  return mir::Stmt{.label = std::move(label), .data = mir::EmptyStmt{}};
}

auto LowerAutomaticVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v, const hir::ProceduralVarDecl& hir_local,
    mir::TypeId type) -> diag::Result<mir::Stmt> {
  auto& block = *frame.current_block;
  const mir::LocalId local_id = frame.bindings->Declare(
      BindingOriginId::Procedural(v.var),
      mir::LocalDecl{.name = hir_local.name, .type = type});
  process.MapProceduralVar(v.var, AutomaticVarBinding{.type = type});

  mir::ExprId init_value{};
  if (v.init.has_value()) {
    auto init_or =
        process.LowerExpr(process.HirBody().exprs.Get(*v.init), frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_value = block.exprs.Add(*std::move(init_or));
  } else {
    init_value = block.exprs.Add(
        BuildDefaultValueFromHir(process.Module(), frame, hir_local.type));
  }
  init_value = ConvertToType(process.Module().Unit(), block, init_value, type);

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::LocalDeclStmt{.target = local_id, .init = init_value}};
}

// A lifetime-extended automatic (LRM 6.21) is a member of the scope's shared
// activation object; its declaration assigns the initial value into that member
// through the handle (`handle->member = init`) rather than into a frame local.
// The slot was recorded when the activation scope opened; consume it here, in
// HIR id order, to register the binding its references resolve through.
auto LowerPromotedVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v, hir::TypeId hir_type, mir::TypeId type)
    -> diag::Result<mir::Stmt> {
  const PromotedVarBinding pb = process.TakePendingActivation(v.var);
  process.MapProceduralVar(v.var, pb);
  auto& block = *frame.current_block;
  const mir::ExprId handle_ref = block.exprs.Add(frame.bindings->MakeReadExpr(
      frame.bindings->EnsureCarrier(pb.handle_origin), block));
  const mir::ExprId target = block.exprs.Add(
      mir::MakeMemberAccessExpr(
          handle_ref, mir::MemberRef{.var = pb.member}, type));
  mir::ExprId init_value{};
  if (v.init.has_value()) {
    auto init_or =
        process.LowerExpr(process.HirBody().exprs.Get(*v.init), frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_value = block.exprs.Add(*std::move(init_or));
  } else {
    init_value = block.exprs.Add(
        BuildDefaultValueFromHir(process.Module(), frame, hir_type));
  }
  init_value = ConvertToType(process.Module().Unit(), block, init_value, type);
  const mir::ExprId assign =
      block.exprs.Add(mir::MakeAssignExpr(target, init_value, type));
  return mir::Stmt{
      .label = std::move(label), .data = mir::ExprStmt{.expr = assign}};
}

}  // namespace

auto LowerVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt> {
  const auto& hir_local = process.HirBody().procedural_vars.Get(v.var);
  const mir::TypeId type = process.Module().TranslateType(hir_local.type);
  if (hir_local.lifetime_extended) {
    return LowerPromotedVarDeclStmt(
        process, frame, std::move(label), v, hir_local.type, type);
  }
  if (hir_local.lifetime == hir::VariableLifetime::kStatic) {
    return LowerStaticVarDeclStmt(
        process, std::move(label), v, hir_local, type);
  }
  return LowerAutomaticVarDeclStmt(
      process, frame, std::move(label), v, hir_local, type);
}

auto LowerReturnStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ReturnStmt& r) -> diag::Result<mir::Stmt> {
  auto& block = *frame.current_block;
  std::optional<mir::ExprId> explicit_value;
  if (r.value.has_value()) {
    auto value_or =
        process.LowerExpr(process.HirBody().exprs.Get(*r.value), frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    explicit_value = block.exprs.Add(*std::move(value_or));
  }
  // The returned value is the completion payload: this return's explicit value
  // (or the implicit result variable) plus each output / inout local, assembled
  // by the subroutine's lowering so the copy-out rides the result (LRM 13.5).
  const std::optional<mir::ExprId> payload =
      process.BuildReturnPayload(block, explicit_value);
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ReturnStmt{
          .value = payload, .is_coroutine_return = frame.is_coroutine_body}};
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

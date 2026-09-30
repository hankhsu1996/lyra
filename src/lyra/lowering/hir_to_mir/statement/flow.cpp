#include "lyra/lowering/hir_to_mir/statement/flow.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>

#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/declared_variable.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerAutomaticVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v, const hir::ProceduralVarDecl& hir_local,
    mir::TypeId type) -> diag::Result<mir::Stmt> {
  auto& block = *frame.current_block;
  mir::CompilationUnit& unit = process.Owner().Unit();
  const DeclaredVariable variable = DeclareVariable(
      unit, *frame.bindings, block, BindingOriginId::Procedural(v.var),
      hir_local.name, type, frame.body_can_wait);
  process.MapProceduralVar(v.var, AutomaticVarBinding{.type = variable.type});

  mir::ExprId init_value{};
  if (hir_local.init.has_value()) {
    auto init_or =
        process.LowerExpr(process.HirBody().exprs.Get(*hir_local.init), frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_value = block.exprs.Add(*std::move(init_or));
  } else {
    init_value = block.exprs.Add(
        BuildDefaultValueFromHir(process.Owner(), block, hir_local.type));
  }
  init_value = ConvertToType(unit, block, init_value, type);

  mir::Stmt initialized = InitializeVariable(unit, block, variable, init_value);
  initialized.label = std::move(label);
  return initialized;
}

// A lifetime-extended automatic (LRM 6.21) is a cell in a field of the scope's
// shared activation frame; its declaration initializes that cell through the
// handle rather than a local's own. The field was recorded when the activation
// scope opened; consume it here, in HIR id order, to register the binding its
// references resolve through.
auto LowerPromotedVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v, const hir::ProceduralVarDecl& hir_local,
    mir::TypeId type) -> diag::Result<mir::Stmt> {
  const PromotedVarBinding pb = process.TakePendingActivation(v.var);
  process.MapProceduralVar(v.var, pb);
  auto& block = *frame.current_block;
  mir::CompilationUnit& unit = process.Owner().Unit();
  const mir::ExprId handle_ref = block.exprs.Add(frame.bindings->MakeReadExpr(
      frame.bindings->EnsureCarrier(pb.handle_origin), block));
  const mir::ExprId target = block.exprs.Add(
      mir::MakeFieldAccessExpr(handle_ref, pb.field, pb.cell_type));
  mir::ExprId init_value{};
  if (hir_local.init.has_value()) {
    auto init_or =
        process.LowerExpr(process.HirBody().exprs.Get(*hir_local.init), frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_value = block.exprs.Add(*std::move(init_or));
  } else {
    init_value = block.exprs.Add(
        BuildDefaultValueFromHir(process.Owner(), block, hir_local.type));
  }
  init_value = ConvertToType(unit, block, init_value, type);
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeCapabilityInstallCallExpr(
                  target, init_value, support::BuiltinFn::kInitialize,
                  unit.builtins.void_type))}};
}

}  // namespace

auto LowerVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt> {
  const auto& hir_local = process.HirBody().procedural_vars.Get(v.var);
  const mir::TypeId type = process.Owner().TranslateType(hir_local.type);
  if (hir_local.lifetime_extended) {
    return LowerPromotedVarDeclStmt(
        process, frame, std::move(label), v, hir_local, type);
  }
  // LRM 6.21: a static-lifetime body local keeps a cell that outlives every
  // activation, so its storage and its binding are both settled before the body
  // lowers and its declaration assignment runs where that cell is brought up,
  // before any process starts. Reaching the declaration is therefore not an
  // event: it binds nothing and emits nothing.
  if (hir_local.lifetime == hir::VariableLifetime::kStatic) {
    return mir::Stmt{.label = std::move(label), .data = mir::EmptyStmt{}};
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
      .label = std::move(label), .data = mir::ReturnStmt{.value = payload}};
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

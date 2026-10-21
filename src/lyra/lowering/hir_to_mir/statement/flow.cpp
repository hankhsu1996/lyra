#include "lyra/lowering/hir_to_mir/statement/flow.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
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

// The value a variable starts at where its declaration is reached, in the
// frame's block: its declaration assignment, or its type's default value where
// the source wrote none, at the variable's declared type.
auto BuildInitialValue(
    ProcessLowerer& process, WalkFrame frame,
    const hir::ProceduralVarDecl& hir_local, mir::TypeId type)
    -> diag::Result<mir::ExprId> {
  auto& block = *frame.current_block;
  mir::ExprId value{};
  if (hir_local.init.has_value()) {
    auto init_or =
        process.LowerExpr(process.HirBody().exprs.Get(*hir_local.init), frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    value = block.exprs.Add(*std::move(init_or));
  } else {
    value = block.exprs.Add(
        BuildDefaultValueFromHir(process.Owner(), block, hir_local.type));
  }
  return ConvertToType(process.Owner().Unit(), block, value, type);
}

// Binds a promoted variable to the cell its scope's entry made, so every
// reference to it -- its own initializer's among them -- reaches that cell.
auto BindPromotedVar(ProcessLowerer& process, hir::ProceduralVarId var)
    -> PromotedVarBinding {
  const PromotedVarBinding promoted = process.TakePendingActivation(var);
  process.MapProceduralVar(var, promoted);
  return promoted;
}

auto InitializePromotedVar(
    ProcessLowerer& process, const WalkFrame& frame,
    const PromotedVarBinding& promoted, mir::ExprId initial) -> mir::Stmt {
  mir::Block& block = *frame.current_block;
  mir::CompilationUnit& unit = process.Owner().Unit();
  const mir::ExprId cell =
      block.exprs.Add(PromotedVarPlace(unit.types, frame, promoted));
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeCapabilityInstallCallExpr(
                  cell, initial, support::BuiltinFn::kInitialize,
                  unit.builtins.void_type))}};
}

}  // namespace

auto LowerVarStart(
    ProcessLowerer& process, WalkFrame frame, hir::ProceduralVarId var,
    const std::optional<std::string>& name, const InitialValue& initial)
    -> diag::Result<mir::Stmt> {
  const auto& hir_local = process.HirBody().procedural_vars.Get(var);
  // A lifetime-extended automatic (LRM 6.21) is a cell held by a shared
  // pointer, recorded where the scope declaring it was entered; its
  // declaration takes that pointer and initializes the cell through it.
  if (hir_local.lifetime_extended) {
    const PromotedVarBinding promoted = BindPromotedVar(process, var);
    auto value = initial();
    if (!value) return std::unexpected(std::move(value.error()));
    return InitializePromotedVar(process, frame, promoted, *value);
  }
  // LRM 6.21: a static-lifetime variable keeps a cell that outlives every
  // activation, so its storage and its binding are both settled before the body
  // lowers and its declaration assignment runs where that cell is brought up,
  // before any process starts. Reaching the declaration is therefore not an
  // event: it binds nothing and emits nothing.
  if (hir_local.lifetime == hir::VariableLifetime::kStatic) {
    return mir::Stmt{.label = std::nullopt, .data = mir::EmptyStmt{}};
  }
  mir::Block& block = *frame.current_block;
  mir::CompilationUnit& unit = process.Owner().Unit();
  const DeclaredVariable variable = DeclareVariable(
      unit, *frame.bindings, block, ProceduralOrigin{.var = var}, name,
      process.Owner().TranslateType(hir_local.type), frame.body_can_wait);
  process.MapProceduralVar(var, AutomaticVarBinding{.type = variable.type});
  auto value = initial();
  if (!value) return std::unexpected(std::move(value.error()));
  return InitializeVariable(unit, block, variable, *value);
}

auto LowerVarDeclaration(
    ProcessLowerer& process, WalkFrame frame, hir::ProceduralVarId var)
    -> diag::Result<mir::Stmt> {
  const auto& hir_local = process.HirBody().procedural_vars.Get(var);
  const mir::TypeId type = process.Owner().TranslateType(hir_local.type);
  return LowerVarStart(process, frame, var, hir_local.name, [&] {
    return BuildInitialValue(process, frame, hir_local, type);
  });
}

auto LowerVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt> {
  auto declared = LowerVarDeclaration(process, frame, v.var);
  if (!declared) return std::unexpected(std::move(declared.error()));
  declared->label = std::move(label);
  return declared;
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
  // (or the implicit result variable) plus each output / inout formal,
  // assembled by the subroutine's lowering so the copy-out rides the result
  // (LRM 13.5).
  const std::optional<mir::ExprId> payload =
      process.BuildReturnPayload(frame, explicit_value);
  return mir::Stmt{
      .label = std::move(label), .data = mir::ReturnStmt{.value = payload}};
}

auto LowerBreakStmt(std::optional<std::string> label, const WalkFrame& frame)
    -> diag::Result<mir::Stmt> {
  if (frame.break_leaves == nullptr) {
    throw InternalError(
        "LowerBreakStmt: a break stands in no loop of its body, which the "
        "front end rejects (LRM 12.8)");
  }
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BreakStmt{.target = frame.break_leaves->Name()}};
}

auto LowerContinueStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt> {
  return mir::Stmt{.label = std::move(label), .data = mir::ContinueStmt{}};
}

}  // namespace lyra::lowering::hir_to_mir

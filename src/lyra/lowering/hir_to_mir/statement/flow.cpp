#include "lyra/lowering/hir_to_mir/statement/flow.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/declared_initial_value.hpp"
#include "lyra/lowering/hir_to_mir/declared_variable.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Emits, where a jump from `frame` stands, what each scope it leaves owes on
// a normal way out, innermost first, out to `until` -- what was owed outside
// the scope the jump stays inside, and nothing for a `return`, which leaves
// every scope of the body.
void EmitNormalCleanups(const WalkFrame& frame, const NormalCleanup* until) {
  for (const NormalCleanup* at = frame.normal_cleanups; at != until;
       at = at->outer) {
    at->emit(frame);
  }
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
  mir::Builder block = frame.Builder();
  mir::CompilationUnit& unit = process.Owner().Unit();
  const mir::ExprId cell =
      block.Add(PromotedVarPlace(unit.types, frame, promoted));
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.Add(
              mir::MakeCapabilityInstallCallExpr(
                  cell, initial, support::BuiltinFn::kInitialize,
                  unit.builtins.void_type))}};
}

}  // namespace

auto DefaultInitialValue(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::ProceduralVarDecl& decl) -> InitialValue {
  return [&process, &frame, &decl] {
    return BuildDeclaredInitialValue(process.Owner(), frame, decl.type);
  };
}

auto LowerVarStart(
    ProcessLowerer& process, WalkFrame frame, hir::ProceduralVarId var,
    const std::optional<std::string>& name, const InitialValue& initial)
    -> mir::Stmt {
  const auto& hir_local = process.HirBody().procedural_vars.Get(var);
  // A lifetime-extended automatic (LRM 6.21) is a cell held by a shared
  // pointer, recorded where the scope declaring it was entered; its
  // declaration takes that pointer and initializes the cell through it.
  if (hir_local.lifetime_extended) {
    const PromotedVarBinding promoted = BindPromotedVar(process, var);
    return InitializePromotedVar(process, frame, promoted, initial());
  }
  // LRM 6.21: a static-lifetime variable keeps a cell that outlives every
  // activation, so its storage and its binding are both settled before the body
  // lowers and its declaration assignment runs where that cell is brought up,
  // before any process starts. Reaching the declaration is therefore not an
  // event: it binds nothing and emits nothing.
  if (hir_local.lifetime == hir::VariableLifetime::kStatic) {
    return mir::Stmt{.label = std::nullopt, .data = mir::EmptyStmt{}};
  }
  mir::Builder block = frame.Builder();
  mir::CompilationUnit& unit = process.Owner().Unit();
  const DeclaredVariable variable = DeclareVariable(
      unit, *frame.bindings, block, ProceduralOrigin{.var = var}, name,
      process.Owner().TranslateType(hir_local.type),
      frame.runs_as_function_call);
  process.MapProceduralVar(var, AutomaticVarBinding{.type = variable.type});
  return InitializeVariable(unit, block, variable, initial());
}

auto LowerVarDeclaration(
    ProcessLowerer& process, WalkFrame frame, hir::ProceduralVarId var)
    -> diag::Result<mir::Stmt> {
  const auto& hir_local = process.HirBody().procedural_vars.Get(var);
  // LRM 6.21, 13.3: an automatic variable takes its default on each entry to
  // its scope, and its declaration assignment is made to it after, so an
  // assignment naming the variable reads that default. A static one was
  // brought up before the body, its assignment with it.
  mir::Stmt started = LowerVarStart(
      process, frame, var, hir_local.name,
      DefaultInitialValue(process, frame, hir_local));
  if (!hir_local.init.has_value() ||
      hir_local.lifetime == hir::VariableLifetime::kStatic) {
    return started;
  }
  const mir::TypeId type = process.Owner().TranslateType(hir_local.type);
  mir::Builder block = frame.Builder();
  block.AppendStmt(std::move(started));
  auto init =
      process.LowerExpr(process.HirBody().exprs.Get(*hir_local.init), frame);
  if (!init) return std::unexpected(std::move(init.error()));
  const mir::ExprId value = ConvertToType(
      process.Owner().Unit(), block, block.Add(*std::move(init)), type);
  const AccessPath variable{
      .owner = block.Add(LowerProceduralVarRefExpr(process, frame, var)),
      .descent = {}};
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.Add(
              BuildStoreExpr(process.Owner().Unit(), block, variable, value))}};
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
  mir::Builder block = frame.Builder();
  std::optional<mir::ExprId> explicit_value;
  if (r.value.has_value()) {
    auto value_or =
        process.LowerExpr(process.HirBody().exprs.Get(*r.value), frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    explicit_value = block.Add(*std::move(value_or));
  }
  // The returned value is the completion payload: this return's explicit value
  // (or the implicit result variable) plus each output / inout formal,
  // assembled by the subroutine's lowering so the copy-out rides the result
  // (LRM 13.5).
  std::optional<mir::ExprId> payload =
      process.BuildReturnPayload(frame, explicit_value);
  // The value is part of the return, so it is evaluated before what leaving
  // the scopes owes runs, as rustc writes the return place before the drops.
  if (payload.has_value() && frame.normal_cleanups != nullptr) {
    const mir::TypeId type = block.Get(*payload).type;
    payload = block.ReadLocal(DeclareLocal(frame, *payload), type);
  }
  EmitNormalCleanups(frame, nullptr);
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
  EmitNormalCleanups(frame, frame.loop_normal_cleanups);
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BreakStmt{.target = frame.break_leaves->Name()}};
}

auto LowerContinueStmt(std::optional<std::string> label, const WalkFrame& frame)
    -> diag::Result<mir::Stmt> {
  EmitNormalCleanups(frame, frame.loop_normal_cleanups);
  return mir::Stmt{.label = std::move(label), .data = mir::ContinueStmt{}};
}

}  // namespace lyra::lowering::hir_to_mir

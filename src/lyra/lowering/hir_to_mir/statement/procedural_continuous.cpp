#include "lyra/lowering/hir_to_mir/statement/procedural_continuous.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/deferred_effect.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/takeover_level.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LevelOperand(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::TakeoverLevel level) -> mir::ExprId {
  return BuildMachineIntLiteral(unit, block, static_cast<std::int64_t>(level));
}

// The capability a takeover acts on, for one place of its target. LRM 10.6.1
// admits only a whole variable and 10.6.2 adds a constant part-select of a net,
// so a descent here means the source named part of something -- which this
// does not carry yet.
auto TakeoverPlace(const AccessPath& place, diag::SourceSpan span)
    -> diag::Result<mir::ExprId> {
  if (!place.descent.empty()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "taking over part of a target is not yet supported (LRM 10.6.2)");
  }
  return std::visit(
      Overloaded{
          [](mir::ExprId owner) -> diag::Result<mir::ExprId> { return owner; },
          // A member of a dynamic type is no variable a takeover may name,
          // which the front end refuses before anything is lowered.
          [](const ObjectProperty&) -> diag::Result<mir::ExprId> {
            throw InternalError(
                "procedural continuous assignment: the target is a class "
                "property, which the front end refuses -- please report this "
                "as a bug");
          }},
      place.owner);
}

// Every place a takeover's target names, in the order the source wrote them: a
// variable or a net, or each member of a concatenation of them (LRM 10.6.1,
// 10.6.2).
auto TakeoverPlaces(
    ProcessLowerer& process, const WalkFrame& frame, const hir::Expr& target)
    -> diag::Result<std::vector<mir::ExprId>> {
  auto lvalue = LowerLvalue(process, target, frame);
  if (!lvalue) return std::unexpected(std::move(lvalue.error()));
  std::vector<AccessPath> named;
  ForEachPlace(
      *lvalue, [&](const AccessPath& place) { named.push_back(place); });
  std::vector<mir::ExprId> places;
  places.reserve(named.size());
  for (const AccessPath& place : named) {
    auto owner = TakeoverPlace(place, target.span);
    if (!owner) return std::unexpected(std::move(owner.error()));
    places.push_back(*owner);
  }
  return places;
}

}  // namespace

auto LowerProceduralContinuousAssignStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ProceduralContinuousAssignStmt& pca) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::BuiltinMirTypes& builtins = unit.builtins;
  const hir::Expr& hir_target = hir_proc.exprs.Get(pca.target);

  mir::Block wrapper;
  const WalkFrame outer = frame.WithBlock(&wrapper);
  ClosureBuilder closure(unit, outer);

  // The takeover of each place is in effect from here, before anything
  // evaluates: a `release` reaching the place in the same time step must find
  // it taken over, and the generation this answers with is what says which
  // evaluation owns the level there.
  auto outer_places = TakeoverPlaces(process, outer, hir_target);
  if (!outer_places) return std::unexpected(std::move(outer_places.error()));
  std::vector<mir::LocalId> generations;
  generations.reserve(outer_places->size());
  for (const mir::ExprId place : *outer_places) {
    const mir::ExprId begin = wrapper.exprs.Add(
        mir::MakeBeginTakeoverCallExpr(
            place, LevelOperand(unit, wrapper, pca.level),
            builtins.machine_int64));
    generations.push_back(DeclareLocal(
        closure.Frame(),
        SnapshotIntoClosure(unit_lowerer, outer, closure, begin)));
  }

  // One evaluation: the source read once and each place driven its share. The
  // target is named again inside it rather than carried in: it is reached from
  // the receiver, and the receiver is an ordinary binding the same capture
  // recursion brings across. What it answers is whether any place still says
  // this evaluation owns the level there, every place having been driven.
  BlockBuilder pass(closure.Frame());
  mir::Block& driving = pass.Body();
  auto source_or =
      process.LowerExpr(hir_proc.exprs.Get(pca.source), pass.Frame());
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  auto inner_target = LowerLvalue(process, hir_target, pass.Frame());
  if (!inner_target) return std::unexpected(std::move(inner_target.error()));
  auto shares = Shares(
      unit_lowerer, pass.Frame(), *inner_target,
      driving.exprs.Add(*std::move(source_or)));
  if (!shares) return std::unexpected(std::move(shares.error()));
  std::vector<mir::LocalId> owned;
  owned.reserve(shares->size());
  for (std::size_t i = 0; i < shares->size(); ++i) {
    const Share& share = (*shares)[i];
    auto place = TakeoverPlace(share.place, hir_target.span);
    if (!place) return std::unexpected(std::move(place.error()));
    owned.push_back(pass.DeclareLocal(
        builtins.machine_bool,
        driving.exprs.Add(
            mir::MakeDriveTakeoverCallExpr(
                *place, LevelOperand(unit, driving, pca.level),
                driving.exprs.Add(
                    mir::MakeLocalRefExpr(
                        generations[i], builtins.machine_int64)),
                share.value, builtins.machine_bool))));
  }
  const auto read_owned = [&](mir::LocalId local) {
    return driving.exprs.Add(
        mir::MakeLocalRefExpr(local, builtins.machine_bool));
  };
  mir::ExprId any_owned = read_owned(owned.back());
  for (std::size_t i = owned.size() - 1; i-- > 0;) {
    any_owned = driving.exprs.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = read_owned(owned[i]),
                    .then_value = driving.exprs.Add(
                        mir::Expr{
                            .data = mir::MachineBoolLiteral{.value = true},
                            .type = builtins.machine_bool}),
                    .else_value = any_owned},
            .type = builtins.machine_bool});
  }
  mir::Block& body = closure.Body();
  const mir::ExprId drive = body.exprs.Add(pass.Build(any_owned));

  // An evaluation whose source nothing can change has no second pass to make,
  // so what is emitted is one drive. A takeover is created per execution of
  // the statement, so an evaluation left waiting on nothing would accumulate.
  if (pca.sensitivity_list.empty()) {
    body.AppendStmt(mir::ExprStmt{.expr = drive});
  } else {
    mir::Block wait_block;
    auto waited = BuildValueChangeWaitStmt(
        wait_block, closure.Frame().WithBlock(&wait_block), process,
        pca.sensitivity_list);
    if (!waited) return std::unexpected(std::move(waited.error()));
    wait_block.AppendStmt(*std::move(waited));
    const mir::BlockId wait_scope =
        body.child_scopes.Add(std::move(wait_block));
    // Evaluating and driving is the loop's own condition, so the source is
    // re-read on every pass and the takeover stops the moment no place says
    // this evaluation owns the level any longer.
    body.AppendStmt(
        mir::ForStmt{
            .init = {}, .condition = drive, .step = {}, .scope = wait_scope});
  }

  wrapper.AppendStmt(
      mir::ExprStmt{
          .expr = wrapper.exprs.Add(
              RunCarrierDetached(process, outer, closure.BuildCoroutine()))});

  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

auto LowerProceduralContinuousEndStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ProceduralContinuousEndStmt& pce) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  const mir::CompilationUnit& unit = process.Owner().Unit();

  // One end per place the target names, so the statement is a block of them.
  BlockBuilder steps(frame);
  mir::Block& block = steps.Body();
  auto places =
      TakeoverPlaces(process, steps.Frame(), hir_proc.exprs.Get(pce.target));
  if (!places) return std::unexpected(std::move(places.error()));
  for (const mir::ExprId place : *places) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(
                mir::MakeEndTakeoverCallExpr(
                    place, LevelOperand(unit, block, pce.level),
                    unit.builtins.void_type))});
  }
  mir::Stmt stmt = steps.BuildStatement();
  stmt.label = std::move(label);
  return stmt;
}

}  // namespace lyra::lowering::hir_to_mir

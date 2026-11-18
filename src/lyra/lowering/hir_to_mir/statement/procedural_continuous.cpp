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
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/deferred_effect.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/takeover_level.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LevelOperand(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::TakeoverLevel level) -> mir::ExprId {
  return mir::BuildMachineIntLiteral(
      unit, block, static_cast<std::int64_t>(level));
}

// One place of a takeover's target as the takeover acts on it: what holds the
// place, and which positions of what it holds the place is, as a pair of
// machine integers: the first and how many (LRM 10.6.2).
struct TakenPart {
  mir::ExprId holder;
  mir::ExprId first;
  mir::ExprId count;
};

// What holds one place of a takeover's target: a variable, a net, or a
// reference naming one.
auto TakeoverHolder(const AccessPath& place) -> mir::ExprId {
  return std::visit(
      Overloaded{
          [](mir::ExprId owner) -> mir::ExprId { return owner; },
          // A member of a dynamic type is no variable a takeover may name,
          // which the front end refuses before anything is lowered.
          [](const ObjectProperty&) -> mir::ExprId {
            throw InternalError(
                "procedural continuous assignment: the target is a class "
                "property, which the front end refuses -- please report this "
                "as a bug");
          }},
      place.owner);
}

// The positions of its holder one place of a takeover's target names. LRM
// 10.6.1 admits only a whole singular variable and 10.6.2 adds a net and a
// constant part-select of one, and a caller may ask for a whole unpacked
// variable or any part of a variable as well, run with the rules 10.6.2 gives a
// whole variable applied to each position, numbered as the holder's type
// numbers them.
auto TakeoverPart(
    mir::CompilationUnit& unit, const WalkFrame& frame, const AccessPath& place,
    diag::SourceSpan span) -> diag::Result<TakenPart> {
  const std::expected<PositionsOfPart, UnnumberedPart> named =
      PositionsNamedBy(unit, frame, place);
  if (!named) {
    switch (named.error()) {
      case UnnumberedPart::kOfDynamicValue:
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedStatementForm,
            "a force on part of a string, a queue, a dynamic array or an "
            "associative array is not yet supported");
      case UnnumberedPart::kTaggedMember:
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedStatementForm,
            "a force on a member of a tagged union is not yet supported");
    }
    throw InternalError("TakeoverPart: unknown unnumbered part");
  }
  // A takeover holds the positions the part has, none where a select names
  // no part (LRM 7.4.5, 11.5.1).
  return TakenPart{
      .holder = TakeoverHolder(place),
      .first = named->reached_first,
      .count = named->reached_count};
}

// Every part a takeover's target names, in the order the source wrote them: a
// variable or a net, a part of one, or each member of a concatenation of them
// (LRM 10.6.1, 10.6.2).
auto TakeoverParts(
    ProcessLowerer& process, const WalkFrame& frame, const hir::Expr& target)
    -> diag::Result<std::vector<TakenPart>> {
  auto lvalue = LowerLvalue(process, target, frame);
  if (!lvalue) return std::unexpected(std::move(lvalue.error()));
  mir::CompilationUnit& unit = process.Owner().Unit();
  const std::vector<AccessPath> named =
      PlacesWritten(unit, frame.Builder(), *lvalue);
  std::vector<TakenPart> parts;
  parts.reserve(named.size());
  for (const AccessPath& place : named) {
    // The positions a place names read its selects' positions more than once,
    // so what reaching it computes is evaluated here, once.
    auto part =
        TakeoverPart(unit, frame, Settled(unit, frame, place), target.span);
    if (!part) return std::unexpected(std::move(part.error()));
    parts.push_back(*part);
  }
  return parts;
}

// What an evaluation hands the takeover of `share`'s place: a value of the
// type its holder holds, of which the takeover reads the positions it holds.
// Where the place is part of that value, the share is written at the part in a
// value of it built for the purpose, whose other positions nothing reads.
auto DrivenValue(
    mir::CompilationUnit& unit, BlockBuilder& pass, const Share& share)
    -> mir::ExprId {
  if (share.place.descent.empty()) {
    return share.value;
  }
  mir::Builder block = pass.Body();
  const mir::TypeId held =
      OwnerValueType(unit, block.Body(), share.place.owner);
  const mir::LocalId built = pass.DeclareLocal(
      held, block.Add(BuildDefaultValueExpr(unit, block, held)));
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(BuildStoreExpr(
              unit, block,
              AccessPath{
                  .owner = block.LocalPlace(built, held),
                  .descent = share.place.descent},
              share.value))});
  return block.ReadLocal(built, held);
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
  mir::Builder wrapper_builder(wrapper, frame.span);
  const WalkFrame outer = frame.WithBlock(&wrapper);
  ClosureBuilder closure(unit, outer, support::ClosureKind::kFnOnce);

  // The takeover of each place is in effect from here, before anything
  // evaluates: a `release` reaching the place in the same time step must find
  // it taken over, and the generation this answers with names the takeover
  // that evaluation drives.
  auto outer_parts = TakeoverParts(process, outer, hir_target);
  if (!outer_parts) return std::unexpected(std::move(outer_parts.error()));
  std::vector<mir::LocalId> generations;
  generations.reserve(outer_parts->size());
  for (const TakenPart& part : *outer_parts) {
    const mir::ExprId begin = wrapper_builder.Add(
        mir::MakeBeginTakeoverCallExpr(
            part.holder, LevelOperand(unit, wrapper_builder, pca.level),
            part.first, part.count, builtins.machine_int64));
    generations.push_back(DeclareLocal(
        closure.Frame(),
        SnapshotValueIntoClosure(unit_lowerer, outer, closure, begin)));
  }

  // One evaluation: the source read once and each place driven its share. The
  // target is named again inside it rather than carried in: it is reached from
  // the receiver, and the receiver is an ordinary binding the same capture
  // recursion brings across. What it answers is whether the takeover of any
  // place still holds a position, every place having been driven.
  BlockBuilder pass(closure.Frame());
  mir::Builder driving = pass.Body();
  auto source_or =
      process.LowerExpr(hir_proc.exprs.Get(pca.source), pass.Frame());
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  auto inner_target = LowerLvalue(process, hir_target, pass.Frame());
  if (!inner_target) return std::unexpected(std::move(inner_target.error()));
  auto shares = Shares(
      unit_lowerer, pass.Frame(), *inner_target,
      driving.Add(*std::move(source_or)));
  if (!shares) return std::unexpected(std::move(shares.error()));
  std::vector<mir::LocalId> owned;
  owned.reserve(shares->size());
  for (std::size_t i = 0; i < shares->size(); ++i) {
    const Share& share = (*shares)[i];
    const mir::ExprId level = LevelOperand(unit, driving, pca.level);
    const mir::ExprId generation =
        driving.ReadLocal(generations[i], builtins.machine_int64);
    const mir::ExprId value = DrivenValue(unit, pass, share);
    owned.push_back(pass.DeclareLocal(
        builtins.machine_bool,
        driving.Add(
            mir::MakeDriveTakeoverCallExpr(
                TakeoverHolder(share.place), level, generation, value,
                builtins.machine_bool))));
  }
  const auto read_owned = [&](mir::LocalId local) {
    return driving.ReadLocal(local, builtins.machine_bool);
  };
  mir::ExprId any_owned = read_owned(owned.back());
  for (std::size_t i = owned.size() - 1; i-- > 0;) {
    any_owned = driving.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = read_owned(owned[i]),
                    .then_value = driving.Add(
                        mir::Expr{
                            .data = mir::MachineBoolLiteral{.value = true},
                            .type = builtins.machine_bool}),
                    .else_value = any_owned},
            .type = builtins.machine_bool});
  }
  mir::Builder body = closure.Body();
  const mir::ExprId drive = body.Add(pass.Build(any_owned));

  // An evaluation whose source nothing can change has no second pass to make,
  // so what is emitted is one drive. A takeover is created per execution of
  // the statement, so an evaluation left waiting on nothing would accumulate.
  if (pca.sensitivity_list.empty()) {
    body.AppendStmt(mir::ExprStmt{.expr = drive});
  } else {
    mir::Block wait_block;
    mir::Builder wait_block_builder = body.Into(wait_block);
    auto waited = BuildContinuousWaitStmt(
        wait_block_builder,
        closure.Frame().WithLoopPreheader(&body.Body()).WithBlock(&wait_block),
        process, pca.sensitivity_list);
    if (!waited) return std::unexpected(std::move(waited.error()));
    wait_block_builder.AppendStmt(*std::move(waited));
    const mir::BlockId wait_scope = body.AddScope(std::move(wait_block));
    // Evaluating and driving is the loop's own condition, so the source is
    // re-read on every pass and the evaluation stops the moment no place's
    // takeover holds a position any longer.
    body.AppendStmt(
        mir::ForStmt{
            .init = {}, .condition = drive, .step = {}, .scope = wait_scope});
  }

  wrapper_builder.AppendStmt(
      mir::ExprStmt{
          .expr = wrapper_builder.Add(RunCarrier(
              process, outer, support::BuiltinFn::kRunDetached,
              closure.BuildCallOnce()))});

  const mir::BlockId scope_id = frame.Builder().AddScope(std::move(wrapper));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

auto LowerProceduralContinuousEndStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ProceduralContinuousEndStmt& pce) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::CompilationUnit& unit = process.Owner().Unit();

  // One end per part the target names, so the statement is a block of them.
  BlockBuilder steps(frame);
  mir::Builder block = steps.Body();
  auto parts =
      TakeoverParts(process, steps.Frame(), hir_proc.exprs.Get(pce.target));
  if (!parts) return std::unexpected(std::move(parts.error()));
  for (const TakenPart& part : *parts) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.Add(
                mir::MakeEndTakeoverCallExpr(
                    part.holder, LevelOperand(unit, block, pce.level),
                    part.first, part.count, unit.builtins.void_type))});
  }
  mir::Stmt stmt = steps.BuildStatement();
  stmt.label = std::move(label);
  return stmt;
}

}  // namespace lyra::lowering::hir_to_mir

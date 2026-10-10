#include "lyra/lowering/hir_to_mir/statement/procedural_continuous.hpp"

#include <array>
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
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
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

// The place `target` names, where it names one.
auto TakeoverTarget(
    ProcessLowerer& process, const WalkFrame& frame, const hir::Expr& target)
    -> diag::Result<mir::ExprId> {
  auto places = TakeoverPlaces(process, frame, target);
  if (!places) return std::unexpected(std::move(places.error()));
  return places->front();
}

// A call on a member that owns no storage (LRM 23.3.3), named by a reference
// to it.
auto MemberCall(
    mir::Block& block, support::BuiltinFn entry,
    std::vector<mir::ExprId> arguments, mir::TypeId type) -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = entry}, std::move(arguments), type));
}

// A force on a member that owns no storage (LRM 10.6.2). The member stands
// for what its connection drives, so the force gives it storage of the force's
// own to name: the evaluation holds a variable for as long as the force lasts,
// evaluates the source into it, and the member, what is bound from it and
// every wait on them name that variable until a release. What drives the
// member is untouched, since nothing is written through to it.
//
// The variable first takes what the member showed, and the forced value is
// then stored into it as any value is, so whoever waits on the member is told
// of the force exactly when it changed what the member shows.
auto LowerForceOnMember(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ProceduralContinuousAssignStmt& pca, mir::TypeId value_type)
    -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::BuiltinMirTypes& builtins = unit.builtins;
  const hir::Expr& hir_target = hir_proc.exprs.Get(pca.target);

  mir::Block wrapper;
  const WalkFrame outer = frame.WithBlock(&wrapper);
  auto outer_target = TakeoverTarget(process, outer, hir_target);
  if (!outer_target) return std::unexpected(std::move(outer_target.error()));
  const mir::ExprId begin = MemberCall(
      wrapper, support::BuiltinFn::kBeginForce, {*outer_target},
      builtins.machine_int64);

  ClosureBuilder closure(unit, outer);
  const WalkFrame inner = closure.Frame();
  const mir::ExprId generation = EvaluatedOnce(
      inner, SnapshotIntoClosure(unit_lowerer, outer, closure, begin));
  mir::Block& body = closure.Body();
  // The member is named afresh at each use: it is reached from the receiver,
  // which the capture recursion brings across.
  const auto member = [&](const WalkFrame& in) {
    return TakeoverTarget(process, in, hir_target);
  };

  const mir::TypeId cell_type = mir::ObservableCellOf(unit.types, value_type);
  const mir::LocalId forced = inner.bindings->DeclareAnonymous(cell_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = forced,
          .init = body.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee = mir::Construct{}, .arguments = {}},
                  .type = cell_type})});
  const auto forced_cell = [&](mir::Block& in) {
    return in.exprs.Add(mir::MakeLocalRefExpr(forced, cell_type));
  };

  auto shown = member(inner);
  if (!shown) return std::unexpected(std::move(shown.error()));
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              mir::MakeCapabilityInstallCallExpr(
                  forced_cell(body),
                  body.exprs.Add(mir::MakeCellLoadCallExpr(*shown, value_type)),
                  support::BuiltinFn::kInitialize, builtins.void_type))});

  auto retargeted = member(inner);
  if (!retargeted) return std::unexpected(std::move(retargeted.error()));
  body.AppendStmt(
      mir::ExprStmt{
          .expr = MemberCall(
              body, support::BuiltinFn::kRetargetMember,
              {*retargeted,
               BuildReferenceArg(unit, body, forced_cell(body), value_type),
               generation},
              builtins.void_type)});

  // Each pass stores what the source evaluates to and stops until the source
  // may have moved or the force has ended; the pass after the force ended
  // finds it so and the evaluation is over, its variable with it.
  mir::Block pass;
  const WalkFrame pass_frame = inner.WithBlock(&pass);
  auto source_or =
      process.LowerExpr(hir_proc.exprs.Get(pca.source), pass_frame);
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  const mir::ExprId source = pass.exprs.Add(*std::move(source_or));
  pass.AppendStmt(
      mir::ExprStmt{
          .expr = pass.exprs.Add(BuildStoreExpr(
              unit, pass, AccessPath{.owner = forced_cell(pass), .descent = {}},
              source))});
  const std::array<StatedPlace, 1> force_ended{
      [&](const WalkFrame& in) -> diag::Result<mir::ExprId> {
        auto watched = member(in);
        if (!watched) return std::unexpected(std::move(watched.error()));
        return MemberCall(
            *in.current_block, support::BuiltinFn::kForceEnded, {*watched},
            mir::ErasedPointer(unit.types));
      }};
  auto waited = BuildValueChangeWaitStmt(
      pass, pass_frame, process, pca.sensitivity_list, force_ended);
  if (!waited) return std::unexpected(std::move(waited.error()));
  pass.AppendStmt(*std::move(waited));

  auto asked = member(inner);
  if (!asked) return std::unexpected(std::move(asked.error()));
  body.AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = MemberCall(
              body, support::BuiltinFn::kStillForcing, {*asked, generation},
              builtins.machine_bool),
          .step = {},
          .scope = body.child_scopes.Add(std::move(pass))});

  wrapper.AppendStmt(
      mir::ExprStmt{
          .expr = wrapper.exprs.Add(
              RunCarrierDetached(process, outer, closure.BuildCoroutine()))});
  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

// The value type of the member `target` is, where it is one that owns no
// storage: what a force on it has to give storage to.
auto MemberValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId target) -> std::optional<mir::TypeId> {
  const auto* reference =
      unit.types.Get(block.exprs.Get(target).type).As<mir::RefType>();
  if (reference == nullptr) return std::nullopt;
  return reference->pointee;
}

// The value type of the member `target` names, where it names one place and
// that place is a member that owns no storage. Asked on a block nothing keeps.
auto ForcedMemberValueType(
    ProcessLowerer& process, const WalkFrame& frame, const hir::Expr& target)
    -> diag::Result<std::optional<mir::TypeId>> {
  mir::Block probe;
  auto places = TakeoverPlaces(process, frame.WithBlock(&probe), target);
  if (!places) return std::unexpected(std::move(places.error()));
  if (places->size() != 1) return std::nullopt;
  return MemberValueType(process.Owner().Unit(), probe, places->front());
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

  // What the target is decides which construct this is.
  {
    auto member = ForcedMemberValueType(process, frame, hir_target);
    if (!member) return std::unexpected(std::move(member.error()));
    if (member->has_value()) {
      if (pca.level != support::TakeoverLevel::kForce) {
        throw InternalError(
            "procedural continuous assignment: an `assign` names a member "
            "that owns no storage, which the front end refuses -- please "
            "report this as a bug");
      }
      return LowerForceOnMember(
          process, frame, std::move(label), pca, **member);
    }
  }

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
        pca.sensitivity_list, {});
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

  const hir::Expr& hir_target = hir_proc.exprs.Get(pce.target);
  auto member = ForcedMemberValueType(process, frame, hir_target);
  if (!member) return std::unexpected(std::move(member.error()));
  if (member->has_value()) {
    if (pce.level != support::TakeoverLevel::kForce) {
      throw InternalError(
          "procedural continuous assignment: a `deassign` names a member "
          "that owns no storage, which the front end refuses -- please report "
          "this as a bug");
    }
    // What the member shows becomes what drives it, stored as any value is
    // into the storage the force gave it, so whoever waits on the member is
    // told exactly when the release changed it (LRM 10.6.2). The member and
    // what follows it then name what drives it again.
    mir::Block scope;
    const WalkFrame in = frame.WithBlock(&scope);
    const auto named = [&] { return TakeoverTarget(process, in, hir_target); };
    auto driven = named();
    if (!driven) return std::unexpected(std::move(driven.error()));
    const mir::ExprId driver = MemberCall(
        scope, support::BuiltinFn::kDriverOfMember, {*driven},
        scope.exprs.Get(*driven).type);
    auto shown = named();
    if (!shown) return std::unexpected(std::move(shown.error()));
    scope.AppendStmt(
        mir::ExprStmt{
            .expr = scope.exprs.Add(BuildStoreExpr(
                process.Owner().Unit(), scope,
                AccessPath{.owner = *shown, .descent = {}},
                scope.exprs.Add(
                    mir::MakeCellLoadCallExpr(driver, **member))))});
    auto released = named();
    if (!released) return std::unexpected(std::move(released.error()));
    scope.AppendStmt(
        mir::ExprStmt{
            .expr = MemberCall(
                scope, support::BuiltinFn::kReleaseMember, {*released},
                unit.builtins.void_type)});
    return mir::Stmt{
        .label = std::move(label),
        .data = mir::BlockStmt{
            .scope = frame.current_block->child_scopes.Add(std::move(scope))}};
  }

  // One end per place the target names, so the statement is a block of them.
  BlockBuilder steps(frame);
  mir::Block& block = steps.Body();
  auto places = TakeoverPlaces(process, steps.Frame(), hir_target);
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

#include "lyra/lowering/hir_to_mir/clocking_block.hpp"

#include <array>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/lowering/hir_to_mir/synthesized_body.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/region.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The cell `field` of the scope's own object, as a place of `frame`'s block.
auto OwnCell(
    const StructuralScopeLowerer& lowerer, mir::Builder block,
    const WalkFrame& frame, mir::ClassFieldTarget field) -> mir::ExprId {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  return BuildOwnFieldExpr(
      unit_lowerer.Unit(), block, frame, field,
      FieldTypeOf(unit_lowerer, field));
}

// What the cell `field` holds, a value of `value_type`.
auto ReadOwnCell(
    const StructuralScopeLowerer& lowerer, mir::Builder block,
    const WalkFrame& frame, mir::ClassFieldTarget field, mir::TypeId value_type)
    -> mir::ExprId {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  return block.Read(block.Add(block.WrapperContents(
      unit.types, OwnCell(lowerer, block, frame, field), value_type)));
}

// `value` stored into the cell `field`, as a statement of `block`.
void StoreOwnCell(
    const StructuralScopeLowerer& lowerer, mir::Builder block,
    const WalkFrame& frame, mir::ClassFieldTarget field, mir::ExprId value) {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::ExprId cell = OwnCell(lowerer, block, frame, field);
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(BuildStoreExpr(
              unit, block, AccessPath{.owner = cell, .descent = {}}, value))});
}

// `region` itself of the slot the amount `amount` names in the scope's time
// unit, handed to `entries` with that scope's unit and precision powers (LRM
// 9.4.1).
auto RegionAfterArguments(
    const StructuralScopeLowerer& lowerer, mir::Builder block,
    support::Region region, mir::ExprId amount, DelayEntries entries)
    -> DelayRead {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  DelayRead read =
      ReadDelayAmount(unit, block, amount, lowerer.Resolution(), entries);
  read.operands.insert(
      read.operands.begin(),
      mir::BuildMachineIntLiteral(
          unit, block, static_cast<std::int64_t>(region)));
  return read;
}

// Whether the one-bit field `field` of the scope's own object is set, as a
// condition.
auto OwnBitIsSet(
    const StructuralScopeLowerer& lowerer, mir::Builder block,
    const WalkFrame& frame, mir::ClassFieldTarget field) -> mir::ExprId {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  return ReduceToCondition(
      unit, block,
      block.AsValue(unit.types, OwnCell(lowerer, block, frame, field)));
}

// The one-bit field `field` of the scope's own object set to `set`.
void SetOwnBit(
    const StructuralScopeLowerer& lowerer, mir::Builder block,
    const WalkFrame& frame, mir::ClassFieldTarget field, bool set) {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(
              mir::MakeAssignExpr(
                  unit.builtins, OwnCell(lowerer, block, frame, field),
                  ConvertToType(
                      unit, block,
                      mir::BuildIntLiteral(unit, block, set ? 1 : 0),
                      unit.builtins.bit1)))});
}

// The trigger of the event the cell `event` holds, as a statement of `frame`'s
// block.
void TriggerOwnEvent(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    mir::ClassFieldTarget event) {
  mir::Builder block = frame.Builder();
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(BuildTriggerCallExpr(
              lowerer.Owner(), block,
              ReadOwnCell(
                  lowerer, block, frame, event,
                  lowerer.Owner().Unit().builtins.event)))});
}

// LRM 14.16.2: what the drives of one output onto a variable give it in one
// Re-NBA pass reaches the variable as one value, the last each gave its part,
// and the passes stay apart. A drive that has reached the pass keeps, if none
// has yet, an effect for the end of it, then waits for that effect to let it
// write the driving variable: the effect first sets the driving variable to
// what the signal holds then, so what no drive wrote is what the signal had,
// wakes every drive of the pass inside the occurrence in the order each began
// to wait, and puts what they left onto the signal in one write.
auto BuildPassCommitStmt(
    const StructuralScopeLowerer& lowerer, const WalkFrame& outer_frame,
    const hir::Clockvar& output, const PassCommitFields& commit)
    -> diag::Result<mir::Stmt> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const mir::ClassFieldTarget driving =
      lowerer.TranslateStructuralDataObject(output.output->driving);
  const mir::TypeId value_type = unit_lowerer.TranslateType(
      hir_scope.structural_data_objects.Get(output.output->driving).type);
  mir::Block steps;
  mir::Builder body = outer_frame.Builder().Into(steps);
  const WalkFrame frame = outer_frame.WithBlock(&steps);
  mir::Block first;
  mir::Builder first_block = body.Into(first);
  const WalkFrame first_frame = frame.WithBlock(&first);

  ClosureBuilder pass_end(unit, first_frame, support::ClosureKind::kFn);
  RunDeferredBodyOnNoThread(unit_lowerer, first_frame, pass_end);
  const WalkFrame end_frame = pass_end.Frame();
  mir::Builder end = pass_end.Body();
  auto signal =
      lowerer.LowerAccessPath(hir_scope.exprs.Get(output.signal), end_frame);
  if (!signal) return std::unexpected(std::move(signal.error()));
  const ReadThenWritten settled =
      ReadThenWrite(unit, end_frame, *std::move(signal));
  StoreOwnCell(lowerer, end, end_frame, driving, settled.incoming);
  TriggerOwnEvent(lowerer, end_frame, commit.applying);
  end.AppendStmt(
      mir::ExprStmt{
          .expr = end.Add(BuildStoreExpr(
              unit, end, settled.place,
              ReadOwnCell(lowerer, end, end_frame, driving, value_type)))});
  SetOwnBit(lowerer, end, end_frame, commit.kept, false);

  SetOwnBit(lowerer, first_block, first_frame, commit.kept, true);
  mir::Expr keeper = BuildRegionEffectsCallExpr(
      unit_lowerer, first_block, support::BuiltinFn::kPassEndEffects);
  first_block.AppendStmt(
      mir::ExprStmt{
          .expr = first_block.Add(BuildEmplacedClosure(
              unit, first_frame, std::move(keeper), pass_end.BuildVoid()))});
  body.AppendIfThen(
      BuildConditionNot(
          unit, body, OwnBitIsSet(lowerer, body, frame, commit.kept)),
      std::move(first));

  auto applied = BuildEventObjectWaitStmt(
      lowerer, frame, body,
      ReadOwnCell(lowerer, body, frame, commit.applying, unit.builtins.event));
  if (!applied) return std::unexpected(std::move(applied.error()));
  body.AppendStmt(*std::move(applied));
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::BlockStmt{
          .scope = outer_frame.Builder().AddScope(std::move(steps))}};
}

}  // namespace

auto ClockingInputReadBy(
    const StructuralScopeLowerer& scope, const hir::Expr& expr)
    -> std::optional<ClockingInputRef> {
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data);
  const auto* routed = primary == nullptr
                           ? nullptr
                           : std::get_if<hir::RoutedValueRef>(&primary->data);
  if (routed == nullptr) return std::nullopt;
  const hir::StructuralScope& hir_scope = scope.HirScope();
  const hir::ValueRoute& route = hir_scope.routes.values.Get(routed->id);
  const auto* own = std::get_if<hir::InUnitBase>(&route.base);
  const auto* leaf = std::get_if<hir::StructuralDataObjectLeaf>(&route.leaf);
  if (own == nullptr || own->hops.value != 0 || !route.steps.empty() ||
      leaf == nullptr) {
    return std::nullopt;
  }
  for (const hir::ClockingBlockId block : hir_scope.clocking_blocks.Ids()) {
    const hir::ClockingBlockDecl& decl = hir_scope.clocking_blocks.Get(block);
    for (const hir::ClockvarId at : decl.clockvars.Ids()) {
      const hir::Clockvar& clockvar = decl.clockvars.Get(at);
      if (clockvar.input.has_value() &&
          clockvar.input->sample == leaf->object) {
        return ClockingInputRef{.block = block, .clockvar = at};
      }
    }
  }
  return std::nullopt;
}

auto LowerClockingSample(
    const StructuralScopeLowerer& scope, const WalkFrame& frame,
    ClockingInputRef input) -> diag::Result<mir::Expr> {
  const hir::StructuralScope& hir_scope = scope.HirScope();
  const hir::Clockvar& clockvar =
      hir_scope.clocking_blocks.Get(input.block).clockvars.Get(input.clockvar);
  return std::visit(
      Overloaded{
          // `#1step` takes the signal's sampled value (LRM 16.5.1).
          [&](const hir::OneStepSkew&) -> diag::Result<mir::Expr> {
            return scope.LowerExpr(
                hir_scope.exprs.Get(clockvar.signal),
                frame.WithReadsAsOf(ReadsAsOf::kPreponed));
          },
          // A delay takes the copy kept that far behind the signal.
          [&](const hir::DelaySkew&) -> diag::Result<mir::Expr> {
            const mir::TypeId type = scope.Owner().TranslateType(
                hir_scope.structural_data_objects.Get(clockvar.input->sample)
                    .type);
            mir::Builder block = frame.Builder();
            return mir::MakeCopyExpr(
                block.Add(block.WrapperContents(
                    scope.Owner().Unit().types,
                    OwnCell(
                        scope, block, frame,
                        *scope.ClockingFieldsOf(input.block)
                             .clockvars.Get(input.clockvar)
                             .copy),
                    type)),
                type);
          }},
      clockvar.input->skew);
}

auto LowerClockingSampler(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    hir::ClockingBlockId id) -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const hir::ClockingBlockDecl& block = hir_scope.clocking_blocks.Get(id);
  const ClockingFields& fields = lowerer.ClockingFieldsOf(id);
  return ScopeExecution(
      lowerer, ctor_frame, block.span,
      [&](const WalkFrame& each) -> diag::Result<void> {
        mir::Builder each_block = each.Builder();
        auto waited =
            BuildEventWaitStmt(lowerer, each, each_block, block.event);
        if (!waited) return std::unexpected(std::move(waited.error()));
        each_block.AppendStmt(*std::move(waited));
        TriggerOwnEvent(lowerer, each, fields.occurrence);

        ClosureBuilder observed(unit, each, support::ClosureKind::kFn);
        RunDeferredBodyOnNoThread(lowerer.Owner(), each, observed);
        const WalkFrame observed_frame = observed.Frame();
        for (const hir::ClockvarId at : block.clockvars.Ids()) {
          const hir::Clockvar& clockvar = block.clockvars.Get(at);
          if (!clockvar.input.has_value()) continue;
          mir::Builder into = observed.Body();
          auto value = LowerClockingSample(
              lowerer, observed_frame,
              ClockingInputRef{.block = id, .clockvar = at});
          if (!value) return std::unexpected(std::move(value.error()));
          StoreOwnCell(
              lowerer, into, observed_frame,
              lowerer.TranslateStructuralDataObject(clockvar.input->sample),
              into.Add(*std::move(value)));
        }
        TriggerOwnEvent(
            lowerer, observed_frame,
            lowerer.TranslateStructuralDataObject(block.block_event));
        mir::Expr keeper = BuildRegionEffectsCallExpr(
            lowerer.Owner(), each_block, support::BuiltinFn::kObservedEffects);
        each_block.AppendStmt(
            mir::ExprStmt{
                .expr = each_block.Add(BuildEmplacedClosure(
                    unit, each, std::move(keeper), observed.BuildVoid()))});
        return {};
      });
}

auto LowerClockingFollower(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    hir::ClockingBlockId id, hir::ClockvarId clockvar)
    -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const hir::Clockvar& followed =
      hir_scope.clocking_blocks.Get(id).clockvars.Get(clockvar);
  const auto* skew = std::get_if<hir::DelaySkew>(&followed.input->skew);
  if (skew == nullptr) {
    throw InternalError(
        "LowerClockingFollower: only an input skewed by a delay keeps a copy");
  }
  const mir::ClassFieldTarget copy =
      *lowerer.ClockingFieldsOf(id).clockvars.Get(clockvar).copy;
  return ScopeExecution(
      lowerer, ctor_frame, followed.span,
      [&](const WalkFrame& each) -> diag::Result<void> {
        mir::Builder each_block = each.Builder();
        auto value =
            lowerer.LowerExpr(hir_scope.exprs.Get(followed.signal), each);
        if (!value) return std::unexpected(std::move(value.error()));
        auto amount =
            lowerer.LowerExpr(hir_scope.exprs.Get(skew->delay.duration), each);
        if (!amount) return std::unexpected(std::move(amount.error()));
        auto [entry, arguments] = RegionAfterArguments(
            lowerer, each_block, support::Region::kNba,
            each_block.Add(*std::move(amount)),
            {.integral = support::BuiltinFn::kRegionEffectsAfter,
             .real = support::BuiltinFn::kRegionEffectsAfterReal});
        ClosureBuilder update(unit, each, support::ClosureKind::kFn);
        const mir::ExprId taken = SnapshotValueIntoClosure(
            lowerer.Owner(), each, update, each_block.Add(*std::move(value)));
        StoreOwnCell(lowerer, update.Body(), update.Frame(), copy, taken);
        mir::Expr keeper = BuildRegionEffectsCallExpr(
            lowerer.Owner(), each_block, entry, std::move(arguments));
        each_block.AppendStmt(
            mir::ExprStmt{
                .expr = each_block.Add(BuildEmplacedClosure(
                    unit, each, std::move(keeper), update.BuildVoid()))});
        auto waited = BuildValueChangeWaitStmt(
            each_block, each, lowerer, followed.signal_reads);
        if (!waited) return std::unexpected(std::move(waited.error()));
        each_block.AppendStmt(*std::move(waited));
        return {};
      });
}

auto LowerClockingLanding(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    hir::ClockingOutputLanding landing,
    std::optional<mir::VirtualDispatchRole> virtual_dispatch)
    -> diag::Result<mir::CallableDecl> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const hir::ClockingBlockDecl& block =
      hir_scope.clocking_blocks.Get(landing.block);
  const hir::Clockvar& output = block.clockvars.Get(landing.clockvar);
  const ClockingFields& fields = lowerer.ClockingFieldsOf(landing.block);
  const mir::ClassFieldTarget occurrence = fields.occurrence;
  const std::optional<PassCommitFields>& commit =
      fields.clockvars.Get(landing.clockvar).commit;
  const std::array params{unit.builtins.int_type};
  const mir::TypeId completion = CompletionPayloadType(unit, {});
  // The skew is waited out from the occurrence the drive matured at, which is
  // when this goes on; a delay of none is that occurrence's own slot.
  const auto delay_amount = [&](mir::Builder body, const WalkFrame& frame,
                                const std::optional<hir::DelayControl>& delay)
      -> diag::Result<mir::ExprId> {
    if (!delay.has_value()) return mir::BuildIntLiteral(unit, body, 0);
    auto amount =
        lowerer.LowerExpr(hir_scope.exprs.Get(delay->duration), frame);
    if (!amount) return std::unexpected(std::move(amount.error()));
    return body.Add(*std::move(amount));
  };
  return ScopeCoroutine(
      lowerer, ctor_frame, output.span, params, completion, virtual_dispatch,
      [&](const WalkFrame& frame,
          std::span<const mir::LocalId> taken) -> diag::Result<void> {
        mir::Builder body = frame.Builder();
        auto matured = BuildOccurrencesWaitStmt(
            lowerer, frame, body.ReadLocal(taken[0], unit.builtins.int_type),
            [&](mir::Builder in, const WalkFrame& at) {
              return ReadOwnCell(
                  lowerer, in, at, occurrence, unit.builtins.event);
            });
        if (!matured) return std::unexpected(std::move(matured.error()));
        body.AppendStmt(*std::move(matured));
        auto amount = std::visit(
            Overloaded{
                [&](const hir::NoSkew&) -> diag::Result<mir::ExprId> {
                  return delay_amount(body, frame, std::nullopt);
                },
                [&](const hir::DelaySkew& skew) -> diag::Result<mir::ExprId> {
                  return delay_amount(body, frame, skew.delay);
                },
                // The next such edge of the clocking event's own signal, and
                // the delay after it where the skew writes one.
                [&](const hir::EdgeSkew& skew) -> diag::Result<mir::ExprId> {
                  const auto* clock =
                      block.event.entries.size() == 1
                          ? std::get_if<hir::ValueChange>(
                                &block.event.entries.front().occurrence)
                          : nullptr;
                  if (clock == nullptr) {
                    throw InternalError(
                        "LowerClockingLanding: an output skewed by an edge is "
                        "carried only where the clocking event is an edge of "
                        "one signal");
                  }
                  hir::ValueChange edge = *clock;
                  edge.edge = skew.edge;
                  auto waited = BuildEventWaitStmt(
                      lowerer, frame, body,
                      hir::EventControl{
                          .entries = {hir::EventEntry{
                              .occurrence = std::move(edge),
                              .condition = std::nullopt}}});
                  if (!waited) {
                    return std::unexpected(std::move(waited.error()));
                  }
                  body.AppendStmt(*std::move(waited));
                  return delay_amount(body, frame, skew.delay);
                }},
            output.output->skew);
        if (!amount) return std::unexpected(std::move(amount.error()));
        auto [entry, arguments] = RegionAfterArguments(
            lowerer, body, support::Region::kReNba, *amount,
            {.integral = support::BuiltinFn::kResumeInRegionAfter,
             .real = support::BuiltinFn::kResumeInRegionAfterReal});
        arguments.insert(
            arguments.begin(), body.Add(BuildCurrentRuntimeCallExpr(unit)));
        const mir::ExprId resumed = body.Add(
            mir::MakeCallExpr(
                mir::Direct{.target = entry}, std::move(arguments),
                unit.builtins.wait));
        body.AppendStmt(BuildStopStmt(unit_lowerer, frame, resumed));
        if (commit.has_value()) {
          auto committed = BuildPassCommitStmt(lowerer, frame, output, *commit);
          if (!committed) {
            return std::unexpected(std::move(committed.error()));
          }
          body.AppendStmt(*std::move(committed));
        }
        body.AppendStmt(
            mir::ReturnStmt{
                .value = body.Add(
                    mir::Expr{
                        .data = mir::CompositeExpr{.parts = {}},
                        .type = completion})});
        return {};
      });
}

}  // namespace lyra::lowering::hir_to_mir

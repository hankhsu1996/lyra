#include "lyra/lowering/hir_to_mir/deferred_effect.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The member access of a drive's target whose instance its landing is made on,
// which the drive names (LRM 14.16).
auto TargetAccessOf(
    const ProcessLowerer& process, const hir::InstanceOfTarget& target)
    -> const hir::InterfaceMemberAccessExpr& {
  const auto* access = std::get_if<hir::InterfaceMemberAccessExpr>(
      &process.HirExprs().Get(target.access).data);
  if (access == nullptr) {
    throw InternalError(
        "TargetAccessOf: a drive landed on the instance its target reaches "
        "through a virtual interface names that target's member access");
  }
  return *access;
}

// The effects of the NBA region of the slot a delay names (LRM 9.4.5).
auto BuildNbaEffectsAfterCall(
    ProcessLowerer& process, WalkFrame frame, const hir::DelayControl& delay)
    -> diag::Result<mir::Expr> {
  auto& unit = process.Owner().Unit();
  mir::Builder block = frame.Builder();
  auto duration_or =
      process.LowerExpr(process.HirBody().exprs.Get(delay.duration), frame);
  if (!duration_or) return std::unexpected(std::move(duration_or.error()));
  DelayRead read = ReadDelayAmount(
      unit, block, block.Add(*std::move(duration_or)), process.Resolution(),
      {.integral = support::BuiltinFn::kNbaEffectsAfter,
       .real = support::BuiltinFn::kNbaEffectsAfterReal});
  return BuildRegionEffectsCallExpr(
      process.Owner(), block, read.entry, std::move(read.operands));
}

// The call of what lands a drive, made in the carrier's body on the instance
// the drive reaches, read where the drive is reached and carried in (LRM
// 14.16): this scope's own object for its own block, the end of a route for a
// block a name reaches, or the instance the target's member access reached
// through a virtual interface.
auto BuildLandingCallee(
    ProcessLowerer& process, const WalkFrame& outer_frame,
    ClosureBuilder& carrier, const hir::ClockingLanding& landing)
    -> mir::Callee {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Builder body = carrier.Body();
  const auto object_of = [&](const hir::LandingReceiver& receiver) {
    return std::visit(
        Overloaded{
            [&](const hir::RoutedObjectRef& routed) -> mir::ExprId {
              return outer_frame.Builder().AsValue(
                  unit.types, process.RouteEnd(outer_frame, routed.id));
            },
            [&](const hir::InstanceOfTarget& target) -> mir::ExprId {
              const auto bound = process.HeldInterfaceBoundTo(
                  TargetAccessOf(process, target).instance.handle);
              if (!bound.has_value()) {
                throw InternalError(
                    "BuildLandingCallee: a drive through a virtual interface "
                    "binds the instance it reaches before its target is "
                    "lowered");
              }
              return *bound;
            }},
        receiver);
  };
  return std::visit(
      Overloaded{
          [&](const hir::ClockingOutputLanding& own) -> mir::Callee {
            mir::Direct direct =
                process.EnclosingScopeLowerer().TranslateClockingLanding(own);
            direct.receiver = BuildObjectDeref(
                unit, body,
                BuildEnclosingScopeReceiver(
                    carrier.Frame(), unit, mir::EnclosingHops{.value = 0}));
            return direct;
          },
          [&](const hir::PublishedLanding& published) -> mir::Callee {
            const mir::ExprId held = SnapshotValueIntoClosure(
                unit_lowerer, outer_frame, carrier,
                object_of(published.receiver));
            const mir::VirtualSlot slot = unit_lowerer.PublishedSubroutineSlot(
                published.scope_class, published.callable);
            return mir::Virtual{
                .receiver = BuildObjectDeref(
                    unit, body, AsIntroducer(unit.types, body, held, slot)),
                .slot = slot};
          }},
      landing);
}

// The wait an event control is, built into the carrier's body. A repeat count
// is how many occurrences of that event the effect waits out; LRM 9.4.5 reads
// it once where the statement is reached, so it is lowered in the outer frame
// and carried in, and a count of none reaches the effect straight away.
auto BuildDueSlotWaitStmt(
    ProcessLowerer& process, WalkFrame outer_frame, ClosureBuilder& carrier,
    const hir::DelayOrEventControl& control) -> diag::Result<mir::Stmt> {
  UnitLowerer& unit_lowerer = process.Owner();
  const WalkFrame carrier_frame = carrier.Frame();
  mir::Builder body = carrier.Body();
  return std::visit(
      Overloaded{
          [&](const hir::EventControl& ec) -> diag::Result<mir::Stmt> {
            return BuildEventWaitStmt(process, carrier_frame, body, ec);
          },
          [&](const hir::RepeatedEventControl& r) -> diag::Result<mir::Stmt> {
            auto count_or = process.LowerExpr(
                process.HirBody().exprs.Get(r.count), outer_frame);
            if (!count_or) return std::unexpected(std::move(count_or.error()));
            const mir::ExprId count = SnapshotValueIntoClosure(
                unit_lowerer, outer_frame, carrier,
                outer_frame.Builder().Add(*std::move(count_or)));
            mir::Block loop_body;
            mir::Builder loop_body_builder = body.Into(loop_body);
            const WalkFrame loop_frame = carrier_frame.WithBlock(&loop_body);
            auto wait_or = BuildEventWaitStmt(
                process, loop_frame, loop_body_builder, r.event);
            if (!wait_or) return std::unexpected(std::move(wait_or.error()));
            loop_body_builder.AppendStmt(*std::move(wait_or));
            return BuildRepeatLoopStmt(
                unit_lowerer.Unit(), carrier_frame, body, count,
                body.AddScope(std::move(loop_body)));
          },
          // LRM 14.16: the drive is landed by the scope declaring its block,
          // on the instance its target is reached on, which waits for the
          // drive to mature at the occurrence its count of cycles names and
          // then for the output's skew, and returns in the Re-NBA region the
          // drive lands in. The count and the instance are read where the
          // statement is reached, as every operand of a deferred effect is.
          [&](const hir::ClockingDriveControl& d) -> diag::Result<mir::Stmt> {
            mir::CompilationUnit& unit = unit_lowerer.Unit();
            mir::Builder outer = outer_frame.Builder();
            mir::ExprId count = mir::BuildIntLiteral(unit, outer, 0);
            if (d.cycles.has_value()) {
              auto cycles = process.LowerExpr(
                  process.HirBody().exprs.Get(*d.cycles), outer_frame);
              if (!cycles) return std::unexpected(std::move(cycles.error()));
              count = outer.Add(*std::move(cycles));
            }
            if (outer.Get(count).type != unit.builtins.int_type) {
              count = outer.Add(BuildValueConversion(
                  unit, outer, count, unit.builtins.int_type));
            }
            const mir::ExprId cycles = SnapshotValueIntoClosure(
                unit_lowerer, outer_frame, carrier, count);
            const mir::Callee callee =
                BuildLandingCallee(process, outer_frame, carrier, d.landing);
            const mir::ExprId landed = body.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{.callee = callee, .arguments = {cycles}},
                    .type = SubroutineCallType(
                        unit, hir::SubroutineKind::kTask,
                        CompletionPayloadType(unit, {}))});
            return BuildAwaitStmt(unit_lowerer, body, landed);
          },
          [](const hir::DelayControl&) -> diag::Result<mir::Stmt> {
            throw InternalError(
                "BuildDueSlotWaitStmt: a delay names the slot outright, so "
                "nothing has to wait to find out which one it is");
          }},
      control);
}

}  // namespace

auto CarrierOf(const hir::DelayOrEventControl& control) -> Carrier {
  return std::visit(
      Overloaded{
          [](const hir::EventControl&) {
            return Carrier{
                .run = support::BuiltinFn::kRunDetached,
                .arrives_in_nba_region = true};
          },
          [](const hir::RepeatedEventControl&) {
            return Carrier{
                .run = support::BuiltinFn::kRunDetached,
                .arrives_in_nba_region = true};
          },
          [](const hir::ClockingDriveControl&) {
            return Carrier{
                .run = support::BuiltinFn::kRunObserving,
                .arrives_in_nba_region = false};
          },
          [](const hir::DelayControl&) -> Carrier {
            throw InternalError(
                "CarrierOf: a delay names the slot outright, so no execution "
                "carries the effect");
          }},
      control);
}

auto KeepInDueRegion(
    ProcessLowerer& process, WalkFrame frame,
    const std::optional<hir::DelayControl>& delay, mir::Expr closure)
    -> diag::Result<mir::Expr> {
  // An effect carrying no control is due in this slot's NBA region (LRM
  // 4.4.2.4).
  auto effects = delay.has_value()
                     ? BuildNbaEffectsAfterCall(process, frame, *delay)
                     : diag::Result<mir::Expr>(BuildRegionEffectsCallExpr(
                           process.Owner(), frame.Builder(),
                           support::BuiltinFn::kNbaEffects));
  if (!effects) return std::unexpected(std::move(effects.error()));
  return BuildEmplacedClosure(
      process.Owner().Unit(), frame, *std::move(effects), std::move(closure));
}

auto AppendArrivalAtDueRegion(
    ProcessLowerer& process, WalkFrame outer_frame, ClosureBuilder& carrier,
    const hir::DelayOrEventControl& control) -> diag::Result<void> {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Builder body = carrier.Body();

  auto wait_or = BuildDueSlotWaitStmt(process, outer_frame, carrier, control);
  if (!wait_or) return std::unexpected(std::move(wait_or.error()));
  body.AppendStmt(*std::move(wait_or));

  // The event has named the slot, and a nonblocking update lands in its
  // thread's NBA region of it, so the carrier goes there before applying it
  // (LRM 4.4.2.4). A synchronous drive's landing has already returned in the
  // Re-NBA region it lands in (LRM 14.16).
  if (CarrierOf(control).arrives_in_nba_region) {
    const mir::ExprId region_wait_id = body.Add(
        mir::MakeCallExpr(
            mir::Direct{.target = support::BuiltinFn::kResumeInNbaRegion}, {},
            unit.builtins.wait));
    body.AppendStmt(
        BuildStopStmt(unit_lowerer, carrier.Frame(), region_wait_id));
  }
  return {};
}

auto DriveInstanceOf(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::AssignExpr& assign)
    -> diag::Result<std::optional<DriveInstance>> {
  const auto* deferred = std::get_if<hir::NonBlockingEffect>(&assign.timing);
  if (deferred == nullptr || !deferred->control.has_value()) {
    return std::nullopt;
  }
  const auto* drive =
      std::get_if<hir::ClockingDriveControl>(&*deferred->control);
  if (drive == nullptr) return std::nullopt;
  const auto* published = std::get_if<hir::PublishedLanding>(&drive->landing);
  if (published == nullptr) return std::nullopt;
  const auto* target = std::get_if<hir::InstanceOfTarget>(&published->receiver);
  if (target == nullptr) return std::nullopt;
  const hir::InterfaceMemberAccessExpr& access =
      TargetAccessOf(process, *target);
  auto object = HeldInterfaceObject(process, frame, access.instance);
  if (!object) return std::unexpected(std::move(object.error()));
  return DriveInstance{
      .handle = access.instance.handle,
      .object = EvaluatedOnce(frame, *object)};
}

auto RunCarrier(
    ProcessLowerer& process, WalkFrame frame, support::BuiltinFn run,
    mir::Expr started) -> mir::Expr {
  mir::Builder block = frame.Builder();
  const mir::ExprId started_id = block.Add(std::move(started));
  const mir::ExprId runtime_id =
      block.Add(BuildCurrentRuntimeCallExpr(process.Owner().Unit()));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = run, .receiver = runtime_id},
              .arguments = {started_id}},
      .type = process.Owner().Unit().builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir

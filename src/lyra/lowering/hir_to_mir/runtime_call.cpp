#include "lyra/lowering/hir_to_mir/runtime_call.hpp"

#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/fixed_name.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/float_width.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildCurrentRuntimeCallExpr(const mir::CompilationUnit& unit)
    -> mir::Expr {
  return mir::MakeCurrentRuntimeCallExpr(unit.builtins.effects);
}

void AppendRuntimeEffectStmt(
    const UnitLowerer& unit_lowerer, mir::Builder block,
    support::BuiltinFn entry, std::vector<mir::ExprId> operands) {
  std::vector<mir::ExprId> arguments;
  arguments.reserve(operands.size() + 1);
  arguments.push_back(
      block.Add(BuildCurrentRuntimeCallExpr(unit_lowerer.Unit())));
  arguments.insert(arguments.end(), operands.begin(), operands.end());
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee = mir::Direct{.target = entry},
                          .arguments = std::move(arguments)},
                  .type = unit_lowerer.Unit().builtins.void_type})});
}

void AppendScheduleAfterReturnRequirement(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame) {
  if (!frame.runs_as_function_call) return;
  AppendRuntimeEffectStmt(
      unit_lowerer, frame.Builder(),
      support::BuiltinFn::kRequireCallerMayScheduleAfterReturn, {});
}

void RunDeferredBodyOnNoThread(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure) {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Builder outer = outer_frame.Builder();
  const mir::ExprId taken = outer.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kTakeNoThreadContext},
                  .arguments = {outer.Add(BuildCurrentRuntimeCallExpr(unit))}},
          .type = unit.builtins.no_thread_context});
  const mir::ExprId in_body =
      SnapshotValueIntoClosure(unit_lowerer, outer_frame, closure, taken);
  mir::Builder body = closure.Body();
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target = support::BuiltinFn::kEnterNoThread,
                                  .receiver = in_body},
                          .arguments = {}},
                  .type = unit.builtins.void_type})});
}

auto BuildWaitStmt(
    const UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId park)
    -> mir::Stmt {
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.Add(
              mir::Expr{
                  .data = mir::WaitExpr{.park = park},
                  .type = unit_lowerer.Unit().builtins.void_type})}};
}

auto BuildParkStmt(
    const UnitLowerer& unit_lowerer, mir::Builder block, mir::LocalId wait)
    -> mir::Stmt {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::ExprId runtime_id =
      block.Add(BuildCurrentRuntimeCallExpr(unit_lowerer.Unit()));
  const mir::ExprId wait_ptr = block.Add(
      mir::MakeAddressOfExpr(
          block.LocalPlace(wait, unit.builtins.wait), unit.builtins.wait_ptr));
  const mir::ExprId call_id = block.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = support::BuiltinFn::kParkAt},
          {runtime_id, wait_ptr}, unit.builtins.machine_bool));
  return BuildWaitStmt(unit_lowerer, block, call_id);
}

auto BuildStopStmt(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId built)
    -> mir::Stmt {
  return BuildParkStmt(
      unit_lowerer, frame.Builder(), DeclareLocal(frame, built));
}

auto BuildAwaitStmt(
    const UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId execution)
    -> mir::Stmt {
  const auto& awaited = unit_lowerer.Unit()
                            .types.Get(block.Get(execution).type)
                            .Get<mir::CoroutineType>();
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.Add(
              mir::Expr{
                  .data = mir::AwaitExpr{.execution = execution},
                  .type = awaited.payload})}};
}

auto BuildStringValueExpr(
    const mir::CompilationUnit& unit, mir::Builder block, std::string text)
    -> mir::ExprId {
  const mir::TypeId string_type = unit.builtins.string;
  const mir::ExprId literal = block.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = std::move(text)},
          .type = string_type});
  return block.Add(
      mir::Expr{
          .data =
              mir::CallExpr{.callee = mir::Construct{}, .arguments = {literal}},
          .type = string_type});
}

auto BuildFilesCallExpr(const UnitLowerer& unit_lowerer, mir::Builder block)
    -> mir::Expr {
  const auto& builtins = unit_lowerer.Unit().builtins;
  const mir::ExprId runtime_id =
      block.Add(BuildCurrentRuntimeCallExpr(unit_lowerer.Unit()));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kFiles,
                      .receiver = runtime_id},
              .arguments = {}},
      .type = builtins.files};
}

namespace {

// `current_runtime().<keeper>(arguments...)`, answering the keeper of deferred
// effects `keeper_type` points to.
auto BuildKeeperCallExpr(
    const UnitLowerer& unit_lowerer, mir::Builder block,
    support::BuiltinFn keeper, std::vector<mir::ExprId> arguments,
    mir::TypeId keeper_type) -> mir::Expr {
  const mir::ExprId runtime_id =
      block.Add(BuildCurrentRuntimeCallExpr(unit_lowerer.Unit()));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = keeper, .receiver = runtime_id},
              .arguments = std::move(arguments)},
      .type = keeper_type};
}

}  // namespace

auto BuildRegionEffectsCallExpr(
    const UnitLowerer& unit_lowerer, mir::Builder block,
    support::BuiltinFn entry, std::vector<mir::ExprId> arguments) -> mir::Expr {
  return BuildKeeperCallExpr(
      unit_lowerer, block, entry, std::move(arguments),
      unit_lowerer.Unit().builtins.deferred_effects_ptr);
}

auto BuildDiagnosticCallExpr(
    const mir::CompilationUnit& unit, mir::Builder block) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kDiagnostic,
                      .receiver = block.Add(
                          mir::MakeCurrentRuntimeCallExpr(
                              unit.builtins.effects))},
              .arguments = {}},
      .type = unit.builtins.diagnostic};
}

auto BuildNameBelowInstanceExpr(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::EnclosingHops out, std::string below) -> mir::ExprId {
  mir::Builder block = frame.Builder();
  const mir::ExprId instance = BuildEnclosingScopeReceiver(frame, unit, out);
  const mir::ExprId below_id =
      BuildStringValueExpr(unit, block, std::move(below));
  return block.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kHierarchicalPathOf,
                          .receiver = BuildObjectDeref(unit, block, instance)},
                  .arguments = {below_id}},
          .type = unit.builtins.string});
}

auto BuildHierarchicalNameExpr(
    const mir::CompilationUnit& unit, const WalkFrame& frame) -> mir::ExprId {
  if (frame.scope_name == nullptr) {
    throw InternalError(
        "BuildHierarchicalNameExpr: a construct was lowered in no scope a "
        "name is known for");
  }
  return std::visit(
      Overloaded{
          [&](const hir::WholeName& name) {
            return BuildStringValueExpr(unit, frame.Builder(), name.text);
          },
          [&](const hir::NameBelowInstance& name) {
            return BuildNameBelowInstanceExpr(
                unit, frame, mir::EnclosingHops{name.scopes_out}, name.text);
          }},
      *frame.scope_name);
}

auto BuildReportCallExpr(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    support::ReportEntries entries, mir::ExprId diagnostic_id,
    mir::ExprId text_id) -> mir::Expr {
  // A body that reports at its caller is the tool's own text, or one written
  // for a declaration the source gave none, so none of its scopes is one the
  // design's author wrote.
  if (frame.bindings->Provides(CallerLocationOrigin{})) {
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = entries.at_caller, .receiver = diagnostic_id},
                .arguments = {text_id}},
        .type = unit.builtins.void_type};
  }
  const mir::ExprId scope_id = BuildHierarchicalNameExpr(unit, frame);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = entries.in_scope, .receiver = diagnostic_id},
              .arguments = {text_id, scope_id}},
      .type = unit.builtins.void_type};
}

void AppendToolReportStmt(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    support::ReportEntries entries, base::DiagMessage text) {
  mir::Builder block = frame.Builder();
  const mir::ExprId diagnostic_id =
      block.Add(BuildDiagnosticCallExpr(unit, block));
  const mir::ExprId text_id =
      BuildStringValueExpr(unit, block, std::move(text).TakeText());
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(BuildReportCallExpr(
              unit, frame, entries, diagnostic_id, text_id))});
}

auto BuildFormatCallExpr(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::ExprId items_array) -> mir::Expr {
  const auto& builtins = unit.builtins;
  const mir::ExprId time_format_id = block.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kTimeFormat,
                          .receiver = block.Add(
                              mir::MakeCurrentRuntimeCallExpr(
                                  builtins.effects))},
                  .arguments = {}},
          .type = builtins.time_format});
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kFormat},
              .arguments = {items_array, time_format_id}},
      .type = builtins.string};
}

auto ReadDelayAmount(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId amount,
    TimeResolution resolution, DelayEntries entries) -> DelayRead {
  const auto with_powers = [&](support::BuiltinFn entry, mir::ExprId read) {
    return DelayRead{
        .entry = entry,
        .operands = {
            read,
            mir::BuildMachineIntLiteral(
                unit, block, std::int64_t{resolution.unit_power}),
            mir::BuildMachineIntLiteral(
                unit, block, std::int64_t{resolution.precision_power})}};
  };
  const auto* real = unit.types.Get(block.Get(amount).type).As<mir::RealType>();
  if (real == nullptr) return with_powers(entries.integral, amount);
  switch (real->width) {
    case support::FloatWidth::k64:
      return with_powers(entries.real, amount);
    case support::FloatWidth::k32:
      return with_powers(
          entries.real, ConvertToType(unit, block, amount, unit.builtins.real));
  }
  throw InternalError("hir_to_mir: unknown real width");
}

}  // namespace lyra::lowering::hir_to_mir

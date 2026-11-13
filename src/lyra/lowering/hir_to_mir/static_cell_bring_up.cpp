#include "lyra/lowering/hir_to_mir/static_cell_bring_up.hpp"

#include <cstdint>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto Word(
    const mir::CompilationUnit& unit, mir::Builder block, std::uint64_t value)
    -> mir::ExprId {
  return block.Add(
      mir::Expr{
          .data =
              mir::MachineIntLiteral{.value = static_cast<std::int64_t>(value)},
          .type = unit.builtins.machine_word});
}

// Calls `fn` on the control `binding` holds, where `frame` lowers.
void CallOnControl(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    const StaticCellBinding& binding, support::BuiltinFn fn,
    std::vector<mir::ExprId> arguments) {
  mir::Builder block = frame.Builder();
  const mir::ExprId cell = block.Add(BuildStaticStorageAccess(
      unit, frame, binding.home, binding.cell_type, mir::EnclosingHops{}));
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(
              mir::MakeCallExpr(
                  mir::Direct{.target = fn, .receiver = cell},
                  std::move(arguments), unit.builtins.void_type))});
}

// States what a statement's control is for -- the type and directive the
// assertion control tasks select it by (LRM 20.11), which also say what it
// keeps -- where its storage is constructed, so that it answers as what it is
// to whatever reaches it first, an initializer of another instance among them.
void DescribeAssertionControl(
    const ProcessLowerer& process, const hir::ProceduralBody& body,
    const WalkFrame& construct_frame, const StaticCellBinding& binding,
    const AssertionControlCell& control) {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Builder block = construct_frame.Builder();
  const hir::ControlledAssertionDecl& decl =
      body.controlled_assertions.Get(control.assertion);
  CallOnControl(
      unit, construct_frame, binding,
      support::BuiltinFn::kAssertionControlDescribe,
      {Word(unit, block, static_cast<std::uint64_t>(decl.type)),
       Word(
           unit, block,
           decl.directive.has_value()
               ? static_cast<std::uint64_t>(*decl.directive)
               : 0)});
}

// Makes a statement's control one the assertion control tasks select (LRM
// 20.11), before any process can run a task: held by the instance whose
// storage it is or, for a class's or a package's, by none, and within each
// named scope it stands in, which owns a disable target that a task naming the
// scope reaches it through.
void RegisterAssertionControl(
    ProcessLowerer& process, const WalkFrame& install_frame,
    const StaticCellBinding& binding, const AssertionControlCell& control) {
  mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Builder block = install_frame.Builder();
  const mir::ExprId runtime = block.Add(BuildCurrentRuntimeCallExpr(unit));
  std::visit(
      Overloaded{
          [&](const InstanceFieldHome&) {
            const mir::ExprId instance =
                block.Read(block.Add(MakeSelfRefExpr(install_frame)));
            CallOnControl(
                unit, install_frame, binding,
                support::BuiltinFn::kAssertionControlRegisterIn,
                {runtime, instance});
          },
          // One a class or the program holds stands on no instance.
          [&](const ClassCellHome&) {
            CallOnControl(
                unit, install_frame, binding,
                support::BuiltinFn::kAssertionControlRegister, {runtime});
          },
          [&](const UnitCellHome&) {
            CallOnControl(
                unit, install_frame, binding,
                support::BuiltinFn::kAssertionControlRegister, {runtime});
          }},
      binding.home);
  for (const hir::ProceduralScopeId scope : control.within) {
    const std::optional<StaticStorageHome>& target =
        process.Scopes().Get(scope).disable_target;
    if (!target.has_value()) continue;
    CallOnControl(
        unit, install_frame, binding,
        support::BuiltinFn::kAssertionControlRegisterWithin,
        {block.Add(BuildCurrentRuntimeCallExpr(unit)),
         CancellationTarget(process, install_frame, *target)});
  }
}

}  // namespace

void BringUpStaticCell(
    ProcessLowerer& process, const hir::ProceduralBody& body,
    const StorageBringUp& bring_up, const StaticCellBinding& binding) {
  std::visit(
      Overloaded{
          [&](const AssertionControlCell& control) {
            DescribeAssertionControl(
                process, body, bring_up.construct, binding, control);
            RegisterAssertionControl(
                process, bring_up.install, binding, control);
          },
          // The assertion installs its own attempts, beside its clock.
          [](const ConcurrentAttemptsCell&) {},
          // A history is filled where the scope keeps its sampled value
          // functions' histories, once what they read is armed.
          [](const SampledHistoryCell&) {}},
      binding.of);
}

}  // namespace lyra::lowering::hir_to_mir

#include "lyra/lowering/hir_to_mir/expression/system/assertion_control.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/support/assertion_control.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// What one call of a task applies, once each argument has been read: what it
// does, what it selects by and how far down it reaches, each a local of the
// frame holding the machine integer, and what it names -- nothing where the
// call writes no list, which is the whole design, and otherwise each name of
// the list that selects something.
struct Applied {
  mir::LocalId control;
  mir::LocalId types;
  mir::LocalId directives;
  mir::LocalId levels;
  std::optional<std::vector<hir::ExprId>> named;
};

// `operand` read as the machine integer the runtime takes it as, or `absent`
// where the call leaves it out, held in a local so each call the task makes
// reads it once evaluated.
auto IntegerArgument(
    ProcessLowerer& process, const WalkFrame& frame,
    std::optional<hir::ExprId> operand, std::uint64_t absent)
    -> diag::Result<mir::LocalId> {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Builder block = frame.Builder();
  if (!operand.has_value()) {
    return DeclareLocal(
        frame, mir::BuildMachineIntLiteral(
                   unit, block, static_cast<std::int64_t>(absent)));
  }
  auto lowered =
      process.LowerExpr(process.HirBody().exprs.Get(*operand), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return DeclareLocal(
      frame, BuildToInt64Call(unit, block, block.Add(*std::move(lowered))));
}

// What the call states, read in the order its arguments stand (LRM 20.11):
// `$assertcontrol`'s own four, or the fixed ones of the task the clause
// defines in its terms followed by the levels, and then the list.
auto ReadArguments(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call,
    const support::AssertionControlSystemSubroutineInfo& info)
    -> diag::Result<Applied> {
  const auto list_from =
      [&](std::size_t first) -> std::optional<std::vector<hir::ExprId>> {
    if (call.arguments.size() <= first) return std::nullopt;
    std::vector<hir::ExprId> named;
    for (std::size_t i = first; i < call.arguments.size(); ++i) {
      if (call.arguments[i].has_value()) named.push_back(*call.arguments[i]);
    }
    return named;
  };
  const auto read = [&](std::optional<hir::ExprId> operand,
                        std::uint64_t absent) {
    return IntegerArgument(process, frame, operand, absent);
  };
  return std::visit(
      Overloaded{
          [&](const support::AssertionControlStatedByCall&)
              -> diag::Result<Applied> {
            auto control = read(OptionalOperand(call, 0), 0);
            if (!control) return std::unexpected(std::move(control.error()));
            auto types =
                read(OptionalOperand(call, 1), support::kEveryAssertionType);
            if (!types) return std::unexpected(std::move(types.error()));
            auto directives =
                read(OptionalOperand(call, 2), support::kEveryDirective);
            if (!directives) {
              return std::unexpected(std::move(directives.error()));
            }
            auto levels = read(OptionalOperand(call, 3), 0);
            if (!levels) return std::unexpected(std::move(levels.error()));
            return Applied{
                .control = *control,
                .types = *types,
                .directives = *directives,
                .levels = *levels,
                .named = list_from(4)};
          },
          [&](const support::AssertionControlFixed& fixed)
              -> diag::Result<Applied> {
            auto control =
                read(std::nullopt, static_cast<std::uint64_t>(fixed.control));
            if (!control) return std::unexpected(std::move(control.error()));
            auto types = read(std::nullopt, fixed.types);
            if (!types) return std::unexpected(std::move(types.error()));
            auto directives = read(std::nullopt, support::kEveryDirective);
            if (!directives) {
              return std::unexpected(std::move(directives.error()));
            }
            auto levels = read(OptionalOperand(call, 0), 0);
            if (!levels) return std::unexpected(std::move(levels.error()));
            return Applied{
                .control = *control,
                .types = *types,
                .directives = *directives,
                .levels = *levels,
                .named = list_from(1)};
          }},
      info.form);
}

}  // namespace

auto LowerAssertionControlSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call,
    const support::AssertionControlSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  BlockBuilder steps(frame);
  const WalkFrame& step_frame = steps.Frame();
  mir::Builder block = steps.Body();
  auto applied = ReadArguments(process, step_frame, call, info);
  if (!applied) return std::unexpected(std::move(applied.error()));
  const mir::TypeId int64 = unit.builtins.machine_int64;
  const auto read = [&](mir::LocalId local) {
    return block.ReadLocal(local, int64);
  };
  const auto apply = [&](support::BuiltinFn entry,
                         std::vector<mir::ExprId> arguments) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target = entry,
                                    .receiver = block.Add(
                                        BuildCurrentRuntimeCallExpr(unit))},
                            .arguments = std::move(arguments)},
                    .type = unit.builtins.void_type})});
  };

  if (!applied->named.has_value()) {
    apply(
        support::BuiltinFn::kAssertControlOfDesign,
        {read(applied->control), read(applied->types),
         read(applied->directives), read(applied->levels)});
  }
  const auto of_scope = [&](const hir::RoutedObjectRef& object) {
    apply(
        support::BuiltinFn::kAssertControlOfScope,
        {block.AsValue(unit.types, process.RouteEnd(step_frame, object.id)),
         read(applied->control), read(applied->types),
         read(applied->directives), read(applied->levels)});
  };
  for (const hir::ExprId named :
       applied->named.value_or(std::vector<hir::ExprId>{})) {
    const auto& primary =
        std::get<hir::PrimaryExpr>(process.HirBody().exprs.Get(named).data);
    std::visit(
        Overloaded{
            [&](const hir::RoutedObjectRef& object) { of_scope(object); },
            [&](const hir::RoutedObjectsRef& objects) {
              for (const hir::RoutedObjectRef& object : objects.objects) {
                of_scope(object);
              }
            },
            [&](const hir::LoopBlocksRef& loop) {
              apply(
                  support::BuiltinFn::kAssertControlOfLoop,
                  {ReachPublishedLoop(
                       process.Owner(), block,
                       block.AsValue(
                           unit.types,
                           process.RouteEnd(step_frame, loop.holder.id)),
                       loop.scope_class, loop.loop),
                   read(applied->control), read(applied->types),
                   read(applied->directives), read(applied->levels)});
            },
            [&](const hir::DisableTarget& extent) {
              apply(
                  support::BuiltinFn::kAssertControlOfBlock,
                  {NamedTarget(process, step_frame, extent),
                   read(applied->control), read(applied->types),
                   read(applied->directives)});
            }},
        std::get<hir::ScopeArgumentRef>(primary.data).scope);
  }
  mir::Stmt stmt = steps.BuildStatement();
  stmt.label = std::move(label);
  return stmt;
}

}  // namespace lyra::lowering::hir_to_mir

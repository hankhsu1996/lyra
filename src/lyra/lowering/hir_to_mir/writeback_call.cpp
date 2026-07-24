#include "lyra/lowering/hir_to_mir/writeback_call.hpp"

#include <algorithm>
#include <cstddef>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/completion_payload.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto PlanWritebackCall(
    ProcessLowerer& process, const hir::CallExpr& call,
    mir::TypeId call_result_type) -> std::optional<WritebackCall> {
  UnitLowerer& unit_lowerer = process.Owner();
  // The payload's result component is the call's own result type, unless the
  // callee is a task or void function, which yields none.
  const std::optional<mir::TypeId> result_type =
      call_result_type == unit_lowerer.Unit().builtins.void_type
          ? std::nullopt
          : std::optional<mir::TypeId>{call_result_type};
  if (const auto* ref =
          std::get_if<hir::StructuralSubroutineRef>(&call.callee)) {
    const hir::SubroutineDecl& decl =
        process.EnclosingScopeLowerer().LookupHirSubroutine(
            ref->hops, ref->subroutine);
    if (!std::ranges::any_of(decl.params, [](const hir::SubroutineParam& p) {
          return hir::RequiresWriteback(p.direction);
        })) {
      return std::nullopt;
    }
    WritebackCall plan;
    plan.is_task = decl.kind == hir::SubroutineKind::kTask;
    plan.result_type = result_type;
    plan.args.reserve(decl.params.size());
    for (const auto& param : decl.params) {
      plan.args.push_back(
          WritebackCall::Arg{
              .direction = param.direction,
              .formal_type = unit_lowerer.TranslateType(
                  decl.body.procedural_vars.Get(param.var).type)});
    }
    plan.callee = process.EnclosingScopeLowerer().TranslateStructuralSubroutine(
        ref->hops, ref->subroutine);
    plan.receiver_hops = mir::EnclosingHops{.value = ref->hops.value};
    return plan;
  }
  if (const auto* ref =
          std::get_if<hir::ExternalUnitSubroutineRef>(&call.callee)) {
    if (!std::ranges::any_of(ref->params, [](const hir::ExternalUnitParam& p) {
          return hir::RequiresWriteback(p.direction);
        })) {
      return std::nullopt;
    }
    WritebackCall plan;
    plan.is_task = ref->kind == hir::SubroutineKind::kTask;
    plan.result_type = result_type;
    plan.args.reserve(ref->params.size());
    for (const auto& param : ref->params) {
      plan.args.push_back(
          WritebackCall::Arg{
              .direction = param.direction,
              .formal_type = unit_lowerer.TranslateType(param.type)});
    }
    plan.callee =
        mir::Direct{.target = unit_lowerer.MakeExternalCallableTarget(*ref)};
    plan.receiver_hops = std::nullopt;
    return plan;
  }
  return std::nullopt;
}

namespace {

// One value the completion payload carries back to a caller place: the actual
// lvalue to write and which payload component supplies it.
struct CompletionWriteback {
  mir::ExprId place{};
  std::size_t component_index = 0;
  mir::TypeId type{};
};

// What the shared body left for the caller to sink: the completion value bound
// to a local, the payload shape needed to project components, and the function
// result component's type (component 0, absent for a task or void function).
struct WritebackBody {
  mir::LocalId completion;
  mir::TypeId payload_type;
  std::size_t component_count = 0;
  std::optional<mir::TypeId> result_type;
};

// Emits into `block` / `frame` the whole call boundary: the leading argument
// (an intra-unit receiver or the ambient runtime handle), each actual bound by
// its
// direction (an `input` value, an `inout`'s incoming value, an `output`'s
// nothing, a `ref` cell alias), the call itself (awaited for a task), the
// completion local, and each output / inout component written back to its
// actual place. The function result component is left for the caller to sink --
// a statement assigns it to its target, an expression returns it.
auto EmitWritebackCallBody(
    ProcessLowerer& process, const WalkFrame& frame, mir::Block& block,
    const hir::CallExpr& call, const WritebackCall& plan)
    -> diag::Result<WritebackBody> {
  if (call.arguments.size() != plan.args.size()) {
    throw InternalError(
        "EmitWritebackCallBody: argument / formal count "
        "mismatch");
  }
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::ProceduralBody& hir_proc = process.HirBody();

  // The completion layout the callee body and this call site share: the
  // function return (if any) then each output / inout value in declaration
  // order, the same order the callee packs them.
  std::vector<mir::TypeId> components;
  if (plan.result_type.has_value()) components.push_back(*plan.result_type);
  for (const auto& arg : plan.args) {
    if (hir::RequiresWriteback(arg.direction)) {
      components.push_back(arg.formal_type);
    }
  }
  const mir::TypeId payload_type = NormalizeCompletionPayload(unit, components);
  const mir::TypeId call_result_type =
      plan.is_task ? unit.types.CoroutineOf(payload_type) : payload_type;

  std::vector<mir::ExprId> call_args;
  call_args.reserve(call.arguments.size() + 1);
  // The leading argument is the callee's ambient handle: the enclosing object's
  // receiver for an intra-unit callable, the runtime handle for a
  // receiver-less cross-unit one.
  call_args.push_back(
      plan.receiver_hops.has_value()
          ? BuildEnclosingScopeReceiver(frame, unit, *plan.receiver_hops)
          : block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer)));

  std::vector<CompletionWriteback> writebacks;
  std::size_t next_component = plan.result_type.has_value() ? 1 : 0;

  for (std::size_t i = 0; i < call.arguments.size(); ++i) {
    const hir::ParamDirection dir = plan.args[i].direction;
    if (!call.arguments[i].has_value()) {
      throw InternalError(
          "EmitWritebackCallBody: positional argument "
          "unexpectedly elided");
    }
    const hir::Expr& hir_arg = hir_proc.exprs.Get(*call.arguments[i]);
    const mir::TypeId formal_type = plan.args[i].formal_type;

    // An `output` passes no argument; an `inout` passes its incoming value.
    // Both bind the actual place for a post-completion writeback.
    if (dir == hir::ParamDirection::kOutput ||
        dir == hir::ParamDirection::kInOut) {
      auto place_or = process.LowerLhsExpr(hir_arg, frame);
      if (!place_or) return std::unexpected(std::move(place_or.error()));
      const mir::ExprId place = block.exprs.Add(*std::move(place_or));
      if (dir == hir::ParamDirection::kInOut) {
        auto value_or = process.LowerExpr(hir_arg, frame);
        if (!value_or) return std::unexpected(std::move(value_or.error()));
        call_args.push_back(block.exprs.Add(*std::move(value_or)));
      }
      writebacks.push_back(
          {.place = place,
           .component_index = next_component++,
           .type = formal_type});
      continue;
    }

    // A `ref` / const-ref formal aliases the actual's cell -- pass the cell
    // (or a `ScopedMutation` for selector / range chains on an observable
    // cell) so the body's `Ref<T>` binds the underlying storage.
    if (dir == hir::ParamDirection::kRef ||
        dir == hir::ParamDirection::kConstRef) {
      auto arg_or = process.LowerLhsExpr(hir_arg, frame);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      mir::ExprId actual_id = block.exprs.Add(*std::move(arg_or));
      const mir::ExprId root_id = FindLhsRootId(unit, block, actual_id);
      const bool root_is_cell = mir::IsObservableCellType(
          unit.types.Get(block.exprs.Get(root_id).type));
      if (root_is_cell && root_id != actual_id) {
        const mir::ExprId services_id =
            block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
        actual_id =
            RewriteLhsRootWithMutate(unit, block, actual_id, services_id);
      }
      call_args.push_back(BuildReferenceArg(
          unit, block, actual_id, block.exprs.Get(actual_id).type));
      continue;
    }

    auto arg_or = process.LowerExpr(hir_arg, frame);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    call_args.push_back(block.exprs.Add(*std::move(arg_or)));
  }

  const mir::ExprId call_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = plan.callee, .arguments = std::move(call_args)},
          .type = call_result_type});

  // The completion value is the awaited payload for a task, the call result for
  // a function; bind it to a local that every writeback projects from (and that
  // the backend reuses as the task's completion slot).
  const mir::ExprId completion_value =
      plan.is_task ? block.exprs.Add(
                         mir::Expr{
                             .data = mir::AwaitExpr{.awaitable = call_id},
                             .type = payload_type})
                   : call_id;
  const mir::LocalId completion = frame.bindings->DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_completion", .type = payload_type});
  block.AppendStmt(
      mir::LocalDeclStmt{.target = completion, .init = completion_value});

  const mir::ExprId services_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  const mir::TypeId void_type = unit.builtins.void_type;
  for (const CompletionWriteback& wb : writebacks) {
    const mir::ExprId value_id = ProjectCompletionComponent(
        block, completion, payload_type, components.size(), wb.component_index,
        wb.type);
    const mir::Expr assign_expr = BuildObservableAssignExpr(
        unit, block, services_id, wb.place, value_id, std::nullopt, wb.type,
        void_type);
    block.AppendStmt(mir::ExprStmt{.expr = block.exprs.Add(assign_expr)});
  }

  return WritebackBody{
      .completion = completion,
      .payload_type = payload_type,
      .component_count = components.size(),
      .result_type = plan.result_type};
}

}  // namespace

auto LowerWritebackCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const WritebackCall& plan,
    std::optional<hir::ExprId> assign_target) -> diag::Result<mir::Stmt> {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  // The result target lvalue is lowered before the call arguments, so a `lhs =
  // f(...)` evaluates its left side first (LRM 11.4); the function-return
  // component is written into it after completion.
  std::optional<mir::ExprId> result_place;
  if (plan.result_type.has_value() && assign_target.has_value()) {
    auto lhs_or =
        process.LowerLhsExpr(hir_proc.exprs.Get(*assign_target), wrapper_frame);
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    result_place = wrapper.exprs.Add(*std::move(lhs_or));
  }

  auto body =
      EmitWritebackCallBody(process, wrapper_frame, wrapper, call, plan);
  if (!body) return std::unexpected(std::move(body.error()));

  if (result_place.has_value()) {
    const mir::ExprId services_id =
        wrapper.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
    const mir::ExprId value_id = ProjectCompletionComponent(
        wrapper, body->completion, body->payload_type, body->component_count, 0,
        *body->result_type);
    const mir::Expr assign_expr = BuildObservableAssignExpr(
        unit, wrapper, services_id, *result_place, value_id, std::nullopt,
        *body->result_type, unit.builtins.void_type);
    wrapper.AppendStmt(mir::ExprStmt{.expr = wrapper.exprs.Add(assign_expr)});
  }

  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

auto LowerWritebackCallExpr(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const WritebackCall& plan) -> diag::Result<mir::Expr> {
  ClosureBuilder closure(process.Owner().Unit(), frame);
  auto body = EmitWritebackCallBody(
      process, closure.Frame(), closure.Body(), call, plan);
  if (!body) return std::unexpected(std::move(body.error()));
  if (!body->result_type.has_value()) {
    throw InternalError(
        "LowerWritebackCallExpr: a call in expression position "
        "has no result to yield");
  }
  const mir::ExprId result_id = ProjectCompletionComponent(
      closure.Body(), body->completion, body->payload_type,
      body->component_count, 0, *body->result_type);
  return BuildClosureCallExpr(
      process.Owner().Unit(), *frame.current_block, closure.Build(result_id));
}

}  // namespace lyra::lowering::hir_to_mir

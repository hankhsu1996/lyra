#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/completion_payload.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The callee-interface facts a subroutine call needs, read uniformly for an
// intra-unit callee (from its HIR declaration) and a cross-unit one (from the
// by-name reference's recorded signature). The callee's kind states its call
// protocol; each argument's direction says how the call transfers it, and its
// component says what the completion hands back to it. `receiver_hops` is the
// enclosing-scope level of an intra-unit callee's receiver; it is absent for a
// receiver-less cross-unit callee, which takes the ambient runtime handle in
// that position instead.
struct SubroutineCallee {
  hir::SubroutineKind kind = hir::SubroutineKind::kFunction;
  std::optional<mir::TypeId> result_type;
  CompletionLayout completion;
  mir::Callee callee;
  std::optional<mir::EnclosingHops> receiver_hops;
};

// Reads the callee's interface out of whichever HIR reference names it.
// Nothing for a callee that is not a user subroutine -- a system, builtin,
// imported, or foreign one -- each of which has its own boundary.
template <ExprLowerer Lowerer>
auto ResolveSubroutineCallee(
    Lowerer& lowerer, const hir::CallExpr& call, mir::TypeId call_result_type)
    -> std::optional<SubroutineCallee> {
  auto& unit_lowerer = lowerer.Owner();
  // The payload's result component is the call's own result type, unless the
  // callee is a task or void function, which yields none.
  const std::optional<mir::TypeId> result_type =
      call_result_type == unit_lowerer.Unit().builtins.void_type
          ? std::nullopt
          : std::optional<mir::TypeId>{call_result_type};
  if (const auto* ref =
          std::get_if<hir::StructuralSubroutineRef>(&call.callee)) {
    const hir::SubroutineDecl& decl =
        lowerer.LookupHirSubroutine(ref->hops, ref->subroutine);
    SubroutineCallee plan;
    plan.kind = decl.kind;
    plan.result_type = result_type;
    plan.completion =
        BuildCompletionLayout(CalleeFormalsOf(unit_lowerer, decl), result_type);
    plan.callee =
        lowerer.TranslateStructuralSubroutine(ref->hops, ref->subroutine);
    plan.receiver_hops = mir::EnclosingHops{.value = ref->hops.value};
    return plan;
  }
  if (const auto* ref =
          std::get_if<hir::ExternalUnitSubroutineRef>(&call.callee)) {
    std::vector<CalleeFormal> formals;
    formals.reserve(ref->params.size());
    for (const auto& param : ref->params) {
      formals.push_back(
          CalleeFormal{
              .direction = param.direction,
              .type = unit_lowerer.TranslateType(param.type)});
    }
    SubroutineCallee plan;
    plan.kind = ref->kind;
    plan.result_type = result_type;
    plan.completion = BuildCompletionLayout(formals, result_type);
    plan.callee =
        mir::Direct{.target = unit_lowerer.MakeExternalCallableTarget(*ref)};
    plan.receiver_hops = std::nullopt;
    return plan;
  }
  return std::nullopt;
}

// A component landing in an actual is what gives a call something to sequence
// once it completes; with none, the call stands on its own.
auto WritesBack(const SubroutineCallee& plan) -> bool {
  return std::ranges::any_of(
      plan.completion.formals, [](const CompletionLayout::Formal& formal) {
        return formal.component.has_value();
      });
}

// One value the completion payload carries back to a caller place: the actual
// lvalue to write and which payload component supplies it.
struct CompletionWriteback {
  mir::ExprId place{};
  base::ComponentIndex component_index{};
  mir::TypeId type{};
};

// The call itself, still unowned, alongside what a caller needs to consume its
// completion: the payload's shape and the actual places its written-back
// components land in.
struct EmittedCall {
  mir::Expr call;
  mir::TypeId payload_type;
  std::vector<CompletionWriteback> writebacks;
};

// Emits the call boundary into `frame`: the leading argument (an intra-unit
// receiver or the ambient runtime handle) and each actual bound by its
// direction -- an `input` value, an `inout`'s incoming value, an `output`'s
// nothing, a `ref` cell alias. The call is typed with the protocol its callee
// states, so whether the caller awaits it is readable from the call alone.
template <ExprLowerer Lowerer>
auto EmitSubroutineCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const SubroutineCallee& plan) -> diag::Result<EmittedCall> {
  if (call.arguments.size() != plan.completion.formals.size()) {
    throw InternalError("EmitSubroutineCall: argument / formal count mismatch");
  }
  auto& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const auto& hir_exprs = lowerer.HirExprs();
  mir::Block& block = *frame.current_block;

  const mir::TypeId payload_type =
      CompletionPayloadType(unit, plan.completion.components);
  const mir::TypeId call_result_type =
      SubroutineCallType(unit, plan.kind, payload_type);

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

  // The arity was matched against the completion layout above, and a user
  // call fills every position it declares.
  const std::vector<hir::ExprId> operands = RequiredOperands(call);
  for (std::size_t i = 0; i < operands.size(); ++i) {
    const CompletionLayout::Formal& formal = plan.completion.formals[i];
    const hir::ParamDirection dir = formal.direction;
    const hir::Expr& hir_arg = hir_exprs.Get(operands[i]);
    const mir::TypeId formal_type = formal.type;

    switch (dir) {
      // An `output` passes no argument; an `inout` passes its incoming value.
      // Both bind the actual place for a post-completion writeback.
      case hir::ParamDirection::kOutput:
      case hir::ParamDirection::kInOut: {
        auto place_or = lowerer.LowerLhsExpr(hir_arg, frame);
        if (!place_or) return std::unexpected(std::move(place_or.error()));
        const mir::ExprId place = block.exprs.Add(*std::move(place_or));
        if (dir == hir::ParamDirection::kInOut) {
          auto value_or = lowerer.LowerExpr(hir_arg, frame);
          if (!value_or) return std::unexpected(std::move(value_or.error()));
          call_args.push_back(block.exprs.Add(*std::move(value_or)));
        }
        writebacks.push_back(
            {.place = place,
             .component_index = *formal.component,
             .type = formal_type});
        break;
      }

      // A ref / const-ref formal aliases what the actual designates (LRM
      // 13.5.2). A bare actual is lent as it stands, so a reference over a
      // capability wrapper aliases the wrapper and keeps the wrapper's own
      // access -- the update event a write fires included. A projected actual
      // designates part of a value, which is not a place a reference can alias,
      // so what is lent is the storage the chain descends into.
      case hir::ParamDirection::kRef:
      case hir::ParamDirection::kConstRef: {
        auto arg_or = lowerer.LowerLhsExpr(hir_arg, frame);
        if (!arg_or) return std::unexpected(std::move(arg_or.error()));
        mir::ExprId actual_id = block.exprs.Add(*std::move(arg_or));
        if (FindLhsRootId(unit, block, actual_id) != actual_id) {
          actual_id = StoragePlaceOf(unit, block, actual_id);
        }
        call_args.push_back(BuildReferenceArg(
            unit, block, actual_id, block.exprs.Get(actual_id).type));
        break;
      }

      case hir::ParamDirection::kInput: {
        auto arg_or = lowerer.LowerExpr(hir_arg, frame);
        if (!arg_or) return std::unexpected(std::move(arg_or.error()));
        call_args.push_back(block.exprs.Add(*std::move(arg_or)));
        break;
      }
    }
  }

  return EmittedCall{
      .call =
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee = plan.callee, .arguments = std::move(call_args)},
              .type = call_result_type},
      .payload_type = payload_type,
      .writebacks = std::move(writebacks)};
}

// Sequences a call whose completion carries values back to caller places: bind
// the completion, write each component to its actual, then yield the result.
// Statements in expression position are a closure invoked where it is built,
// and the closure states the same protocol its callee does -- a task's
// completion is awaited inside the body, so the body completes as a coroutine
// its own caller awaits in turn (LRM 13.3, 13.5).
template <ExprLowerer Lowerer>
auto LowerWritingBackCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const SubroutineCallee& plan) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  ClosureBuilder closure(unit, frame);

  auto emitted = EmitSubroutineCall(lowerer, closure.Frame(), call, plan);
  if (!emitted) return std::unexpected(std::move(emitted.error()));

  mir::Block& body = closure.Body();
  const mir::TypeId payload_type = emitted->payload_type;
  const mir::ExprId call_id = body.exprs.Add(std::move(emitted->call));
  const mir::ExprId completion_value =
      unit.types.IsCoroutine(body.exprs.Get(call_id).type)
          ? body.exprs.Add(
                mir::Expr{
                    .data = mir::AwaitExpr{.awaitable = call_id},
                    .type = payload_type})
          : call_id;
  const mir::LocalId completion = closure.Frame().bindings->DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_completion", .type = payload_type});
  body.AppendStmt(
      mir::LocalDeclStmt{.target = completion, .init = completion_value});

  for (const CompletionWriteback& wb : emitted->writebacks) {
    const mir::ExprId value_id = ProjectCompletionComponent(
        body, completion, payload_type, wb.component_index, wb.type);
    const mir::Expr assign_expr =
        BuildStoreExpr(unit, body, wb.place, value_id, std::nullopt, wb.type);
    body.AppendStmt(mir::ExprStmt{.expr = body.exprs.Add(assign_expr)});
  }

  // A task's completion is awaited inside the body, so the body completes as a
  // coroutine and the expression is that coroutine: the enabler awaits it, the
  // same protocol a bare task enable states.
  if (plan.kind == hir::SubroutineKind::kTask) {
    return closure.BuildCoroutine();
  }
  mir::Expr closure_value =
      plan.result_type.has_value()
          ? closure.Build(ProjectCompletionComponent(
                body, completion, payload_type, base::ComponentIndex{},
                *plan.result_type))
          : closure.BuildVoid();
  return BuildClosureCallExpr(
      unit, *frame.current_block, std::move(closure_value));
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    mir::TypeId result_type) -> std::optional<diag::Result<mir::Expr>> {
  const std::optional<SubroutineCallee> callee =
      ResolveSubroutineCallee(lowerer, call, result_type);
  if (!callee.has_value()) return std::nullopt;

  if (WritesBack(*callee)) {
    // Writing a value back reaches a caller's storage, which only a procedural
    // statement does; the frontend rejects an output / inout call outside
    // procedural code (LRM 13.4), so one arriving in a structural context is a
    // compiler-invariant violation rather than a user-diagnosable form.
    if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
      return LowerWritingBackCall(lowerer, frame, call, *callee);
    } else {
      throw InternalError(
          "LowerSubroutineCall: a structural subroutine call carries an "
          "output or inout argument");
    }
  }

  // With nothing to sequence, the call is the expression, and its result is
  // read straight out of the completion it yields. A void callee's completion
  // carries no component to read, so the call stands as the expression itself.
  auto emitted = EmitSubroutineCall(lowerer, frame, call, *callee);
  if (!emitted) {
    return diag::Result<mir::Expr>{std::unexpected(std::move(emitted.error()))};
  }
  if (!callee->result_type.has_value()) {
    return diag::Result<mir::Expr>{std::move(emitted->call)};
  }
  mir::Block& block = *frame.current_block;
  const mir::ExprId completion = block.exprs.Add(std::move(emitted->call));
  return diag::Result<mir::Expr>{mir::Expr{
      .data =
          mir::TupleGetExpr{
              .tuple = completion, .index = base::ComponentIndex{}},
      .type = *callee->result_type}};
}

template auto LowerSubroutineCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&, mir::TypeId)
    -> std::optional<diag::Result<mir::Expr>>;
template auto LowerSubroutineCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&, mir::TypeId)
    -> std::optional<diag::Result<mir::Expr>>;

}  // namespace lyra::lowering::hir_to_mir

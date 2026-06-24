#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/completion_payload.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/aggregates.hpp"
#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/lowering/hir_to_mir/expression/inside.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/statement/assignment.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/statement/branches.hpp"
#include "lyra/lowering/hir_to_mir/statement/flow.hpp"
#include "lyra/lowering/hir_to_mir/statement/fork_join.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto DispatchLowerExpr(
    ProcessLowerer& process, const hir::Expr& expr, WalkFrame frame,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerHirPrimaryExprProc(process, frame, p.data, result_type);
          },
          [&](const hir::UnaryExpr& u) -> diag::Result<mir::Expr> {
            return LowerHirUnaryExpr(process, frame, u, result_type);
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            return LowerHirBinaryExpr(process, frame, b, result_type);
          },
          [&](const hir::ConditionalExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConditionalExpr(process, frame, c, result_type);
          },
          [&](const hir::AssignExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignExprProc(
                process, frame, a, expr.span, result_type);
          },
          [&](const hir::IncDecExpr& inc) -> diag::Result<mir::Expr> {
            return LowerHirIncDecExprProc(process, frame, inc, result_type);
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            return LowerHirConversionExpr(process, frame, cv, result_type);
          },
          [&](const hir::CallExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirCallExpr(process, frame, c, expr.span, result_type);
          },
          [&](const hir::InsideExpr& in) -> diag::Result<mir::Expr> {
            return LowerHirInsideExpr(process, frame, in, result_type);
          },
          [&](const hir::ElementSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirElementSelectExpr(process, frame, sel, result_type);
          },
          [&](const hir::RangeSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirRangeSelectExpr(process, frame, sel, result_type);
          },
          [&](const hir::MemberAccessExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirMemberAccessExpr(process, frame, sel, result_type);
          },
          [&](const hir::ConcatExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConcatExpr(process, frame, c, result_type);
          },
          [&](const hir::ReplicationExpr& r) -> diag::Result<mir::Expr> {
            return LowerHirReplicationExpr(process, frame, r, result_type);
          },
          [&](const hir::AssignmentPatternExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternExpr(
                process, frame, a, result_type);
          },
          [&](const hir::AssignmentPatternReplicationExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternReplicationExpr(
                process, frame, a, result_type);
          },
          [&](const hir::DynamicArrayNewExpr& n) -> diag::Result<mir::Expr> {
            return LowerHirDynamicArrayNewExprProc(
                process, frame, n, result_type);
          },
          [&](const hir::AssociativeAssignmentPatternExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssociativeAssignmentPatternExpr(
                process, frame, a, result_type);
          },
      },
      expr.data);
}

}  // namespace

auto ProcessLowerer::LowerExpr(const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = module_->TranslateType(expr.type);
  auto raw_or = DispatchLowerExpr(*this, expr, frame, result_type);
  if (!raw_or) return raw_or;
  if (mir::IsObservableCellType(module_->Unit().GetType(raw_or->type))) {
    const mir::ExprId cell_id =
        frame.current_block->exprs.Add(*std::move(raw_or));
    return mir::MakeObservableGetCallExpr(cell_id, result_type);
  }
  return raw_or;
}

auto ProcessLowerer::LowerLhsExpr(const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = module_->TranslateType(expr.type);
  // LHS dispatch: addressable kinds only. The leaf kinds (PrimaryExpr,
  // ConcatExpr) reach through the standard handlers -- their `Expr.type` is
  // already cell-typed for observable storage, exactly what the LHS chain
  // needs. Selector kinds recurse via `LowerLhsExpr` so the base stays
  // cell-rooted; index / range bounds remain values (lowered via `LowerExpr`).
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerHirPrimaryExprProc(*this, frame, p.data, result_type);
          },
          [&](const hir::ElementSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirElementSelectExprLhs(*this, frame, sel, result_type);
          },
          [&](const hir::RangeSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirRangeSelectExprLhs(*this, frame, sel, result_type);
          },
          [&](const hir::MemberAccessExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirMemberAccessExprLhs(*this, frame, sel, result_type);
          },
          [&](const hir::ConcatExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConcatExpr(*this, frame, c, result_type);
          },
          [](const auto&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "ProcessLowerer::LowerLhsExpr: non-addressable HIR expression "
                "in LHS context (ValidateAssignableProcExpr should have "
                "rejected it)");
          },
      },
      expr.data);
}

auto ProcessLowerer::LowerStmt(const hir::Stmt& stmt, WalkFrame frame)
    -> diag::Result<mir::Stmt> {
  return std::visit(
      Overloaded{
          [&](const hir::EmptyStmt&) { return LowerEmptyStmt(stmt.label); },
          [&](const hir::VarDeclStmt& v) {
            return LowerVarDeclStmt(*this, frame, stmt.label, v);
          },
          [&](const hir::ExprStmt& e) {
            return LowerExprStmt(*this, frame, stmt.label, e);
          },
          [&](const hir::BlockStmt& b) {
            return LowerBlockStmt(*this, frame, stmt.label, b);
          },
          [&](const hir::ForkStmt& f) {
            return LowerForkStmt(*this, frame, stmt.label, f);
          },
          [&](const hir::IfStmt& i) {
            return LowerIfStmt(*this, frame, stmt.label, i, stmt.span);
          },
          [&](const hir::CaseStmt& c) {
            return LowerCaseStmt(*this, frame, stmt.label, c, stmt.span);
          },
          [&](const hir::CaseInsideStmt& c) {
            return LowerCaseInsideStmt(*this, frame, stmt.label, c, stmt.span);
          },
          [&](const hir::ForStmt& f) {
            return LowerForStmt(*this, frame, stmt.label, f);
          },
          [&](const hir::WhileStmt& w) {
            return LowerWhileStmt(*this, frame, stmt.label, w);
          },
          [&](const hir::RepeatStmt& r) {
            return LowerRepeatStmt(*this, frame, stmt.label, r);
          },
          [&](const hir::DoWhileStmt& d) {
            return LowerDoWhileStmt(*this, frame, stmt.label, d);
          },
          [&](const hir::ForeverStmt& f) {
            return LowerForeverStmt(*this, frame, stmt.label, f);
          },
          [&](const hir::BreakStmt& b) {
            return LowerBreakStmt(stmt.label, b.target);
          },
          [&](const hir::ContinueStmt&) {
            return LowerContinueStmt(stmt.label);
          },
          [&](const hir::ReturnStmt& r) {
            return LowerReturnStmt(*this, frame, stmt.label, r);
          },
          [&](const hir::TimedStmt& t) {
            return LowerTimedStmt(*this, frame, stmt.label, stmt.span, t);
          },
          [&](const hir::EventTriggerStmt& et) {
            return LowerEventTriggerStmt(*this, frame, stmt.label, et);
          },
          [&](const hir::WaitStmt& w) {
            return LowerWaitStmt(*this, frame, stmt.label, w);
          },
      },
      stmt.data);
}

namespace {

auto LowerStraightLineBodyInto(ProcessLowerer& process, WalkFrame frame)
    -> diag::Result<void> {
  const hir::ProceduralBody& body = process.HirBody();
  auto lowered = process.LowerStmt(body.stmts.Get(body.root_stmt), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  auto& body_block = *frame.current_block;
  body_block.AppendStmt(*std::move(lowered));
  return {};
}

auto LowerStraightLineProcess(ProcessLowerer& process, mir::ProcessKind kind)
    -> diag::Result<mir::Process> {
  const WalkFrame& parent = process.OwnerCtorFrame();
  mir::Block process_block;
  const mir::LocalId self_id = process_block.vars.Add(
      mir::LocalDecl{
          .name = "self", .type = parent.current_class->self_pointer_type});
  const WalkFrame body_frame = parent.WithBlock(&process_block)
                                   .WithSelfBinding(self_id, parent.block_depth)
                                   .WithCoroutineBody(true);
  auto lowered = LowerStraightLineBodyInto(process, body_frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  // A process is a coroutine; it completes by falling off its end through
  // `co_return`, a real body statement (LRM 9.2).
  process_block.AppendStmt(
      mir::ReturnStmt{.value = std::nullopt, .is_coroutine_return = true});
  return mir::Process{
      .kind = kind,
      .code = mir::MethodDecl{
          .name = std::string{process.CallableName()},
          .result_type = process.Module().Unit().builtins.coroutine,
          .params = {},
          .root_block = std::move(process_block),
          .form = mir::MethodForm::kStatic}};
}

// Wraps the body in a `forever` loop. `implicit_sensitivity`, if present,
// is materialised into a `SensitivityWaitStmt` appended after the lowered
// body -- the always_comb / always_latch (LRM 9.2.2.2.1) tail. `always` /
// `always_ff` pass nullptr because the body itself carries any timing.
auto LowerForeverProcess(
    ProcessLowerer& process,
    const std::vector<hir::SensitivityEntry>* implicit_sensitivity)
    -> diag::Result<mir::Process> {
  const WalkFrame& parent = process.OwnerCtorFrame();
  mir::Block process_block;
  const mir::LocalId self_id = process_block.vars.Add(
      mir::LocalDecl{
          .name = "self", .type = parent.current_class->self_pointer_type});
  mir::Block body_block;
  {
    const WalkFrame body_frame =
        parent.WithBlock(&body_block)
            .WithSelfBinding(self_id, parent.block_depth)
            .WithCoroutineBody(true)
            .Deeper();
    auto lowered = LowerStraightLineBodyInto(process, body_frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    if (implicit_sensitivity != nullptr) {
      body_block.AppendStmt(MakeSensitivityWaitStmt(
          body_block, body_frame, process.Owner(), *implicit_sensitivity));
    }
  }

  const mir::BlockId body_scope_id =
      process_block.child_scopes.Add(std::move(body_block));
  process_block.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ForStmt{
              .init = {},
              .condition = std::nullopt,
              .step = {},
              .scope = body_scope_id}});
  process_block.AppendStmt(
      mir::ReturnStmt{.value = std::nullopt, .is_coroutine_return = true});
  return mir::Process{
      .kind = mir::ProcessKind::kInitial,
      .code = mir::MethodDecl{
          .name = std::string{process.CallableName()},
          .result_type = process.Module().Unit().builtins.coroutine,
          .params = {},
          .root_block = std::move(process_block),
          .form = mir::MethodForm::kStatic}};
}

}  // namespace

auto ProcessLowerer::Run(const hir::Process& src)
    -> diag::Result<mir::Process> {
  switch (src.kind) {
    case hir::ProcessKind::kInitial:
      return LowerStraightLineProcess(*this, mir::ProcessKind::kInitial);
    case hir::ProcessKind::kFinal:
      return LowerStraightLineProcess(*this, mir::ProcessKind::kFinal);
    case hir::ProcessKind::kAlways:
    case hir::ProcessKind::kAlwaysFf:
      return LowerForeverProcess(*this, nullptr);
    case hir::ProcessKind::kAlwaysComb:
    case hir::ProcessKind::kAlwaysLatch:
      return LowerForeverProcess(*this, &src.implicit_sensitivity_list);
  }
  throw InternalError("ProcessLowerer::Run: unknown HIR ProcessKind");
}

auto ProcessLowerer::Run(const hir::StructuralSubroutineDecl& src)
    -> diag::Result<mir::MethodDecl> {
  const WalkFrame& parent = owner_ctor_frame_;
  mir::Block body_block;
  const mir::LocalId self_id = body_block.vars.Add(
      mir::LocalDecl{
          .name = "self", .type = parent.current_class->self_pointer_type});
  // A task body suspends on timing controls and renders as a coroutine; a
  // function body executes synchronously.
  const bool body_is_coroutine = src.kind == hir::SubroutineKind::kTask;
  const WalkFrame body_frame = parent.WithBlock(&body_block)
                                   .WithSelfBinding(self_id, parent.block_depth)
                                   .WithCoroutineBody(body_is_coroutine);

  // Formals normalize into the signature's data flow (LRM 13.5). An `input` and
  // a `ref` / `const ref` are parameters (the latter carries a `Ref<T>`); an
  // `output` is not a parameter but a default-initialized body local whose
  // final value rides the completion payload (copy-out at completion, not a
  // live alias); an `inout` is both an input parameter and a payload component.
  // Every formal is a body local at depth 0 so the body's references resolve.
  completion_decl_depth_ = body_frame.block_depth;
  std::vector<mir::MethodParam> params;
  for (const auto& param : src.params) {
    const auto& hir_var = src.body.procedural_vars.Get(param.var);
    const mir::TypeId value_type = module_->TranslateType(hir_var.type);
    const hir::ParamDirection dir = param.direction;

    if (dir == hir::ParamDirection::kOutput) {
      const mir::ExprId default_init = body_block.exprs.Add(
          BuildDefaultValueExpr(*module_, body_frame, value_type));
      const mir::LocalRef local = body_block.AppendLocal(
          mir::LocalDecl{.name = hir_var.name, .type = value_type},
          default_init);
      MapProceduralVar(
          param.var, AutomaticVarBinding{
                         .declaration_procedural_depth = body_frame.block_depth,
                         .var = local.var,
                         .type = value_type});
      output_pack_vars_.push_back(local.var);
      output_pack_types_.push_back(value_type);
      continue;
    }

    // A `ref` / `const ref` formal's parameter type is a `Ref<T>` over the
    // value type (LRM 13.5.2); every other parameter carries the value type.
    const bool by_reference = dir == hir::ParamDirection::kRef ||
                              dir == hir::ParamDirection::kConstRef;
    const mir::TypeId type =
        by_reference
            ? module_->Unit().AddType(
                  mir::TypeData{mir::RefType{
                      .pointee = value_type,
                      .is_const = dir == hir::ParamDirection::kConstRef}})
            : value_type;
    const mir::LocalId mir_var =
        body_block.vars.Add(mir::LocalDecl{.name = hir_var.name, .type = type});
    MapProceduralVar(
        param.var, AutomaticVarBinding{
                       .declaration_procedural_depth = body_frame.block_depth,
                       .var = mir_var,
                       .type = type});
    params.push_back(mir::MethodParam{.name = hir_var.name, .type = type});
    if (dir == hir::ParamDirection::kInOut) {
      output_pack_vars_.push_back(mir_var);
      output_pack_types_.push_back(value_type);
    }
  }

  // LRM 13.4.1 implicit result variable. A non-void function's same-name var is
  // a default-initialized body local (named distinctly from the C++ method so a
  // self-recursive call still resolves to the method): the leading
  // completion-payload component, the value a fall-through or value-less
  // `return` carries. void functions and tasks have none.
  std::vector<mir::TypeId> payload_components;
  if (src.result_var.has_value()) {
    const mir::TypeId ret_type = module_->TranslateType(src.result_type);
    const mir::ExprId default_init = body_block.exprs.Add(
        BuildDefaultValueExpr(*module_, body_frame, ret_type));
    const mir::LocalRef result_ref = body_block.AppendLocal(
        mir::LocalDecl{.name = "_lyra_result", .type = ret_type}, default_init);
    MapProceduralVar(
        *src.result_var,
        AutomaticVarBinding{
            .declaration_procedural_depth = body_frame.block_depth,
            .var = result_ref.var,
            .type = ret_type});
    result_var_ = result_ref.var;
    result_value_type_ = ret_type;
    payload_components.push_back(ret_type);
  }
  for (const mir::TypeId t : output_pack_types_) {
    payload_components.push_back(t);
  }

  // The completion payload is the function return (if any) plus each output /
  // inout value, normalized by count. A task wraps it in the coroutine call
  // protocol (LRM 13.3); a function's result is the payload itself (LRM 13.4).
  completion_payload_type_ =
      NormalizeCompletionPayload(module_->Unit(), payload_components);
  const mir::TypeId result_type =
      body_is_coroutine ? CoroutineOf(module_->Unit(), completion_payload_type_)
                        : completion_payload_type_;

  auto lowered = LowerStraightLineBodyInto(*this, body_frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));

  // Close the body with a trailing return of the fall-through payload. Absent
  // when the payload is empty (a void function or a task with no outputs); the
  // backend then supplies the bare `co_return;` for a coroutine.
  if (auto trailing =
          BuildReturnPayload(body_block, body_frame, std::nullopt)) {
    body_block.AppendStmt(
        mir::ReturnStmt{
            .value = trailing,
            .is_coroutine_return = body_frame.is_coroutine_body});
  } else if (body_is_coroutine) {
    // A task is a coroutine with no result value: it completes by falling off
    // its end through `co_return` (LRM 13.3). The completion is a real body
    // statement, not a backend-appended epilogue.
    body_block.AppendStmt(
        mir::ReturnStmt{.value = std::nullopt, .is_coroutine_return = true});
  }

  return mir::MethodDecl{
      .name = src.name,
      .result_type = result_type,
      .params = std::move(params),
      .root_block = std::move(body_block),
      .form = mir::MethodForm::kStatic};
}

auto ProcessLowerer::BuildReturnPayload(
    mir::Block& block, const WalkFrame& frame,
    std::optional<mir::ExprId> explicit_value) -> std::optional<mir::ExprId> {
  std::vector<mir::ExprId> components;
  if (result_var_.has_value()) {
    components.push_back(
        explicit_value.has_value()
            ? *explicit_value
            : block.exprs.Add(
                  mir::MakeLocalRefExpr(
                      frame.block_depth - completion_decl_depth_, *result_var_,
                      result_value_type_)));
  }
  for (std::size_t i = 0; i < output_pack_vars_.size(); ++i) {
    components.push_back(block.exprs.Add(
        mir::MakeLocalRefExpr(
            frame.block_depth - completion_decl_depth_, output_pack_vars_[i],
            output_pack_types_[i])));
  }
  if (components.empty()) return std::nullopt;
  if (components.size() == 1) return components.front();
  return block.exprs.Add(
      mir::Expr{
          .data = mir::TupleExpr{.components = std::move(components)},
          .type = completion_payload_type_});
}

}  // namespace lyra::lowering::hir_to_mir

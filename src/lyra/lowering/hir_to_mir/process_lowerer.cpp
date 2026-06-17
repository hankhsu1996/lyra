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
#include "lyra/mir/expr.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto ProcessLowerer::LowerExpr(const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = module_->TranslateType(expr.type);
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerHirPrimaryExprProc(*this, frame, p.data, result_type);
          },
          [&](const hir::UnaryExpr& u) -> diag::Result<mir::Expr> {
            return LowerHirUnaryExprProc(*this, frame, u, result_type);
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            return LowerHirBinaryExprProc(*this, frame, b, result_type);
          },
          [&](const hir::ConditionalExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConditionalExprProc(*this, frame, c, result_type);
          },
          [&](const hir::AssignExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignExprProc(
                *this, frame, a, expr.span, result_type);
          },
          [&](const hir::IncDecExpr& inc) -> diag::Result<mir::Expr> {
            return LowerHirIncDecExprProc(*this, frame, inc, result_type);
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            return LowerHirConversionExprProc(*this, frame, cv, result_type);
          },
          [&](const hir::CallExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirCallExprProc(
                *this, frame, c, expr.span, result_type);
          },
          [&](const hir::InsideExpr& in) -> diag::Result<mir::Expr> {
            return LowerHirInsideExprProc(*this, frame, in, result_type);
          },
          [&](const hir::ElementSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirElementSelectExprProc(
                *this, frame, sel, result_type);
          },
          [&](const hir::RangeSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirRangeSelectExprProc(*this, frame, sel, result_type);
          },
          [&](const hir::MemberAccessExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirMemberAccessExprProc(*this, frame, sel, result_type);
          },
          [&](const hir::ConcatExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConcatExprProc(*this, frame, c, result_type);
          },
          [&](const hir::ReplicationExpr& r) -> diag::Result<mir::Expr> {
            return LowerHirReplicationExprProc(*this, frame, r, result_type);
          },
          [&](const hir::AssignmentPatternExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternExprProc(
                *this, frame, a, result_type);
          },
          [&](const hir::AssignmentPatternReplicationExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternReplicationExprProc(
                *this, frame, a, result_type);
          },
          [&](const hir::DynamicArrayNewExpr& n) -> diag::Result<mir::Expr> {
            return LowerHirDynamicArrayNewExprProc(
                *this, frame, n, result_type);
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
            return LowerIfStmt(*this, frame, stmt.label, i);
          },
          [&](const hir::CaseStmt& c) {
            return LowerCaseStmt(*this, frame, stmt.label, c);
          },
          [&](const hir::CaseInsideStmt& c) {
            return LowerCaseInsideStmt(*this, frame, stmt.label, c);
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
          [&](const hir::BreakStmt&) { return LowerBreakStmt(stmt.label); },
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
  auto lowered = process.LowerStmt(body.stmts.at(body.root_stmt.value), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  auto& body_scope = *frame.current_procedural_scope;
  body_scope.AppendStmt(*std::move(lowered));
  return {};
}

auto LowerStraightLineProcess(
    ProcessLowerer& process, WalkFrame frame, std::string name,
    mir::ProcessKind kind) -> diag::Result<mir::Process> {
  // The process root scope is also its static frame: a static-lifetime body
  // local lands here (instance-owned, LRM 6.21 / 9.3.4) instead of in the
  // coroutine activation, the same shape a subroutine uses.
  mir::ProceduralScope process_scope;
  const mir::ProceduralVarId self_id = process_scope.AddProceduralVar(
      mir::ProceduralVarDecl{
          .name = "self",
          .type = process.Module().Unit().builtins.self_pointer});
  const WalkFrame body_frame =
      frame.WithProceduralScope(&process_scope)
          .WithStaticFrameScope(&process_scope)
          .WithSelfBinding(self_id, frame.procedural_depth);
  auto lowered = LowerStraightLineBodyInto(process, body_frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return mir::Process{
      .kind = kind,
      .name = std::move(name),
      .root_procedural_scope = std::move(process_scope),
      .static_locals = process.TakeStaticLocals()};
}

// Wraps the body in a `forever` loop. `tail_stmt`, if present, is appended
// after the lowered body inside the loop -- carries the materialised
// SensitivityWaitStmt for always_comb / always_latch (LRM 9.2.2.2.1),
// nullopt for `always` / `always_ff` where the body itself carries any
// timing.
auto LowerForeverProcess(
    ProcessLowerer& process, WalkFrame frame, std::string name,
    std::optional<mir::Stmt> tail_stmt) -> diag::Result<mir::Process> {
  // The static frame is the process root scope, which sits outside the
  // forever loop, so a static body local persists across the loop iterations
  // that model successive activations (LRM 6.21). The loop body lowers one
  // depth in, so a body reference reaches the frame by one hop.
  mir::ProceduralScope process_scope;
  const mir::ProceduralVarId self_id = process_scope.AddProceduralVar(
      mir::ProceduralVarDecl{
          .name = "self",
          .type = process.Module().Unit().builtins.self_pointer});
  mir::ProceduralScope body_scope;
  {
    const WalkFrame body_frame =
        frame.WithStaticFrameScope(&process_scope)
            .WithProceduralScope(&body_scope)
            .WithSelfBinding(self_id, frame.procedural_depth)
            .Deeper();
    auto lowered = LowerStraightLineBodyInto(process, body_frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    if (tail_stmt.has_value()) {
      body_scope.AppendStmt(*std::move(tail_stmt));
    }
  }

  const mir::ProceduralScopeId body_scope_id =
      process_scope.AddChildScope(std::move(body_scope));
  process_scope.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ForStmt{
              .init = {},
              .condition = std::nullopt,
              .step = {},
              .scope = body_scope_id}});
  return mir::Process{
      .kind = mir::ProcessKind::kInitial,
      .name = std::move(name),
      .root_procedural_scope = std::move(process_scope),
      .static_locals = process.TakeStaticLocals()};
}

auto LowerSubroutineKind(hir::SubroutineKind kind) -> mir::SubroutineKind {
  switch (kind) {
    case hir::SubroutineKind::kTask:
      return mir::SubroutineKind::kTask;
    case hir::SubroutineKind::kFunction:
      return mir::SubroutineKind::kFunction;
  }
  throw InternalError("LowerSubroutineKind: unknown hir::SubroutineKind");
}

auto LowerParamDirection(hir::ParamDirection dir) -> mir::ParamDirection {
  switch (dir) {
    case hir::ParamDirection::kInput:
      return mir::ParamDirection::kInput;
    case hir::ParamDirection::kOutput:
      return mir::ParamDirection::kOutput;
    case hir::ParamDirection::kInOut:
      return mir::ParamDirection::kInOut;
    case hir::ParamDirection::kRef:
      return mir::ParamDirection::kRef;
    case hir::ParamDirection::kConstRef:
      return mir::ParamDirection::kConstRef;
  }
  throw InternalError("LowerParamDirection: unknown hir::ParamDirection");
}

}  // namespace

auto ProcessLowerer::Run(
    WalkFrame parent_frame, std::string name, const hir::Process& src)
    -> diag::Result<mir::Process> {
  switch (src.kind) {
    case hir::ProcessKind::kInitial:
      return LowerStraightLineProcess(
          *this, parent_frame, std::move(name), mir::ProcessKind::kInitial);
    case hir::ProcessKind::kFinal:
      return LowerStraightLineProcess(
          *this, parent_frame, std::move(name), mir::ProcessKind::kFinal);
    case hir::ProcessKind::kAlways:
    case hir::ProcessKind::kAlwaysFf:
      return LowerForeverProcess(
          *this, parent_frame, std::move(name), std::nullopt);
    case hir::ProcessKind::kAlwaysComb:
    case hir::ProcessKind::kAlwaysLatch:
      return LowerForeverProcess(
          *this, parent_frame, std::move(name),
          BuildSensitivityWaitStmt(*scope_, src.implicit_sensitivity_list));
  }
  throw InternalError("ProcessLowerer::Run: unknown HIR ProcessKind");
}

auto ProcessLowerer::Run(
    WalkFrame parent_frame, const hir::StructuralSubroutineDecl& src)
    -> diag::Result<mir::StructuralSubroutineDecl> {
  mir::ProceduralScope body_scope;
  const mir::ProceduralVarId self_id = body_scope.AddProceduralVar(
      mir::ProceduralVarDecl{
          .name = "self", .type = module_->Unit().builtins.self_pointer});
  const WalkFrame body_frame =
      parent_frame.WithProceduralScope(&body_scope)
          .WithStaticFrameScope(&body_scope)
          .WithSelfBinding(self_id, parent_frame.procedural_depth);

  // Formals are procedural vars of the body with no VarDeclStmt: pre-register
  // them in the body scope at depth 0 so the body's references resolve. The
  // backend renders these as C++ parameters rather than in-body declarations.
  std::vector<mir::SubroutineParam> params;
  params.reserve(src.params.size());
  for (const auto& param : src.params) {
    const auto& hir_var = src.body.procedural_vars.at(param.var.value);
    const mir::TypeId type = module_->TranslateType(hir_var.type);
    // A ref / const ref formal aliases the actual's cell, so its body var is a
    // reference binding the backend renders as `Ref<T>` (LRM 13.5.2).
    const mir::VariableBinding binding =
        (param.direction == hir::ParamDirection::kRef ||
         param.direction == hir::ParamDirection::kConstRef)
            ? mir::VariableBinding::kReference
            : mir::VariableBinding::kValue;
    const mir::ProceduralVarId mir_var = body_scope.AddProceduralVar(
        mir::ProceduralVarDecl{
            .name = hir_var.name, .type = type, .binding = binding});
    MapProceduralVar(
        param.var,
        ProceduralVarBinding{
            .declaration_procedural_depth = body_frame.procedural_depth,
            .var = mir_var});
    params.push_back(
        mir::SubroutineParam{
            .name = hir_var.name,
            .type = type,
            .direction = LowerParamDirection(param.direction)});
  }

  // LRM 13.4.1 implicit result variable desugar. A non-void function returns
  // the current value of its implicit same-name variable on fall-through; an
  // assignment to the function name is a write to that variable, and an
  // explicit `return expr` overrides it because it exits before the trailing
  // read. Materialize the variable as a default-initialized body local
  // (named distinctly from the C++ method so a self-recursive call still
  // resolves to the method), then close the body with `return <result>`.
  // void functions and tasks have no result variable and no trailing return.
  const mir::TypeId result_type = module_->TranslateType(src.result_type);
  std::optional<mir::ProceduralVarRef> result_ref;
  if (src.result_var.has_value()) {
    const mir::ExprId default_init =
        SynthesizeDefaultValueExpr(*module_, body_frame, result_type);
    result_ref = body_scope.AppendLocal(
        mir::ProceduralVarDecl{.name = "_lyra_result", .type = result_type},
        default_init);
    MapProceduralVar(
        *src.result_var,
        ProceduralVarBinding{
            .declaration_procedural_depth = body_frame.procedural_depth,
            .var = result_ref->var});
  }

  auto lowered = LowerStraightLineBodyInto(*this, body_frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));

  if (result_ref.has_value()) {
    const mir::ExprId result_read =
        body_scope.AddExpr(mir::Expr{.data = *result_ref, .type = result_type});
    body_scope.AppendStmt(mir::ReturnStmt{.value = result_read});
  }

  return mir::StructuralSubroutineDecl{
      .name = src.name,
      .kind = LowerSubroutineKind(src.kind),
      .result_type = result_type,
      .params = std::move(params),
      .root_procedural_scope = std::move(body_scope),
      .static_locals = TakeStaticLocals()};
}

}  // namespace lyra::lowering::hir_to_mir

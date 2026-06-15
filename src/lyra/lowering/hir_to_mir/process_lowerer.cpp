#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerStraightLineBodyInto(ProcessLowerer& process, WalkFrame frame)
    -> diag::Result<void> {
  const hir::ProceduralBody& body = process.HirBody();
  auto lowered = LowerStmt(process, frame, body.stmts.at(body.root_stmt.value));
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  auto& body_scope = *frame.current_procedural_scope;
  body_scope.AppendStmt(*std::move(lowered));
  return {};
}

auto LowerStraightLineProcess(
    ProcessLowerer& process, WalkFrame frame, mir::ProcessKind kind)
    -> diag::Result<mir::Process> {
  // The process root scope is also its static frame: a static-lifetime body
  // local lands here (instance-owned, LRM 6.21 / 9.3.4) instead of in the
  // coroutine activation, the same shape a subroutine uses.
  mir::ProceduralScope process_scope;
  const WalkFrame body_frame = frame.WithProceduralScope(&process_scope)
                                   .WithStaticFrameScope(&process_scope);
  auto lowered = LowerStraightLineBodyInto(process, body_frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return mir::Process{
      .kind = kind,
      .root_procedural_scope = std::move(process_scope),
      .static_locals = process.TakeStaticLocals()};
}

// Wraps the body in a `forever` loop. `tail_stmt`, if present, is appended
// after the lowered body inside the loop -- carries the materialised
// SensitivityWaitStmt for always_comb / always_latch (LRM 9.2.2.2.1), nullopt
// for `always` / `always_ff` where the body itself carries any timing.
auto LowerForeverProcess(
    ProcessLowerer& process, WalkFrame frame,
    std::optional<mir::Stmt> tail_stmt) -> diag::Result<mir::Process> {
  // The static frame is the process root scope, which sits outside the
  // forever loop, so a static body local persists across the loop iterations
  // that model successive activations (LRM 6.21). The loop body lowers one
  // depth in, so a body reference reaches the frame by one hop.
  mir::ProceduralScope process_scope;
  mir::ProceduralScope body_scope;
  {
    const WalkFrame body_frame = frame.WithStaticFrameScope(&process_scope)
                                     .WithProceduralScope(&body_scope)
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

auto ProcessLowerer::Run(WalkFrame parent_frame, const hir::Process& src)
    -> diag::Result<mir::Process> {
  switch (src.kind) {
    case hir::ProcessKind::kInitial:
      return LowerStraightLineProcess(
          *this, parent_frame, mir::ProcessKind::kInitial);
    case hir::ProcessKind::kFinal:
      return LowerStraightLineProcess(
          *this, parent_frame, mir::ProcessKind::kFinal);
    case hir::ProcessKind::kAlways:
    case hir::ProcessKind::kAlwaysFf:
      return LowerForeverProcess(*this, parent_frame, std::nullopt);
    case hir::ProcessKind::kAlwaysComb:
    case hir::ProcessKind::kAlwaysLatch:
      return LowerForeverProcess(
          *this, parent_frame,
          BuildSensitivityWaitStmt(*scope_, src.implicit_sensitivity_list));
  }
  throw InternalError("ProcessLowerer::Run: unknown HIR ProcessKind");
}

auto ProcessLowerer::Run(
    WalkFrame parent_frame, const hir::StructuralSubroutineDecl& src)
    -> diag::Result<mir::StructuralSubroutineDecl> {
  mir::ProceduralScope body_scope;
  const WalkFrame body_frame = parent_frame.WithProceduralScope(&body_scope)
                                   .WithStaticFrameScope(&body_scope);

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
  // read. Materialize the variable as a default-initialized body local (named
  // distinctly from the C++ method so a self-recursive call still resolves to
  // the method), then close the body with `return <result>`. void functions
  // and tasks have no result variable and no trailing return.
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

#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"

#include <expected>
#include <functional>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/concurrent_assertion.hpp"
#include "lyra/lowering/hir_to_mir/covergroup_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/randomization.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/statement/assertions.hpp"
#include "lyra/lowering/hir_to_mir/statement/assignment.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/statement/branches.hpp"
#include "lyra/lowering/hir_to_mir/statement/flow.hpp"
#include "lyra/lowering/hir_to_mir/statement/foreach.hpp"
#include "lyra/lowering/hir_to_mir/statement/fork_join.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/statement/procedural_continuous.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/target_landing.hpp"

namespace lyra::lowering::hir_to_mir {

auto PromotedVarPlace(
    const mir::TypePool& types, const WalkFrame& frame,
    const PromotedVarBinding& binding) -> mir::Expr {
  mir::Block& block = *frame.current_block;
  const mir::ExprId handle = block.exprs.Add(frame.bindings->MakeBindingPlace(
      frame.bindings->EnsureCarrier(binding.handle_origin), block));
  return mir::MakeDerefExpr(
      block.DerefOperand(types, handle), binding.cell_type);
}

auto ProcessLowerer::LowerStmt(const hir::Stmt& stmt, WalkFrame frame)
    -> diag::Result<mir::Stmt> {
  return LowerStmt(
      stmt, std::move(frame), support::TargetLanding::kPastTheTarget);
}

auto ProcessLowerer::LowerStmt(
    const hir::Stmt& stmt, WalkFrame frame, support::TargetLanding landing)
    -> diag::Result<mir::Stmt> {
  const diag::FailureContext at(stmt.span);
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
            return LowerBlockStmt(*this, frame, stmt.label, b, landing);
          },
          [&](const hir::ForkStmt& f) {
            return LowerForkStmt(*this, frame, stmt.label, f, landing);
          },
          [&](const hir::IfStmt& i) {
            return LowerIfStmt(*this, frame, stmt.label, i, stmt.span);
          },
          [&](const hir::CaseStmt& c) {
            return LowerCaseStmt(*this, frame, stmt.label, c, stmt.span);
          },
          [&](const hir::PatternCaseStmt& c) {
            return LowerPatternCaseStmt(*this, frame, stmt.label, c, stmt.span);
          },
          [&](const hir::AssertStmt& a) {
            return LowerAssertStmt(*this, frame, stmt.label, a, stmt.span);
          },
          [&](const hir::CoverStmt& c) {
            return LowerCoverStmt(*this, frame, stmt.label, c, stmt.span);
          },
          [&](const hir::ConcurrentAssertStmt&) -> diag::Result<mir::Stmt> {
            return std::unexpected(
                diag::Make(
                    stmt.span, diag::DiagCode::kUnsupportedStatementForm,
                    "a concurrent assertion embedded in a procedure is not yet "
                    "supported")
                    .WithNote("pass '--assertions skip' to leave it out"));
          },
          [&](const hir::ConcurrentCoverStmt&) -> diag::Result<mir::Stmt> {
            return std::unexpected(
                diag::Make(
                    stmt.span, diag::DiagCode::kUnsupportedStatementForm,
                    "a concurrent cover statement is not yet supported")
                    .WithNote("pass '--assertions skip' to leave it out"));
          },
          [&](const hir::ExpectStmt& e) {
            return LowerExpectStmt(*this, frame, stmt.label, e, stmt.span);
          },
          [&](const hir::ForStmt& f) {
            return LowerForStmt(*this, frame, stmt.label, f);
          },
          [&](const hir::ForeachStmt& f) {
            return LowerForeachStmt(*this, frame, stmt.label, f);
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
          [&](const hir::BreakStmt&) {
            return LowerBreakStmt(stmt.label, frame);
          },
          [&](const hir::ContinueStmt&) {
            return LowerContinueStmt(stmt.label, frame);
          },
          [&](const hir::ReturnStmt& r) {
            return LowerReturnStmt(*this, frame, stmt.label, r);
          },
          [&](const hir::TimedStmt& t) {
            return LowerTimedStmt(*this, frame, stmt.label, t);
          },
          [&](const hir::EventTriggerStmt& et) {
            return LowerEventTriggerStmt(*this, frame, stmt.label, et);
          },
          [&](const hir::WaitStmt& w) {
            return LowerWaitStmt(*this, frame, stmt.label, w);
          },
          [&](const hir::WaitForkStmt&) {
            return LowerWaitForkStmt(*this, frame, stmt.label);
          },
          [&](const hir::DisableForkStmt&) {
            return LowerDisableForkStmt(*this, frame, stmt.label);
          },
          [&](const hir::DisableStmt& d) {
            return LowerDisableStmt(*this, frame, stmt.label, d);
          },
          [&](const hir::ProceduralContinuousAssignStmt& pca) {
            return LowerProceduralContinuousAssignStmt(
                *this, frame, stmt.label, pca);
          },
          [&](const hir::ProceduralContinuousEndStmt& pce) {
            return LowerProceduralContinuousEndStmt(
                *this, frame, stmt.label, pce);
          },
          [&](const hir::ConstraintStmt& c) {
            return LowerConstraintStmt(*this, frame, stmt.label, c);
          },
      },
      stmt.data);
}

namespace {

auto LowerStraightLineBodyInto(
    ProcessLowerer& process, WalkFrame frame, support::TargetLanding landing)
    -> diag::Result<void> {
  const hir::ProceduralBody& body = process.HirBody();
  const hir::Stmt& root = body.stmts.Get(process.HirRootStmt());
  // A body whose root statement is the block of its own scope begins and ends
  // as that block does.
  const auto* block = std::get_if<hir::BlockStmt>(&root.data);
  if (block != nullptr && block->scope == body.root_scope) {
    auto lowered = process.LowerStmt(root, frame, landing);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    frame.current_block->AppendStmt(*std::move(lowered));
    return {};
  }
  // Any other body is its scope's one statement, so what a covergroup samples
  // at the scope (LRM 19.3) is triggered around it here, its end on every
  // normal way out.
  const ScopeBlockEvents& events =
      process.Scopes().Get(body.root_scope).block_events;
  AppendBlockEventTriggers(process, frame, events.at_begin);
  const NormalCleanup end{
      .emit =
          [&](const WalkFrame& at) {
            AppendBlockEventTriggers(process, at, events.at_end);
          },
      .outer = frame.normal_cleanups};
  auto lowered = process.LowerStmt(
      root, events.at_end.empty() ? frame : frame.WithNormalCleanup(end),
      landing);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  frame.current_block->AppendStmt(*std::move(lowered));
  end.emit(frame);
  return {};
}

// What the way a procedure that runs its statement once is executed decides of
// its body.
struct BodyOfExecution {
  // Where a `disable` of the procedure's statement lands.
  support::TargetLanding landing;
  bool can_wait;
  mir::TypeId result_type;
};

auto LowerStraightLineProcedure(
    ProcessLowerer& process, const BodyOfExecution& body)
    -> diag::Result<mir::CallableCode> {
  const WalkFrame& parent = process.OwnerCtorFrame();
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(process.Owner().Unit(), code);
  const mir::LocalId self_id = bindings.Declare(
      ReceiverOrigin{}, parent.current_class.cls->self_pointer_type);
  code.params = {self_id};
  const WalkFrame body_frame =
      process
          .InRootScope(parent.WithBlock(&code.Body()).WithBindings(&bindings))
          .WithBodyCanWait(body.can_wait);
  auto lowered = LowerStraightLineBodyInto(process, body_frame, body.landing);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  // A procedure completes by falling off its end, which is a real body
  // statement rather than an implicit exit (LRM 9.2). A process's coroutine
  // result type is what makes that return a coroutine completion.
  code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  code.result_type = body.result_type;
  return code;
}

// What follows a forever process's body each time round, appended to the
// body's block.
using AfterBody = std::function<void(mir::Block&)>;

// Wraps the body in a `forever` loop. `ahead` adds what the process does once,
// before the loop, to the block the frame it is handed is writing, and
// answers what follows the body each time round.
template <typename Ahead>
auto LowerForeverProcess(ProcessLowerer& process, Ahead ahead)
    -> diag::Result<mir::CallableCode> {
  const WalkFrame& parent = process.OwnerCtorFrame();
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(process.Owner().Unit(), code);
  const mir::LocalId self_id = bindings.Declare(
      ReceiverOrigin{}, parent.current_class.cls->self_pointer_type);
  code.params = {self_id};
  const WalkFrame entry_frame = process.InRootScope(
      parent.WithBlock(&code.Body()).WithBindings(&bindings));
  diag::Result<AfterBody> after_body = ahead(entry_frame);
  if (!after_body) return std::unexpected(std::move(after_body.error()));
  mir::Block body_block;
  {
    const WalkFrame body_frame = entry_frame.WithBlock(&body_block);
    auto lowered = LowerStraightLineBodyInto(
        process, body_frame, support::TargetLanding::kPastTheTarget);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    (*after_body)(body_block);
  }

  const mir::BlockId body_scope_id =
      code.Body().child_scopes.Add(std::move(body_block));
  code.Body().AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id});
  code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  code.result_type = process.Owner().Unit().builtins.coroutine_void;
  return code;
}

// An `always` / `always_ff`: the body carries whatever timing it has, so
// nothing follows it.
auto LowerAlwaysProcess(ProcessLowerer& process)
    -> diag::Result<mir::CallableCode> {
  return LowerForeverProcess(
      process, [](const WalkFrame&) -> diag::Result<AfterBody> {
        return AfterBody{[](mir::Block&) {}};
      });
}

// An `always_comb` / `always_latch` (LRM 9.2.2.2.1): its list is collected
// once, ahead of the loop, and waited on after the body each time round.
auto LowerImplicitListProcess(ProcessLowerer& process, const hir::Reads& reads)
    -> diag::Result<mir::CallableCode> {
  return LowerForeverProcess(
      process, [&](const WalkFrame& entry_frame) -> diag::Result<AfterBody> {
        auto held = HoldImplicitList(entry_frame, process, reads);
        if (!held) return std::unexpected(std::move(held.error()));
        return AfterBody{[&process, held = *held](mir::Block& body) {
          body.AppendStmt(BuildParkStmt(process.Owner(), body, held));
        }};
      });
}

// A formal that brings a value in: the parameter the call hands it through,
// which the variable starts at. Where the body cannot wait the variable is the
// parameter itself. A static subroutine's is the one variable every call shares
// (LRM 13.3.2), and the call assigns it the value it hands in.
auto DeclareParameterVar(
    ProcessLowerer& process, const WalkFrame& frame, hir::ProceduralVarId var,
    const hir::ProceduralVarDecl& decl) -> mir::LocalId {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& body = *frame.current_block;
  const mir::TypeId value_type = unit_lowerer.TranslateType(decl.type);
  const bool is_static = decl.lifetime == hir::VariableLifetime::kStatic;
  if (!is_static && !frame.body_can_wait && !decl.lifetime_extended) {
    const mir::LocalId parameter = frame.bindings->DeclareProcedural(
        ProceduralOrigin{.var = var}, decl.name, value_type);
    process.MapProceduralVar(var, AutomaticVarBinding{.type = value_type});
    return parameter;
  }
  const mir::LocalId parameter = frame.bindings->DeclareAnonymous(value_type);
  if (is_static) {
    const AccessPath variable{
        .owner = body.exprs.Add(LowerProceduralVarRefExpr(process, frame, var)),
        .descent = {}};
    body.AppendStmt(
        mir::ExprStmt{
            .expr = body.exprs.Add(BuildStoreExpr(
                unit, body, variable, body.ReadLocal(parameter, value_type)))});
    return parameter;
  }
  body.AppendStmt(LowerVarStart(process, frame, var, decl.name, [&] {
    return body.ReadLocal(parameter, value_type);
  }));
  return parameter;
}

// The variables a subroutine's scope declares before its first statement: its
// formals and, for a non-void function, its result (LRM 13.4.1).
auto DeclaredAhead(const hir::SubroutineDecl& decl)
    -> std::vector<hir::ProceduralVarId> {
  std::vector<hir::ProceduralVarId> declared;
  declared.reserve(decl.params.size() + 1);
  for (const auto& param : decl.params) {
    declared.push_back(param.var);
  }
  if (decl.result_var.has_value()) {
    declared.push_back(*decl.result_var);
  }
  return declared;
}

// Whether a subroutine's body can wait, which a task's can and a function's
// and a constraint block's cannot (LRM 13.4.4, 18.5).
auto CanWait(hir::SubroutineKind kind) -> bool {
  switch (kind) {
    case hir::SubroutineKind::kTask:
      return true;
    case hir::SubroutineKind::kFunction:
    case hir::SubroutineKind::kConstraintBlock:
    case hir::SubroutineKind::kStaticConstraintBlock:
      return false;
  }
  throw InternalError("CanWait: unknown SubroutineKind");
}

}  // namespace

auto ProcessLowerer::Run(const hir::Process& src)
    -> diag::Result<mir::CallableCode> {
  const diag::FailureContext at(src.span);
  const mir::CompilationUnit& unit = owner_->Unit();
  return std::visit(
      Overloaded{
          // An initial procedure is a process of its own (LRM 9.5): nothing
          // of it follows its statement, so a `disable` of that statement
          // ends the process, and its body is the coroutine a thread runs.
          [&](const hir::InitialProcess&) {
            return LowerStraightLineProcedure(
                *this, BodyOfExecution{
                           .landing = support::TargetLanding::kAtExecutionEnd,
                           .can_wait = true,
                           .result_type = unit.builtins.coroutine_void});
          },
          // The final procedures are function calls one process makes in turn
          // (LRM 9.2.3): leaving one returns to the call, which goes on to the
          // next, and a function cannot wait (LRM 13.4.4).
          [&](const hir::FinalProcess&) {
            return LowerStraightLineProcedure(
                *this, BodyOfExecution{
                           .landing = support::TargetLanding::kPastTheTarget,
                           .can_wait = false,
                           .result_type = unit.builtins.void_type});
          },
          [&](const hir::AlwaysProcess&) { return LowerAlwaysProcess(*this); },
          [&](const hir::AlwaysFfProcess&) {
            return LowerAlwaysProcess(*this);
          },
          [&](const hir::AlwaysCombProcess& comb) {
            return LowerImplicitListProcess(*this, comb.implicit_reads);
          },
          [&](const hir::AlwaysLatchProcess& latch) {
            return LowerImplicitListProcess(*this, latch.implicit_reads);
          }},
      src.kind);
}

auto ProcessLowerer::EnterSubroutineBody(
    const hir::SubroutineDecl& src, mir::CallableCode& code,
    CallableBindings& bindings) -> BoundImplicitParameters {
  const WalkFrame& parent = owner_ctor_frame_;
  // A task or function is a scope the source named (LRM 23.9), so the body
  // starts inside it and `%m` there reports the task, not the scope around
  // it. What it takes ahead of its formals is what the class declaring it
  // states; a subroutine of a namespace unit (LRM 26.3) has no class and takes
  // nothing.
  const WalkFrame entry_frame =
      InRootScope(parent.WithBlock(&code.Body()).WithBindings(&bindings))
          .WithBodyCanWait(CanWait(src.kind));
  if (parent.current_class.cls == nullptr) {
    return BoundImplicitParameters{.params = {}, .frame = entry_frame};
  }
  return BindImplicitParameters(
      *owner_, entry_frame, owner_->GetClassShape(parent.current_class.id),
      src.is_static ? CallableForm::kTypeAssociated
                    : CallableForm::kInstanceMember);
}

auto ProcessLowerer::DeclareReferenceFormal(
    CallableBindings& bindings, hir::ProceduralVarId var,
    const hir::ProceduralVarDecl& declared, mir::TypeId type) -> mir::LocalId {
  const mir::LocalId parameter = bindings.DeclareProcedural(
      ProceduralOrigin{.var = var}, declared.name, type);
  MapProceduralVar(var, AutomaticVarBinding{.type = type});
  return parameter;
}

template <typename AfterFormals>
auto ProcessLowerer::LowerSubroutine(
    const hir::SubroutineDecl& src, AfterFormals after_formals)
    -> diag::Result<mir::CallableCode> {
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(owner_->Unit(), code);
  BoundImplicitParameters bound = EnterSubroutineBody(src, code, bindings);
  WalkFrame body_frame = bound.frame;
  std::vector<mir::LocalId> params = std::move(bound.params);
  const mir::CompilationUnit& unit = owner_->Unit();

  // A formal or the result a detached branch names lives in a cell the body's
  // entry makes, kept alive by every branch naming it (LRM 6.21); it starts
  // where the others do, and the completion reads it from that cell.
  OpenActivationScope(*this, body_frame, DeclaredAhead(src));

  // Formals normalize into the signature's data flow (LRM 13.5). Every formal
  // is a binding in the callable, identified by its HIR id; one that is no
  // parameter is a default-initialized body variable instead, whose final value
  // rides the completion payload -- copied out at completion rather than
  // aliased live. An `inout` is both a parameter and a payload component.
  for (const auto& param : src.params) {
    const auto& hir_var = src.body.procedural_vars.Get(param.var);
    const hir::ParamDirection dir = param.direction;
    const std::optional<mir::TypeId> param_type =
        ParamTypeOf(owner_->Unit(), owner_->TranslateType(hir_var.type), dir);

    if (!param_type.has_value()) {
      code.Body().AppendStmt(LowerVarStart(
          *this, body_frame, param.var, hir_var.name,
          DefaultInitialValue(*this, body_frame, hir_var)));
      output_vars_.push_back(param.var);
      continue;
    }

    if (unit.types.Get(*param_type).Is<mir::VariableRefType>()) {
      params.push_back(
          DeclareReferenceFormal(bindings, param.var, hir_var, *param_type));
      continue;
    }

    params.push_back(
        DeclareParameterVar(*this, body_frame, param.var, hir_var));
    if (dir == hir::ParamDirection::kInOut) {
      output_vars_.push_back(param.var);
    }
  }

  // LRM 13.4.1 implicit result variable. A non-void function's same-name var is
  // a default-initialized body variable (declared without its source name,
  // which is the function's own and has to keep resolving to the function for
  // a self-recursive call): the leading completion-payload component, the value
  // a fall-through or value-less `return` carries. void functions and tasks
  // have none.
  if (src.result_var.has_value()) {
    code.Body().AppendStmt(LowerVarStart(
        *this, body_frame, *src.result_var, std::nullopt,
        DefaultInitialValue(
            *this, body_frame, src.body.procedural_vars.Get(*src.result_var))));
    result_var_ = src.result_var;
  }

  // A definition produces the completion its declaration fixes, so it reads
  // that interface from the declaration rather than assembling it from the
  // locals it just built.
  const mir::TypeId result_type = SubroutineCallTypeOf(*owner_, src);
  result_type_ = result_type;

  auto after = after_formals(code, bindings, body_frame, params);
  if (!after) return std::unexpected(std::move(after.error()));

  // A task carries a name, so any task can be a `disable` target (LRM 9.6.2)
  // and every task is therefore a region that consumes the effect naming it:
  // each activation leaves through its own body end and completes normally
  // there, so the enabling statement resumes and the completion payload is
  // still produced (the LRM leaves a disabled task's output values
  // unspecified). A function cannot be named and never suspends, so it needs
  // no region.
  const std::optional<StaticStorageHome> disable_target =
      owner_->Unit().types.Get(result_type).Is<mir::CoroutineType>()
          ? RootScope().disable_target
          : std::nullopt;
  if (disable_target.has_value()) {
    // The region brackets the whole body, so every activation of the task is
    // inside the target for as long as it runs -- which is what makes one
    // `disable` reach them all (LRM 9.6.2).
    mir::Block body_block;
    const WalkFrame inner_frame = body_frame.WithBlock(&body_block);
    auto lowered = LowerStraightLineBodyInto(
        *this, inner_frame, support::TargetLanding::kPastTheTarget);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    code.body->AppendStmt(BuildTargetExtent(
        *this, body_frame, std::move(body_block), *disable_target,
        support::TargetLanding::kPastTheTarget));
  } else {
    auto lowered = LowerStraightLineBodyInto(
        *this, body_frame, support::TargetLanding::kPastTheTarget);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
  }

  // Close the body with a trailing return of the fall-through payload, the same
  // completion a body falling off its end carries (LRM 13.3). The completion is
  // a real body statement, not a backend-appended epilogue.
  code.Body().AppendStmt(
      mir::ReturnStmt{.value = BuildReturnPayload(body_frame, std::nullopt)});

  code.params = std::move(params);
  code.result_type = result_type;
  return code;
}

auto ProcessLowerer::Run(const hir::SubroutineDecl& src)
    -> diag::Result<mir::CallableCode> {
  switch (src.kind) {
    case hir::SubroutineKind::kTask:
    case hir::SubroutineKind::kFunction:
      break;
    // A constraint block is handed the problem of the call of `randomize()`
    // calling it (LRM 18.5), after its object, and states its constraints to
    // it; the handle is held in a local for the body to name.
    case hir::SubroutineKind::kConstraintBlock:
    case hir::SubroutineKind::kStaticConstraintBlock:
      return LowerSubroutine(
          src,
          [&](mir::CallableCode&, CallableBindings& bindings, WalkFrame& frame,
              std::vector<mir::LocalId>& params) -> diag::Result<void> {
            mir::CompilationUnit& unit = owner_->Unit();
            const mir::TypeId handed =
                *ConstraintProblemParamTypeOf(unit, src.kind);
            const mir::LocalId param = bindings.DeclareAnonymous(handed);
            params.push_back(param);
            mir::Block& block = *frame.current_block;
            const mir::LocalId held =
                bindings.DeclareAnonymous(unit.builtins.constraint_problem);
            block.AppendStmt(
                mir::LocalDeclStmt{
                    .target = held,
                    .init = block.Read(block.exprs.Add(
                        mir::MakeDerefExpr(
                            block.ReadLocal(param, handed),
                            unit.builtins.constraint_problem)))});
            frame = frame.WithConstraintProblem(held);
            return {};
          });
  }
  const std::optional<mir::TypeId> report_type =
      ReportParamTypeOf(owner_->Unit(), src.kind);
  // A task is never run by a waited evaluation, so it has one form.
  if (!report_type.has_value()) {
    return LowerSubroutine(
        src,
        [](mir::CallableCode&, CallableBindings&, WalkFrame&,
           std::vector<mir::LocalId>&) -> diag::Result<void> { return {}; });
  }

  // A function is handed where to report what a call of it reads (LRM 9.4.2),
  // after its formals. The form a waited evaluation runs states there what its
  // body reaches as it reaches it, and is made from the same body by a lowerer
  // of its own; the form every other call runs states nothing and enters the
  // first where it is handed a report, so a body run for no evaluation carries
  // nothing of what one does, and hands the functions it calls none.
  ProcessLowerer recording_lowerer = *this;
  auto recording_code = recording_lowerer.LowerSubroutine(
      src,
      [&](mir::CallableCode&, CallableBindings& bindings, WalkFrame& frame,
          std::vector<mir::LocalId>& params) -> diag::Result<void> {
        const mir::LocalId report = bindings.DeclareAnonymous(*report_type);
        params.push_back(report);
        auto stated =
            recording_lowerer.BuildRecordingPrologue(frame, src.reads, report);
        if (!stated) return std::unexpected(std::move(stated.error()));
        frame = frame.WithReportingReachedTo(report);
        return {};
      });
  if (!recording_code) {
    return std::unexpected(std::move(recording_code.error()));
  }
  // It is declared beside the form that enters it -- a member of the class
  // that declares the function, or of the unit where none does -- so it is
  // entered on the same object.
  mir::CallableDecl recording_decl{
      .code = *std::move(recording_code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt};
  mir::DirectTarget recording;
  if (mir::Class* declaring = owner_ctor_frame_.current_class.cls;
      declaring != nullptr) {
    recording = mir::CallableTarget{
        .owner = owner_ctor_frame_.current_class.id,
        .slot = declaring->callables.Add(std::move(recording_decl))};
  } else {
    recording = mir::UnitCallableTarget{
        .slot = owner_->Unit().callables.Add(std::move(recording_decl))};
  }

  return LowerSubroutine(
      src,
      [&](mir::CallableCode& code, CallableBindings& bindings, WalkFrame& frame,
          std::vector<mir::LocalId>& params) -> diag::Result<void> {
        const mir::LocalId report = bindings.DeclareAnonymous(*report_type);
        params.push_back(report);
        ForwardToRecording(frame, code, params, report, recording);
        return {};
      });
}

void ProcessLowerer::RegisterConstructorFormals(
    const hir::SubroutineDecl& ctor, const WalkFrame& frame,
    std::vector<mir::LocalId>& params) {
  OpenActivationScope(*this, frame, DeclaredAhead(ctor));
  for (const auto& param : ctor.params) {
    switch (param.direction) {
      case hir::ParamDirection::kInput:
        params.push_back(DeclareParameterVar(
            *this, frame, param.var, ctor.body.procedural_vars.Get(param.var)));
        break;
      case hir::ParamDirection::kRef:
      case hir::ParamDirection::kConstRef: {
        const hir::ProceduralVarDecl& declared =
            ctor.body.procedural_vars.Get(param.var);
        params.push_back(DeclareReferenceFormal(
            *frame.bindings, param.var, declared,
            *ParamTypeOf(
                owner_->Unit(), owner_->TranslateType(declared.type),
                param.direction)));
        break;
      }
      case hir::ParamDirection::kOutput:
      case hir::ParamDirection::kInOut:
        throw InternalError(
            "ProcessLowerer::RegisterConstructorFormals: a construction "
            "carries nothing back to an 'output' or 'inout' actual, and "
            "AST-to-HIR refuses a constructor declaring one");
    }
  }
}

auto ProcessLowerer::LowerConstructorBodyInto(const WalkFrame& frame)
    -> diag::Result<void> {
  return LowerStraightLineBodyInto(
      *this, frame, support::TargetLanding::kPastTheTarget);
}

namespace {

// The design asked a wait to watch what `why` says cannot be watched yet. The
// body compiled whether or not a wait reached it, so the request fails here,
// at the wait that asked (LRM 9.4.2).
void AppendRefuseReport(
    const mir::CompilationUnit& unit, mir::Block& block,
    const std::string& why) {
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeCallExpr(
                  mir::Direct{.target = support::BuiltinFn::kRefuseReport},
                  {block.exprs.Add(
                      mir::MakeStringLiteral(unit.builtins.string, why))},
                  unit.builtins.void_type))});
}

}  // namespace

auto ProcessLowerer::ReportCellsRead(
    const WalkFrame& frame, const hir::Reads& reads, mir::LocalId report)
    -> diag::Result<void> {
  if (reads.unreportable.has_value()) {
    AppendRefuseReport(
        owner_->Unit(), *frame.current_block, *reads.unreportable);
    return {};
  }
  return ReportCells(*this, frame, report, reads.cells);
}

auto ProcessLowerer::BuildConstructionReport(
    const WalkFrame& frame, const hir::Reads& reads, mir::LocalId report)
    -> diag::Result<void> {
  mir::Block& block = *frame.current_block;
  mir::Block asked;
  auto reported = ReportCellsRead(frame.WithBlock(&asked), reads, report);
  if (!reported) return std::unexpected(std::move(reported.error()));
  block.AppendIfThen(
      BuildReportHanded(owner_->Unit(), block, report), std::move(asked));
  return {};
}

auto ProcessLowerer::BuildRecordingPrologue(
    const WalkFrame& frame, const hir::Reads& reads, mir::LocalId report)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = owner_->Unit();
  mir::Block& block = *frame.current_block;

  // An implicit list is collected without running anything (LRM 9.2.2.2.1):
  // the function states what its text reads and writes, has each function it
  // calls do the same, one level deeper, and hands back the defaults its
  // result and outputs start at. Past the depth bound the functions are ones a
  // shallower level of the same cycle already stated.
  if (!reads.unreportable.has_value()) {
    mir::Block entered;
    auto listed = ReportReads(*this, frame.WithBlock(&entered), reads, report);
    if (!listed) return std::unexpected(std::move(listed.error()));
    entered.AppendStmt(
        mir::ExprStmt{
            .expr = BuildReportCall(
                unit, entered, report, support::BuiltinFn::kReadReportLeave, {},
                unit.builtins.void_type)});
    mir::Block collecting;
    const mir::ExprId entered_at = BuildReportCall(
        unit, collecting, report, support::BuiltinFn::kReadReportEnter, {},
        unit.builtins.machine_int64);
    collecting.AppendIfThen(
        collecting.exprs.Add(
            mir::MakeBinary(
                unit, collecting, mir::BinaryOp::kInequality, entered_at,
                mir::BuildMachineIntLiteral(unit, collecting, 0),
                unit.builtins.machine_bool)),
        std::move(entered));
    collecting.AppendStmt(
        mir::ReturnStmt{
            .value = BuildReturnPayload(
                frame.WithBlock(&collecting), std::nullopt)});
    const mir::ExprId runs_the_body = BuildReportCall(
        unit, block, report, support::BuiltinFn::kReadReportRunsTheBody, {},
        unit.builtins.machine_int64);
    block.AppendIfThen(
        block.exprs.Add(
            mir::MakeBinary(
                unit, block, mir::BinaryOp::kEquality, runs_the_body,
                mir::BuildMachineIntLiteral(unit, block, 0),
                unit.builtins.machine_bool)),
        std::move(collecting));
  }

  // A waited evaluation runs the function, since it needs its value, and the
  // function states the cells its text reads before its body states what it
  // reaches through a handle.
  return ReportCellsRead(frame, reads, report);
}

void ProcessLowerer::ForwardToRecording(
    const WalkFrame& frame, const mir::CallableCode& code,
    std::span<const mir::LocalId> params, mir::LocalId report,
    mir::DirectTarget recording) {
  const mir::CompilationUnit& unit = owner_->Unit();
  mir::Block& block = *frame.current_block;
  mir::Block forwarding;
  // The recording form is entered on the object this one was, and handed
  // every other parameter as it was handed them.
  mir::Direct callee{.target = recording};
  std::vector<mir::ExprId> arguments;
  arguments.reserve(params.size());
  for (const mir::LocalId param : params) {
    const mir::ExprId value =
        forwarding.ReadLocal(param, code.locals.Get(param).type);
    if (param == code.receiver) {
      callee.receiver = BuildObjectDeref(unit, forwarding, value);
      continue;
    }
    arguments.push_back(value);
  }
  const mir::ExprId call = forwarding.exprs.Add(
      mir::MakeCallExpr(std::move(callee), std::move(arguments), result_type_));
  if (result_type_ == unit.builtins.void_type) {
    forwarding.AppendStmt(mir::ExprStmt{.expr = call});
    forwarding.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  } else {
    forwarding.AppendStmt(mir::ReturnStmt{.value = call});
  }
  block.AppendIfThen(
      BuildReportHanded(unit, block, report), std::move(forwarding));
}

auto ProcessLowerer::BuildReturnPayload(
    const WalkFrame& frame, std::optional<mir::ExprId> explicit_value)
    -> std::optional<mir::ExprId> {
  mir::Block& block = *frame.current_block;
  const mir::Type& result_ty = owner_->Unit().types.Get(result_type_);
  const mir::TypeId payload_type =
      result_ty.Is<mir::CoroutineType>()
          ? result_ty.Get<mir::CoroutineType>().payload
          : result_type_;
  if (payload_type == owner_->Unit().builtins.void_type) return std::nullopt;

  mir::CompilationUnit& unit = owner_->Unit();
  std::vector<mir::ExprId> components;
  const auto read = [&](hir::ProceduralVarId var) {
    return PathValue(
        unit, block,
        AccessPath{
            .owner =
                block.exprs.Add(LowerProceduralVarRefExpr(*this, frame, var)),
            .descent = {}});
  };
  if (result_var_.has_value()) {
    components.push_back(
        explicit_value.has_value() ? *explicit_value : read(*result_var_));
  }
  for (const hir::ProceduralVarId output : output_vars_) {
    components.push_back(read(output));
  }
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(components)},
          .type = payload_type});
}

}  // namespace lyra::lowering::hir_to_mir

#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/declared_variable.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/statement/assertions.hpp"
#include "lyra/lowering/hir_to_mir/statement/assignment.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/statement/branches.hpp"
#include "lyra/lowering/hir_to_mir/statement/flow.hpp"
#include "lyra/lowering/hir_to_mir/statement/fork_join.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/statement/procedural_continuous.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

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
            return diag::Fail(
                stmt.span, diag::DiagCode::kUnsupportedStatementForm,
                "a concurrent assertion embedded in a procedure is not yet "
                "lowered; pass --assertions skip to elide it");
          },
          [&](const hir::ConcurrentCoverStmt&) -> diag::Result<mir::Stmt> {
            return diag::Fail(
                stmt.span, diag::DiagCode::kUnsupportedStatementForm,
                "a concurrent cover statement is not yet lowered; pass "
                "--assertions skip to elide it");
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
      },
      stmt.data);
}

namespace {

auto LowerStraightLineBodyInto(ProcessLowerer& process, WalkFrame frame)
    -> diag::Result<void> {
  const hir::ProceduralBody& body = process.HirBody();
  auto lowered =
      process.LowerStmt(body.stmts.Get(process.HirRootStmt()), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  auto& body_block = *frame.current_block;
  body_block.AppendStmt(*std::move(lowered));
  return {};
}

auto LowerStraightLineProcess(ProcessLowerer& process)
    -> diag::Result<mir::CallableCode> {
  const WalkFrame& parent = process.OwnerCtorFrame();
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(process.Owner().Unit(), code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(), parent.current_class->self_pointer_type);
  code.params = {self_id};
  const WalkFrame body_frame =
      parent.WithBlock(&code.Body())
          .WithBindings(&bindings)
          .WithScopeNameBorrowedHandle(
              process.RootScope().NameBorrowedHandle());
  auto lowered = LowerStraightLineBodyInto(process, body_frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  // A process completes by falling off its end, which is a real body statement
  // rather than an implicit exit (LRM 9.2). Its coroutine result type is what
  // makes that return a coroutine completion.
  code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  code.result_type = process.Owner().Unit().builtins.coroutine_void;
  return code;
}

// Wraps the body in a `forever` loop. `implicit_sensitivity`, if present, is
// materialised into a value-change wait appended after the lowered body -- the
// always_comb / always_latch (LRM 9.2.2.2.1) tail. `always` / `always_ff` pass
// nullptr because the body itself carries any timing.
auto LowerForeverProcess(
    ProcessLowerer& process,
    const std::vector<hir::SensitivityEntry>* implicit_sensitivity)
    -> diag::Result<mir::CallableCode> {
  const WalkFrame& parent = process.OwnerCtorFrame();
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(process.Owner().Unit(), code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(), parent.current_class->self_pointer_type);
  code.params = {self_id};
  mir::Block body_block;
  {
    const WalkFrame body_frame =
        parent.WithBlock(&body_block)
            .WithBindings(&bindings)
            .WithScopeNameBorrowedHandle(
                process.RootScope().NameBorrowedHandle());
    auto lowered = LowerStraightLineBodyInto(process, body_frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    if (implicit_sensitivity != nullptr) {
      auto waited = BuildValueChangeWaitStmt(
          body_block, body_frame, process, *implicit_sensitivity,
          support::BuiltinFn::kWaitAny);
      if (!waited) return std::unexpected(std::move(waited.error()));
      body_block.AppendStmt(*std::move(waited));
    }
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

// A variable that starts at its type's default value (LRM Table 6-7 for a
// scalar, Table 7-1 for an aggregate): a formal that brings no value in, or the
// implicit result variable, both of which a body may read before writing.
auto DeclareDefaulted(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::Block& body,
    BindingOriginId origin, const std::optional<std::string>& name,
    hir::TypeId type) -> DeclaredVariable {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const DeclaredVariable variable = DeclareVariable(
      unit, *frame.bindings, body, origin, name,
      unit_lowerer.TranslateType(type), frame.body_can_wait);
  body.AppendStmt(InitializeVariable(
      unit, body, variable,
      body.exprs.Add(BuildDefaultValueFromHir(unit_lowerer, body, type))));
  return variable;
}

// A formal that brings a value in: the parameter the call hands it through,
// and the variable, which starts at that value. Where the body cannot wait the
// variable is the parameter itself.
auto DeclareParameter(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::Block& body,
    BindingOriginId origin, const std::optional<std::string>& name,
    mir::TypeId value_type) -> std::pair<mir::LocalId, DeclaredVariable> {
  if (!frame.body_can_wait) {
    const mir::LocalId parameter =
        frame.bindings->DeclareProcedural(origin, name, value_type);
    return {
        parameter, DeclaredVariable{.local = parameter, .type = value_type}};
  }
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::LocalId parameter = frame.bindings->Declare(
      BindingOriginId::Synthesized(unit_lowerer.NextSynthesizedSite(), 0),
      value_type);
  const DeclaredVariable variable = DeclareVariable(
      unit, *frame.bindings, body, origin, name, value_type,
      frame.body_can_wait);
  body.AppendStmt(InitializeVariable(
      unit, body, variable,
      body.exprs.Add(mir::MakeLocalRefExpr(parameter, value_type))));
  return {parameter, variable};
}

// Whether a subroutine's body can wait, which a task's can and a function's
// cannot (LRM 13.4.4).
auto CanWait(hir::SubroutineKind kind) -> bool {
  switch (kind) {
    case hir::SubroutineKind::kTask:
      return true;
    case hir::SubroutineKind::kFunction:
      return false;
  }
  throw InternalError("CanWait: unknown SubroutineKind");
}

}  // namespace

auto ProcessLowerer::Run(const hir::Process& src)
    -> diag::Result<mir::CallableCode> {
  switch (src.kind) {
    case hir::ProcessKind::kInitial:
    case hir::ProcessKind::kFinal:
      return LowerStraightLineProcess(*this);
    case hir::ProcessKind::kAlways:
    case hir::ProcessKind::kAlwaysFf:
      return LowerForeverProcess(*this, nullptr);
    case hir::ProcessKind::kAlwaysComb:
    case hir::ProcessKind::kAlwaysLatch:
      return LowerForeverProcess(*this, &src.implicit_sensitivity_list);
  }
  throw InternalError("ProcessLowerer::Run: unknown HIR ProcessKind");
}

auto ProcessLowerer::Run(const hir::SubroutineDecl& src)
    -> diag::Result<mir::CallableCode> {
  const WalkFrame& parent = owner_ctor_frame_;
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(owner_->Unit(), code);
  // A task or function is a scope the source named (LRM 23.9), so the body
  // starts in that scope's own name node and `%m` inside it reports the task,
  // not the instance around it. What it takes ahead of its formals is what the
  // class declaring it states; a subroutine of a namespace unit (LRM 26.3) has
  // no class and takes nothing.
  const WalkFrame entry_frame =
      parent.WithBlock(&code.Body())
          .WithBindings(&bindings)
          .WithScopeNameBorrowedHandle(RootScope().NameBorrowedHandle())
          .WithBodyCanWait(CanWait(src.kind));
  BoundImplicitParameters bound =
      parent.current_class == nullptr
          ? BoundImplicitParameters{.params = {}, .frame = entry_frame}
          : BindImplicitParameters(
                entry_frame, owner_->GetClassShape(parent.current_class_id),
                src.is_static ? CallableForm::kTypeAssociated
                              : CallableForm::kInstanceMember);
  const WalkFrame& body_frame = bound.frame;
  std::vector<mir::LocalId> params = std::move(bound.params);
  const mir::CompilationUnit& unit = owner_->Unit();

  // Formals normalize into the signature's data flow (LRM 13.5). Every formal
  // is a binding in the callable, identified by its HIR id; one that is no
  // parameter is a default-initialized body local instead, whose final value
  // rides the completion payload -- copied out at completion rather than
  // aliased live. An `inout` is both a parameter and a payload component.
  for (const auto& param : src.params) {
    const auto& hir_var = src.body.procedural_vars.Get(param.var);
    const mir::TypeId value_type = owner_->TranslateType(hir_var.type);
    const hir::ParamDirection dir = param.direction;
    const std::optional<mir::TypeId> param_type =
        ParamTypeOf(*owner_, hir_var.type, dir);

    const BindingOriginId origin = BindingOriginId::Procedural(param.var);
    if (!param_type.has_value()) {
      const DeclaredVariable variable = DeclareDefaulted(
          *owner_, body_frame, code.Body(), origin, hir_var.name, hir_var.type);
      MapProceduralVar(param.var, AutomaticVarBinding{.type = variable.type});
      output_locals_.push_back(variable);
      continue;
    }

    // A `ref` formal names its actual's storage, which reports its own writes
    // (LRM 13.5.2), so it is the parameter itself.
    if (unit.types.Get(*param_type).Is<mir::RefType>()) {
      const mir::LocalId mir_var =
          bindings.DeclareProcedural(origin, hir_var.name, *param_type);
      MapProceduralVar(param.var, AutomaticVarBinding{.type = *param_type});
      params.push_back(mir_var);
      continue;
    }

    const auto [parameter, variable] = DeclareParameter(
        *owner_, body_frame, code.Body(), origin, hir_var.name, value_type);
    MapProceduralVar(param.var, AutomaticVarBinding{.type = variable.type});
    params.push_back(parameter);
    if (dir == hir::ParamDirection::kInOut) {
      output_locals_.push_back(variable);
    }
  }

  // LRM 13.4.1 implicit result variable. A non-void function's same-name var is
  // a default-initialized body local (named distinctly from the C++ method so a
  // self-recursive call still resolves to the method): the leading
  // completion-payload component, the value a fall-through or value-less
  // `return` carries. void functions and tasks have none.
  if (src.result_var.has_value()) {
    const DeclaredVariable variable = DeclareDefaulted(
        *owner_, body_frame, code.Body(),
        BindingOriginId::Procedural(*src.result_var), std::nullopt,
        src.result_type);
    MapProceduralVar(
        *src.result_var, AutomaticVarBinding{.type = variable.type});
    result_var_ = variable;
  }

  // A definition produces the completion its declaration fixes, so it reads
  // that interface from the declaration rather than assembling it from the
  // locals it just built.
  const mir::TypeId result_type = SubroutineCallTypeOf(*owner_, src);
  result_type_ = result_type;

  // A function is handed where to report what a call of it reads (LRM 9.4.2),
  // after its formals. Handed one, it reports instead of running.
  if (const std::optional<mir::TypeId> report_type =
          ReportParamTypeOf(owner_->Unit(), src.kind)) {
    const mir::LocalId report = bindings.DeclareAnonymous(*report_type);
    params.push_back(report);
    auto asked = BuildReportPrologue(body_frame, src.reads, report);
    if (!asked) return std::unexpected(std::move(asked.error()));
  }

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
    auto lowered = LowerStraightLineBodyInto(*this, inner_frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    code.body->AppendStmt(BuildCancellableRegion(
        *this, body_frame, std::move(body_block), *disable_target));
  } else {
    auto lowered = LowerStraightLineBodyInto(*this, body_frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
  }

  // Close the body with a trailing return of the fall-through payload, the same
  // completion a body falling off its end carries (LRM 13.3). The completion is
  // a real body statement, not a backend-appended epilogue.
  code.Body().AppendStmt(
      mir::ReturnStmt{.value = BuildReturnPayload(code.Body(), std::nullopt)});

  code.params = std::move(params);
  code.result_type = result_type;
  return code;
}

auto ProcessLowerer::RegisterConstructorFormals(
    const hir::SubroutineDecl& ctor, const WalkFrame& frame,
    std::vector<mir::LocalId>& params) -> diag::Result<void> {
  for (const auto& param : ctor.params) {
    if (param.direction != hir::ParamDirection::kInput) {
      throw InternalError(
          "ProcessLowerer::RegisterConstructorFormals: a non-input "
          "constructor formal reached MIR lowering; AST-to-HIR rejects these");
    }
    const auto& hir_var = ctor.body.procedural_vars.Get(param.var);
    const mir::TypeId value_type = owner_->TranslateType(hir_var.type);
    const auto [parameter, variable] = DeclareParameter(
        *owner_, frame, *frame.current_block,
        BindingOriginId::Procedural(param.var), hir_var.name, value_type);
    MapProceduralVar(param.var, AutomaticVarBinding{.type = variable.type});
    params.push_back(parameter);
  }
  return {};
}

auto ProcessLowerer::LowerConstructorBodyInto(const WalkFrame& frame)
    -> diag::Result<void> {
  return LowerStraightLineBodyInto(*this, frame);
}

auto ProcessLowerer::BuildReportPrologue(
    const WalkFrame& frame, const hir::Reads& reads, mir::LocalId report)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = owner_->Unit();
  mir::Block& block = *frame.current_block;
  mir::Block asked;
  const WalkFrame asked_frame = frame.WithBlock(&asked);
  if (reads.unreportable.has_value()) {
    asked.AppendStmt(
        mir::ExprStmt{
            .expr = asked.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target =
                                        support::BuiltinFn::kRefuseReport},
                            .arguments = {asked.exprs.Add(
                                mir::MakeStringLiteral(
                                    unit.builtins.string,
                                    *reads.unreportable))}},
                    .type = unit.builtins.void_type})});
  } else {
    // The report bounds how deep reports nest, and past the bound this one
    // reports nothing of its own.
    mir::Block entered;
    auto reported =
        ReportReads(*this, asked_frame.WithBlock(&entered), reads, report);
    if (!reported) return std::unexpected(std::move(reported.error()));
    entered.AppendStmt(
        mir::ExprStmt{
            .expr = BuildReportCall(
                unit, entered, report, support::BuiltinFn::kReadReportLeave, {},
                unit.builtins.void_type)});
    const mir::ExprId goes_on = asked.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kInequality,
                    .lhs = BuildReportCall(
                        unit, asked, report,
                        support::BuiltinFn::kReadReportEnter, {},
                        unit.builtins.machine_int64),
                    .rhs = BuildMachineIntLiteral(unit, asked, 0)},
            .type = unit.builtins.machine_bool});
    asked.AppendStmt(
        mir::IfStmt{
            .condition = goes_on,
            .then_scope = asked.child_scopes.Add(std::move(entered)),
            .else_scope = std::nullopt});
  }
  // What the function would have settled is no part of a report, so it hands
  // back the defaults its result and outputs start at.
  asked.AppendStmt(
      mir::ReturnStmt{.value = BuildReturnPayload(asked, std::nullopt)});

  const mir::ExprId handed = block.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kInequality,
                  .lhs = block.exprs.Add(
                      mir::MakeLocalRefExpr(
                          report, unit.builtins.read_report_ptr)),
                  .rhs = block.exprs.Add(
                      mir::Expr{
                          .data = mir::NullLiteral{},
                          .type = unit.builtins.read_report_ptr})},
          .type = unit.builtins.machine_bool});
  block.AppendStmt(
      mir::IfStmt{
          .condition = handed,
          .then_scope = block.child_scopes.Add(std::move(asked)),
          .else_scope = std::nullopt});
  return {};
}

auto ProcessLowerer::BuildReturnPayload(
    mir::Block& block, std::optional<mir::ExprId> explicit_value)
    -> std::optional<mir::ExprId> {
  const mir::Type& result_ty = owner_->Unit().types.Get(result_type_);
  const mir::TypeId payload_type =
      result_ty.Is<mir::CoroutineType>()
          ? result_ty.Get<mir::CoroutineType>().payload
          : result_type_;
  if (payload_type == owner_->Unit().builtins.void_type) return std::nullopt;

  mir::CompilationUnit& unit = owner_->Unit();
  std::vector<mir::ExprId> components;
  if (result_var_.has_value()) {
    components.push_back(
        explicit_value.has_value() ? *explicit_value
                                   : ReadVariable(unit, block, *result_var_));
  }
  for (const DeclaredVariable& output : output_locals_) {
    components.push_back(ReadVariable(unit, block, output));
  }
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(components)},
          .type = payload_type});
}

}  // namespace lyra::lowering::hir_to_mir

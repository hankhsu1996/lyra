#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"

#include <cstddef>
#include <expected>
#include <optional>
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
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/statement/assertions.hpp"
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
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

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
            return LowerTimedStmt(*this, frame, stmt.label, t, stmt.span);
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

auto LowerStraightLineProcess(ProcessLowerer& process)
    -> diag::Result<mir::CallableCode> {
  const WalkFrame& parent = process.OwnerCtorFrame();
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(process.Owner().Unit(), code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{
          .name = "self", .type = parent.current_class->self_pointer_type});
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
      BindingOriginId::Receiver(),
      mir::LocalDecl{
          .name = "self", .type = parent.current_class->self_pointer_type});
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
      body_block.AppendStmt(BuildValueChangeWaitStmt(
          body_block, body_frame, process.EnclosingScopeLowerer(),
          *implicit_sensitivity));
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
  std::vector<mir::LocalId> params;
  // A callable's leading parameter is the ambient handle its body reaches
  // enclosing state through. An instance method (LRM 8.6) takes `self`, the
  // pointer to its object; a package callable (LRM 26.3) has no object and
  // instead takes the runtime handle directly -- the receiver-less peer of
  // `self`, through which it wakes a package variable's subscribers or suspends
  // a task. A static class method (LRM 8.10) has an owner class but no object,
  // so it takes neither. The handle is seeded for every callable of its form,
  // never derived from whether the body happens to use it, so no call site
  // re-derives the signature.
  const bool has_receiver = parent.current_class != nullptr && !src.is_static;
  body_has_receiver_ = has_receiver;
  if (has_receiver) {
    params.push_back(bindings.Declare(
        BindingOriginId::Receiver(),
        mir::LocalDecl{
            .name = "self", .type = parent.current_class->self_pointer_type}));
  } else if (parent.current_class == nullptr) {
    params.push_back(bindings.Declare(
        BindingOriginId::Runtime(),
        mir::LocalDecl{
            .name = "runtime", .type = owner_->Unit().builtins.effects}));
  }
  // A task or function is a scope the source named (LRM 23.9), so the body
  // starts in that scope's own name node and `%m` inside it reports the task,
  // not the instance around it.
  const WalkFrame body_frame =
      parent.WithBlock(&code.Body())
          .WithBindings(&bindings)
          .WithScopeNameBorrowedHandle(RootScope().NameBorrowedHandle());

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

    if (!param_type.has_value()) {
      const mir::ExprId default_init = code.Body().exprs.Add(
          BuildDefaultValueFromHir(*owner_, body_frame, hir_var.type));
      const mir::LocalId local = bindings.Declare(
          BindingOriginId::Procedural(param.var),
          mir::LocalDecl{.name = hir_var.name, .type = value_type});
      code.Body().AppendStmt(
          mir::LocalDeclStmt{.target = local, .init = default_init});
      MapProceduralVar(param.var, AutomaticVarBinding{.type = value_type});
      output_pack_vars_.push_back(local);
      output_pack_types_.push_back(value_type);
      continue;
    }

    const mir::LocalId mir_var = bindings.Declare(
        BindingOriginId::Procedural(param.var),
        mir::LocalDecl{.name = hir_var.name, .type = *param_type});
    MapProceduralVar(param.var, AutomaticVarBinding{.type = *param_type});
    params.push_back(mir_var);
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
  if (src.result_var.has_value()) {
    const mir::TypeId ret_type = owner_->TranslateType(src.result_type);
    const mir::ExprId default_init = code.Body().exprs.Add(
        BuildDefaultValueFromHir(*owner_, body_frame, src.result_type));
    const mir::LocalId result_local = bindings.Declare(
        BindingOriginId::Procedural(*src.result_var),
        mir::LocalDecl{.name = "_lyra_result", .type = ret_type});
    code.Body().AppendStmt(
        mir::LocalDeclStmt{.target = result_local, .init = default_init});
    MapProceduralVar(*src.result_var, AutomaticVarBinding{.type = ret_type});
    result_var_ = result_local;
    result_value_type_ = ret_type;
  }

  // A definition produces the completion its declaration fixes, so it reads
  // that interface from the declaration rather than assembling it from the
  // locals it just built.
  const mir::TypeId result_type = SubroutineCallTypeOf(*owner_, src);
  result_type_ = result_type;

  // A task carries a name, so any task can be a `disable` target (LRM 9.6.2)
  // and every task is therefore a region that consumes the effect naming it:
  // each activation leaves through its own body end and completes normally
  // there, so the enabling statement resumes and the completion payload is
  // still produced (the LRM leaves a disabled task's output values
  // unspecified). A function cannot be named and never suspends, so it needs
  // no region, and neither does a body with no object to reach the target
  // through.
  const std::optional<mir::FieldId> cancel_target =
      owner_->Unit().types.Get(result_type).Is<mir::CoroutineType>() &&
              has_receiver
          ? RootScope().cancellation_target
          : std::nullopt;
  if (cancel_target.has_value()) {
    // The region brackets the whole body, so every activation of the task is
    // inside the target for as long as it runs -- which is what makes one
    // `disable` reach them all (LRM 9.6.2).
    mir::Block body_block;
    const WalkFrame inner_frame = body_frame.WithBlock(&body_block);
    auto lowered = LowerStraightLineBodyInto(*this, inner_frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    code.body->AppendStmt(BuildCancellableRegion(
        *this, body_frame, std::move(body_block), *cancel_target));
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
    const mir::LocalId mir_var = frame.bindings->Declare(
        BindingOriginId::Procedural(param.var),
        mir::LocalDecl{.name = hir_var.name, .type = value_type});
    MapProceduralVar(param.var, AutomaticVarBinding{.type = value_type});
    params.push_back(mir_var);
  }
  return {};
}

auto ProcessLowerer::LowerConstructorBodyInto(const WalkFrame& frame)
    -> diag::Result<void> {
  return LowerStraightLineBodyInto(*this, frame);
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

  std::vector<mir::ExprId> components;
  if (result_var_.has_value()) {
    components.push_back(
        explicit_value.has_value()
            ? *explicit_value
            : block.exprs.Add(
                  mir::MakeLocalRefExpr(*result_var_, result_value_type_)));
  }
  for (std::size_t i = 0; i < output_pack_vars_.size(); ++i) {
    components.push_back(block.exprs.Add(
        mir::MakeLocalRefExpr(output_pack_vars_[i], output_pack_types_[i])));
  }
  return block.exprs.Add(
      mir::Expr{
          .data = mir::TupleExpr{.components = std::move(components)},
          .type = payload_type});
}

}  // namespace lyra::lowering::hir_to_mir

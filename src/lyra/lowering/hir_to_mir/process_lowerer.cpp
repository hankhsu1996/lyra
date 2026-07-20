#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"

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
#include "lyra/lowering/hir_to_mir/completion_payload.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
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
          [&](const hir::WaitForkStmt&) {
            return LowerWaitForkStmt(*this, frame, stmt.label);
          },
          [&](const hir::DisableForkStmt&) {
            return LowerDisableForkStmt(*this, frame, stmt.label);
          },
      },
      stmt.data);
}

auto ProcessLowerer::BuildStaticStorageAccess(
    const WalkFrame& frame, StaticStoragePlacement placement) const
    -> mir::Expr {
  mir::Block& block = *frame.current_block;
  const mir::CompilationUnit& unit = owner_->Unit();

  // Intra-unit access is a typed segment: descend from the body's `self`
  // through each intervening materialized scope's borrowed companion member,
  // then project the static cell. A static whose physical owner is the
  // enclosing class has an empty chain and is reached directly on `self`.
  const std::vector<mir::FieldId> chain =
      storage_plan_->CompanionChainTo(placement.owner);
  mir::ExprId receiver = block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  for (const mir::FieldId step : chain) {
    const mir::TypeId receiver_type = block.exprs.Get(receiver).type;
    const auto* ptr =
        std::get_if<mir::PointerType>(&unit.types.Get(receiver_type).data);
    if (ptr == nullptr) {
      throw InternalError(
          "ProcessLowerer::BuildStaticStorageAccess: companion chain step "
          "receiver is not a pointer type");
    }
    const auto* obj =
        std::get_if<mir::ObjectType>(&unit.types.Get(ptr->pointee).data);
    if (obj == nullptr) {
      throw InternalError(
          "ProcessLowerer::BuildStaticStorageAccess: companion chain step "
          "receiver's pointee is not an intra-unit object type");
    }
    // The enclosing class is still being built when this lowering runs, so
    // member types come from the published shape, not the not-yet-committed
    // mir class.
    const mir::TypeId step_type =
        owner_->GetClassShape(obj->class_id).fields.Get(step).type;
    receiver = block.exprs.Add(
        mir::MakeFieldAccessExpr(
            receiver, mir::FieldTarget{.owner = obj->class_id, .slot = step},
            step_type));
  }
  // The enclosing class is the one bound on the frame; a procedural scope
  // owner consults the materialization table.
  const mir::ClassId owner_class_id = std::visit(
      Overloaded{
          [&](EnclosingClass) -> mir::ClassId {
            return frame.current_class_id;
          },
          [&](hir::ProceduralScopeId sid) -> mir::ClassId {
            return storage_plan_->ScopeMaterialization(sid).class_id;
          }},
      placement.owner);
  const mir::TypeId field_type =
      owner_->GetClassShape(owner_class_id).fields.Get(placement.field).type;
  return mir::MakeFieldAccessExpr(
      receiver,
      mir::FieldTarget{.owner = owner_class_id, .slot = placement.field},
      field_type);
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
  mir::CallableCode code;
  CallableBindings bindings(process.Owner().Unit(), code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{
          .name = "self", .type = parent.current_class->self_pointer_type});
  code.params = {self_id};
  const WalkFrame body_frame =
      parent.WithBlock(&code.body).WithBindings(&bindings);
  auto lowered = LowerStraightLineBodyInto(process, body_frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  // A process completes by falling off its end, which is a real body statement
  // rather than an implicit exit (LRM 9.2). Its coroutine result type is what
  // makes that return a coroutine completion.
  code.body.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
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
  mir::CallableCode code;
  CallableBindings bindings(process.Owner().Unit(), code);
  const mir::LocalId self_id = bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{
          .name = "self", .type = parent.current_class->self_pointer_type});
  code.params = {self_id};
  mir::Block body_block;
  {
    const WalkFrame body_frame =
        parent.WithBlock(&body_block).WithBindings(&bindings);
    auto lowered = LowerStraightLineBodyInto(process, body_frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    if (implicit_sensitivity != nullptr) {
      body_block.AppendStmt(MakeValueChangeWaitStmt(
          body_block, body_frame, process.EnclosingScopeLowerer(),
          *implicit_sensitivity));
    }
  }

  const mir::BlockId body_scope_id =
      code.body.child_scopes.Add(std::move(body_block));
  code.body.AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id});
  code.body.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
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
  mir::CallableCode code;
  CallableBindings bindings(owner_->Unit(), code);
  std::vector<mir::LocalId> params;
  // A callable's leading parameter is the ambient handle its body reaches
  // enclosing state through. An instance method (LRM 8.6) takes `self`, the
  // pointer to its object; a package callable (LRM 26.3) has no object and
  // instead takes the engine services directly -- the receiver-less peer of
  // `self`, through which it wakes a package variable's subscribers or suspends
  // a task. A static class method (LRM 8.10) has an owner class but no object,
  // so it takes neither. The handle is seeded for every callable of its form,
  // never derived from whether the body happens to use it, so no call site
  // re-derives the signature.
  const bool has_receiver = parent.current_class != nullptr && !src.is_static;
  if (has_receiver) {
    params.push_back(bindings.Declare(
        BindingOriginId::Receiver(),
        mir::LocalDecl{
            .name = "self", .type = parent.current_class->self_pointer_type}));
  } else if (parent.current_class == nullptr) {
    params.push_back(bindings.Declare(
        BindingOriginId::Services(),
        mir::LocalDecl{
            .name = "services", .type = owner_->Unit().builtins.services}));
  }
  // A task body suspends on timing controls, so its call protocol is the
  // coroutine one; a function body executes synchronously.
  const bool body_is_coroutine = src.kind == hir::SubroutineKind::kTask;
  const WalkFrame body_frame =
      parent.WithBlock(&code.body).WithBindings(&bindings);

  // Formals normalize into the signature's data flow (LRM 13.5). An `input` and
  // a `ref` / `const ref` are parameters (the latter carries a `Ref<T>`); an
  // `output` is not a parameter but a default-initialized body local whose
  // final value rides the completion payload (copy-out at completion, not a
  // live alias); an `inout` is both an input parameter and a payload component.
  // Every formal is a binding in the callable, identified by its HIR id.
  for (const auto& param : src.params) {
    const auto& hir_var = src.body.procedural_vars.Get(param.var);
    const mir::TypeId value_type = owner_->TranslateType(hir_var.type);
    const hir::ParamDirection dir = param.direction;

    if (dir == hir::ParamDirection::kOutput) {
      const mir::ExprId default_init = code.body.exprs.Add(
          BuildDefaultValueFromHir(*owner_, body_frame, hir_var.type));
      const mir::LocalId local = bindings.Declare(
          BindingOriginId::Procedural(param.var),
          mir::LocalDecl{.name = hir_var.name, .type = value_type});
      code.body.AppendStmt(
          mir::LocalDeclStmt{.target = local, .init = default_init});
      MapProceduralVar(param.var, AutomaticVarBinding{.type = value_type});
      output_pack_vars_.push_back(local);
      output_pack_types_.push_back(value_type);
      continue;
    }

    // A `ref` / `const ref` formal's parameter type is a `Ref<T>` over the
    // value type (LRM 13.5.2); every other parameter carries the value type.
    const bool by_reference = dir == hir::ParamDirection::kRef ||
                              dir == hir::ParamDirection::kConstRef;
    const mir::TypeId type =
        by_reference
            ? owner_->Unit().types.Intern(
                  mir::RefType{
                      .pointee = value_type,
                      .mutability = dir == hir::ParamDirection::kConstRef
                                        ? mir::Mutability::kReadOnly
                                        : mir::Mutability::kMutable})
            : value_type;
    const mir::LocalId mir_var = bindings.Declare(
        BindingOriginId::Procedural(param.var),
        mir::LocalDecl{.name = hir_var.name, .type = type});
    MapProceduralVar(param.var, AutomaticVarBinding{.type = type});
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
  std::vector<mir::TypeId> payload_components;
  if (src.result_var.has_value()) {
    const mir::TypeId ret_type = owner_->TranslateType(src.result_type);
    const mir::ExprId default_init = code.body.exprs.Add(
        BuildDefaultValueFromHir(*owner_, body_frame, src.result_type));
    const mir::LocalId result_local = bindings.Declare(
        BindingOriginId::Procedural(*src.result_var),
        mir::LocalDecl{.name = "_lyra_result", .type = ret_type});
    code.body.AppendStmt(
        mir::LocalDeclStmt{.target = result_local, .init = default_init});
    MapProceduralVar(*src.result_var, AutomaticVarBinding{.type = ret_type});
    result_var_ = result_local;
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
      NormalizeCompletionPayload(owner_->Unit(), payload_components);
  const mir::TypeId result_type =
      body_is_coroutine
          ? owner_->Unit().types.CoroutineOf(completion_payload_type_)
          : completion_payload_type_;

  auto lowered = LowerStraightLineBodyInto(*this, body_frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));

  // Close the body with a trailing return of the fall-through payload, or with
  // a bare return when the payload is empty (a void function, or a task with no
  // outputs, which completes by falling off its end -- LRM 13.3). Either way
  // the completion is a real body statement, not a backend-appended epilogue.
  if (auto trailing = BuildReturnPayload(code.body, std::nullopt)) {
    code.body.AppendStmt(mir::ReturnStmt{.value = trailing});
  } else if (body_is_coroutine) {
    code.body.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  }

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
  if (components.empty()) return std::nullopt;
  if (components.size() == 1) return components.front();
  return block.exprs.Add(
      mir::Expr{
          .data = mir::TupleExpr{.components = std::move(components)},
          .type = completion_payload_type_});
}

}  // namespace lyra::lowering::hir_to_mir

#include "lyra/lowering/hir_to_mir/extern_binding.hpp"

#include <cstddef>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {
namespace {

auto ObjectTypeOf(mir::CompilationUnit& unit, mir::ClassId cls) -> mir::TypeId {
  return unit.types.Intern(mir::Type{mir::ObjectType{.of = cls}});
}

// The one behavior of the callback `binding` states.
auto InvokeSlot(const ExternBinding& binding) -> mir::VirtualSlot {
  return mir::VirtualSlot{
      .owner_class = binding.callback, .slot = mir::CallableId{0}};
}

// A call of the definition `handle` holds, on `arguments`.
auto CallOfDefinition(
    mir::CompilationUnit& unit, mir::Builder block,
    const ExternBinding& binding, mir::ExprId handle,
    std::vector<mir::ExprId> arguments) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Virtual{
                      .receiver = block.Deref(
                          unit.types, handle,
                          ObjectTypeOf(unit, binding.callback)),
                      .slot = InvokeSlot(binding)},
              .arguments = std::move(arguments)},
      .type = binding.invoke.result_type};
}

// The object the body of `code`, a method of `cls`, runs on, as a place.
auto OwnObject(
    mir::CompilationUnit& unit, mir::Builder block,
    const mir::CallableCode& code, mir::ClassId cls) -> mir::ExprId {
  const mir::LocalId self = *code.receiver;
  return block.Deref(
      unit.types, block.ReadLocal(self, code.locals.Get(self).type),
      ObjectTypeOf(unit, cls));
}

// A body stating `prototype` as a method of `cls`: the same formals and answer,
// taken on an object of `cls`, and nothing in it yet. It is lowered from
// `declared`, where the subroutine it stands for is declared.
auto BodyOfPrototype(
    UnitLowerer& unit_lowerer, const mir::CallableCode& prototype,
    mir::ClassId cls, diag::SourceSpan declared) -> mir::CallableCode {
  mir::CallableCode code = prototype;
  code.locals = {};
  code.named_locals = {};
  code.span = declared;
  std::vector<mir::LocalId> params{
      code.AddLocal(unit_lowerer.SelfPointerTo(cls))};
  for (const mir::LocalId param : prototype.ParamsAfterReceiver()) {
    params.push_back(code.AddLocal(prototype.locals.Get(param).type));
  }
  code.receiver = params.front();
  if (prototype.caller_location.has_value()) {
    code.caller_location = params.back();
  }
  code.params = std::move(params);
  code.body.emplace();
  return code;
}

// The formals of `code` past its object, each read as what it holds. Where the
// body was called is no formal: the stage that states it hands it to every
// call taking one, and a definition the design wrote takes none.
auto FormalsOf(mir::Builder block, const mir::CallableCode& code)
    -> std::vector<mir::ExprId> {
  std::vector<mir::ExprId> formals;
  for (const mir::LocalId param : code.ParamsAfterReceiver()) {
    if (param == code.caller_location) continue;
    formals.push_back(block.ReadLocal(param, code.locals.Get(param).type));
  }
  return formals;
}

// Ends the body of `code` with `call`, which takes what the body took: its
// result is the body's, and a body that is itself an execution waits for the
// one the call started and completes with what that completed with (LRM 13.3).
void AnswerWithCall(
    mir::CompilationUnit& unit, mir::Builder block, mir::CallableCode& code,
    mir::Expr call) {
  const mir::ExprId called = block.Add(std::move(call));
  const auto* execution =
      unit.types.Get(code.result_type).As<mir::CoroutineType>();
  const mir::TypeId answer =
      execution != nullptr ? execution->payload : code.result_type;
  const mir::ExprId finished =
      execution != nullptr
          ? block.Add(
                mir::Expr{
                    .data = mir::AwaitExpr{.execution = called},
                    .type = answer})
          : called;
  const mir::LocalId held = code.AddLocal(answer);
  block.AppendStmt(mir::LocalDeclStmt{.target = held, .init = finished});
  block.AppendStmt(mir::ReturnStmt{.value = block.ReadLocal(held, answer)});
}

// Runs the definition `handle` holds on the formals of the task `declared` the
// body at `frame` is a body of, as they stand there, and writes what it
// completes with to the task's `output` and `inout` formals.
auto RunDefinition(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::SubroutineDecl& declared, const ExternBinding& binding,
    mir::ExprId handle) -> diag::Result<void> {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Builder block = frame.Builder();
  std::vector<mir::ExprId> formals;
  std::vector<CompletionWriteback> writebacks;
  const CompletionLayout layout = BuildCompletionLayout(
      CalleeFormalsOf(unit_lowerer, declared), std::nullopt);
  for (std::size_t i = 0; i < declared.params.size(); ++i) {
    const hir::SubroutineParam& param = declared.params[i];
    const mir::TypeId value = unit_lowerer.TranslateType(param.type);
    const auto named = [&] {
      return block.Add(LowerFormalVariableExpr(process, frame, param.variable));
    };
    const auto read = [&] {
      return PathValue(
          unit, block, AccessPath{.owner = named(), .descent = {}});
    };
    const auto written = [&] {
      writebacks.push_back(
          CompletionWriteback{
              .target =
                  Lvalue{
                      .form = AccessPath{.owner = named(), .descent = {}},
                      .type = value,
                      .source_type = param.type,
                      .span = frame.span},
              .component = *layout.formals[i].component,
              .type = value});
    };
    switch (param.direction) {
      case hir::ParamDirection::kInput:
        formals.push_back(read());
        break;
      case hir::ParamDirection::kOutput:
        written();
        break;
      case hir::ParamDirection::kInOut:
        formals.push_back(read());
        written();
        break;
      // The reference itself is handed on, so the definition reaches what the
      // caller storage.
      case hir::ParamDirection::kRef:
      case hir::ParamDirection::kConstRef:
        formals.push_back(block.Read(named()));
        break;
    }
  }
  const mir::TypeId payload = unit.types.Get(binding.invoke.result_type)
                                  .Get<mir::CoroutineType>()
                                  .payload;
  auto completed = BindCompletion(
      unit_lowerer, frame,
      CallOfDefinition(unit, block, binding, handle, std::move(formals)),
      payload, writebacks);
  if (!completed) return std::unexpected(std::move(completed.error()));
  return {};
}

// What the object at `frame` keeps in its field `storage`, as a place.
auto ExternStoragePlace(
    mir::CompilationUnit& unit, const WalkFrame& frame, mir::ClassId owner,
    mir::FieldId storage, mir::TypeId holder) -> mir::ExprId {
  mir::Builder block = frame.Builder();
  return block.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(
              unit, block, block.Read(block.Add(MakeSelfRefExpr(frame)))),
          mir::ClassFieldTarget{.owner = owner, .slot = storage}, holder));
}

// Starts every definition the object keeps in its field `storage` and waits for
// those alone (LRM 25.7.4). A process of its own does the starting, so the
// definitions are the only children it has, and `wait fork` means them.
auto RunEveryDefinition(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::SubroutineDecl& declared, mir::ClassId owner,
    mir::FieldId storage, const ExternBinding& binding) -> diag::Result<void> {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const auto& builtins = unit.builtins;
  const mir::TypeId queue = ExternStorageType(unit, binding);
  mir::Builder block = frame.Builder();

  const mir::ExprId runtime = block.Add(BuildCurrentRuntimeCallExpr(unit));
  ClosureBuilder starting(unit, frame, support::ClosureKind::kFnOnce);
  const WalkFrame starter = starting.Frame().WithRunsAsFunctionCall(false);
  mir::Builder start = starter.Builder();

  const mir::LocalId left = starter.bindings->DeclareAnonymous(queue);
  start.AppendStmt(
      mir::LocalDeclStmt{
          .target = left,
          .init = start.Read(
              ExternStoragePlace(unit, starter, owner, storage, queue))});
  const mir::ExprId any_left = ReduceToCondition(
      unit, start,
      start.Add(BuildBuiltinMethodCall(
          unit, start, support::BuiltinFn::kSize, start.LocalPlace(left, queue),
          {}, builtins.int_type)));

  mir::Block loop;
  const WalkFrame each = starter.WithBlock(&loop);
  mir::Builder step = each.Builder();
  const BindingOriginId taken_origin = DeclareSynthesizedLocal(
      unit_lowerer, each,
      step.Add(BuildBuiltinMethodCall(
          unit, step, support::BuiltinFn::kPopFront,
          step.LocalPlace(left, queue), {}, binding.handle)));
  ClosureBuilder running(unit, each, support::ClosureKind::kFnOnce);
  const WalkFrame branch = running.Frame().WithRunsAsFunctionCall(false);
  mir::Builder in_branch = branch.Builder();
  const mir::Expr place = branch.bindings->MakeBindingPlace(
      branch.bindings->EnsureCarrier(taken_origin), in_branch);
  auto ran = RunDefinition(
      process, branch, declared, binding, in_branch.Read(in_branch.Add(place)));
  if (!ran) return ran;
  const mir::ExprId started = step.Add(running.BuildCallOnce());
  const mir::ExprId one = step.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = {started}},
          .type = mir::MachineArrayOf(unit.types, builtins.coroutine_void, 1)});
  step.AppendStmt(
      mir::ExprStmt{
          .expr = step.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target = support::BuiltinFn::kSpawnAll},
                          .arguments =
                              {step.Add(BuildCurrentRuntimeCallExpr(unit)),
                               one}},
                  .type = builtins.void_type})});
  start.AppendStmt(
      mir::WhileStmt{
          .condition = any_left, .scope = start.AddScope(std::move(loop))});
  start.AppendStmt(BuildStopStmt(
      unit_lowerer, starter,
      start.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = support::BuiltinFn::kWaitFork},
                      .arguments = {start.Add(
                          BuildCurrentRuntimeCallExpr(unit))}},
              .type = builtins.wait})));

  const mir::ExprId branches = block.Add(
      mir::Expr{
          .data =
              mir::CompositeExpr{
                  .parts = {block.Add(starting.BuildCallOnce())}},
          .type = mir::MachineArrayOf(unit.types, builtins.coroutine_void, 1)});
  block.AppendStmt(BuildStopStmt(
      unit_lowerer, frame,
      block.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kForkWaitAll},
                      .arguments = {runtime, branches}},
              .type = builtins.wait})));
  return {};
}

}  // namespace

auto ExternStorageType(mir::CompilationUnit& unit, const ExternBinding& binding)
    -> mir::TypeId {
  switch (binding.form) {
    case hir::ExternForm::kExtern:
      return binding.handle;
    case hir::ExternForm::kExternForkJoin:
      return mir::QueueOf(
          unit.types, binding.handle, support::UnboundedQueue{});
  }
  throw InternalError("hir_to_mir: unknown ExternForm");
}

auto WriteExternTaskBody(
    ProcessLowerer& process, const WalkFrame& body_frame,
    const hir::SubroutineDecl& declared, mir::ClassId owner,
    mir::FieldId storage, const ExternBinding& binding) -> diag::Result<void> {
  mir::CompilationUnit& unit = process.Owner().Unit();
  // The task has no body of its own, so what is written for it is lowered from
  // its declaration.
  const WalkFrame frame = body_frame.WithSpan(declared.span);
  mir::Builder block = frame.Builder();
  switch (binding.form) {
    case hir::ExternForm::kExtern: {
      const mir::ExprId held = block.Read(
          ExternStoragePlace(unit, frame, owner, storage, binding.handle));
      return RunDefinition(process, frame, declared, binding, held);
    }
    case hir::ExternForm::kExternForkJoin: {
      for (const hir::SubroutineParam& param : declared.params) {
        switch (param.direction) {
          case hir::ParamDirection::kRef:
          case hir::ParamDirection::kConstRef:
            return diag::Fail(
                frame.span, diag::DiagCode::kUnsupportedStatementForm,
                "a 'ref' formal of an 'extern forkjoin' task is not yet "
                "supported");
          case hir::ParamDirection::kInput:
          case hir::ParamDirection::kOutput:
          case hir::ParamDirection::kInOut:
            break;
        }
      }
      // No connected module defined it: a run-time error, and the call returns
      // with no effect (LRM 25.7.4).
      const mir::TypeId queue = ExternStorageType(unit, binding);
      const mir::ExprId none = BuildConditionNot(
          unit, block,
          ReduceToCondition(
              unit, block,
              block.Add(BuildBuiltinMethodCall(
                  unit, block, support::BuiltinFn::kSize,
                  ExternStoragePlace(unit, frame, owner, storage, queue), {},
                  unit.builtins.int_type))));
      mir::Block report;
      AppendToolReportStmt(
          process.Owner().Unit(), frame.WithBlock(&report),
          support::kErrorReport,
          base::DiagMessage(
              "no connected module defines the task '{}'", declared.name));
      block.AppendIfThen(none, std::move(report));
      return RunEveryDefinition(
          process, frame, declared, owner, storage, binding);
    }
  }
  throw InternalError("hir_to_mir: unknown ExternForm");
}

void InitializeExternStorage(
    mir::CompilationUnit& unit, const WalkFrame& frame, mir::ClassId owner,
    mir::FieldId storage, const ExternBinding& binding) {
  switch (binding.form) {
    // Nothing is storage until the definer resolves, and nothing calls it
    // before.
    case hir::ExternForm::kExtern:
      return;
    case hir::ExternForm::kExternForkJoin: {
      mir::Builder block = frame.Builder();
      const mir::TypeId queue = ExternStorageType(unit, binding);
      const mir::ExprId target =
          ExternStoragePlace(unit, frame, owner, storage, queue);
      block.AppendStmt(
          mir::ExprStmt{
              .expr = block.Add(
                  mir::MakeAssignExpr(
                      unit.builtins, target,
                      block.Add(BuildDefaultValueExpr(unit, block, queue))))});
      return;
    }
  }
  throw InternalError("hir_to_mir: unknown ExternForm");
}

auto ExternFunctionCode(
    UnitLowerer& unit_lowerer, mir::ClassId owner, mir::FieldId storage,
    const ExternBinding& binding, diag::SourceSpan declared)
    -> mir::CallableCode {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::CallableCode code =
      BodyOfPrototype(unit_lowerer, binding.invoke, owner, declared);
  mir::Builder block(code.Body(), code.span);

  const mir::ExprId kept = block.Read(block.Add(
      mir::MakeFieldAccessExpr(
          OwnObject(unit, block, code, owner),
          mir::ClassFieldTarget{.owner = owner, .slot = storage},
          binding.handle)));
  AnswerWithCall(
      unit, block, code,
      CallOfDefinition(unit, block, binding, kept, FormalsOf(block, code)));
  return code;
}

auto ExternBindCode(
    UnitLowerer& unit_lowerer, mir::ClassId owner, mir::FieldId storage,
    const ExternBinding& binding, diag::SourceSpan declared)
    -> mir::CallableCode {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::CallableCode code =
      BodyOfPrototype(unit_lowerer, binding.bind_prototype, owner, declared);
  mir::Builder block(code.Body(), code.span);
  const mir::ExprId target = block.Add(
      mir::MakeFieldAccessExpr(
          OwnObject(unit, block, code, owner),
          mir::ClassFieldTarget{.owner = owner, .slot = storage},
          ExternStorageType(unit, binding)));
  const mir::ExprId handed =
      block.ReadLocal(code.ParamsAfterReceiver().front(), binding.handle);
  switch (binding.form) {
    case hir::ExternForm::kExtern:
      block.AppendStmt(
          mir::ExprStmt{
              .expr = block.Add(
                  mir::MakeAssignExpr(unit.builtins, target, handed))});
      return code;
    case hir::ExternForm::kExternForkJoin:
      block.AppendStmt(
          mir::ExprStmt{
              .expr = block.Add(BuildBuiltinMethodCall(
                  unit, block, support::BuiltinFn::kPushBack, target, {handed},
                  unit.builtins.void_type))});
      return code;
  }
  throw InternalError("hir_to_mir: unknown ExternForm");
}

auto DefineExternCallback(
    UnitLowerer& unit_lowerer, mir::ClassId owner, mir::CallableId definition,
    const ExternBinding& binding) -> mir::ClassId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::ClassId id = unit.DeclareClass();
  const mir::TypeId self_pointer = unit_lowerer.SelfPointerTo(id);
  const mir::TypeId owner_pointer = unit_lowerer.SelfPointerTo(owner);

  mir::Class cls{};
  cls.identity = unit_lowerer.NameUnnamedClass();
  cls.base = mir::ClassRef{mir::ObjectRootRef{}};
  cls.implements = {binding.callback};
  cls.is_final = true;
  cls.self_pointer_type = self_pointer;
  const mir::FieldId held =
      cls.fields.Add(mir::FieldDecl{.type = owner_pointer});

  // Made with the object it runs on, which outlives it.
  mir::CallableCode constructor =
      mir::CallableCode::Defined(diag::SourceSpan{});
  const mir::LocalId self = constructor.AddLocal(self_pointer);
  const mir::LocalId object = constructor.AddLocal(owner_pointer);
  constructor.params = {self, object};
  constructor.receiver = self;
  constructor.result_type = unit.builtins.void_type;
  {
    mir::Builder block(constructor.Body(), constructor.span);
    const mir::ExprId target = block.Add(
        mir::MakeFieldAccessExpr(
            OwnObject(unit, block, constructor, id),
            mir::ClassFieldTarget{.owner = id, .slot = held}, owner_pointer));
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.Add(
                mir::MakeAssignExpr(
                    unit.builtins, target,
                    block.ReadLocal(object, owner_pointer)))});
  }
  cls.constructor = mir::ConstructorDecl{.code = std::move(constructor)};

  mir::CallableCode invoke =
      BodyOfPrototype(unit_lowerer, binding.invoke, id, binding.invoke.span);
  {
    mir::Builder block(invoke.Body(), invoke.span);
    const mir::ExprId object_held = block.Read(block.Add(
        mir::MakeFieldAccessExpr(
            OwnObject(unit, block, invoke, id),
            mir::ClassFieldTarget{.owner = id, .slot = held}, owner_pointer)));
    const mir::ExprId on =
        block.Deref(unit.types, object_held, ObjectTypeOf(unit, owner));
    std::vector<mir::ExprId> formals = FormalsOf(block, invoke);
    AnswerWithCall(
        unit, block, invoke,
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target =
                                mir::CallableTarget{
                                    .owner = owner, .slot = definition},
                            .receiver = on},
                    .arguments = std::move(formals)},
            .type = invoke.result_type});
  }
  const mir::CallableId answering = cls.callables.Add(
      mir::CallableDecl{
          .code = std::move(invoke),
          .foreign = std::nullopt,
          .virtual_dispatch = mir::IntroducesVirtualSlot{},
          .defining_unit = std::nullopt});
  cls.named_callables.push_back(
      mir::NamedCallable{.name = "invoke", .body = answering});
  cls.conforming.push_back(
      mir::ConformingBehavior{
          .interface_behavior = InvokeSlot(binding),
          .answered_by =
              mir::VirtualSlot{.owner_class = id, .slot = answering}});
  unit.DefineClass(id, std::move(cls));
  return id;
}

}  // namespace lyra::lowering::hir_to_mir

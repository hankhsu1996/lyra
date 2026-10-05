#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A scope of this unit, reached through that unit's own layout: `hops`
// enclosing edges out, then one owned child per descent step. A scope that
// encloses the caller is the empty descent.
struct EnclosingScopeReceiver {
  mir::EnclosingHops hops;
  std::span<const hir::OwnedChildStep> descent;
};

// The same scope, handed to a callee that dispatches on nothing: a
// receiver-less callable of a class that scope declares reaches what the class
// keeps for itself, which is that instance's, and has no object to reach it
// through (LRM 6.22, 8.10). It is the same value as the receiver above and a
// different fact about the call, which is why it is a separate origin rather
// than the same one read two ways.
struct DeclaringScopeArgument {
  hir::DeclaringInstanceReach reach;
};

// The object the source named a method on (LRM 8.6), in whichever receiver
// form it wrote.
struct CalledObject {
  hir::MethodReceiver source;
};

// An object across an instance boundary, reached through an endpoint sealed at
// elaboration. The endpoint holds the pointer, so the value is read straight
// off it and reaching the object traverses nothing.
struct SealedObject {
  hir::RoutedObjectRef reference;
};

// An interface instance reached through a virtual interface (LRM 25.9) -- the
// one it holds, or one declared inside that -- which is evaluated when the call
// runs and fails the simulation when the handle is null.
struct HeldInterface {
  hir::InterfaceInstanceAccessExpr access;
};

// Where a callee's first parameter comes from, when it is one the source wrote
// no expression for. It is named by its origin rather than carried as a value:
// what it evaluates to depends on the block the call is finally emitted into,
// which is settled after the callee is planned. A callee taking no such
// parameter has none of these.
using AmbientHandle = std::variant<
    EnclosingScopeReceiver, DeclaringScopeArgument, CalledObject, SealedObject,
    HeldInterface>;

// Whether the value an origin names is the object the callee dispatches on. A
// scope, a called object, and a sealed endpoint each name one; the declaring
// scope handed to a callee with no object is an ordinary first argument.
auto BindsAsReceiver(const AmbientHandle& handle) -> bool {
  return std::visit(
      Overloaded{
          [](const EnclosingScopeReceiver&) { return true; },
          [](const CalledObject&) { return true; },
          [](const SealedObject&) { return true; },
          [](const HeldInterface&) { return true; },
          [](const DeclaringScopeArgument&) { return false; }},
      handle);
}

// The callee is named outright, and `handle`, where the callee takes one, names
// what its first parameter binds -- the object it dispatches on, or the
// instance its class belongs to. A subroutine of a namespace unit (LRM 26.3)
// and a type-associated function of a class one declares (LRM 8.10) take
// neither.
struct NamedCallee {
  mir::Direct callee;
  std::optional<AmbientHandle> handle;
};

// The callee is a slot, and which implementation runs is the receiver's dynamic
// type to decide (LRM 8.20). The receiver rides the callee rather than the
// argument list, so nothing leads the arguments the source wrote.
struct DispatchedCallee {
  AmbientHandle receiver;
  mir::VirtualSlot slot;
};

using CalleeForm = std::variant<NamedCallee, DispatchedCallee>;

// The callee-interface facts a subroutine call needs, read uniformly however
// the callee is named: from its HIR declaration when this unit holds one, and
// from what another unit published about it when that unit does.
struct SubroutineCallee {
  hir::SubroutineKind kind = hir::SubroutineKind::kFunction;
  std::optional<mir::TypeId> result_type;
  CompletionLayout completion;
  CalleeForm form;
};

// A callee this unit can name: the target a direct call spells, and the slot it
// fills where it takes part in dispatch (LRM 8.20).
struct NamedTarget {
  mir::Direct direct;
  std::optional<mir::VirtualSlot> slot;
};

// What a call reads off a class method it reaches: the interface it marshals
// against, and what the call reaches the body by. Where these come from
// differs by whether this unit declares the class; what a call then does with
// them does not.
struct MethodCalleeFacts {
  hir::SubroutineKind kind = hir::SubroutineKind::kFunction;
  std::vector<CalleeFormal> formals;
  NamedTarget target;
};

// Reads those facts off whichever callee the reference names. The visit is
// exhaustive, so a callee kind added later is read here rather than falling
// silently to one side.
auto ReadMethodCallee(
    UnitLowerer& unit_lowerer, const hir::MethodCallee& callee)
    -> MethodCalleeFacts {
  return std::visit(
      Overloaded{
          [&](const hir::LocalClassMethodTarget& local) -> MethodCalleeFacts {
            const hir::SubroutineDecl& decl = unit_lowerer.Hir()
                                                  .classes.Get(local.owner)
                                                  .methods.Get(local.method);
            return MethodCalleeFacts{
                .kind = decl.kind,
                .formals = CalleeFormalsOf(unit_lowerer, decl),
                .target = NamedTarget{
                    .direct =
                        mir::Direct{
                            .target =
                                mir::CallableTarget{
                                    .owner = unit_lowerer.TranslateClass(
                                        local.owner),
                                    .slot = unit_lowerer.TranslateMethod(
                                        local.owner, local.method)}},
                    .slot = unit_lowerer.LocalVirtualSlotOf(local)}};
          },
          [&](const hir::ExternalMethodCallee& ext) -> MethodCalleeFacts {
            return MethodCalleeFacts{
                .kind = ext.interface.kind,
                .formals = CalleeFormalsOf(unit_lowerer, ext.interface),
                .target = NamedTarget{
                    .direct =
                        mir::Direct{
                            .target = unit_lowerer.MakeExternalMethodTarget(
                                ext.target)},
                    .slot = ext.slot.transform(
                        [&](const hir::ExternalDispatchSlot& published) {
                          return mir::VirtualSlot{
                              unit_lowerer.MakeExternalVirtualSlot(published)};
                        })}};
          }},
      callee);
}

// What any call to a class method states about its boundary, whatever object
// it runs on: what the callee marshals against and yields, and how it is
// named.
auto PlanMethodBoundary(
    const MethodCalleeFacts& facts, std::optional<mir::TypeId> result_type,
    CalleeForm form) -> SubroutineCallee {
  return SubroutineCallee{
      .kind = facts.kind,
      .result_type = result_type,
      .completion = BuildCompletionLayout(facts.formals, result_type),
      .form = std::move(form)};
}

// Plans a call to an instance method (LRM 8.6), which leads with the object
// the source named. It dispatches when the callee fills a slot and the source
// did not demand the base's implementation, which `super` demands whatever
// role the callee carries (LRM 8.15).
auto PlanInstanceMethodCall(
    UnitLowerer& unit_lowerer, const hir::MethodCallRef& ref,
    std::optional<mir::TypeId> result_type) -> SubroutineCallee {
  MethodCalleeFacts facts = ReadMethodCallee(unit_lowerer, ref.callee);
  const AmbientHandle object{CalledObject{.source = ref.receiver}};
  std::optional<mir::VirtualSlot> slot = facts.target.slot;
  if (slot.has_value() &&
      !std::holds_alternative<hir::SuperReceiver>(ref.receiver)) {
    return PlanMethodBoundary(
        facts, result_type,
        DispatchedCallee{.receiver = object, .slot = *std::move(slot)});
  }
  return PlanMethodBoundary(
      facts, result_type,
      NamedCallee{.callee = facts.target.direct, .handle = object});
}

// Plans a call to a type-associated method (LRM 8.10), which has no object to
// dispatch on. One of a class a structural scope declares leads with that
// scope's instance: it reaches what the class keeps for itself, which is that
// instance's, and has no object to reach it through (LRM 6.22).
auto PlanTypeAssociatedMethodCall(
    UnitLowerer& unit_lowerer, const hir::StaticMethodCallRef& ref,
    std::optional<mir::TypeId> result_type) -> SubroutineCallee {
  const MethodCalleeFacts facts = ReadMethodCallee(unit_lowerer, ref.callee);
  return PlanMethodBoundary(
      facts, result_type,
      NamedCallee{
          .callee = facts.target.direct,
          .handle = ref.declaring_instance.transform(
              [](const hir::DeclaringInstanceReach& reach) {
                return AmbientHandle{DeclaringScopeArgument{.reach = reach}};
              })});
}

// Reads the callee's interface out of whichever HIR reference names it, and
// nothing for a callee that is not a user subroutine -- a system, builtin,
// imported, or foreign one, each of which has its own boundary. Every user
// subroutine goes through this one reading, so a call site cannot state an
// interface the callee's definition does not have. The visit is exhaustive, so
// a callee kind added later is classified here rather than falling silently to
// one side.
template <ExprLowerer Lowerer>
auto PlanSubroutineCall(
    Lowerer& lowerer, const hir::CallExpr& call, mir::TypeId call_result_type)
    -> std::optional<SubroutineCallee> {
  auto& unit_lowerer = lowerer.Owner();
  // The payload's result component is the call's own result type, unless the
  // callee is a task or void function, which yields none.
  const std::optional<mir::TypeId> result_type =
      call_result_type == unit_lowerer.Unit().builtins.void_type
          ? std::nullopt
          : std::optional<mir::TypeId>{call_result_type};
  using Planned = std::optional<SubroutineCallee>;
  return std::visit(
      Overloaded{
          [&](const hir::StructuralSubroutineRef& ref) -> Planned {
            const hir::SubroutineDecl& decl = lowerer.LookupHirSubroutine(
                ref.hops, ref.descent, ref.subroutine);
            SubroutineCallee plan;
            plan.kind = decl.kind;
            plan.result_type = result_type;
            plan.completion = BuildCompletionLayout(
                CalleeFormalsOf(unit_lowerer, decl), result_type);
            plan.form = NamedCallee{
                .callee = lowerer.TranslateStructuralSubroutine(
                    ref.hops, ref.descent, ref.subroutine),
                .handle = AmbientHandle{EnclosingScopeReceiver{
                    .hops = mir::EnclosingHops{.value = ref.hops.value},
                    .descent = ref.descent}}};
            return plan;
          },
          [&](const hir::ExternalUnitSubroutineRef& ref) -> Planned {
            SubroutineCallee plan;
            plan.kind = ref.interface.kind;
            plan.result_type = result_type;
            plan.completion = BuildCompletionLayout(
                CalleeFormalsOf(unit_lowerer, ref.interface), result_type);
            plan.form = NamedCallee{
                .callee =
                    mir::Direct{
                        .target =
                            unit_lowerer.MakeNamespaceCallableTarget(ref)},
                .handle = std::nullopt};
            return plan;
          },
          [&](const hir::ExternalUnitMethodRef& ref) -> Planned {
            const hir::PublishedCallable& published =
                unit_lowerer.Hir()
                    .external_scope_classes.Get(ref.scope_class)
                    .signature.callables.Get(ref.callable);
            SubroutineCallee plan;
            plan.kind = published.interface.kind;
            plan.result_type = result_type;
            plan.completion = BuildCompletionLayout(
                CalleeFormalsOf(unit_lowerer, published.interface),
                result_type);
            plan.form = NamedCallee{
                .callee =
                    mir::Direct{
                        .target = unit_lowerer.MakeExternalUnitMethodTarget(
                            ref.scope_class, ref.callable)},
                .handle = std::visit(
                    Overloaded{
                        [](const hir::RoutedObjectRef& routed) {
                          return AmbientHandle{
                              SealedObject{.reference = routed}};
                        },
                        [](const hir::InterfaceInstanceAccessExpr& held) {
                          return AmbientHandle{HeldInterface{.access = held}};
                        }},
                    ref.receiver)};
            return plan;
          },
          [&](const hir::MethodCallRef& ref) -> Planned {
            return PlanInstanceMethodCall(unit_lowerer, ref, result_type);
          },
          [&](const hir::StaticMethodCallRef& ref) -> Planned {
            return PlanTypeAssociatedMethodCall(unit_lowerer, ref, result_type);
          },
          [](const hir::SystemSubroutineRef&) -> Planned {
            return std::nullopt;
          },
          [](const hir::BuiltinMethodRef&) -> Planned { return std::nullopt; },
          [](const hir::EnumMethodRef&) -> Planned { return std::nullopt; },
          [](const hir::PastValueRef&) -> Planned { return std::nullopt; },
          [](const hir::ValueChangeRef&) -> Planned { return std::nullopt; },
          [](const hir::ForeignImportRef&) -> Planned { return std::nullopt; }},
      call.callee);
}

// A component landing in an actual is what gives a call something to sequence
// once it completes; with none, the call stands on its own.
auto WritesBack(const SubroutineCallee& plan) -> bool {
  return std::ranges::any_of(
      plan.completion.formals, [](const CompletionLayout::Formal& formal) {
        return formal.component.has_value();
      });
}

// The call itself, still unowned, alongside what a caller needs to consume its
// completion: the payload's shape and the actual places its written-back
// components land in.
struct EmittedCall {
  mir::Expr call;
  mir::TypeId payload_type;
  std::vector<CompletionWriteback> writebacks;
};

// A callee once its operands exist: how the call names it, and the engine
// handle leading the arguments the source wrote where the callee takes one.
struct ResolvedCallee {
  mir::Callee callee;
  std::optional<mir::ExprId> leading;
};

// The borrowed pointer an instance method's body reads as its `self`. An
// explicit handle evaluates and then derefs the managed wrapper to reach the
// object; a receiver that is the enclosing method's own object and a `super`
// qualifier both read its self binding, which is already such a pointer -- the
// three differ in which implementation runs, not in where the receiver comes
// from.
template <ExprLowerer Lowerer>
auto BuildReceiverPointer(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::MethodReceiver& receiver) -> diag::Result<mir::ExprId> {
  mir::Block& block = *frame.current_block;
  const auto* handle = std::get_if<hir::HandleReceiver>(&receiver);
  if (handle == nullptr) {
    return block.exprs.Add(
        MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  }
  const mir::TypePool& types = lowerer.Owner().Unit().types;
  auto handle_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(handle->expr), frame);
  if (!handle_or) return std::unexpected(std::move(handle_or.error()));
  const mir::TypeId handle_type = handle_or->type;
  const mir::TypeId object_type =
      types.Get(handle_type).Get<mir::ManagedRefType>().pointee;
  const mir::ExprId handle_id = block.exprs.Add(*std::move(handle_or));
  const mir::ExprId object_id = block.exprs.Add(
      mir::Expr{
          .data = mir::DerefExpr{.pointer = handle_id}, .type = object_type});
  return block.exprs.Add(
      mir::MakeAddressOfExpr(
          object_id, types.Intern(
                         mir::Type{mir::PointerType{
                             .pointee = object_type,
                             .ownership = mir::PointerOwnership::kBorrowed,
                             .mutability = mir::Mutability::kMutable}})));
}

// Evaluates the ambient handle a callee's first parameter binds, in the block
// the call is being emitted into.
template <ExprLowerer Lowerer>
auto BuildAmbientHandle(
    Lowerer& lowerer, const WalkFrame& frame, const AmbientHandle& handle)
    -> diag::Result<mir::ExprId> {
  return std::visit(
      Overloaded{
          [&](const EnclosingScopeReceiver& r) -> diag::Result<mir::ExprId> {
            // The descent runs from the receiver the climb produced, the same
            // way a route's own descent does.
            return DescendOwnedChildren(
                lowerer.ScopeAt(
                    hir::StructuralHops{
                        .value = static_cast<std::uint32_t>(r.hops.value)},
                    {}),
                *frame.current_block,
                BuildEnclosingScopeReceiver(
                    frame, lowerer.Owner().Unit(), r.hops),
                r.descent);
          },
          [&](const DeclaringScopeArgument& a) -> diag::Result<mir::ExprId> {
            return BuildImplicitInstanceArgument(lowerer, frame, a.reach);
          },
          [&](const CalledObject& o) -> diag::Result<mir::ExprId> {
            return BuildReceiverPointer(lowerer, frame, o.source);
          },
          [&](const SealedObject& s) -> diag::Result<mir::ExprId> {
            return lowerer.RouteEnd(frame, s.reference.id);
          },
          [&](const HeldInterface& h) -> diag::Result<mir::ExprId> {
            return HeldInterfaceObject(lowerer, frame, h.access);
          }},
      handle);
}

// How the call names its callee and what leads the arguments the source wrote,
// evaluated in the block `frame` is writing.
template <ExprLowerer Lowerer>
auto ResolveCallee(
    Lowerer& lowerer, const WalkFrame& frame, const SubroutineCallee& plan)
    -> diag::Result<ResolvedCallee> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  return std::visit(
      Overloaded{
          [&](const NamedCallee& named) -> diag::Result<ResolvedCallee> {
            if (!named.handle.has_value()) {
              return ResolvedCallee{
                  .callee = named.callee, .leading = std::nullopt};
            }
            auto handle_or = BuildAmbientHandle(lowerer, frame, *named.handle);
            if (!handle_or) {
              return std::unexpected(std::move(handle_or.error()));
            }
            if (!BindsAsReceiver(*named.handle)) {
              return ResolvedCallee{
                  .callee = named.callee, .leading = *handle_or};
            }
            return ResolvedCallee{
                .callee =
                    mir::Direct{
                        .target = named.callee.target,
                        .receiver = BuildObjectDeref(unit, block, *handle_or)},
                .leading = std::nullopt};
          },
          [&](const DispatchedCallee& dispatched)
              -> diag::Result<ResolvedCallee> {
            auto receiver_or =
                BuildAmbientHandle(lowerer, frame, dispatched.receiver);
            if (!receiver_or) {
              return std::unexpected(std::move(receiver_or.error()));
            }
            return ResolvedCallee{
                .callee =
                    mir::Virtual{
                        .receiver = BuildObjectDeref(
                            unit, block,
                            AsIntroducer(
                                unit.types, block, *receiver_or,
                                dispatched.slot)),
                        .slot = dispatched.slot},
                .leading = std::nullopt};
          }},
      plan.form);
}

// Emits the call boundary into `frame`: the callee with whatever leads the
// arguments the source wrote, and each actual bound by its direction -- an
// `input` value, an `inout`'s incoming value, an `output`'s nothing, a `ref`
// cell alias. The call is typed with the protocol its callee states, so whether
// the caller awaits it is readable from the call alone.
template <ExprLowerer Lowerer>
auto EmitSubroutineCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const SubroutineCallee& plan) -> diag::Result<EmittedCall> {
  if (call.arguments.size() != plan.completion.formals.size()) {
    throw InternalError("EmitSubroutineCall: argument / formal count mismatch");
  }
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto& hir_exprs = lowerer.HirExprs();
  mir::Block& block = *frame.current_block;

  const mir::TypeId payload_type =
      CompletionPayloadType(unit, plan.completion.components);
  const mir::TypeId call_result_type =
      SubroutineCallType(unit, plan.kind, payload_type);

  auto resolved_or = ResolveCallee(lowerer, frame, plan);
  if (!resolved_or) return std::unexpected(std::move(resolved_or.error()));

  std::vector<mir::ExprId> call_args;
  call_args.reserve(call.arguments.size() + 1);
  if (resolved_or->leading.has_value()) {
    call_args.push_back(*resolved_or->leading);
  }

  std::vector<CompletionWriteback> writebacks;

  // A user call fills every position its completion layout declares, and the
  // two counts were matched on entry.
  const std::vector<hir::ExprId> operands = RequiredOperands(call);
  for (std::size_t i = 0; i < operands.size(); ++i) {
    const CompletionLayout::Formal& formal = plan.completion.formals[i];
    const hir::Expr& hir_arg = hir_exprs.Get(operands[i]);

    // Exhaustive over the directions, so one added to the language is bound
    // here rather than silently passed as a value.
    switch (formal.direction) {
      // An `output` passes no argument, and binds the actual's place for the
      // writeback once the call completes.
      case hir::ParamDirection::kOutput: {
        auto place_or = lowerer.LowerLhsExpr(hir_arg, frame);
        if (!place_or) return std::unexpected(std::move(place_or.error()));
        writebacks.push_back(
            {.place = *std::move(place_or),
             .component = *formal.component,
             .type = formal.type});
        break;
      }

      // An `inout` passes its incoming value and is written back the same way,
      // and the source writes the actual once (LRM 13.5).
      case hir::ParamDirection::kInOut: {
        auto place_or = lowerer.LowerLhsExpr(hir_arg, frame);
        if (!place_or) return std::unexpected(std::move(place_or.error()));
        ReadThenWritten actual =
            ReadThenWrite(lowerer.Owner(), frame, *std::move(place_or));
        call_args.push_back(actual.incoming);
        writebacks.push_back(
            {.place = std::move(actual.place),
             .component = *formal.component,
             .type = formal.type});
        break;
      }

      // A ref / const-ref formal aliases what the actual designates (LRM
      // 13.5.2), which is the whole of a variable or a class property or a
      // part of one; either way a write through it is a write of what holds
      // that storage, told to whoever waits on it when it lands.
      case hir::ParamDirection::kRef:
      case hir::ParamDirection::kConstRef: {
        auto arg_or = lowerer.LowerLhsExpr(hir_arg, frame);
        if (!arg_or) return std::unexpected(std::move(arg_or.error()));
        call_args.push_back(PathReference(unit, block, *arg_or));
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

  // A call an evaluation makes that states what it reaches hands that report
  // on, and the function states what it reads before it runs (LRM 9.4.2); any
  // other call hands none.
  if (const std::optional<mir::TypeId> report =
          ReportParamTypeOf(unit, plan.kind)) {
    call_args.push_back(block.exprs.Add(
        frame.reports_reached_to.has_value()
            ? mir::MakeLocalRefExpr(*frame.reports_reached_to, *report)
            : mir::Expr{.data = mir::NullLiteral{}, .type = *report}));
  }

  return EmittedCall{
      .call =
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee = std::move(resolved_or->callee),
                      .arguments = std::move(call_args)},
              .type = call_result_type},
      .payload_type = payload_type,
      .writebacks = std::move(writebacks)};
}

// Where a written-back call left its completion: the local it is bound to and
// the payload type to project components out of it by.
struct BoundCompletion {
  mir::LocalId completion;
  mir::TypeId payload_type;
};

// Emits the call, binds its completion, and writes each output component back
// to its actual, into whichever body `frame` is currently writing.
template <ExprLowerer Lowerer>
auto EmitWritingBackSteps(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const SubroutineCallee& plan) -> diag::Result<BoundCompletion> {
  auto emitted = EmitSubroutineCall(lowerer, frame, call, plan);
  if (!emitted) return std::unexpected(std::move(emitted.error()));

  const mir::TypeId payload_type = emitted->payload_type;
  return BoundCompletion{
      .completion = BindCompletion(
          lowerer.Owner().Unit(), frame, std::move(emitted->call), payload_type,
          emitted->writebacks),
      .payload_type = payload_type};
}

template <ExprLowerer Lowerer>
auto LowerWritingBackCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const SubroutineCallee& plan) -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();

  // A task's completion is awaited inside the body, so the body completes as a
  // coroutine and the expression is that coroutine: the enabler awaits it, the
  // same protocol a bare task enable states (LRM 13.3, 13.5). The body is a
  // callable value because its caller drives it, which is what separates it
  // from the sequencing below.
  if (plan.kind == hir::SubroutineKind::kTask) {
    ClosureBuilder closure(unit, frame);
    auto bound = EmitWritingBackSteps(lowerer, closure.Frame(), call, plan);
    if (!bound) return std::unexpected(std::move(bound.error()));
    return closure.BuildCoroutine();
  }

  // A function's completion is bound and written back here and now, so the
  // steps are one block expression and the call stays where it was written.
  if (!plan.result_type.has_value()) {
    throw InternalError(
        "LowerWritingBackCall: a call that settles no value has no expression "
        "to stand as -- please report this as a bug");
  }
  BlockBuilder steps(frame);
  auto bound = EmitWritingBackSteps(lowerer, steps.Frame(), call, plan);
  if (!bound) return std::unexpected(std::move(bound.error()));
  return steps.Build(ProjectCompletionComponent(
      steps.Body(), bound->completion, bound->payload_type, kCompletionResult,
      *plan.result_type));
}

}  // namespace

auto LowerSubroutineCallStmtForm(
    ProcessLowerer& lowerer, WalkFrame frame,
    const std::optional<std::string>& label, const hir::CallExpr& call,
    mir::TypeId result_type) -> std::optional<diag::Result<mir::Stmt>> {
  const std::optional<SubroutineCallee> planned =
      PlanSubroutineCall(lowerer, call, result_type);
  if (!planned.has_value()) return std::nullopt;
  const SubroutineCallee& callee = *planned;

  // Only a function that writes back and settles no value of its own. A task
  // hands its caller a coroutine to await, and a valued function hands it the
  // component it read, so both are expressions the caller consumes.
  const bool writes_back_only = WritesBack(callee) &&
                                callee.kind != hir::SubroutineKind::kTask &&
                                !callee.result_type.has_value();
  if (!writes_back_only) return std::nullopt;

  BlockBuilder steps(frame);
  auto bound = EmitWritingBackSteps(lowerer, steps.Frame(), call, callee);
  if (!bound) {
    return diag::Result<mir::Stmt>{std::unexpected(std::move(bound.error()))};
  }
  mir::Stmt stmt = steps.BuildStatement();
  stmt.label = label;
  return diag::Result<mir::Stmt>{std::move(stmt)};
}

template <ExprLowerer Lowerer>
auto LowerSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    mir::TypeId result_type) -> std::optional<diag::Result<mir::Expr>> {
  const std::optional<SubroutineCallee> planned =
      PlanSubroutineCall(lowerer, call, result_type);
  if (!planned.has_value()) return std::nullopt;
  const SubroutineCallee& callee = *planned;

  if (WritesBack(callee)) {
    // Writing a value back reaches a caller's storage, which only a procedural
    // statement does; the frontend rejects an output / inout call outside
    // procedural code (LRM 13.4), so one arriving in a structural context is a
    // compiler-invariant violation rather than a user-diagnosable form.
    if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
      return LowerWritingBackCall(lowerer, frame, call, callee);
    } else {
      throw InternalError(
          "LowerSubroutineCall: a structural subroutine call carries an "
          "output or inout argument");
    }
  }

  // With nothing to sequence, the call is the expression, and its result is
  // read straight out of the completion it yields. A void callee's completion
  // carries no component to read, so the call stands as the expression itself.
  auto emitted = EmitSubroutineCall(lowerer, frame, call, callee);
  if (!emitted) {
    return diag::Result<mir::Expr>{std::unexpected(std::move(emitted.error()))};
  }
  if (!callee.result_type.has_value()) {
    return diag::Result<mir::Expr>{std::move(emitted->call)};
  }
  mir::Block& block = *frame.current_block;
  const mir::ExprId completion = block.exprs.Add(std::move(emitted->call));
  return diag::Result<mir::Expr>{mir::MakeComponentExpr(
      completion, kCompletionResult, *callee.result_type)};
}

namespace {

// The handle a call is made through, where the source wrote one: a class
// handle a method is called on, or a virtual interface holding the instance an
// interface's subroutine is called on. A report stands where nothing has tested
// it, so it is tested before the call is made on it.
auto HandleCalledThrough(const hir::SubroutineRef& callee)
    -> std::optional<hir::ExprId> {
  return std::visit(
      Overloaded{
          [](const hir::MethodCallRef& method) -> std::optional<hir::ExprId> {
            if (const auto* handle =
                    std::get_if<hir::HandleReceiver>(&method.receiver)) {
              return handle->expr;
            }
            return std::nullopt;
          },
          [](const hir::ExternalUnitMethodRef& method)
              -> std::optional<hir::ExprId> {
            if (const auto* held =
                    std::get_if<hir::InterfaceInstanceAccessExpr>(
                        &method.receiver)) {
              return held->handle;
            }
            return std::nullopt;
          },
          // Each of these is made on nothing, or on an object elaboration
          // bound, which names something whenever it is reached.
          [](const hir::StructuralSubroutineRef&)
              -> std::optional<hir::ExprId> { return std::nullopt; },
          [](const hir::StaticMethodCallRef&) -> std::optional<hir::ExprId> {
            return std::nullopt;
          },
          [](const hir::ExternalUnitSubroutineRef&)
              -> std::optional<hir::ExprId> { return std::nullopt; },
          // A report calls only functions a source declared, never these.
          [](const hir::SystemSubroutineRef&) -> std::optional<hir::ExprId> {
            throw InternalError(
                "HandleCalledThrough: a report calls a system subroutine");
          },
          [](const hir::BuiltinMethodRef&) -> std::optional<hir::ExprId> {
            throw InternalError(
                "HandleCalledThrough: a report calls a built-in method");
          },
          [](const hir::EnumMethodRef&) -> std::optional<hir::ExprId> {
            throw InternalError(
                "HandleCalledThrough: a report calls an enumeration method");
          },
          [](const hir::PastValueRef&) -> std::optional<hir::ExprId> {
            throw InternalError(
                "HandleCalledThrough: a report calls a sampled value function");
          },
          [](const hir::ValueChangeRef&) -> std::optional<hir::ExprId> {
            throw InternalError(
                "HandleCalledThrough: a report calls a sampled value function");
          },
          [](const hir::ForeignImportRef&) -> std::optional<hir::ExprId> {
            throw InternalError(
                "HandleCalledThrough: a report calls a foreign import");
          }},
      callee);
}

}  // namespace

template <ExprLowerer Lowerer>
auto EmitReportingCall(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::ReportingCall& reporting, mir::LocalId report)
    -> diag::Result<void> {
  // Nothing was left to make the call on, so the report watches every object
  // instead and there is no call to make.
  if (reporting.receiver == hir::ReportedArgument::kDefaulted) {
    return {};
  }
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto& hir_exprs = lowerer.HirExprs();
  const hir::Expr& hir_call = hir_exprs.Get(reporting.call);
  const auto* call = std::get_if<hir::CallExpr>(&hir_call.data);
  if (call == nullptr) {
    throw InternalError(
        "EmitReportingCall: a reporting call names an expression that is not "
        "a call");
  }
  const std::optional<SubroutineCallee> planned = PlanSubroutineCall(
      lowerer, *call, lowerer.Owner().TranslateType(hir_call.type));
  if (!planned.has_value() || planned->kind != hir::SubroutineKind::kFunction) {
    throw InternalError(
        "EmitReportingCall: a reporting call names something other than a "
        "function");
  }
  const SubroutineCallee& plan = *planned;
  if (call->arguments.size() != plan.completion.formals.size() ||
      reporting.arguments.size() != plan.completion.formals.size()) {
    throw InternalError("EmitReportingCall: argument / formal count mismatch");
  }

  // Where the call is made through a handle, it is made only when the handle
  // names something, in a block of its own.
  mir::Block guarded;
  WalkFrame at = frame;
  const std::optional<hir::ExprId> through = HandleCalledThrough(call->callee);
  if (through.has_value()) {
    at = frame.WithBlock(&guarded);
  }
  mir::Block& block = *at.current_block;

  const mir::TypeId payload_type =
      CompletionPayloadType(unit, plan.completion.components);
  const mir::TypeId call_result_type =
      SubroutineCallType(unit, plan.kind, payload_type);
  auto resolved = ResolveCallee(lowerer, at, plan);
  if (!resolved) return std::unexpected(std::move(resolved.error()));

  std::vector<mir::ExprId> call_args;
  if (resolved->leading.has_value()) {
    call_args.push_back(*resolved->leading);
  }
  const std::vector<hir::ExprId> operands = RequiredOperands(*call);
  for (std::size_t i = 0; i < operands.size(); ++i) {
    const CompletionLayout::Formal& formal = plan.completion.formals[i];
    const hir::Expr& hir_arg = hir_exprs.Get(operands[i]);
    const bool evaluated =
        reporting.arguments[i] == hir::ReportedArgument::kEvaluated;
    switch (formal.direction) {
      // What the function would write back is no part of a report, which
      // writes nothing back.
      case hir::ParamDirection::kOutput:
        break;
      case hir::ParamDirection::kInput:
      case hir::ParamDirection::kInOut: {
        if (!evaluated) {
          call_args.push_back(block.exprs.Add(
              BuildDefaultValueFromHir(lowerer.Owner(), block, hir_arg.type)));
          break;
        }
        auto value = lowerer.LowerExpr(hir_arg, at);
        if (!value) return std::unexpected(std::move(value.error()));
        call_args.push_back(block.exprs.Add(*std::move(value)));
        break;
      }
      case hir::ParamDirection::kRef:
      case hir::ParamDirection::kConstRef: {
        if (!evaluated) {
          throw InternalError(
              "EmitReportingCall: a `ref` actual the report cannot name "
              "reached a call, where the reporting function refuses first");
        }
        auto place = lowerer.LowerLhsExpr(hir_arg, at);
        if (!place) return std::unexpected(std::move(place.error()));
        call_args.push_back(PathReference(unit, block, *place));
        break;
      }
    }
  }
  call_args.push_back(block.exprs.Add(
      mir::MakeLocalRefExpr(report, unit.builtins.read_report_ptr)));
  // What a call made on an object or through a virtual interface reads is
  // reached through that handle, which the report keeps apart (LRM 9.2.2.2.1
  // takes nothing of it into an implicit list; LRM 9.4.2 waits on it).
  const auto on_the_report = [&](support::BuiltinFn entry) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = BuildReportCall(
                unit, block, report, entry, {}, unit.builtins.void_type)});
  };
  const bool on_a_handle = reporting.receiver.has_value();
  if (on_a_handle) {
    on_the_report(support::BuiltinFn::kReadReportEnterCallOnHandle);
  }
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee = std::move(resolved->callee),
                          .arguments = std::move(call_args)},
                  .type = call_result_type})});
  if (on_a_handle) {
    on_the_report(support::BuiltinFn::kReadReportLeaveCallOnHandle);
  }

  if (through.has_value()) {
    mir::Block& outer = *frame.current_block;
    auto handle = lowerer.LowerExpr(hir_exprs.Get(*through), frame);
    if (!handle) return std::unexpected(std::move(handle.error()));
    outer.AppendStmt(
        mir::IfStmt{
            .condition = ReduceToCondition(
                unit, outer, outer.exprs.Add(*std::move(handle))),
            .then_scope = outer.child_scopes.Add(std::move(guarded)),
            .else_scope = std::nullopt});
  }
  return {};
}

template auto EmitReportingCall(
    ProcessLowerer&, const WalkFrame&, const hir::ReportingCall&, mir::LocalId)
    -> diag::Result<void>;
template auto EmitReportingCall(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::ReportingCall&,
    mir::LocalId) -> diag::Result<void>;

template auto LowerSubroutineCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&, mir::TypeId)
    -> std::optional<diag::Result<mir::Expr>>;
template auto LowerSubroutineCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&, mir::TypeId)
    -> std::optional<diag::Result<mir::Expr>>;

}  // namespace lyra::lowering::hir_to_mir

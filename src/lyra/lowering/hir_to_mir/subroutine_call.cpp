#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
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

// Where the value leading a call's arguments comes from, for a callee whose
// first parameter is one it does not get from the source. The source wrote no
// expression for it, so it is named by its origin rather than carried as a
// value: what it evaluates to depends on the block the call is finally emitted
// into, which is settled after the callee is planned. A callee taking no such
// parameter has none of these.

// A scope of this unit, reached by climbing that unit's own layout.
struct EnclosingScopeReceiver {
  mir::EnclosingHops hops;
};

// The runtime the calling process runs under, which every effect entry takes.
struct AmbientRuntimeHandle {};

// The object the source named a method on (LRM 8.6), in whichever of the three
// receiver forms it wrote.
struct CalledObject {
  hir::MethodReceiver source;
};

// An object across an instance boundary, reached through an endpoint sealed at
// elaboration. The endpoint holds the pointer, so the value is read straight
// off it and reaching the object traverses nothing.
struct SealedObject {
  hir::RoutedRef reference;
};
using AmbientHandle = std::variant<
    EnclosingScopeReceiver, AmbientRuntimeHandle, CalledObject, SealedObject>;

// The callee is named outright, and `handle`, where the callee takes one,
// leads the arguments the source wrote. A type-associated function (LRM 8.10)
// takes none.
struct NamedCallee {
  mir::Callee callee;
  std::optional<AmbientHandle> handle;
};

// The callee is a slot, and which implementation runs is the receiver's dynamic
// type to decide (LRM 8.20). The receiver rides the callee rather than the
// argument list, so nothing leads the arguments the source wrote.
struct DispatchedCallee {
  hir::MethodReceiver receiver;
  mir::VirtualSlot slot;
};

using CalleeForm = std::variant<NamedCallee, DispatchedCallee>;

// The callee-interface facts a subroutine call needs, read uniformly however
// the callee is named: from its HIR declaration when this unit holds one, and
// from the by-name reference's recorded interface when another unit does.
struct SubroutineCallee {
  hir::SubroutineKind kind = hir::SubroutineKind::kFunction;
  std::optional<mir::TypeId> result_type;
  CompletionLayout completion;
  CalleeForm form;
};

// Reads the virtual slot a method participates in off its stated dispatch role.
// Every participating method already names its slot -- an introducer names its
// own (owner, id), an intra-unit override was populated with the canonical
// intra-unit id at class lowering, and a cross-unit override names the
// introducing (unit, class, method) triple -- so this is a one-arm dispatch,
// never a chain walk. A method whose slot was introduced in another compilation
// unit resolves by name, reached through the declaring unit's header.
auto CanonicalVirtualSlot(
    mir::ClassId self_owner, mir::CallableId self_slot,
    const mir::VirtualDispatchRole& role) -> mir::VirtualSlot {
  return std::visit(
      Overloaded{
          [&](const mir::IntroducesVirtualSlot&) -> mir::VirtualSlot {
            return mir::LocalVirtualSlot{
                .owner_class = self_owner, .slot = self_slot};
          },
          [](const mir::OverridesIntraUnitSlot& s) -> mir::VirtualSlot {
            return mir::LocalVirtualSlot{
                .owner_class = s.slot_owner, .slot = s.slot_id};
          },
          [](const mir::OverridesExternalSlot& e) -> mir::VirtualSlot {
            return mir::ExternalVirtualSlot{
                .unit_name = e.unit_name,
                .class_name = e.class_name,
                .method_name = e.method_name};
          }},
      role);
}

// The target a direct call to another unit's method names. An instance method
// takes its receiver as the leading argument and a type-associated one takes
// none (LRM 8.10), which the callee's own declaration answers.
auto ExternalMethodTargetOf(
    UnitLowerer& unit_lowerer, const hir::ExternalMethodCallee& callee)
    -> mir::DirectTarget {
  if (callee.is_static) {
    return unit_lowerer.MakeExternalStaticMethodTarget(callee.target);
  }
  return unit_lowerer.MakeExternalMethodTarget(callee.target);
}

// What a call reads off a class method it reaches: the interface it marshals
// against, the target a direct call names, and the slot the callee fills where
// it takes part in dispatch (LRM 8.20). Where these come from differs by
// whether this unit declares the class; what a call then does with them does
// not.
struct MethodCalleeFacts {
  hir::SubroutineKind kind = hir::SubroutineKind::kFunction;
  std::vector<CalleeFormal> formals;
  mir::Callee direct;
  std::optional<mir::VirtualSlot> slot;
};

auto ReadMethodCallee(
    UnitLowerer& unit_lowerer, const hir::MethodCallee& callee)
    -> MethodCalleeFacts {
  if (const auto* ext = std::get_if<hir::ExternalMethodCallee>(&callee)) {
    MethodCalleeFacts facts{
        .kind = ext->interface.kind,
        .formals = CalleeFormalsOf(unit_lowerer, ext->interface),
        .direct =
            mir::Direct{
                .target = ExternalMethodTargetOf(unit_lowerer, *ext),
                .qualification = std::nullopt},
        .slot = std::nullopt};
    if (ext->is_virtual) {
      facts.slot = unit_lowerer.MakeExternalVirtualSlot(ext->target);
    }
    return facts;
  }
  const auto& local = std::get<hir::LocalClassMethodTarget>(callee);
  const hir::SubroutineDecl& decl =
      unit_lowerer.Hir().classes.Get(local.owner).methods.Get(local.method);
  const mir::ClassId owner = unit_lowerer.TranslateClass(local.owner);
  const mir::CallableId slot{local.method.value};
  // A method's dispatch role is queried through the unit's declarations, not
  // its class registry: while any peer body is lowering the registry is
  // one-way, so a read there would leak lowering order into the reading site.
  const auto& signature =
      unit_lowerer.GetClassShape(owner).callable_signatures.Get(slot);
  return MethodCalleeFacts{
      .kind = decl.kind,
      .formals = CalleeFormalsOf(unit_lowerer, decl),
      .direct =
          mir::Direct{
              .target = mir::CallableTarget{.owner = owner, .slot = slot},
              .qualification = std::nullopt},
      .slot = signature.virtual_dispatch.transform(
          [&](const mir::VirtualDispatchRole& role) {
            return CanonicalVirtualSlot(owner, slot, role);
          })};
}

// Plans a call to a class method, instance or type-associated (LRM 8.6, 8.10).
// It dispatches when the callee fills a slot and the source did not demand the
// base's implementation: `super` demands it whatever role the callee carries
// (LRM 8.15), and a type-associated function has no receiver to dispatch on.
auto PlanClassMethodCall(
    UnitLowerer& unit_lowerer, const hir::MethodCallee& callee,
    const std::optional<hir::MethodReceiver>& receiver,
    std::optional<mir::TypeId> result_type) -> SubroutineCallee {
  MethodCalleeFacts facts = ReadMethodCallee(unit_lowerer, callee);
  const bool through_super =
      receiver.has_value() &&
      std::holds_alternative<hir::SuperReceiver>(*receiver);
  const bool dispatches =
      receiver.has_value() && !through_super && facts.slot.has_value();

  SubroutineCallee plan;
  plan.kind = facts.kind;
  plan.result_type = result_type;
  plan.completion = BuildCompletionLayout(facts.formals, result_type);
  plan.form =
      dispatches
          ? CalleeForm{DispatchedCallee{
                .receiver = *receiver, .slot = *std::move(facts.slot)}}
          : CalleeForm{NamedCallee{
                .callee = std::move(facts.direct),
                .handle = receiver.transform([](const hir::MethodReceiver& r) {
                  return AmbientHandle{CalledObject{.source = r}};
                })}};
  return plan;
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
            const hir::SubroutineDecl& decl =
                lowerer.LookupHirSubroutine(ref.hops, ref.subroutine);
            SubroutineCallee plan;
            plan.kind = decl.kind;
            plan.result_type = result_type;
            plan.completion = BuildCompletionLayout(
                CalleeFormalsOf(unit_lowerer, decl), result_type);
            plan.form = NamedCallee{
                .callee = lowerer.TranslateStructuralSubroutine(
                    ref.hops, ref.subroutine),
                .handle = AmbientHandle{EnclosingScopeReceiver{
                    .hops = mir::EnclosingHops{.value = ref.hops.value}}}};
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
                        .target = unit_lowerer.MakeExternalCallableTarget(ref)},
                .handle = AmbientHandle{AmbientRuntimeHandle{}}};
            return plan;
          },
          [&](const hir::ExternalUnitMethodRef& ref) -> Planned {
            const hir::PublishedCallable& promised =
                unit_lowerer.Hir()
                    .external_unit_objects.Get(ref.object)
                    .callables.Get(ref.callable);
            SubroutineCallee plan;
            plan.kind = promised.kind;
            plan.result_type = result_type;
            plan.completion = BuildCompletionLayout(
                CalleeFormalsOf(
                    unit_lowerer,
                    hir::ExternalCalleeInterface{
                        .kind = promised.kind, .params = promised.params}),
                result_type);
            plan.form = NamedCallee{
                .callee =
                    mir::Direct{
                        .target = unit_lowerer.MakeExternalUnitMethodTarget(
                            ref.object, ref.callable)},
                .handle =
                    AmbientHandle{SealedObject{.reference = ref.receiver}}};
            return plan;
          },
          [&](const hir::MethodCallRef& ref) -> Planned {
            return PlanClassMethodCall(
                unit_lowerer, ref.callee, ref.receiver, result_type);
          },
          [&](const hir::StaticMethodCallRef& ref) -> Planned {
            return PlanClassMethodCall(
                unit_lowerer, ref.callee, std::nullopt, result_type);
          },
          [](const hir::SystemSubroutineRef&) -> Planned {
            return std::nullopt;
          },
          [](const hir::BuiltinMethodRef&) -> Planned { return std::nullopt; },
          [](const hir::ForeignImportRef&) -> Planned { return std::nullopt; },
          [](const hir::ImportedMethodRef&) -> Planned {
            return std::nullopt;
          }},
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

// A callee once its operands exist: how the call names it, and the value that
// leads the arguments the source wrote. A dispatched call has no leading value,
// its receiver riding the callee itself.
struct ResolvedCallee {
  mir::Callee callee;
  std::optional<mir::ExprId> leading;
};

// The borrowed pointer an instance method's body reads as its `self`. An
// explicit handle evaluates and then derefs the managed wrapper to reach the
// object; an implicit self and a `super` qualifier both read the enclosing
// method's own self binding, which is already such a pointer -- the three
// differ in which implementation runs, not in where the receiver comes from.
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
            return BuildEnclosingScopeReceiver(
                frame, lowerer.Owner().Unit(), r.hops);
          },
          [&](const AmbientRuntimeHandle&) -> diag::Result<mir::ExprId> {
            return frame.current_block->exprs.Add(
                BuildCurrentRuntimeCallExpr(lowerer.Owner()));
          },
          [&](const CalledObject& o) -> diag::Result<mir::ExprId> {
            return BuildReceiverPointer(lowerer, frame, o.source);
          },
          [&](const SealedObject& s) -> diag::Result<mir::ExprId> {
            const RoutedRefMeta& meta = lowerer.RoutedRefTarget(s.reference.id);
            return frame.current_block->exprs.Add(
                BuildStructuralFieldAccessExpr(
                    frame, lowerer.Owner().Unit(), mir::EnclosingHops{0},
                    meta.target));
          }},
      handle);
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
  auto& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const auto& hir_exprs = lowerer.HirExprs();
  mir::Block& block = *frame.current_block;

  const mir::TypeId payload_type =
      CompletionPayloadType(unit, plan.completion.components);
  const mir::TypeId call_result_type =
      SubroutineCallType(unit, plan.kind, payload_type);

  auto resolved_or = std::visit(
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
            return ResolvedCallee{
                .callee = named.callee, .leading = *handle_or};
          },
          [&](const DispatchedCallee& dispatched)
              -> diag::Result<ResolvedCallee> {
            auto receiver_or =
                BuildReceiverPointer(lowerer, frame, dispatched.receiver);
            if (!receiver_or) {
              return std::unexpected(std::move(receiver_or.error()));
            }
            return ResolvedCallee{
                .callee =
                    mir::Virtual{
                        .receiver = *receiver_or, .slot = dispatched.slot},
                .leading = std::nullopt};
          }},
      plan.form);
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
      // An `output` passes no argument; an `inout` passes its incoming value.
      // Both bind the actual place for a post-completion writeback.
      case hir::ParamDirection::kOutput:
      case hir::ParamDirection::kInOut: {
        auto place_or = lowerer.LowerLhsExpr(hir_arg, frame);
        if (!place_or) return std::unexpected(std::move(place_or.error()));
        const mir::ExprId place = block.exprs.Add(*std::move(place_or));
        if (formal.direction == hir::ParamDirection::kInOut) {
          auto value_or = lowerer.LowerExpr(hir_arg, frame);
          if (!value_or) return std::unexpected(std::move(value_or.error()));
          call_args.push_back(block.exprs.Add(*std::move(value_or)));
        }
        writebacks.push_back(
            {.place = place,
             .component = *formal.component,
             .type = formal.type});
        break;
      }

      // A ref / const-ref formal aliases what the actual designates (LRM
      // 13.5.2). A bare actual is lent as it stands, so a reference over a
      // capability wrapper aliases the wrapper and keeps the wrapper's own
      // access -- the update event a write fires included. A projected actual
      // designates part of a value, which is not a place a reference can
      // alias, so what is lent is the storage the chain descends into.
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
      steps.Body(), bound->completion, bound->payload_type,
      base::ComponentIndex{}, *plan.result_type));
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
  return diag::Result<mir::Expr>{mir::MakeComponentAccessExpr(
      completion, base::ComponentIndex{}, *callee.result_type)};
}

template auto LowerSubroutineCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&, mir::TypeId)
    -> std::optional<diag::Result<mir::Expr>>;
template auto LowerSubroutineCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&, mir::TypeId)
    -> std::optional<diag::Result<mir::Expr>>;

}  // namespace lyra::lowering::hir_to_mir

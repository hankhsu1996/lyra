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
#include "lyra/hir/subroutine_prototype.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/equivalence_identity.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/synthesized_function.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The object of the caller's own scope, reached through this unit's own layout.
struct OwnScopeReceiver {};

// A scope of this unit, handed to a callee that dispatches on nothing: a
// receiver-less callable of a class that scope declares reaches what the class
// keeps for itself, which is that instance's, and has no object to reach it
// through (LRM 6.22, 8.10). It is a scope's object, as the receiver above is,
// and a different fact about the call, which is why it is a separate origin
// rather than the same one read two ways.
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
    OwnScopeReceiver, DeclaringScopeArgument, CalledObject, SealedObject,
    HeldInterface>;

// Whether the value an origin names is the object the callee dispatches on. A
// scope, a called object, and a sealed endpoint each name one; the declaring
// scope handed to a callee with no object is an ordinary first argument.
auto BindsAsReceiver(const AmbientHandle& handle) -> bool {
  return std::visit(
      Overloaded{
          [](const OwnScopeReceiver&) { return true; },
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
// against, and what the call reaches the body by.
struct MethodCalleeFacts {
  hir::SubroutineKind kind = hir::SubroutineKind::kFunction;
  std::vector<CalleeFormal> formals;
  NamedTarget target;
};

// Reads those facts off the declaration of the method the reference names,
// whichever unit defines its class.
auto ReadMethodCallee(
    UnitLowerer& unit_lowerer, const hir::ClassMethodTarget& callee)
    -> MethodCalleeFacts {
  const hir::ClassRegistry& classes = unit_lowerer.Hir().classes;
  const std::optional<mir::VirtualSlot> slot =
      classes.BehaviorOf(callee).transform(
          [&](const hir::ClassMethodTarget& behavior) {
            return unit_lowerer.VirtualSlotOf(behavior);
          });
  const hir::CalleeInterface& interface = classes.Declaration(callee.owner)
                                              .methods.Get(callee.method)
                                              .prototype.interface;
  return MethodCalleeFacts{
      .kind = interface.kind,
      .formals = CalleeFormalsOf(unit_lowerer, interface),
      .target = NamedTarget{
          .direct =
              mir::Direct{.target = unit_lowerer.TranslateMethodTarget(callee)},
          .slot = slot}};
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
// the source named, and nothing for a hook the library runs (LRM 18.6.2,
// 8.26.9). It dispatches when the callee fills a slot and the source did not
// demand the base's implementation, which `super` demands whatever role the
// callee carries (LRM 8.15).
auto PlanInstanceMethodCall(
    UnitLowerer& unit_lowerer, const hir::MethodCallRef& ref,
    std::optional<mir::TypeId> result_type) -> std::optional<SubroutineCallee> {
  const std::optional<hir::ClassMethodTarget> runs =
      unit_lowerer.MethodRunBy(ref.callee);
  if (!runs.has_value()) {
    return std::nullopt;
  }
  MethodCalleeFacts facts = ReadMethodCallee(unit_lowerer, *runs);
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
            const hir::SubroutineDecl& decl =
                lowerer.LookupHirSubroutine(ref.subroutine);
            SubroutineCallee plan;
            plan.kind = decl.kind;
            plan.result_type = result_type;
            plan.completion = BuildCompletionLayout(
                CalleeFormalsOf(unit_lowerer, decl), result_type);
            plan.form = NamedCallee{
                .callee = lowerer.TranslateStructuralSubroutine(ref.subroutine),
                .handle = AmbientHandle{OwnScopeReceiver{}}};
            return plan;
          },
          [&](const hir::NamespaceSubroutineRef& ref) -> Planned {
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
          [&](const hir::PublishedScopeMethodRef& ref) -> Planned {
            const hir::SubroutinePrototype& published =
                unit_lowerer.Hir()
                    .scope_classes.Get(ref.scope_class)
                    .callables.Get(ref.callable);
            SubroutineCallee plan;
            plan.kind = published.interface.kind;
            plan.result_type = result_type;
            plan.completion = BuildCompletionLayout(
                CalleeFormalsOf(unit_lowerer, published.interface),
                result_type);
            plan.form = DispatchedCallee{
                .receiver = std::visit(
                    Overloaded{
                        [](const hir::RoutedObjectRef& routed) {
                          return AmbientHandle{
                              SealedObject{.reference = routed}};
                        },
                        [](const hir::InterfaceInstanceAccessExpr& held) {
                          return AmbientHandle{HeldInterface{.access = held}};
                        }},
                    ref.receiver),
                .slot = unit_lowerer.PublishedSubroutineSlot(
                    ref.scope_class, ref.callable)};
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
          [](const hir::ArrayIteratorMethodRef&) -> Planned {
            return std::nullopt;
          },
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
    return block.exprs.Add(MakeSelfRefExpr(frame));
  }
  auto handle_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(handle->expr), frame);
  if (!handle_or) return std::unexpected(std::move(handle_or.error()));
  return BuildObjectPointer(
      lowerer.Owner().Unit(), block, block.exprs.Add(*std::move(handle_or)));
}

// Evaluates the ambient handle a callee's first parameter binds, in the block
// the call is being emitted into.
template <ExprLowerer Lowerer>
auto BuildAmbientHandle(
    Lowerer& lowerer, const WalkFrame& frame, const AmbientHandle& handle)
    -> diag::Result<mir::ExprId> {
  return std::visit(
      Overloaded{
          [&](const OwnScopeReceiver&) -> diag::Result<mir::ExprId> {
            return BuildEnclosingScopeReceiver(
                frame, lowerer.Owner().Unit(), mir::EnclosingHops{.value = 0});
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

// Whether an actual of type `actual` is handed to a formal of type `formal`
// as a value of any type: a formal of `dynamic_type` takes a value of whichever
// type its actual has, and keeps that type with it (LRM 15.4.9). An actual
// already of that type is handed on as it is.
auto TakenAsAny(
    const UnitLowerer& unit_lowerer, mir::TypeId formal, hir::TypeId actual)
    -> bool {
  return unit_lowerer.Unit().types.Get(formal).Is<mir::AnyType>() &&
         !unit_lowerer.Hir().types.Get(actual).Is<hir::DynamicType>();
}

// The identity a value of `type` is kept with, as the machine text a call
// names it by.
auto IdentityText(
    UnitLowerer& unit_lowerer, mir::Block& block, hir::TypeId type)
    -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  return block.exprs.Add(
      mir::MakeStringLiteral(
          unit.builtins.machine_c_string,
          EquivalenceIdentityOf(unit_lowerer, type)));
}

// `value`, of `type`, as a value of the type `any` holding a copy of it, kept
// at the type every type equivalent to `type` is kept at.
auto MadeAny(
    UnitLowerer& unit_lowerer, mir::Block& block, hir::TypeId type,
    mir::ExprId value, mir::TypeId any) -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId kept =
      WithoutQueueBounds(unit.types, unit_lowerer.TranslateType(type));
  return block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = support::BuiltinFn::kMakeAny},
          {ConvertToType(unit, block, value, kept),
           IdentityText(unit_lowerer, block, type)},
          any));
}

// The variable `place` names, of `type`, as a reference of the type
// `reference` to a value of any type (LRM 13.5.2, 15.4.9), as Rust makes a
// `&mut dyn Any` of a `&mut T`. A value received through it is written into a
// local of the frame, of the type every type equivalent to `type` is kept at,
// and a closure the frame lends then assigns that local to the variable, which
// brings it to the variable's own type as any assignment would. The frame
// outlasts the call the reference is handed to, so both live as long as it.
auto SeenAsAny(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, hir::TypeId type,
    const AccessPath& place, mir::TypeId reference) -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId seen = unit_lowerer.TranslateType(type);
  const mir::TypeId kept = WithoutQueueBounds(unit.types, seen);
  const auto reference_to = [&](const AccessPath& path, mir::TypeId at) {
    return AtType(
        unit, block, PathReference(unit, block, path),
        *ParamTypeOf(unit, at, hir::ParamDirection::kRef));
  };
  const AccessPath incoming{
      .owner = block.LocalPlace(
          DeclareLocal(
              frame, block.exprs.Add(BuildDefaultValueExpr(unit, block, kept))),
          kept),
      .descent = {}};

  ClosureBuilder carry(unit, frame, support::ClosureKind::kFn);
  mir::Block& body = carry.Body();
  const AccessPath received{
      .owner = SnapshotPlaceIntoClosure(
          unit_lowerer, frame, carry, reference_to(incoming, kept)),
      .descent = {}};
  // The call the variable is bound for runs past the statement building the
  // closure, so what keeps the variable's storage is held by the frame.
  const mir::ExprId bound = AtType(
      unit, block,
      PathBoundReference(
          unit, block, place,
          [&](mir::ExprId leased) {
            return block.LocalPlace(
                DeclareLocal(frame, leased), block.exprs.Get(leased).type);
          }),
      *ParamTypeOf(unit, seen, hir::ParamDirection::kRef));
  const AccessPath variable{
      .owner = SnapshotPlaceIntoClosure(unit_lowerer, frame, carry, bound),
      .descent = {}};
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(BuildStoreExpr(
              unit, body, variable,
              ConvertToType(
                  unit, body, PathValue(unit, body, received), seen)))});
  const mir::ExprId made = block.exprs.Add(carry.BuildVoid());
  const mir::TypeId closure_type = block.exprs.Get(made).type;
  const mir::ExprId lent = BuildBorrowedClosure(
      unit, block, block.LocalPlace(DeclareLocal(frame, made), closure_type));

  return block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{
              .target = support::BuiltinFn::kSeeAsAny,
              .type_arguments = {kept}},
          {reference_to(incoming, kept),
           IdentityText(unit_lowerer, block, type), lent},
          reference));
}

}  // namespace

template <ExprLowerer Lowerer>
auto EmitBoundActual(
    Lowerer& lowerer, const WalkFrame& frame, const hir::Expr& actual,
    hir::ParamDirection direction, mir::TypeId type)
    -> diag::Result<mir::ExprId> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  switch (direction) {
    case hir::ParamDirection::kRef:
    case hir::ParamDirection::kConstRef: {
      auto named = lowerer.LowerLhsExpr(actual, frame);
      if (!named) return std::unexpected(std::move(named.error()));
      const mir::TypeId reference = *ParamTypeOf(unit, type, direction);
      return TakenAsAny(lowerer.Owner(), type, actual.type)
                 ? SeenAsAny(
                       lowerer.Owner(), frame, actual.type, *named, reference)
                 : AtType(
                       unit, block,
                       PathBoundReference(
                           unit, block, *named,
                           [](mir::ExprId leased) { return leased; }),
                       reference);
    }
    case hir::ParamDirection::kInput: {
      auto lowered = lowerer.LowerExpr(actual, frame);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      const mir::ExprId value = block.exprs.Add(*std::move(lowered));
      return TakenAsAny(lowerer.Owner(), type, actual.type)
                 ? MadeAny(lowerer.Owner(), block, actual.type, value, type)
                 : value;
    }
    case hir::ParamDirection::kOutput:
    case hir::ParamDirection::kInOut:
      throw InternalError(
          "EmitBoundActual: an 'output' or 'inout' actual is a place written "
          "back once the call completes, never an argument bound alone");
  }
  throw InternalError("EmitBoundActual: unknown parameter direction");
}

template auto EmitBoundActual(
    ProcessLowerer&, const WalkFrame&, const hir::Expr&, hir::ParamDirection,
    mir::TypeId) -> diag::Result<mir::ExprId>;
template auto EmitBoundActual(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::Expr&,
    hir::ParamDirection, mir::TypeId) -> diag::Result<mir::ExprId>;

namespace {

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
        auto place_or = LowerLvalue(lowerer, hir_arg, frame);
        if (!place_or) return std::unexpected(std::move(place_or.error()));
        writebacks.push_back(
            {.target = *std::move(place_or),
             .component = *formal.component,
             .type = formal.type});
        break;
      }

      // An `inout` passes its incoming value, brought to the formal's type as
      // an assignment to the formal would bring it, and is written back the
      // same way; the source writes the actual once (LRM 13.5).
      case hir::ParamDirection::kInOut: {
        auto place_or = LowerLvalue(lowerer, hir_arg, frame);
        if (!place_or) return std::unexpected(std::move(place_or.error()));
        auto actual =
            ReadThenWrite(lowerer.Owner(), frame, *std::move(place_or));
        if (!actual) return std::unexpected(std::move(actual.error()));
        call_args.push_back(
            ConvertToType(unit, block, actual->incoming, formal.type));
        writebacks.push_back(
            {.target = std::move(actual->lvalue),
             .component = *formal.component,
             .type = formal.type});
        break;
      }

      case hir::ParamDirection::kRef:
      case hir::ParamDirection::kConstRef:
      case hir::ParamDirection::kInput: {
        auto bound = EmitBoundActual(
            lowerer, frame, hir_arg, formal.direction, formal.type);
        if (!bound) return std::unexpected(std::move(bound.error()));
        call_args.push_back(*bound);
        break;
      }
    }
  }

  // A call made where an evaluation states what it reaches hands that report
  // on, so the function states what it reads as it runs (LRM 9.4.2); any other
  // call hands none.
  if (ReportParamTypeOf(unit, plan.kind).has_value()) {
    call_args.push_back(BuildReportHandedOn(unit, frame));
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
  auto completion = BindCompletion(
      lowerer.Owner(), frame, std::move(emitted->call), payload_type,
      emitted->writebacks);
  if (!completion) return std::unexpected(std::move(completion.error()));
  return BoundCompletion{
      .completion = *completion, .payload_type = payload_type};
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
    ClosureBuilder closure(unit, frame, support::ClosureKind::kFnOnce);
    auto bound = EmitWritingBackSteps(lowerer, closure.Frame(), call, plan);
    if (!bound) return std::unexpected(std::move(bound.error()));
    return closure.BuildCallOnce();
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

auto BuildMethodCall(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::ClassMethodTarget& callee, std::optional<mir::ExprId> receiver,
    std::span<const mir::ExprId> arguments, hir::TypeId result_type)
    -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const MethodCalleeFacts facts = ReadMethodCallee(unit_lowerer, callee);
  if (facts.kind != hir::SubroutineKind::kFunction) {
    throw InternalError(
        "BuildMethodCall: a call no source expression stands for is of a "
        "function");
  }
  if (arguments.size() != facts.formals.size()) {
    throw InternalError("BuildMethodCall: argument / formal count mismatch");
  }
  // A method of an object dispatches where it fills a slot and is called by
  // name otherwise; a type-associated one is called by name on nothing.
  mir::Callee named = facts.target.direct;
  if (receiver.has_value()) {
    const mir::ExprId object =
        BuildObjectPointer(unit, block, block.AsValue(unit.types, *receiver));
    if (facts.target.slot.has_value()) {
      named = mir::Virtual{
          .receiver = BuildObjectDeref(
              unit, block,
              AsIntroducer(unit.types, block, object, *facts.target.slot)),
          .slot = *facts.target.slot};
    } else {
      named = mir::Direct{
          .target = facts.target.direct.target,
          .receiver = BuildObjectDeref(unit, block, object)};
    }
  }
  std::vector<mir::ExprId> bound;
  bound.reserve(arguments.size());
  for (std::size_t i = 0; i < arguments.size(); ++i) {
    const CalleeFormal& formal = facts.formals[i];
    switch (formal.direction) {
      case hir::ParamDirection::kInput:
        bound.push_back(ConvertToType(
            unit, block, block.AsValue(unit.types, arguments[i]), formal.type));
        break;
      case hir::ParamDirection::kRef:
      case hir::ParamDirection::kConstRef:
        bound.push_back(AtType(
            unit, block, arguments[i],
            *ParamTypeOf(unit, formal.type, formal.direction)));
        break;
      case hir::ParamDirection::kOutput:
      case hir::ParamDirection::kInOut:
        throw InternalError(
            "BuildMethodCall: a formal written back once the call completes");
    }
  }
  const mir::TypeId result = unit_lowerer.TranslateType(result_type);
  return BuildFunctionCall(
      unit, frame, std::move(named), std::move(bound),
      result == unit.builtins.void_type ? std::nullopt
                                        : std::optional<mir::TypeId>{result});
}

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
  return diag::Result<mir::Expr>{mir::MakeFieldAccessExpr(
      completion, mir::ComponentFieldTarget{.position = kCompletionResult},
      *callee.result_type)};
}

template <ExprLowerer Lowerer>
auto EmitReportingCall(
    Lowerer& lowerer, const WalkFrame& frame, hir::ExprId call_id,
    mir::LocalId report) -> diag::Result<void> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const hir::Expr& hir_call = lowerer.HirExprs().Get(call_id);
  const auto* call = std::get_if<hir::CallExpr>(&hir_call.data);
  if (call == nullptr) {
    throw InternalError(
        "EmitReportingCall: a call an implicit list follows names an "
        "expression that is not a call");
  }
  const std::optional<SubroutineCallee> planned = PlanSubroutineCall(
      lowerer, *call, lowerer.Owner().TranslateType(hir_call.type));
  if (!planned.has_value() || planned->kind != hir::SubroutineKind::kFunction) {
    throw InternalError(
        "EmitReportingCall: a call an implicit list follows names something "
        "other than a function");
  }
  const SubroutineCallee& plan = *planned;
  mir::Block& block = *frame.current_block;

  const mir::TypeId payload_type =
      CompletionPayloadType(unit, plan.completion.components);
  const mir::TypeId call_result_type =
      SubroutineCallType(unit, plan.kind, payload_type);
  auto resolved = ResolveCallee(lowerer, frame, plan);
  if (!resolved) return std::unexpected(std::move(resolved.error()));

  std::vector<mir::ExprId> call_args;
  if (resolved->leading.has_value()) {
    call_args.push_back(*resolved->leading);
  }
  const std::vector<hir::ExprId> operands = RequiredOperands(*call);
  if (operands.size() != plan.completion.formals.size()) {
    throw InternalError("EmitReportingCall: argument / formal count mismatch");
  }
  for (std::size_t i = 0; i < operands.size(); ++i) {
    const CompletionLayout::Formal& formal = plan.completion.formals[i];
    const hir::TypeId actual_type = lowerer.HirExprs().Get(operands[i]).type;
    switch (formal.direction) {
      // What the function would write back is no part of the list, which
      // writes nothing back.
      case hir::ParamDirection::kOutput:
        break;
      // An `inout` actual is of its own type, and the function takes the
      // formal's.
      case hir::ParamDirection::kInput:
      case hir::ParamDirection::kInOut: {
        const mir::ExprId value = block.exprs.Add(
            BuildDefaultValueFromHir(lowerer.Owner(), block, actual_type));
        call_args.push_back(
            TakenAsAny(lowerer.Owner(), formal.type, actual_type)
                ? MadeAny(
                      lowerer.Owner(), block, actual_type, value, formal.type)
                : ConvertToType(unit, block, value, formal.type));
        break;
      }
      // What the function reads through a `ref` formal is what the caller
      // names there, which the caller's own text states. A reference to a value
      // of any type refers to nothing here, since the function writes nothing
      // through it.
      case hir::ParamDirection::kRef:
      case hir::ParamDirection::kConstRef: {
        const mir::TypeId reference =
            *ParamTypeOf(unit, formal.type, formal.direction);
        if (unit.types.Get(formal.type).Is<mir::AnyType>()) {
          call_args.push_back(block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee = mir::Construct{}, .arguments = {}},
                  .type = reference}));
          break;
        }
        const mir::TypeId value_type =
            lowerer.Owner().TranslateType(actual_type);
        const mir::LocalId held = DeclareLocal(
            frame, block.exprs.Add(BuildDefaultValueFromHir(
                       lowerer.Owner(), block, actual_type)));
        call_args.push_back(AtType(
            unit, block,
            BuildReferenceArg(
                unit, block,
                block.exprs.Add(mir::MakeLocalRefExpr(held, value_type)),
                value_type),
            reference));
        break;
      }
    }
  }
  call_args.push_back(block.ReadLocal(report, unit.builtins.read_report_ptr));
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee = std::move(resolved->callee),
                          .arguments = std::move(call_args)},
                  .type = call_result_type})});
  return {};
}

template <ExprLowerer Lowerer>
auto EmitConstructionActuals(
    Lowerer& lowerer, const WalkFrame& frame, hir::ClassId cls,
    std::span<const hir::ExprId> actuals)
    -> diag::Result<std::vector<mir::ExprId>> {
  const std::vector<CalleeFormal> formals =
      ConstructorFormalsOf(lowerer.Owner(), cls);
  if (actuals.size() != formals.size()) {
    throw InternalError(
        "EmitConstructionActuals: argument / formal count mismatch");
  }
  std::vector<mir::ExprId> args;
  args.reserve(actuals.size());
  for (std::size_t i = 0; i < actuals.size(); ++i) {
    auto bound = EmitBoundActual(
        lowerer, frame, lowerer.HirExprs().Get(actuals[i]),
        formals[i].direction, formals[i].type);
    if (!bound) return std::unexpected(std::move(bound.error()));
    args.push_back(*bound);
  }
  return args;
}

template auto EmitConstructionActuals(
    ProcessLowerer&, const WalkFrame&, hir::ClassId,
    std::span<const hir::ExprId>) -> diag::Result<std::vector<mir::ExprId>>;
template auto EmitConstructionActuals(
    const StructuralScopeLowerer&, const WalkFrame&, hir::ClassId,
    std::span<const hir::ExprId>) -> diag::Result<std::vector<mir::ExprId>>;

template auto EmitReportingCall(
    ProcessLowerer&, const WalkFrame&, hir::ExprId, mir::LocalId)
    -> diag::Result<void>;
template auto EmitReportingCall(
    const StructuralScopeLowerer&, const WalkFrame&, hir::ExprId, mir::LocalId)
    -> diag::Result<void>;

template auto LowerSubroutineCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&, mir::TypeId)
    -> std::optional<diag::Result<mir::Expr>>;
template auto LowerSubroutineCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&, mir::TypeId)
    -> std::optional<diag::Result<mir::Expr>>;

}  // namespace lyra::lowering::hir_to_mir

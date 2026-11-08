#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
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
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/equivalence_identity.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/synthesized_function.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/library_decl.hpp"

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

// The call of `behavior` on the object `object` points at, seen as the class
// introducing the behavior, a class or an interface class (LRM 8.20, 8.26.5).
auto DispatchingCallee(
    const UnitLowerer& unit_lowerer, mir::Builder block, mir::ExprId object,
    const mir::VirtualSlot& behavior) -> mir::Callee {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::ExprId receiver = BuildObjectDeref(
      unit, block, AsIntroducer(unit.types, block, object, behavior));
  return mir::Virtual{.receiver = receiver, .slot = behavior};
}

// The callee is a slot, and which implementation runs is the receiver's dynamic
// type to decide (LRM 8.20). The receiver rides the callee rather than the
// argument list, so nothing leads the arguments the source wrote.
struct DispatchedCallee {
  AmbientHandle receiver;
  mir::VirtualSlot slot;
};

using CalleeForm = std::variant<NamedCallee, DispatchedCallee>;

// The function evaluating a formal's default (LRM 13.5.3), a function of
// whatever declares the callee, made on the same handle the callee is, so a
// call omitting the formal asks the callee's own scope for its value; what it
// answers, and the type of that.
struct DefaultEvaluation {
  CalleeForm form;
  mir::TypeId type;
  Passing passing;
};

// The callee-interface facts a subroutine call needs, read uniformly however
// the callee is named: from its HIR declaration when this unit holds one, and
// from what another unit published about it when that unit does. `defaults`
// holds an evaluation at the position of each formal with a default.
struct SubroutineCallee {
  hir::SubroutineKind kind = hir::SubroutineKind::kFunction;
  std::optional<mir::TypeId> result_type;
  CompletionLayout completion;
  CalleeForm form;
  std::vector<std::optional<DefaultEvaluation>> defaults;
};

// A callee this unit can name: the target a direct call spells, and the slot it
// fills where it takes part in dispatch (LRM 8.20).
struct NamedTarget {
  mir::Direct direct;
  std::optional<mir::VirtualSlot> slot;
};

// A method of the same class evaluating a formal's default, the type it
// answers, and whether it runs on the object the call does: a constant of an
// interface class's is evaluated on none, whichever class that object is of
// (LRM 8.26.8).
struct MethodDefault {
  mir::Direct evaluator;
  mir::TypeId type;
  Passing passing;
  bool on_object = true;
};

// What a call reads off a class method it reaches: the interface it marshals
// against, what the call reaches the body by, and, at the position of each
// formal with a default, how it is evaluated.
struct MethodCalleeFacts {
  hir::SubroutineKind kind = hir::SubroutineKind::kFunction;
  std::vector<CalleeFormal> formals;
  NamedTarget target;
  std::vector<std::optional<MethodDefault>> defaults;
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
  const hir::DeclaredMethods& methods =
      classes.Declaration(callee.owner).methods;
  const hir::DeclaredMethod& declared = methods.Get(callee.method);
  const hir::CalleeInterface& interface = declared.prototype.interface;
  std::vector<std::optional<MethodDefault>> defaults;
  for (const std::optional<hir::MethodId>& evaluator : declared.defaults) {
    defaults.push_back(evaluator.transform([&](hir::MethodId method) {
      const hir::DeclaredMethod& evaluating = methods.Get(method);
      return MethodDefault{
          .evaluator =
              mir::Direct{
                  .target = unit_lowerer.TranslateMethodTarget(
                      hir::ClassMethodTarget{
                          .owner = callee.owner, .method = method})},
          .type = unit_lowerer.TranslateType(evaluating.prototype.result_type),
          .passing = ResultPassingOf(evaluating.prototype.result_direction),
          .on_object = !std::holds_alternative<hir::TypeAssociated>(
              evaluating.dispatch)};
    }));
  }
  return MethodCalleeFacts{
      .kind = interface.kind,
      .formals = CalleeFormalsOf(unit_lowerer, interface),
      .target =
          NamedTarget{
              .direct =
                  mir::Direct{
                      .target = unit_lowerer.TranslateMethodTarget(callee)},
              .slot = slot},
      .defaults = std::move(defaults)};
}

// What any call to a class method states about its boundary, whatever object
// it runs on: what the callee marshals against and yields, and how it is
// named.
// A formal's default is evaluated by a method of the class on the same handle
// the call is made on, named outright: the default is the one of the method
// the call names, whichever body the object's class runs for it.
auto PlanMethodBoundary(
    const MethodCalleeFacts& facts, std::optional<mir::TypeId> result_type,
    CalleeForm form, std::optional<AmbientHandle> handle,
    std::optional<AmbientHandle> handle_of_class) -> SubroutineCallee {
  std::vector<std::optional<DefaultEvaluation>> defaults;
  for (const std::optional<MethodDefault>& value : facts.defaults) {
    defaults.push_back(value.transform([&](const MethodDefault& method) {
      return DefaultEvaluation{
          .form = CalleeForm{NamedCallee{
              .callee = method.evaluator,
              .handle = method.on_object ? handle : handle_of_class}},
          .type = method.type,
          .passing = method.passing};
    }));
  }
  return SubroutineCallee{
      .kind = facts.kind,
      .result_type = result_type,
      .completion = BuildCompletionLayout(facts.formals, result_type),
      .form = std::move(form),
      .defaults = std::move(defaults)};
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
        DispatchedCallee{.receiver = object, .slot = *std::move(slot)}, object,
        std::nullopt);
  }
  return PlanMethodBoundary(
      facts, result_type,
      NamedCallee{.callee = facts.target.direct, .handle = object}, object,
      std::nullopt);
}

// Plans a call to a type-associated method (LRM 8.10), which has no object to
// dispatch on. One of a class a structural scope declares leads with that
// scope's instance: it reaches what the class keeps for itself, which is that
// instance's, and has no object to reach it through (LRM 6.22).
auto PlanTypeAssociatedMethodCall(
    UnitLowerer& unit_lowerer, const hir::StaticMethodCallRef& ref,
    std::optional<mir::TypeId> result_type) -> SubroutineCallee {
  const MethodCalleeFacts facts = ReadMethodCallee(unit_lowerer, ref.callee);
  const std::optional<AmbientHandle> handle = ref.declaring_instance.transform(
      [](const hir::DeclaringInstanceReach& reach) {
        return AmbientHandle{DeclaringScopeArgument{.reach = reach}};
      });
  return PlanMethodBoundary(
      facts, result_type,
      NamedCallee{.callee = facts.target.direct, .handle = handle}, handle,
      handle);
}

// How a call names a subroutine of its own scope, on that scope's object.
template <ExprLowerer Lowerer>
auto FormOfOwnSubroutine(
    const Lowerer& lowerer, const hir::StructuralSubroutineRef& ref)
    -> CalleeForm {
  return NamedCallee{
      .callee = lowerer.TranslateStructuralSubroutine(ref.subroutine),
      .handle = AmbientHandle{OwnScopeReceiver{}}};
}

// How a call names a subroutine of a namespace, on no object.
auto FormOfNamespaceSubroutine(
    UnitLowerer& unit_lowerer, const hir::NamespaceSubroutineRef& ref)
    -> CalleeForm {
  return NamedCallee{
      .callee =
          mir::Direct{.target = unit_lowerer.MakeNamespaceCallableTarget(ref)},
      .handle = std::nullopt};
}

// The object a call of a subroutine another scope published is made on.
auto ReceiverOf(const hir::PublishedScopeMethodRef& ref) -> AmbientHandle {
  return std::visit(
      Overloaded{
          [](const hir::RoutedObjectRef& routed) {
            return AmbientHandle{SealedObject{.reference = routed}};
          },
          [](const hir::InterfaceInstanceAccessExpr& held) {
            return AmbientHandle{HeldInterface{.access = held}};
          }},
      ref.receiver);
}

// How a call names a subroutine another scope published, by its slot.
auto FormOfPublishedSubroutine(
    const UnitLowerer& unit_lowerer, const hir::PublishedScopeMethodRef& ref)
    -> CalleeForm {
  return DispatchedCallee{
      .receiver = ReceiverOf(ref),
      .slot =
          unit_lowerer.PublishedSubroutineSlot(ref.scope_class, ref.callable)};
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
            plan.form = FormOfOwnSubroutine(lowerer, ref);
            for (const hir::SubroutineParam& param : decl.params) {
              plan.defaults.push_back(param.default_evaluator.transform(
                  [&](hir::StructuralSubroutineId evaluator) {
                    const hir::SubroutineDecl& evaluating =
                        lowerer.LookupHirSubroutine(evaluator);
                    return DefaultEvaluation{
                        .form = FormOfOwnSubroutine(
                            lowerer,
                            hir::StructuralSubroutineRef{
                                .subroutine = evaluator}),
                        .type =
                            unit_lowerer.TranslateType(evaluating.result_type),
                        .passing =
                            ResultPassingOf(evaluating.result_direction)};
                  }));
            }
            return plan;
          },
          [&](const hir::NamespaceSubroutineRef& ref) -> Planned {
            const hir::NamespaceDeclaration& declared =
                unit_lowerer.Hir().namespaces.Declaration(ref.owner);
            const hir::NamespaceSubroutine& listed =
                declared.subroutines.Get(ref.subroutine);
            const hir::CalleeInterface& interface = listed.prototype.interface;
            SubroutineCallee plan;
            plan.kind = interface.kind;
            plan.result_type = result_type;
            plan.completion = BuildCompletionLayout(
                CalleeFormalsOf(unit_lowerer, interface), result_type);
            plan.form = FormOfNamespaceSubroutine(unit_lowerer, ref);
            for (const std::optional<hir::NamespaceSubroutineId>& evaluator :
                 listed.defaults) {
              plan.defaults.push_back(
                  evaluator.transform([&](hir::NamespaceSubroutineId callable) {
                    const hir::SubroutinePrototype& evaluating =
                        declared.subroutines.Get(callable).prototype;
                    return DefaultEvaluation{
                        .form = FormOfNamespaceSubroutine(
                            unit_lowerer,
                            hir::NamespaceSubroutineRef{
                                .owner = ref.owner, .subroutine = callable}),
                        .type =
                            unit_lowerer.TranslateType(evaluating.result_type),
                        .passing =
                            ResultPassingOf(evaluating.result_direction)};
                  }));
            }
            return plan;
          },
          [&](const hir::PublishedScopeMethodRef& ref) -> Planned {
            const hir::ScopeClassSignature& scope =
                unit_lowerer.Hir().scope_classes.Get(ref.scope_class);
            const hir::ScopeCallable& listed =
                scope.callables.Get(ref.callable);
            const hir::SubroutinePrototype& published = listed.prototype;
            SubroutineCallee plan;
            plan.kind = published.interface.kind;
            plan.result_type = result_type;
            plan.completion = BuildCompletionLayout(
                CalleeFormalsOf(unit_lowerer, published.interface),
                result_type);
            plan.form = FormOfPublishedSubroutine(unit_lowerer, ref);
            for (const std::optional<hir::PublishedCallableId>& evaluator :
                 listed.defaults) {
              plan.defaults.push_back(
                  evaluator.transform([&](hir::PublishedCallableId callable) {
                    const hir::SubroutinePrototype& evaluating =
                        scope.callables.Get(callable).prototype;
                    return DefaultEvaluation{
                        .form = FormOfPublishedSubroutine(
                            unit_lowerer,
                            hir::PublishedScopeMethodRef{
                                .receiver = ref.receiver,
                                .scope_class = ref.scope_class,
                                .callable = callable}),
                        .type =
                            unit_lowerer.TranslateType(evaluating.result_type),
                        .passing =
                            ResultPassingOf(evaluating.result_direction)};
                  }));
            }
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
  mir::Builder block = frame.Builder();
  const auto* handle = std::get_if<hir::HandleReceiver>(&receiver);
  if (handle == nullptr) {
    return block.Add(MakeSelfRefExpr(frame));
  }
  auto handle_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(handle->expr), frame);
  if (!handle_or) return std::unexpected(std::move(handle_or.error()));
  return BuildObjectPointer(
      lowerer.Owner().Unit(), block, block.Add(*std::move(handle_or)));
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

// Where the first parameter of the callee `form` names comes from, or nothing
// for a callee taking none.
auto AmbientHandleOf(const CalleeForm& form) -> std::optional<AmbientHandle> {
  return std::visit(
      Overloaded{
          [](const NamedCallee& named) { return named.handle; },
          [](const DispatchedCallee& dispatched) {
            return std::optional{dispatched.receiver};
          }},
      form);
}

// The class handle the source named the object of the call `form` names
// through, where it named one (LRM 8.4).
auto ClassHandleOf(const CalleeForm& form) -> std::optional<hir::ExprId> {
  const std::optional<AmbientHandle> handle = AmbientHandleOf(form);
  const auto* called =
      handle.has_value() ? std::get_if<CalledObject>(&*handle) : nullptr;
  const auto* through = called == nullptr
                            ? nullptr
                            : std::get_if<hir::HandleReceiver>(&called->source);
  if (through == nullptr) return std::nullopt;
  return through->expr;
}

// The class handle a call holds the object it is made on through, where it
// holds one: the one the source named the object by, where the call leaves a
// formal to a default, `omits_any`, whose evaluation the object has to outlive.
auto HeldHandleOf(const CalleeForm& form, bool omits_any)
    -> std::optional<hir::ExprId> {
  if (!omits_any) return std::nullopt;
  return ClassHandleOf(form);
}

// A component landing in an actual is what gives a call something to sequence
// once it completes.
auto WritesBack(const SubroutineCallee& plan) -> bool {
  return std::ranges::any_of(
      plan.completion.formals, [](const CompletionLayout::Formal& formal) {
        return formal.component.has_value();
      });
}

// Whether the call `call` of the callee `plan` takes steps of its own around
// the call it makes, which then lives in a scope of its own: it writes values
// back once the callee completes, or it holds the object it is made on while
// the defaults it leaves are evaluated and the callee runs. With neither, the
// call stands on its own.
auto TakesSteps(
    const base::Arena<hir::Expr, hir::ExprId>& exprs, const hir::CallExpr& call,
    const SubroutineCallee& plan) -> bool {
  return WritesBack(plan) ||
         HeldHandleOf(plan.form, OmitsAny(exprs, RequiredOperands(call)))
             .has_value();
}

// The value the handle of `form` names, evaluated in the block `frame` is
// writing, or nothing for a callee taking none.
template <ExprLowerer Lowerer>
auto EvaluateHandle(
    Lowerer& lowerer, const WalkFrame& frame, const CalleeForm& form)
    -> diag::Result<std::optional<mir::ExprId>> {
  const std::optional<AmbientHandle> handle = AmbientHandleOf(form);
  if (!handle.has_value()) return std::nullopt;
  auto value = BuildAmbientHandle(lowerer, frame, *handle);
  if (!value) return std::unexpected(std::move(value.error()));
  return std::optional{*value};
}

// How a call names the callee `form` names and what leads the arguments the
// source wrote, where its handle evaluated to `handle`.
auto ResolveCalleeOn(
    const UnitLowerer& unit_lowerer, mir::Builder block, const CalleeForm& form,
    std::optional<mir::ExprId> handle) -> ResolvedCallee {
  return std::visit(
      Overloaded{
          [&](const NamedCallee& named) -> ResolvedCallee {
            if (!named.handle.has_value()) {
              return ResolvedCallee{
                  .callee = named.callee, .leading = std::nullopt};
            }
            if (!BindsAsReceiver(*named.handle)) {
              return ResolvedCallee{.callee = named.callee, .leading = handle};
            }
            return ResolvedCallee{
                .callee =
                    mir::Direct{
                        .target = named.callee.target,
                        .receiver = BuildObjectDeref(
                            unit_lowerer.Unit(), block, *handle)},
                .leading = std::nullopt};
          },
          [&](const DispatchedCallee& dispatched) -> ResolvedCallee {
            return ResolvedCallee{
                .callee = DispatchingCallee(
                    unit_lowerer, block, *handle, dispatched.slot),
                .leading = std::nullopt};
          }},
      form);
}

// What the handle of `form` names, evaluated once for a call that leaves a
// formal to a default: a default is evaluated on the object the callee is made
// on, so the one value stands at both. Where the call holds its object through
// `held`, the class handle the source named it by, the call keeps a share of
// the handle for as long as it runs, in the scope of its own `frame` writes,
// and opens it once on it: a default may let go of every other handle to the
// object, or point the one the source named at another, and the call is still
// made on the object it began with (LRM 8.4, 13.5.3).
template <ExprLowerer Lowerer>
auto EvaluateSharedHandle(
    Lowerer& lowerer, const WalkFrame& frame, const CalleeForm& form,
    std::optional<hir::ExprId> held)
    -> diag::Result<std::optional<mir::ExprId>> {
  if (held.has_value()) {
    auto value = lowerer.LowerExpr(lowerer.HirExprs().Get(*held), frame);
    if (!value) return std::unexpected(std::move(value.error()));
    mir::Builder block = frame.Builder();
    const mir::ExprId held = block.Add(*std::move(value));
    const mir::TypeId type = block.Get(held).type;
    const mir::LocalId share = DeclareLocal(frame, held);
    return std::optional{EvaluatedOnce(
        frame,
        BuildObjectPointer(
            lowerer.Owner().Unit(), block, block.ReadLocal(share, type)))};
  }
  auto handle = EvaluateHandle(lowerer, frame, form);
  if (!handle) return std::unexpected(std::move(handle.error()));
  return handle->transform(
      [&](mir::ExprId value) { return EvaluatedOnce(frame, value); });
}

// What a function evaluating a default answers, of the function `form` names
// with `handle`, called with `earlier`, what the call binds each formal before
// it, and handing on the report `frame` hands on, so what the default reads is
// stated where the callee's own reads are.
auto CallDefaultEvaluator(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const CalleeForm& form,
    std::optional<mir::ExprId> handle, std::span<const mir::ExprId> earlier,
    mir::TypeId answered) -> mir::ExprId {
  ResolvedCallee evaluator =
      ResolveCalleeOn(unit_lowerer, frame.Builder(), form, handle);
  std::vector<mir::ExprId> arguments;
  arguments.reserve(earlier.size() + 1);
  if (evaluator.leading.has_value()) arguments.push_back(*evaluator.leading);
  arguments.insert(arguments.end(), earlier.begin(), earlier.end());
  return BuildFunctionCall(
      unit_lowerer.Unit(), frame, std::move(evaluator.callee),
      std::move(arguments), answered);
}

auto EvaluateOmitted(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const SubroutineCallee& plan, std::size_t position,
    std::optional<mir::ExprId> handle, std::span<const mir::ExprId> earlier)
    -> EvaluatedDefault {
  if (position >= plan.defaults.size() ||
      !plan.defaults[position].has_value()) {
    throw InternalError(
        "EvaluateOmitted: an omitted formal has a default its callee "
        "evaluates");
  }
  const DefaultEvaluation& evaluation = *plan.defaults[position];
  return EvaluatedDefault{
      .answer = CallDefaultEvaluator(
          unit_lowerer, frame, evaluation.form, handle, earlier,
          PassedTypeOf(
              unit_lowerer.Unit(), evaluation.type, evaluation.passing)),
      .type = evaluation.type};
}

// Whether an actual of type `actual` is handed to a formal of type `formal`
// as a value of any type: a formal of `dynamic_type` takes a value of whichever
// type its actual has, and keeps that type with it (LRM 15.4.9). An actual
// already of that type is handed on as it is.
auto TakenAsAny(
    const UnitLowerer& unit_lowerer, mir::TypeId formal, hir::TypeId actual)
    -> bool {
  return unit_lowerer.Unit().types.Get(formal).IsDeclaredBy(
             support::LibraryDecl::kAny) &&
         !unit_lowerer.Hir().types.Get(actual).Is<hir::DynamicType>();
}

// The identity a value of `type` is kept with, as the machine text a call
// names it by.
auto IdentityText(
    UnitLowerer& unit_lowerer, mir::Builder block, hir::TypeId type)
    -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  return block.Add(
      mir::MakeStringLiteral(
          unit.builtins.machine_c_string,
          EquivalenceIdentityOf(unit_lowerer, type)));
}

// `value`, of `type`, as a value of the type `any` holding a copy of it, kept
// at the type every type equivalent to `type` is kept at.
auto MadeAny(
    UnitLowerer& unit_lowerer, mir::Builder block, hir::TypeId type,
    mir::ExprId value, mir::TypeId any) -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId kept =
      WithoutQueueBounds(unit.types, unit_lowerer.TranslateType(type));
  return block.Add(
      mir::MakeCallExpr(
          mir::Direct{.target = support::BuiltinFn::kAnyMake},
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
  mir::Builder block = frame.Builder();
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
              frame, block.Add(BuildDefaultValueExpr(unit, block, kept))),
          kept),
      .descent = {}};

  ClosureBuilder carry(unit, frame, support::ClosureKind::kFn);
  mir::Builder body = carry.Body();
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
            const mir::LocalId local = DeclareLocal(frame, leased);
            return block.LocalPlace(local, block.Get(leased).type);
          }),
      *ParamTypeOf(unit, seen, hir::ParamDirection::kRef));
  const AccessPath variable{
      .owner = SnapshotPlaceIntoClosure(unit_lowerer, frame, carry, bound),
      .descent = {}};
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.Add(BuildStoreExpr(
              unit, body, variable,
              ConvertToType(
                  unit, body, PathValue(unit, body, received), seen)))});
  const mir::ExprId made = block.Add(carry.BuildVoid());
  const mir::TypeId closure_type = block.Get(made).type;
  const mir::ExprId lent = BuildBorrowedClosure(
      unit, block, block.LocalPlace(DeclareLocal(frame, made), closure_type));

  return block.Add(
      mir::MakeCallExpr(
          mir::Direct{
              .target = support::BuiltinFn::kSeeAsAny,
              .type_arguments = {kept}},
          {reference_to(incoming, kept),
           IdentityText(unit_lowerer, block, type), lent},
          reference));
}

}  // namespace

auto PlaceAnswered(
    const WalkFrame& frame, mir::ExprId reference, mir::TypeId type,
    const hir::Expr& omitted) -> Lvalue {
  mir::Builder block = frame.Builder();
  const mir::TypeId held = block.Get(reference).type;
  return Lvalue{
      .form =
          AccessPath{
              .owner = block.LocalPlace(DeclareLocal(frame, reference), held),
              .descent = {}},
      .type = type,
      .source_type = omitted.type,
      .span = omitted.span};
}

namespace {

auto ReferenceToHeld(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::LocalId held,
    mir::TypeId type) -> mir::ExprId {
  mir::Builder block = frame.Builder();
  return EvaluatedOnce(
      frame,
      AtType(
          unit, block,
          BuildReferenceArg(
              unit, block, block.Add(mir::MakeLocalRefExpr(held, type)), type),
          *ParamTypeOf(unit, type, hir::ParamDirection::kRef)));
}

}  // namespace

auto OmitsAny(
    const base::Arena<hir::Expr, hir::ExprId>& exprs,
    std::span<const hir::ExprId> operands) -> bool {
  return std::ranges::any_of(operands, [&](hir::ExprId id) {
    return std::holds_alternative<hir::CalleeDefaultExpr>(exprs.Get(id).data);
  });
}

auto HeldActuals::HoldValue(
    const WalkFrame& frame, std::size_t position, mir::ExprId value)
    -> mir::ExprId {
  if (!omits_any_) return value;
  mir::Builder block = frame.Builder();
  const mir::TypeId type = block.Get(value).type;
  const mir::LocalId held = DeclareLocal(frame, value);
  earlier_[position] = ReferenceToHeld(*unit_, frame, held, type);
  return block.ReadLocal(held, type);
}

auto HeldActuals::HoldReference(
    const WalkFrame& frame, std::size_t position, mir::ExprId reference)
    -> mir::ExprId {
  if (!omits_any_) return reference;
  earlier_[position] = EvaluatedOnce(frame, reference);
  return earlier_[position];
}

void HeldActuals::HoldStartValue(
    const WalkFrame& frame, std::size_t position, mir::TypeId type) {
  if (!omits_any_) return;
  mir::Builder block = frame.Builder();
  earlier_[position] = ReferenceToHeld(
      *unit_, frame,
      DeclareLocal(
          frame, block.Add(BuildDefaultValueExpr(*unit_, block, type))),
      type);
}

template <ExprLowerer Lowerer>
auto EvaluateImportDefault(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::ImportDeclaringScope& scope, std::size_t position,
    std::span<const mir::ExprId> earlier) -> diag::Result<EvaluatedDefault> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const auto named = [&](const auto& defaults) -> const auto& {
    if (position >= defaults.size() || !defaults[position].has_value()) {
      throw InternalError(
          "EvaluateImportDefault: a formal left to its default has one its "
          "scope evaluates");
    }
    return *defaults[position];
  };
  const DefaultEvaluation evaluation = std::visit(
      Overloaded{
          [&](const hir::OwnDeclaringScope& own) {
            const hir::StructuralSubroutineId evaluator = named(own.defaults);
            const hir::SubroutineDecl& declared =
                lowerer.LookupHirSubroutine(evaluator);
            return DefaultEvaluation{
                .form = FormOfOwnSubroutine(
                    lowerer,
                    hir::StructuralSubroutineRef{.subroutine = evaluator}),
                .type = unit_lowerer.TranslateType(declared.result_type),
                .passing = ResultPassingOf(declared.result_direction)};
          },
          [&](const hir::PublishedDeclaringScope& published) {
            const hir::PublishedCallableId evaluator =
                named(published.defaults);
            const hir::SubroutinePrototype& prototype =
                unit_lowerer.Hir()
                    .scope_classes.Get(published.scope_class)
                    .callables.Get(evaluator)
                    .prototype;
            return DefaultEvaluation{
                .form = FormOfPublishedSubroutine(
                    unit_lowerer,
                    hir::PublishedScopeMethodRef{
                        .receiver = published.receiver,
                        .scope_class = published.scope_class,
                        .callable = evaluator}),
                .type = unit_lowerer.TranslateType(prototype.result_type),
                .passing = ResultPassingOf(prototype.result_direction)};
          },
          [&](const hir::NamespaceDeclaringScope& space) {
            const hir::NamespaceSubroutineId evaluator = named(space.defaults);
            const hir::SubroutinePrototype& prototype =
                unit_lowerer.Hir()
                    .namespaces.SubroutineOf(space.owner, evaluator)
                    .prototype;
            return DefaultEvaluation{
                .form = FormOfNamespaceSubroutine(
                    unit_lowerer,
                    hir::NamespaceSubroutineRef{
                        .owner = space.owner, .subroutine = evaluator}),
                .type = unit_lowerer.TranslateType(prototype.result_type),
                .passing = ResultPassingOf(prototype.result_direction)};
          }},
      scope);
  auto handle = EvaluateHandle(lowerer, frame, evaluation.form);
  if (!handle) return std::unexpected(std::move(handle.error()));
  return EvaluatedDefault{
      .answer = CallDefaultEvaluator(
          unit_lowerer, frame, evaluation.form, *handle, earlier,
          PassedTypeOf(
              unit_lowerer.Unit(), evaluation.type, evaluation.passing)),
      .type = evaluation.type};
}

template auto EvaluateImportDefault(
    ProcessLowerer&, const WalkFrame&, const hir::ImportDeclaringScope&,
    std::size_t, std::span<const mir::ExprId>)
    -> diag::Result<EvaluatedDefault>;
template auto EvaluateImportDefault(
    const StructuralScopeLowerer&, const WalkFrame&,
    const hir::ImportDeclaringScope&, std::size_t, std::span<const mir::ExprId>)
    -> diag::Result<EvaluatedDefault>;

template <ExprLowerer Lowerer>
auto EmitBoundActual(
    Lowerer& lowerer, const WalkFrame& frame, const hir::Expr& actual,
    Passing passing, mir::TypeId type) -> diag::Result<mir::ExprId> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Builder block = frame.Builder();
  switch (passing) {
    case Passing::kByReference:
    case Passing::kByReadOnlyReference: {
      auto named = lowerer.LowerLhsExpr(actual, frame);
      if (!named) return std::unexpected(std::move(named.error()));
      const mir::TypeId reference = PassedTypeOf(unit, type, passing);
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
    case Passing::kByValue: {
      auto lowered = lowerer.LowerExpr(actual, frame);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      const mir::ExprId value = block.Add(*std::move(lowered));
      // LRM 13.5.1, 10.8: an input passes its value into the formal as an
      // assignment to it would.
      return TakenAsAny(lowerer.Owner(), type, actual.type)
                 ? MadeAny(lowerer.Owner(), block, actual.type, value, type)
                 : AtType(unit, block, value, type);
    }
  }
  throw InternalError("EmitBoundActual: unknown passing");
}

template auto EmitBoundActual(
    ProcessLowerer&, const WalkFrame&, const hir::Expr&, Passing, mir::TypeId)
    -> diag::Result<mir::ExprId>;
template auto EmitBoundActual(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::Expr&, Passing,
    mir::TypeId) -> diag::Result<mir::ExprId>;

namespace {

// What a call binds its formals to: the arguments handed the callee, in order,
// and where each value it completes with is written back.
struct BoundFormals {
  std::vector<mir::ExprId> arguments;
  std::vector<CompletionWriteback> writebacks;
};

// Binds each formal to what the call wrote or to what `answered` evaluates of
// its default, in the order a call evaluates them.
template <ExprLowerer Lowerer>
auto BindFormals(
    Lowerer& lowerer, const WalkFrame& frame,
    std::span<const CompletionLayout::Formal> formals,
    std::span<const hir::ExprId> operands,
    const std::function<diag::Result<EvaluatedDefault>(
        std::size_t, std::span<const mir::ExprId>)>& answered)
    -> diag::Result<BoundFormals> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto& hir_exprs = lowerer.HirExprs();
  mir::Builder block = frame.Builder();
  HeldActuals held(unit, operands.size(), OmitsAny(hir_exprs, operands));
  std::vector<std::optional<mir::ExprId>> arguments(operands.size());
  std::vector<std::optional<CompletionWriteback>> writebacks(operands.size());

  const auto bind = [&](std::size_t i) -> diag::Result<void> {
    const CompletionLayout::Formal& formal = formals[i];
    const hir::Expr& hir_arg = hir_exprs.Get(operands[i]);
    const bool omitted =
        std::holds_alternative<hir::CalleeDefaultExpr>(hir_arg.data);
    // What a call writes back to: the actual it wrote, or the place the default
    // of a formal it omitted names (LRM 13.5.3).
    const auto written_place = [&]() -> diag::Result<Lvalue> {
      if (!omitted) return LowerLvalue(lowerer, hir_arg, frame);
      auto place = answered(i, held.Before(i));
      if (!place) return std::unexpected(std::move(place.error()));
      return PlaceAnswered(frame, place->answer, place->type, hir_arg);
    };

    // Exhaustive over the directions, so one added to the language is bound
    // here rather than silently passed as a value.
    switch (formal.direction) {
      // An `output` passes no argument, and binds the actual's place for the
      // writeback once the call completes.
      case hir::ParamDirection::kOutput: {
        auto place_or = written_place();
        if (!place_or) return std::unexpected(std::move(place_or.error()));
        writebacks[i] = CompletionWriteback{
            .target = *std::move(place_or),
            .component = *formal.component,
            .type = formal.type};
        held.HoldStartValue(frame, i, formal.type);
        return {};
      }

      // An `inout` passes its incoming value, brought to the formal's type as
      // an assignment to the formal would bring it, and is written back the
      // same way; the source writes the actual once (LRM 13.5).
      case hir::ParamDirection::kInOut: {
        auto place_or = written_place();
        if (!place_or) return std::unexpected(std::move(place_or.error()));
        auto actual = ReadThenWrite(unit, frame, *std::move(place_or));
        if (!actual) return std::unexpected(std::move(actual.error()));
        arguments[i] = held.HoldValue(
            frame, i,
            ConvertToType(unit, block, actual->incoming, formal.type));
        writebacks[i] = CompletionWriteback{
            .target = std::move(actual->lvalue),
            .component = *formal.component,
            .type = formal.type};
        return {};
      }

      case hir::ParamDirection::kRef:
      case hir::ParamDirection::kConstRef: {
        if (omitted) {
          auto reference = answered(i, held.Before(i));
          if (!reference) return std::unexpected(std::move(reference.error()));
          arguments[i] = held.HoldReference(
              frame, i,
              AtType(
                  unit, block, reference->answer,
                  *ParamTypeOf(unit, formal.type, formal.direction)));
          return {};
        }
        auto bound = EmitBoundActual(
            lowerer, frame, hir_arg, *ParamPassingOf(formal.direction),
            formal.type);
        if (!bound) return std::unexpected(std::move(bound.error()));
        arguments[i] = held.HoldReference(frame, i, *bound);
        return {};
      }

      // An omitted input is the value the callee's default evaluates to.
      case hir::ParamDirection::kInput: {
        if (omitted) {
          auto value = answered(i, held.Before(i));
          if (!value) return std::unexpected(std::move(value.error()));
          arguments[i] = held.HoldValue(
              frame, i, AtType(unit, block, value->answer, formal.type));
          return {};
        }
        auto bound = EmitBoundActual(
            lowerer, frame, hir_arg, Passing::kByValue, formal.type);
        if (!bound) return std::unexpected(std::move(bound.error()));
        arguments[i] = held.HoldValue(frame, i, *bound);
        return {};
      }
    }
    throw InternalError("BindFormals: unknown parameter direction");
  };
  if (auto visited = ForEachInCallOrder(hir_exprs, operands, bind); !visited) {
    return std::unexpected(std::move(visited.error()));
  }

  BoundFormals bound;
  for (const std::optional<mir::ExprId>& argument : arguments) {
    if (argument.has_value()) bound.arguments.push_back(*argument);
  }
  for (std::optional<CompletionWriteback>& writeback : writebacks) {
    if (writeback.has_value()) {
      bound.writebacks.push_back(*std::move(writeback));
    }
  }
  return bound;
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
  mir::Builder block = frame.Builder();

  const mir::TypeId payload_type =
      CompletionPayloadType(unit, plan.completion.components);
  const mir::TypeId call_result_type =
      SubroutineCallType(unit, plan.kind, payload_type);

  // The object the call is made on is formed after the arguments the source
  // wrote, as in any call, unless a default needs it first: then it is formed
  // as the first default is reached, once, and every default and the callee
  // are made on it, as a construction creates its object there (LRM 13.5).
  const std::vector<hir::ExprId> operands = RequiredOperands(call);
  const std::optional<hir::ExprId> held =
      HeldHandleOf(plan.form, OmitsAny(hir_exprs, operands));
  std::optional<std::optional<mir::ExprId>> handle;
  auto bound = BindFormals(
      lowerer, frame, plan.completion.formals, operands,
      [&](std::size_t position, std::span<const mir::ExprId> earlier)
          -> diag::Result<EvaluatedDefault> {
        if (!handle.has_value()) {
          auto shared = EvaluateSharedHandle(lowerer, frame, plan.form, held);
          if (!shared) return std::unexpected(std::move(shared.error()));
          handle = *shared;
        }
        return EvaluateOmitted(
            lowerer.Owner(), frame, plan, position, *handle, earlier);
      });
  if (!bound) return std::unexpected(std::move(bound.error()));
  if (!handle.has_value()) {
    auto evaluated = EvaluateHandle(lowerer, frame, plan.form);
    if (!evaluated) return std::unexpected(std::move(evaluated.error()));
    handle = *evaluated;
  }
  ResolvedCallee resolved =
      ResolveCalleeOn(lowerer.Owner(), block, plan.form, *handle);

  std::vector<mir::ExprId> call_args;
  call_args.reserve(call.arguments.size() + 1);
  if (resolved.leading.has_value()) {
    call_args.push_back(*resolved.leading);
  }
  call_args.insert(
      call_args.end(), bound->arguments.begin(), bound->arguments.end());

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
                      .callee = std::move(resolved.callee),
                      .arguments = std::move(call_args)},
              .type = call_result_type},
      .payload_type = payload_type,
      .writebacks = std::move(bound->writebacks)};
}

// Where a call taking steps of its own left its completion: the local it is
// bound to and the payload type to project components out of it by.
struct BoundCompletion {
  mir::LocalId completion;
  mir::TypeId payload_type;
};

// Emits the call, binds its completion, and writes each output component back
// to its actual, into whichever body `frame` is currently writing, which is
// the scope of the call's own: what the call holds ends with it.
template <ExprLowerer Lowerer>
auto EmitCallSteps(
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
auto LowerCallInItsOwnScope(
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
    auto bound = EmitCallSteps(lowerer, closure.Frame(), call, plan);
    if (!bound) return std::unexpected(std::move(bound.error()));
    return closure.BuildCallOnce();
  }

  // A function's completion is bound and written back here and now, so the
  // steps are one block expression and the call stays where it was written.
  // It yields the function's result, or, for one settling none -- a loop's
  // step may be such a call (LRM 12.7.1) -- the completion itself, as such a
  // call standing on its own does.
  BlockBuilder steps(frame);
  auto bound = EmitCallSteps(lowerer, steps.Frame(), call, plan);
  if (!bound) return std::unexpected(std::move(bound.error()));
  if (!plan.result_type.has_value()) {
    return steps.Build(
        steps.Body().ReadLocal(bound->completion, bound->payload_type));
  }
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
  mir::Builder block = frame.Builder();
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
      named =
          DispatchingCallee(unit_lowerer, block, object, *facts.target.slot);
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

  // Only a function that takes steps of its own and settles no value. A task
  // hands its caller a coroutine to await, and a valued function hands it the
  // component it read, so both are expressions the caller consumes.
  const bool steps_only = TakesSteps(lowerer.HirExprs(), call, callee) &&
                          callee.kind != hir::SubroutineKind::kTask &&
                          !callee.result_type.has_value();
  if (!steps_only) return std::nullopt;

  BlockBuilder steps(frame);
  auto bound = EmitCallSteps(lowerer, steps.Frame(), call, callee);
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

  if (TakesSteps(lowerer.HirExprs(), call, callee)) {
    // Writing a value back reaches a caller's storage, which only a procedural
    // statement does; the frontend rejects an output / inout call outside
    // procedural code (LRM 13.4), so one arriving in a structural context is a
    // compiler-invariant violation rather than a user-diagnosable form.
    if constexpr (!std::same_as<Lowerer, ProcessLowerer>) {
      if (WritesBack(callee)) {
        throw InternalError(
            "LowerSubroutineCall: a structural subroutine call carries an "
            "output or inout argument");
      }
    }
    return LowerCallInItsOwnScope(lowerer, frame, call, callee);
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
  mir::Builder block = frame.Builder();
  const mir::ExprId completion = block.Add(std::move(emitted->call));
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
  // Built from the call the list follows, so a callee that reports where it
  // was called is told that call.
  mir::Builder block = frame.Builder().At(hir_call.span);

  const mir::TypeId payload_type =
      CompletionPayloadType(unit, plan.completion.components);
  const mir::TypeId call_result_type =
      SubroutineCallType(unit, plan.kind, payload_type);
  const std::vector<hir::ExprId> operands = RequiredOperands(*call);
  const bool omits_any = OmitsAny(lowerer.HirExprs(), operands);
  // What the function reads is fixed by its text, so the order the call's
  // operands run in decides nothing here, and the object is formed first.
  auto handle = omits_any ? EvaluateSharedHandle(
                                lowerer, frame, plan.form,
                                HeldHandleOf(plan.form, omits_any))
                          : EvaluateHandle(lowerer, frame, plan.form);
  if (!handle) return std::unexpected(std::move(handle.error()));
  ResolvedCallee resolved =
      ResolveCalleeOn(lowerer.Owner(), block, plan.form, *handle);

  std::vector<mir::ExprId> call_args;
  if (resolved.leading.has_value()) {
    call_args.push_back(*resolved.leading);
  }
  if (operands.size() != plan.completion.formals.size()) {
    throw InternalError("EmitReportingCall: argument / formal count mismatch");
  }
  HeldActuals held(unit, operands.size(), omits_any);
  for (std::size_t i = 0; i < operands.size(); ++i) {
    const CompletionLayout::Formal& formal = plan.completion.formals[i];
    const hir::TypeId actual_type = lowerer.HirExprs().Get(operands[i]).type;
    switch (formal.direction) {
      // What the function would write back is no part of the list, which
      // writes nothing back.
      case hir::ParamDirection::kOutput:
        held.HoldStartValue(frame, i, formal.type);
        break;
      // An `inout` actual is of its own type, and the function takes the
      // formal's.
      case hir::ParamDirection::kInput:
      case hir::ParamDirection::kInOut: {
        // An omitted input is the callee's default, which is part of what the
        // call reads, so the function evaluating it is asked here too.
        if (formal.direction == hir::ParamDirection::kInput &&
            std::holds_alternative<hir::CalleeDefaultExpr>(
                lowerer.HirExprs().Get(operands[i]).data)) {
          call_args.push_back(held.HoldValue(
              frame, i,
              EvaluateOmitted(
                  lowerer.Owner(), frame.WithReportingReachedTo(report), plan,
                  i, *handle, held.Before(i))
                  .answer));
          break;
        }
        const mir::ExprId value = block.Add(
            BuildDefaultValueFromHir(lowerer.Owner(), block, actual_type));
        call_args.push_back(held.HoldValue(
            frame, i,
            TakenAsAny(lowerer.Owner(), formal.type, actual_type)
                ? MadeAny(
                      lowerer.Owner(), block, actual_type, value, formal.type)
                : ConvertToType(unit, block, value, formal.type)));
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
        if (unit.types.Get(formal.type)
                .IsDeclaredBy(support::LibraryDecl::kAny)) {
          call_args.push_back(held.HoldReference(
              frame, i,
              block.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee = mir::Construct{}, .arguments = {}},
                      .type = reference})));
          break;
        }
        const mir::TypeId value_type =
            lowerer.Owner().TranslateType(actual_type);
        const mir::LocalId temporary = DeclareLocal(
            frame, block.Add(BuildDefaultValueFromHir(
                       lowerer.Owner(), block, actual_type)));
        call_args.push_back(held.HoldReference(
            frame, i,
            AtType(
                unit, block,
                BuildReferenceArg(
                    unit, block,
                    block.Add(mir::MakeLocalRefExpr(temporary, value_type)),
                    value_type),
                reference)));
        break;
      }
    }
  }
  call_args.push_back(block.ReadLocal(report, unit.builtins.read_report_ptr));
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee = std::move(resolved.callee),
                          .arguments = std::move(call_args)},
                  .type = call_result_type})});
  return {};
}

template <ExprLowerer Lowerer>
auto EmitConstructionActuals(
    Lowerer& lowerer, const WalkFrame& frame, hir::ClassId cls,
    std::span<const hir::ExprId> actuals, std::span<const mir::ExprId> leading,
    const std::function<mir::ExprId()>& object)
    -> diag::Result<std::vector<mir::ExprId>> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Builder block = frame.Builder();
  const CompletionLayout layout = BuildCompletionLayout(
      ConstructorFormalsOf(unit_lowerer, cls), std::nullopt);
  if (actuals.size() != layout.formals.size()) {
    throw InternalError(
        "EmitConstructionActuals: argument / formal count mismatch");
  }
  const hir::ClassDeclaration& declaration =
      unit_lowerer.Hir().classes.Declaration(cls);
  auto bound = BindFormals(
      lowerer, frame, layout.formals, actuals,
      [&](std::size_t position, std::span<const mir::ExprId> earlier)
          -> diag::Result<EvaluatedDefault> {
        const std::optional<hir::MethodId>& method =
            declaration.constructor->defaults[position];
        if (!method.has_value()) {
          throw InternalError(
              "EmitConstructionActuals: a constructor's formal a construction "
              "omits has a default the class evaluates");
        }
        const hir::SubroutinePrototype& evaluating =
            declaration.methods.Get(*method).prototype;
        std::vector<mir::ExprId> arguments(leading.begin(), leading.end());
        arguments.insert(arguments.end(), earlier.begin(), earlier.end());
        const mir::TypeId type =
            unit_lowerer.TranslateType(evaluating.result_type);
        return EvaluatedDefault{
            .answer = BuildFunctionCall(
                unit, frame,
                mir::Direct{
                    .target = unit_lowerer.TranslateMethodTarget(
                        hir::ClassMethodTarget{
                            .owner = cls, .method = *method}),
                    .receiver = BuildObjectDeref(unit, block, object())},
                std::move(arguments),
                PassedTypeOf(
                    unit, type, ResultPassingOf(evaluating.result_direction))),
            .type = type};
      });
  if (!bound) return std::unexpected(std::move(bound.error()));
  return std::move(bound->arguments);
}

template auto EmitConstructionActuals(
    ProcessLowerer&, const WalkFrame&, hir::ClassId,
    std::span<const hir::ExprId>, std::span<const mir::ExprId>,
    const std::function<mir::ExprId()>&)
    -> diag::Result<std::vector<mir::ExprId>>;
template auto EmitConstructionActuals(
    const StructuralScopeLowerer&, const WalkFrame&, hir::ClassId,
    std::span<const hir::ExprId>, std::span<const mir::ExprId>,
    const std::function<mir::ExprId()>&)
    -> diag::Result<std::vector<mir::ExprId>>;

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

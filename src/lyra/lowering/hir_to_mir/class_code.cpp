#include "lyra/lowering/hir_to_mir/class_code.hpp"

#include <array>
#include <cstddef>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/class_registry.hpp"
#include "lyra/hir/declared_method.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/runtime_class.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Whether `a` and `b` take the same parameters, the first as an object alike,
// and answer the same.
auto SameSignature(const mir::CallableCode& a, const mir::CallableCode& b)
    -> bool {
  if (a.params.size() != b.params.size() || a.result_type != b.result_type) {
    return false;
  }
  const bool a_takes_receiver = a.TakesReceiver();
  if (a_takes_receiver != b.TakesReceiver()) {
    return false;
  }
  for (std::size_t i = 0; i < a.params.size(); ++i) {
    const mir::TypeId a_type = a.locals.Get(a.params[i]).type;
    if (a_type != b.locals.Get(b.params[i]).type) {
      return false;
    }
  }
  return true;
}

// What `code` takes and answers, by type identity, for a report.
auto SignatureText(const mir::CallableCode& code) -> std::string {
  std::string text = code.TakesReceiver() ? "(receiver " : "(";
  for (const mir::LocalId param : code.params) {
    text += std::format("{} ", code.locals.Get(param).type.value);
  }
  return text + std::format(") -> {}", code.result_type.value);
}

}  // namespace

auto OperationPrototype(std::span<const mir::TypeId> params, mir::TypeId result)
    -> mir::CallableCode {
  mir::CallableCode prototype;
  for (const mir::TypeId type : params) {
    prototype.params.push_back(prototype.AddLocal(type));
  }
  prototype.result_type = result;
  return prototype;
}

namespace {

// What the code of a class's operation taking `params` and answering `result`
// is entered with, the first of the params being its object where `on_object`
// says it runs on one.
auto ClassOperation(
    std::span<const mir::TypeId> params, mir::TypeId result, bool on_object)
    -> mir::CallableCode {
  mir::CallableCode prototype = OperationPrototype(params, result);
  if (on_object) prototype.receiver = prototype.params.front();
  return prototype;
}

}  // namespace

auto ClassConstructorPrototype(UnitLowerer& unit_lowerer, hir::ClassId cls)
    -> mir::CallableCode {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  std::vector<mir::TypeId> params{
      unit_lowerer.SelfPointerTo(unit_lowerer.ClassIdentity(cls)),
      unit.builtins.scope_ptr};
  for (const CalleeFormal& formal : ConstructorFormalsOf(unit_lowerer, cls)) {
    if (const std::optional<mir::TypeId> type =
            ParamTypeOf(unit, formal.type, formal.direction)) {
      params.push_back(*type);
    }
  }
  params.push_back(unit.builtins.read_report_ptr);
  return ClassOperation(params, unit.builtins.void_type, true);
}

auto ClassMethodPrototype(
    UnitLowerer& unit_lowerer, hir::ClassId cls, hir::MethodId method)
    -> mir::CallableCode {
  const hir::DeclaredMethod& stated =
      unit_lowerer.Hir().classes.Declaration(cls).methods.Get(method);
  const mir::TypeId object =
      unit_lowerer.SelfPointerTo(unit_lowerer.ClassIdentity(cls));
  const mir::CallableCode taken =
      PrototypeOf(unit_lowerer, stated.prototype, object);
  const bool on_object = std::visit(
      Overloaded{
          [](const hir::TypeAssociated&) { return false; },
          [](const hir::NotVirtual&) { return true; },
          [](const hir::IntroducesVirtual&) { return true; },
          [](const hir::OverridesVirtual&) { return true; }},
      stated.dispatch);
  std::vector<mir::TypeId> params{
      on_object ? object : unit_lowerer.Unit().builtins.scope_ptr};
  for (const mir::LocalId param : taken.ParamsAfterReceiver()) {
    params.push_back(taken.locals.Get(param).type);
  }
  return ClassOperation(params, taken.result_type, on_object);
}

auto ClassHookPrototype(
    UnitLowerer& unit_lowerer, hir::ClassId cls,
    support::LibraryCallback callback) -> mir::CallableCode {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId object =
      unit_lowerer.SelfPointerTo(unit_lowerer.ClassIdentity(cls));
  switch (callback) {
    case support::LibraryCallback::kClassRandVariables:
    case support::LibraryCallback::kClassConstrain:
    case support::LibraryCallback::kClassRandObjects: {
      const std::array params{
          object, *ConstraintProblemParamTypeOf(
                      unit, hir::SubroutineKind::kConstraintBlock)};
      return ClassOperation(params, unit.builtins.void_type, true);
    }
    case support::LibraryCallback::kClassPreRandomize:
    case support::LibraryCallback::kClassPostRandomize:
      return ClassOperation(std::array{object}, unit.builtins.void_type, true);
    case support::LibraryCallback::kScopeResolve:
    case support::LibraryCallback::kScopeInitialize:
    case support::LibraryCallback::kScopeCreateProcesses:
    case support::LibraryCallback::kScopeDescribe:
      break;
  }
  throw InternalError(
      "ClassHookPrototype: a class answers only what randomize() asks of it "
      "-- please report this as a bug");
}

auto SlotPrototype(const mir::CallableCode& operation, mir::TypeId self)
    -> mir::CallableCode {
  mir::CallableCode prototype;
  const mir::LocalId receiver = prototype.AddLocal(self);
  prototype.params.push_back(receiver);
  prototype.receiver = receiver;
  for (const mir::LocalId param : operation.params) {
    prototype.params.push_back(
        prototype.AddLocal(operation.locals.Get(param).type));
  }
  prototype.result_type = operation.result_type;
  return prototype;
}

auto EntryOf(
    UnitLowerer& unit_lowerer, mir::ClassId id,
    const DeclaringInstance& instance, const ClassCodePoint& point,
    mir::ClassId published) -> mir::CallableCode {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::CallableCode& operation =
      unit_lowerer.ClassCodePrototypeOf(published, id, point);
  const mir::CallableId slot =
      unit_lowerer.ClassCodeSlotOf(published, id, point);
  mir::CallableCode entry = mir::CallableCode::Defined();
  for (const mir::LocalId param : operation.params) {
    entry.params.push_back(entry.AddLocal(operation.locals.Get(param).type));
  }
  if (operation.TakesReceiver()) entry.receiver = entry.params.front();
  entry.result_type = operation.result_type;
  mir::Block& block = entry.Body();
  const auto read = [&](mir::LocalId param) {
    return block.ReadLocal(param, entry.locals.Get(param).type);
  };
  // The construction has written the instance into the object by the time any
  // other operation runs on it.
  const mir::ExprId handed = std::visit(
      Overloaded{
          [&](const ClassConstructorCode&) { return read(entry.params[1]); },
          [&](const ClassCallableCode&) {
            if (!entry.receiver.has_value()) {
              return read(entry.params.front());
            }
            return block.Read(block.exprs.Add(
                mir::MakeFieldAccessExpr(
                    BuildObjectDeref(unit, block, read(*entry.receiver)),
                    mir::ClassFieldTarget{.owner = id, .slot = instance.member},
                    instance.type)));
          }},
      point);
  std::vector<mir::ExprId> arguments;
  arguments.reserve(entry.params.size());
  for (const mir::LocalId param : entry.params) {
    arguments.push_back(read(param));
  }
  const mir::ExprId call = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Virtual{
                          .receiver = BuildObjectDeref(
                              unit, block,
                              ObjectAs(
                                  block, handed,
                                  unit_lowerer.SelfPointerTo(published))),
                          .slot =
                              mir::VirtualSlot{
                                  .owner_class = published, .slot = slot}},
                  .arguments = std::move(arguments)},
          .type = entry.result_type});
  CompleteWith(unit, block, call, entry.result_type);
  return entry;
}

auto StateClassFromDeclaration(UnitLowerer& unit_lowerer, hir::ClassId cls)
    -> mir::Class {
  const hir::ClassRegistry& classes = unit_lowerer.Hir().classes;
  const hir::ClassDeclaration& declared = classes.Declaration(cls);
  const mir::ClassId id = unit_lowerer.ClassIdentity(cls);

  mir::Class stated{};
  stated.identity = classes.PathOf(cls);
  stated.self_pointer_type = unit_lowerer.SelfPointerTo(id);
  const std::optional<DeclaringInstance> instance =
      unit_lowerer.StateDeclaredLayout(cls, stated);
  stated.conforming = unit_lowerer.ConformanceOf(cls);
  // Only a class belonging to an instance has code in the body realizing its
  // scope; an interface class has none (LRM 8.26).
  const auto entry = [&](const ClassCodePoint& point) {
    return EntryOf(
        unit_lowerer, id, *instance, point,
        unit_lowerer.PublishedClassOf(*declared.instance_scope));
  };

  // Each method at its position, under its name or, for the one making a
  // covergroup the class embeds, under that covergroup's (LRM 19.4.1). One the
  // declaration leaves pure has no code anywhere (LRM 8.21) and states only
  // what its definitions take.
  for (const hir::MethodId method : declared.methods.Ids()) {
    const hir::DeclaredMethod& stated_method = declared.methods.Get(method);
    const mir::CallableId callable{method.value};
    const ClassCodePoint point = ClassCallableCode{.callable = callable};
    stated.named_callables.push_back(
        mir::NamedCallable{
            .name = stated_method.makes.has_value()
                        ? hir::CovergroupFactoryName(
                              unit_lowerer.Hir().covergroups.PathOf(
                                  *stated_method.makes))
                        : stated_method.prototype.name,
            .body = callable});
    stated.callables.Add(
        mir::CallableDecl{
            .code = hir::IsLeftAbstract(stated_method.dispatch)
                        ? PrototypeOf(
                              unit_lowerer, stated_method.prototype,
                              stated.self_pointer_type)
                        : entry(point),
            .foreign = std::nullopt,
            .virtual_dispatch = unit_lowerer.DispatchRoleOf(
                hir::ClassMethodTarget{.owner = cls, .method = method}),
            .defining_unit = std::nullopt});
  }
  // What `randomize()` asks of the class (LRM 18.6), and the form a waited
  // evaluation calls of each function (LRM 9.4.2), each at the identity the
  // class took for it.
  const auto add_entry = [&](mir::CallableId callable) {
    const mir::CallableId added = stated.callables.Add(
        mir::CallableDecl{
            .code = entry(ClassCallableCode{.callable = callable}),
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt,
            .defining_unit = std::nullopt});
    if (added != callable) {
      throw InternalError(
          "StateClassFromDeclaration: a class's callables are its methods, "
          "then its hooks, then its reporting forms -- please report this as "
          "a bug");
    }
  };
  if (const std::optional<RandomizationHooks>& hooks =
          unit_lowerer.RandomizationHooksOf(cls)) {
    for (const auto& [body, callback] : hooks->Callbacks()) {
      stated.library_callbacks.push_back(
          mir::LibraryCallbackBody{.callback = callback, .body = body});
      add_entry(body);
    }
  }
  for (const auto& [_, form] : unit_lowerer.ReportingFormsOf(cls)) {
    add_entry(form);
  }
  if (!declared.is_interface_class) {
    stated.constructor =
        mir::ConstructorDecl{.code = entry(ClassConstructorCode{})};
  }
  return stated;
}

void MoveCodeIntoBody(
    UnitLowerer& unit_lowerer, mir::ClassId id, const mir::Class& compiled,
    const WalkFrame& declaring_frame, mir::ClassId published) {
  mir::Class& body = *declaring_frame.current_class.cls;
  const auto move_into_body = [&](const ClassCodePoint& point,
                                  mir::CallableCode code) {
    // What every override takes was read off the class's declaration, so the
    // code this body compiled has to take and answer what that states.
    const mir::CallableCode& operation =
        unit_lowerer.ClassCodePrototypeOf(published, id, point);
    if (!SameSignature(code, operation)) {
      const std::string point_text = std::visit(
          Overloaded{
              [](const ClassConstructorCode&) {
                return std::string{"the constructor"};
              },
              [](const ClassCallableCode& callable) {
                return std::format("callable {}", callable.callable.value);
              }},
          point);
      const std::string code_text = SignatureText(code);
      const std::string operation_text = SignatureText(operation);
      throw InternalError(
          std::format(
              "MoveCodeIntoBody: the code a body compiled for {} of class {} "
              "takes {} where the class's declaration states {} -- please "
              "report this as a bug",
              point_text, id.value, code_text, operation_text));
    }
    const mir::LocalId self = code.AddLocal(body.self_pointer_type);
    code.params.insert(code.params.begin(), self);
    code.receiver = self;
    body.callables.Add(
        mir::CallableDecl{
            .code = std::move(code),
            .foreign = std::nullopt,
            .virtual_dispatch =
                mir::OverridesVirtualSlot{
                    .overridden =
                        mir::VirtualSlot{
                            .owner_class = published,
                            .slot = unit_lowerer.ClassCodeSlotOf(
                                published, id, point)}},
            .defining_unit = std::nullopt});
  };

  if (compiled.constructor.has_value()) {
    move_into_body(ClassConstructorCode{}, compiled.constructor->code);
  }
  for (const mir::CallableId callable : compiled.callables.Ids()) {
    const mir::CallableCode& code = compiled.callables.Get(callable).code;
    if (code.body.has_value()) {
      move_into_body(ClassCallableCode{.callable = callable}, code);
    }
  }
}

}  // namespace lyra::lowering::hir_to_mir

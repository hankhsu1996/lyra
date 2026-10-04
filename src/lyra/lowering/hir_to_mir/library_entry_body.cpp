#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"

#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto AddBody(mir::Class& cls, mir::CallableCode code) -> mir::CallableId {
  return cls.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
}

// An entry before its statements are written: `self` reads the object it was
// handed as the class the body belongs to, and `args` read what it takes after
// the object.
struct EntryBody {
  mir::CallableCode code;
  mir::ExprId self;
  std::vector<mir::ExprId> args;
};

auto OpenEntryBody(
    const mir::Class& cls, mir::TypeId object,
    std::span<const mir::TypeId> params, mir::TypeId result) -> EntryBody {
  mir::CallableCode code = mir::CallableCode::Defined();
  const mir::LocalId handed = code.AddLocal(object);
  code.params = {handed};
  code.result_type = result;
  mir::Block& body = code.Body();
  const mir::ExprId self = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CastExpr{
                  .operand =
                      body.exprs.Add(mir::MakeLocalRefExpr(handed, object))},
          .type = cls.self_pointer_type});
  std::vector<mir::ExprId> args;
  for (const mir::TypeId type : params) {
    const mir::LocalId param = code.AddLocal(type);
    code.params.push_back(param);
    args.push_back(body.exprs.Add(mir::MakeLocalRefExpr(param, type)));
  }
  return EntryBody{
      .code = std::move(code), .self = self, .args = std::move(args)};
}

// Ends `code` with `call`, a call resulting in `result`, completing as that
// call does. A call completing as a coroutine is awaited, which completes this
// body as a coroutine too, with what that one completed with.
void CompleteWith(
    const mir::CompilationUnit& unit, mir::CallableCode& code, mir::ExprId call,
    mir::TypeId result) {
  mir::Block& body = code.Body();
  const auto* coroutine = unit.types.Get(result).As<mir::CoroutineType>();
  const mir::TypeId completion =
      coroutine != nullptr ? coroutine->payload : result;
  const mir::ExprId completed =
      coroutine != nullptr ? body.exprs.Add(
                                 mir::Expr{
                                     .data = mir::AwaitExpr{.execution = call},
                                     .type = completion})
                           : call;
  if (completion == unit.builtins.void_type) {
    body.AppendStmt(mir::ExprStmt{.expr = completed});
    body.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  } else {
    body.AppendStmt(mir::ReturnStmt{.value = completed});
  }
}

auto PrototypeOf(const mir::Class& cls, mir::CallableId callable) -> Prototype {
  const mir::CallableCode& code = cls.callables.Get(callable).code;
  Prototype prototype{.params = {}, .result = code.result_type};
  for (const mir::LocalId formal : code.ParamsAfterReceiver()) {
    prototype.params.push_back(code.locals.Get(formal).type);
  }
  return prototype;
}

// The entry calling `callable` of class `id` on the object, and no other body
// in its place: the function a method is entered through where the caller
// holds an address and no object of the class.
auto ForwardingBody(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& cls,
    mir::CallableId callable, mir::TypeId object) -> mir::CallableCode {
  const Prototype prototype = PrototypeOf(cls, callable);
  EntryBody entry =
      OpenEntryBody(cls, object, prototype.params, prototype.result);
  const mir::ExprId call = entry.code.Body().exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target =
                              mir::CallableTarget{
                                  .owner = id, .slot = callable},
                          .receiver = BuildObjectDeref(
                              unit, entry.code.Body(), entry.self)},
                  .arguments = std::move(entry.args)},
          .type = prototype.result});
  CompleteWith(unit, entry.code, call, prototype.result);
  return std::move(entry.code);
}

}  // namespace

auto AddFieldAddressEntry(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    mir::FieldId slot, mir::TypeId object) -> mir::CallableId {
  const mir::TypeId erased = mir::ErasedPointer(unit.types);
  EntryBody entry = OpenEntryBody(cls, object, {}, erased);
  const mir::TypeId field_type = cls.fields.Get(slot).type;
  mir::Block& body = entry.code.Body();
  const mir::ExprId field = body.exprs.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(unit, body, entry.self),
          mir::ClassFieldTarget{.owner = id, .slot = slot}, field_type));
  const mir::ExprId address = body.exprs.Add(
      mir::MakeAddressOfExpr(
          field, unit.types.Intern(
                     mir::Type{mir::PointerType{
                         .pointee = field_type,
                         .ownership = mir::PointerOwnership::kBorrowed}})));
  body.AppendStmt(
      mir::ReturnStmt{
          .value = body.exprs.Add(
              mir::Expr{
                  .data = mir::CastExpr{.operand = address}, .type = erased})});
  return AddBody(cls, std::move(entry.code));
}

auto AddDispatchingEntry(
    const mir::CompilationUnit& unit, mir::Class& cls,
    const mir::VirtualSlot& slot, const Prototype& prototype,
    mir::TypeId object) -> mir::CallableId {
  EntryBody entry =
      OpenEntryBody(cls, object, prototype.params, prototype.result);
  mir::Block& body = entry.code.Body();
  const mir::ExprId call = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Virtual{
                          .receiver = BuildObjectDeref(
                              unit, body,
                              AsIntroducer(unit.types, body, entry.self, slot)),
                          .slot = slot},
                  .arguments = std::move(entry.args)},
          .type = prototype.result});
  CompleteWith(unit, entry.code, call, prototype.result);
  return AddBody(cls, std::move(entry.code));
}

auto EntryOf(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    mir::CallableId callable, mir::TypeId object) -> mir::CallableId {
  if (!cls.callables.Get(callable).code.TakesReceiver()) {
    return callable;
  }
  return AddBody(cls, ForwardingBody(unit, id, cls, callable, object));
}

auto NamedEntries(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    std::span<const mir::NamedCallable> named, mir::TypeId object)
    -> std::vector<mir::NamedCallable> {
  std::vector<mir::NamedCallable> entries;
  for (const mir::NamedCallable& name : named) {
    const std::optional<mir::VirtualDispatchRole> role =
        cls.callables.Get(name.body).virtual_dispatch;
    const bool defined_here = std::holds_alternative<mir::DefinedHere>(
        mir::FormOf(cls.callables.Get(name.body)));
    if (role.has_value()) {
      entries.push_back(
          mir::NamedCallable{
              .name = name.name,
              .body = AddDispatchingEntry(
                  unit, cls, CanonicalVirtualSlot(id, name.body, *role),
                  PrototypeOf(cls, name.body), object)});
    } else if (defined_here) {
      entries.push_back(
          mir::NamedCallable{
              .name = name.name,
              .body = EntryOf(unit, id, cls, name.body, object)});
    }
  }
  return entries;
}

}  // namespace lyra::lowering::hir_to_mir

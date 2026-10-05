#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"

#include <optional>
#include <utility>
#include <vector>

#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto AddBody(mir::Class& cls, mir::CallableCode code) -> mir::CallableId {
  return cls.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
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

// The entry calling `callable` of class `id` on the object, and no other body
// in its place: handed the object as `object`, it views it as the class and
// passes on what it takes after the object.
auto ForwardingBody(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& cls,
    mir::CallableId callable, mir::TypeId object) -> mir::CallableCode {
  const mir::CallableCode& target = cls.callables.Get(callable).code;
  const mir::TypeId result = target.result_type;
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
  for (const mir::LocalId formal : target.ParamsAfterReceiver()) {
    const mir::TypeId type = target.locals.Get(formal).type;
    const mir::LocalId param = code.AddLocal(type);
    code.params.push_back(param);
    args.push_back(body.exprs.Add(mir::MakeLocalRefExpr(param, type)));
  }
  const mir::ExprId call = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target =
                              mir::CallableTarget{
                                  .owner = id, .slot = callable},
                          .receiver = BuildObjectDeref(unit, body, self)},
                  .arguments = std::move(args)},
          .type = result});
  CompleteWith(unit, code, call, result);
  return code;
}

}  // namespace

auto ForwardingMethod(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& cls,
    mir::CallableId callable, mir::TypeId object) -> mir::CallableCode {
  mir::CallableCode code = ForwardingBody(unit, id, cls, callable, object);
  code.receiver = code.params.front();
  return code;
}

auto EntryOf(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    mir::CallableId callable, mir::TypeId object) -> mir::CallableId {
  if (!cls.callables.Get(callable).code.TakesReceiver()) {
    return callable;
  }
  return AddBody(cls, ForwardingBody(unit, id, cls, callable, object));
}

}  // namespace lyra::lowering::hir_to_mir

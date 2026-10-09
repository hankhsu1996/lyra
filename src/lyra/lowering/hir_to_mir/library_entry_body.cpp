#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"

#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/class_definition.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto AddBody(mir::Class& cls, mir::CallableCode code) -> mir::CallableId {
  return cls.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
}

// Ends `body` with `call`, a call resulting in `result`, completing as that
// call does. A call completing as a coroutine is awaited, which completes the
// body as a coroutine too, with what that one completed with.
void CompleteWith(
    const mir::CompilationUnit& unit, mir::Block& body, mir::ExprId call,
    mir::TypeId result) {
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

// Ends `body`, a block of `code`, by calling `callable` of class `id` on the
// object `code` was handed first, viewed as that class, with what it was
// handed after the object.
void EnterBodyOn(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    mir::Block& body, mir::ClassId id, const mir::Class& cls,
    mir::CallableId callable) {
  const mir::LocalId handed = code.params.front();
  const mir::ExprId self = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CastExpr{
                  .operand = body.exprs.Add(
                      mir::MakeLocalRefExpr(
                          handed, code.locals.Get(handed).type))},
          .type = cls.self_pointer_type});
  std::vector<mir::ExprId> args;
  for (const mir::LocalId param : std::span{code.params}.subspan(1)) {
    args.push_back(body.exprs.Add(
        mir::MakeLocalRefExpr(param, code.locals.Get(param).type)));
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
          .type = code.result_type});
  CompleteWith(unit, body, call, code.result_type);
}

// A body taking the object as `object` and then what `target` takes after its
// receiver, resulting in what `target` does, with nothing in it yet.
auto TakingWhatItTakes(const mir::CallableCode& target, mir::TypeId object)
    -> mir::CallableCode {
  mir::CallableCode code = mir::CallableCode::Defined();
  code.params = {code.AddLocal(object)};
  code.result_type = target.result_type;
  for (const mir::LocalId formal : target.ParamsAfterReceiver()) {
    code.params.push_back(code.AddLocal(target.locals.Get(formal).type));
  }
  return code;
}

// The entry calling `callable` of class `id` on the object, and no other body
// in its place: handed the object as `object`, it views it as the class and
// passes on what it takes after the object.
auto ForwardingBody(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& cls,
    mir::CallableId callable, mir::TypeId object) -> mir::CallableCode {
  mir::CallableCode code =
      TakingWhatItTakes(cls.callables.Get(callable).code, object);
  EnterBodyOn(unit, code, code.Body(), id, cls, callable);
  return code;
}

}  // namespace

auto ForwardingMethod(
    const mir::CompilationUnit& unit,
    std::span<const mir::CallableTarget> bodies, mir::TypeId object)
    -> mir::CallableCode {
  if (bodies.empty()) {
    throw InternalError(
        "ForwardingMethod: a class a unit published of a scope is realized by "
        "at least one class");
  }
  const mir::CallableTarget& last = bodies.back();
  mir::CallableCode code = TakingWhatItTakes(
      unit.GetClass(last.owner).callables.Get(last.slot).code, object);
  code.receiver = code.params.front();
  mir::Block& body = code.Body();
  for (const mir::CallableTarget& one : bodies.first(bodies.size() - 1)) {
    const mir::LocalId handed = code.params.front();
    const mir::ExprId is_this_one = body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kIsOfClass,
                            .receiver = BuildObjectDeref(
                                unit, body,
                                body.exprs.Add(
                                    mir::MakeLocalRefExpr(handed, object)))},
                    .arguments = {BuildDefinitionRead(
                        unit, body,
                        mir::IntraUnitClassRef{.class_id = one.owner})}},
            .type = unit.builtins.machine_bool});
    mir::Block entered;
    EnterBodyOn(
        unit, code, entered, one.owner, unit.GetClass(one.owner), one.slot);
    body.AppendStmt(
        mir::IfStmt{
            .condition = is_this_one,
            .then_scope = body.child_scopes.Add(std::move(entered)),
            .else_scope = std::nullopt});
  }
  EnterBodyOn(
      unit, code, body, last.owner, unit.GetClass(last.owner), last.slot);
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

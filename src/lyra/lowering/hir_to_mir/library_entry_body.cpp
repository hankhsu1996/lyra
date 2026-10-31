#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"

#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto AddBody(mir::Class& cls, mir::CallableCode code) -> mir::CallableId {
  return cls.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
}

// The entry calling `callable` of class `id` on the object, and no other body
// in its place, of the prototype `entry`: handed the object first, it views it
// as the class and passes on what it takes after the object.
auto EntryBody(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& cls,
    mir::CallableId callable, mir::TypeId entry) -> mir::CallableCode {
  const auto& prototype = unit.types.Get(entry).Get<mir::MachineFunctionType>();
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  for (const mir::TypeId param : prototype.params) {
    code.params.push_back(code.AddLocal(param));
  }
  code.result_type = prototype.result;
  const mir::LocalId handed = code.params.front();

  mir::Builder body(code.Body(), diag::SourceSpan{});
  const mir::ExprId self = body.Add(
      mir::Expr{
          .data =
              mir::CastExpr{
                  .operand =
                      body.ReadLocal(handed, code.locals.Get(handed).type)},
          .type = cls.self_pointer_type});
  std::vector<mir::ExprId> args;
  for (const mir::LocalId param : std::span{code.params}.subspan(1)) {
    args.push_back(body.ReadLocal(param, code.locals.Get(param).type));
  }
  const mir::ExprId call = body.Add(
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
  return code;
}

}  // namespace

void CompleteWith(
    const mir::CompilationUnit& unit, mir::Builder body, mir::ExprId call,
    mir::TypeId result) {
  const auto* coroutine = unit.types.Get(result).As<mir::CoroutineType>();
  const mir::TypeId completion =
      coroutine != nullptr ? coroutine->payload : result;
  const mir::ExprId completed =
      coroutine != nullptr ? body.Add(
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

auto EntryOf(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    mir::CallableId callable, mir::TypeId entry) -> mir::CallableId {
  if (!cls.callables.Get(callable).code.TakesReceiver()) {
    return callable;
  }
  return AddBody(cls, EntryBody(unit, id, cls, callable, entry));
}

auto EntryPrototypeOf(
    const mir::CompilationUnit& unit, const mir::CallableCode& target,
    mir::TypeId object) -> mir::TypeId {
  if (!target.TakesReceiver()) {
    throw InternalError(
        "hir_to_mir: an entry is asked for of a body that takes no object -- "
        "please report this as a bug");
  }
  std::vector<mir::TypeId> params{object};
  for (const mir::LocalId formal : target.ParamsAfterReceiver()) {
    params.push_back(target.locals.Get(formal).type);
  }
  return unit.types.Intern(
      mir::Type{mir::MachineFunctionType{
          .params = std::move(params), .result = target.result_type}});
}

}  // namespace lyra::lowering::hir_to_mir

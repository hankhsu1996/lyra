#include "lyra/lowering/hir_to_mir/forwarding_entry.hpp"

#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildForwardingEntry(
    mir::CompilationUnit& unit, const mir::Class& cls, mir::ClassId cls_id,
    mir::CallableId target, mir::TypeId receiver_type) -> mir::CallableCode {
  const mir::CallableCode& forwarded = cls.callables.Get(target).code;
  mir::CallableCode code = mir::CallableCode::Defined();
  const mir::LocalId receiver = code.AddLocal(receiver_type);
  code.params.push_back(receiver);
  // The body's own receiver leads its params, and the entry supplies it from
  // the one it was handed rather than forwarding one; what the entry takes
  // beyond that are the formals the source wrote.
  const std::span<const mir::LocalId> formals =
      std::span{forwarded.params}.subspan(
          forwarded.HasReceiver(cls.self_pointer_type) ? 1 : 0);
  std::vector<mir::ExprId> arguments;
  arguments.reserve(formals.size());
  for (const mir::LocalId formal : formals) {
    const mir::LocalDecl& decl = forwarded.locals.Get(formal);
    const mir::LocalId param = code.AddLocal(decl.type);
    code.params.push_back(param);
    arguments.push_back(
        code.Body().exprs.Add(mir::MakeLocalRefExpr(param, decl.type)));
  }
  code.result_type = forwarded.result_type;

  const mir::ExprId handed =
      code.Body().exprs.Add(mir::MakeLocalRefExpr(receiver, receiver_type));
  const mir::ExprId typed = code.Body().exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = handed},
          .type = cls.self_pointer_type});
  const mir::ExprId call = code.Body().exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target =
                              mir::CallableTarget{
                                  .owner = cls_id, .slot = target},
                          .receiver = typed},
                  .arguments = std::move(arguments)},
          .type = forwarded.result_type});
  // A task suspends its caller until it completes (LRM 13.3), so the entry
  // suspends too: it awaits the body and hands back the completion, which is
  // what whoever entered it awaits in turn. Anything else completes where it is
  // called and its result is the entry's.
  const mir::Type& result = unit.types.Get(forwarded.result_type);
  if (const auto* coroutine = result.As<mir::CoroutineType>()) {
    const mir::LocalId completion = code.AddLocal(coroutine->payload);
    code.Body().AppendStmt(
        mir::LocalDeclStmt{
            .target = completion,
            .init = code.Body().exprs.Add(
                mir::Expr{
                    .data = mir::AwaitExpr{.awaitable = call},
                    .type = coroutine->payload})});
    code.Body().AppendStmt(
        mir::ReturnStmt{
            .value = code.Body().exprs.Add(
                mir::MakeLocalRefExpr(completion, coroutine->payload))});
  } else if (result.Is<mir::VoidType>()) {
    code.Body().AppendStmt(mir::ExprStmt{.expr = call});
    code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  } else {
    code.Body().AppendStmt(mir::ReturnStmt{.value = call});
  }
  return code;
}

}  // namespace lyra::lowering::hir_to_mir

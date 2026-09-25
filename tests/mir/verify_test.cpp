#include "lyra/mir/verify.hpp"

#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
namespace {

// A body of the unit's namespace named `f`, returning `result`, whose one
// statement is a nested block waiting on something -- nested, because a
// lowering writes most statements into a child scope rather than the body's
// top level.
void AddAwaitingBody(CompilationUnit& unit, TypeId result) {
  CallableCode code = CallableCode::Defined();
  code.result_type = result;
  Block inner;
  const ExprId registration = inner.exprs.Add(
      Expr{
          .data = MachineBoolLiteral{.value = true},
          .type = unit.builtins.machine_bool});
  const ExprId wait = inner.exprs.Add(
      Expr{
          .data = WaitExpr{.registration = registration},
          .type = unit.builtins.void_type});
  inner.AppendStmt(ExprStmt{.expr = wait});
  const BlockId scope = code.Body().child_scopes.Add(std::move(inner));
  code.Body().AppendStmt(BlockStmt{.scope = scope});
  const CallableId id = unit.callables.Add(
      CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
  unit.named_callables.push_back(NamedCallable{.name = "f", .body = id});
}

// A body that returns before anything else runs has nothing to resume it, so a
// suspension in one is refused and the refusal names the body; the same
// suspension in a body whose call protocol is the coroutine one is what that
// protocol is for.
TEST(MirVerifyTest, ASuspensionIsRefusedOnlyWhereNothingCouldResumeIt) {
  CompilationUnit function_unit;
  function_unit.name = "U";
  AddAwaitingBody(function_unit, function_unit.builtins.int_type);
  try {
    Verify(function_unit);
    FAIL() << "a function body holding an await was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find("'f' of unit 'U'"), std::string::npos) << message;
    EXPECT_NE(message.find("nothing could resume it"), std::string::npos)
        << message;
  }

  CompilationUnit coroutine_unit;
  coroutine_unit.name = "U";
  AddAwaitingBody(coroutine_unit, coroutine_unit.builtins.coroutine_void);
  EXPECT_NO_THROW(Verify(coroutine_unit));
}

// Awaiting an execution and waiting on a registration end differently, so each
// is refused where its operand is what the other waits on.
TEST(MirVerifyTest, ASuspensionWaitsOnWhatItsKindWaitsOn) {
  CompilationUnit unit;
  unit.name = "U";
  CallableCode code = CallableCode::Defined();
  code.result_type = unit.builtins.coroutine_void;
  const ExprId answer = code.Body().exprs.Add(
      Expr{
          .data = MachineBoolLiteral{.value = true},
          .type = unit.builtins.machine_bool});
  const ExprId await = code.Body().exprs.Add(
      Expr{
          .data = AwaitExpr{.execution = answer},
          .type = unit.builtins.void_type});
  code.Body().AppendStmt(ExprStmt{.expr = await});
  unit.callables.Add(
      CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
  try {
    Verify(unit);
    FAIL() << "an await on a registration's answer was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find("not an execution"), std::string::npos) << message;
  }
}

}  // namespace
}  // namespace lyra::mir

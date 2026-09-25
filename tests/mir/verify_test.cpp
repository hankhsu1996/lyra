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
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {
namespace {

// A body of the unit's namespace named `f`, returning `result`, whose one
// statement is a nested block awaiting something -- nested, because a lowering
// writes most statements into a child scope rather than the body's top level.
void AddAwaitingBody(CompilationUnit& unit, TypeId result) {
  CallableCode code = CallableCode::Defined();
  code.result_type = result;
  Block inner;
  const ExprId awaited =
      inner.exprs.Add(MakeStringLiteral(unit.builtins.string, "awaited"));
  const ExprId await = inner.exprs.Add(
      Expr{
          .data = AwaitExpr{.awaitable = awaited},
          .type = unit.builtins.void_type});
  inner.AppendStmt(ExprStmt{.expr = await});
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
    EXPECT_NE(
        std::string(error.what()).find("'f' of unit 'U'"), std::string::npos)
        << error.what();
  }

  CompilationUnit coroutine_unit;
  coroutine_unit.name = "U";
  AddAwaitingBody(coroutine_unit, coroutine_unit.builtins.coroutine_void);
  EXPECT_NO_THROW(Verify(coroutine_unit));
}

// A body of the unit's namespace named `g` whose one statement is a nested
// block holding an extent, and the extent's cleanup calls `in_cleanup`.
void AddExtentBody(CompilationUnit& unit, support::BuiltinFn in_cleanup) {
  CallableCode code = CallableCode::Defined();
  code.result_type = unit.builtins.void_type;
  Block cleanup;
  const ExprId call = cleanup.exprs.Add(
      Expr{
          .data =
              CallExpr{.callee = Direct{.target = in_cleanup}, .arguments = {}},
          .type = unit.builtins.void_type});
  cleanup.AppendStmt(ExprStmt{.expr = call});
  Block inner;
  inner.AppendFinally(Block{}, std::move(cleanup));
  const BlockId scope = code.Body().child_scopes.Add(std::move(inner));
  code.Body().AppendStmt(BlockStmt{.scope = scope});
  const CallableId id = unit.callables.Add(
      CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
  unit.named_callables.push_back(NamedCallable{.name = "g", .body = id});
}

// A cleanup runs on every way out of its body, a departure included, so one
// that could depart itself is refused and the refusal names the body; one that
// only calls an entry declared to return is what a cleanup is.
TEST(MirVerifyTest, ACleanupIsRefusedWhereItCouldDepart) {
  CompilationUnit departing_unit;
  departing_unit.name = "U";
  AddExtentBody(departing_unit, support::BuiltinFn::kDisable);
  try {
    Verify(departing_unit);
    FAIL() << "a cleanup calling an entry that can depart was accepted";
  } catch (const InternalError& error) {
    EXPECT_NE(
        std::string(error.what()).find("'g' of unit 'U'"), std::string::npos)
        << error.what();
  }

  CompilationUnit returning_unit;
  returning_unit.name = "U";
  AddExtentBody(returning_unit, support::BuiltinFn::kLeaveDpiScope);
  EXPECT_NO_THROW(Verify(returning_unit));
}

}  // namespace
}  // namespace lyra::mir

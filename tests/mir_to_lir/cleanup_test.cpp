#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <tuple>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::mir_to_lir {
namespace {

auto AddEmptyBody(mir::CompilationUnit& unit) -> mir::CallableId {
  mir::CallableCode code = mir::CallableCode::Defined();
  code.result_type = unit.builtins.void_type;
  return unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
}

// A namespace unit whose one named body holds an extent, the extent's cleanup
// calling `in_cleanup`.
auto UnitWithCleanupCalling(support::BuiltinFn in_cleanup)
    -> mir::CompilationUnit {
  mir::CompilationUnit unit;
  unit.name = "U";
  unit.content = mir::BroughtUpNamespace{
      .install_storage = AddEmptyBody(unit),
      .initialize_storage = AddEmptyBody(unit)};
  mir::CallableCode code = mir::CallableCode::Defined();
  code.result_type = unit.builtins.void_type;
  mir::Block cleanup;
  const mir::ExprId call = cleanup.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = in_cleanup}, .arguments = {}},
          .type = unit.builtins.void_type});
  cleanup.AppendStmt(mir::ExprStmt{.expr = call});
  code.Body().AppendFinally(mir::Block{}, std::move(cleanup));
  const mir::CallableId id = unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
  unit.named_callables.push_back(mir::NamedCallable{.name = "g", .body = id});
  return unit;
}

// A cleanup runs on every way out of its body, a departure included, so a
// departure starting inside one would have nowhere to go; lowering refuses to
// give it a landing.
TEST(MirToLirCleanupTest, ACleanupThatCouldDepartIsRefused) {
  const mir::CompilationUnit unit =
      UnitWithCleanupCalling(support::BuiltinFn::kDisable);
  try {
    std::ignore = LowerUnit(unit);
    FAIL() << "a cleanup calling an entry that can depart was lowered";
  } catch (const InternalError& error) {
    EXPECT_NE(std::string(error.what()).find("cleanup"), std::string::npos)
        << error.what();
  }
}

}  // namespace
}  // namespace lyra::lowering::mir_to_lir

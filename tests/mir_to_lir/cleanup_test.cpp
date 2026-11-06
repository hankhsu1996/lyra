#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/verify.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/phase.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "tests/mir_to_lir/lowered.hpp"

namespace lyra::lowering::mir_to_lir {
namespace {

auto AddEmptyBody(mir::CompilationUnit& unit) -> mir::CallableId {
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  return unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
}

void AppendCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::BuiltinFn callee) {
  const mir::ExprId call = block.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = callee}, .arguments = {}},
          .type = unit.builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = call});
}

// A namespace unit whose one named body holds an extent: a body `fill_body`
// writes, paired with a cleanup `fill_cleanup` writes.
template <typename FillBody, typename FillCleanup>
auto UnitWithExtent(const FillBody& fill_body, const FillCleanup& fill_cleanup)
    -> mir::CompilationUnit {
  mir::CompilationUnit unit;
  unit.name = "U";
  unit.phase = mir::Phase::kLegalized;
  unit.body_name = "U";
  unit.content = mir::BroughtUpNamespace{
      .install_storage = AddEmptyBody(unit),
      .initialize_storage = AddEmptyBody(unit)};
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  mir::Block body;
  fill_body(unit, mir::Builder(body, diag::SourceSpan{}));
  mir::Block cleanup;
  fill_cleanup(unit, mir::Builder(cleanup, diag::SourceSpan{}));
  mir::Builder code_body(code.Body(), diag::SourceSpan{});
  code_body.AppendFinally(std::move(body), std::move(cleanup));
  const mir::CallableId id = unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
  unit.named_callables.push_back(mir::NamedCallable{.name = "g", .body = id});
  return unit;
}

// A namespace unit whose one named body makes two calls to `callee` in one
// scope.
auto UnitCallingTwice(support::BuiltinFn callee) -> mir::CompilationUnit {
  mir::CompilationUnit unit;
  unit.name = "U";
  unit.phase = mir::Phase::kLegalized;
  unit.body_name = "U";
  unit.content = mir::BroughtUpNamespace{
      .install_storage = AddEmptyBody(unit),
      .initialize_storage = AddEmptyBody(unit)};
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  for (int i = 0; i < 2; ++i) {
    AppendCall(unit, mir::Builder(code.Body(), diag::SourceSpan{}), callee);
  }
  const mir::CallableId id = unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
  unit.named_callables.push_back(mir::NamedCallable{.name = "f", .body = id});
  return unit;
}

// How many blocks of `function` take what is unwinding.
auto LandingsIn(const lir::Function& function) -> int {
  int landings = 0;
  for (const lir::BasicBlock& block : function.blocks) {
    if (block.instrs.empty()) {
      continue;
    }
    const auto* taken = std::get_if<lir::AssignInstr>(&block.instrs[0].data);
    if (taken != nullptr &&
        std::holds_alternative<lir::LandingInstr>(taken->value)) {
      ++landings;
    }
  }
  return landings;
}

// Two calls in one scope that can both depart owe the same on the way out, so
// they land in one landing, as clang's calls of one scope share a landing pad.
TEST(MirToLirCleanupTest, CallsThatOweTheSameShareALanding) {
  const mir::CompilationUnit unit =
      UnitCallingTwice(support::BuiltinFn::kDisable);
  const diag::Result<lir::CompilationUnit> lowered = Lowered(unit);
  ASSERT_TRUE(lowered.has_value());
  EXPECT_NO_THROW(lir::Verify(*lowered));
  int calling = 0;
  for (const lir::Function& function : lowered->functions) {
    const int landings = LandingsIn(function);
    if (landings == 0) {
      continue;
    }
    ++calling;
    EXPECT_EQ(landings, 1) << function.name;
  }
  EXPECT_EQ(calling, 1);
}

// The lowering of `unit` refuses it, saying a cleanup is what holds the way out
// it could not lower.
void ExpectRefused(const mir::CompilationUnit& unit, std::string_view what) {
  try {
    std::ignore = Lowered(unit);
    ADD_FAILURE() << "lowered a cleanup that " << what;
  } catch (const InternalError& error) {
    EXPECT_NE(std::string(error.what()).find("cleanup"), std::string::npos)
        << error.what();
  }
}

// A cleanup runs on every way out of its body, a departure included, so a way
// out starting inside one would have nowhere to go. The unit's verification
// refuses such a cleanup, and a build that skips it still has the lowering
// refuse it, however the body reaches the cleanup.
TEST(MirToLirCleanupTest, ACleanupThatCouldLeaveIsRefused) {
  const auto nothing = [](const mir::CompilationUnit&, mir::Builder) {};
  const auto departs = [](const mir::CompilationUnit& unit,
                          mir::Builder block) {
    AppendCall(unit, block, support::BuiltinFn::kDisable);
  };
  ExpectRefused(
      UnitWithExtent(nothing, departs),
      "can depart, reached by falling off its body's end");
  ExpectRefused(
      UnitWithExtent(departs, departs),
      "can depart, reached by a departure from its body");
  ExpectRefused(
      UnitWithExtent(
          departs,
          [](const mir::CompilationUnit&, mir::Builder block) {
            block.AppendStmt(mir::ReturnStmt{});
          }),
      "returns, reached by a departure from its body");
}

}  // namespace
}  // namespace lyra::lowering::mir_to_lir

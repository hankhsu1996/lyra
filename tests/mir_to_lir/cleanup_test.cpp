#include <cstdint>
#include <gtest/gtest.h>
#include <set>
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
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_decl.hpp"
#include "tests/mir_to_lir/lowered.hpp"

namespace lyra::lowering::mir_to_lir {
namespace {

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

// A namespace unit whose one named body holds `extents` extents one after
// another, each a body `fill_body` writes, paired with a cleanup `fill_cleanup`
// writes.
template <typename FillBody, typename FillCleanup>
auto UnitWithExtents(
    int extents, const FillBody& fill_body, const FillCleanup& fill_cleanup)
    -> mir::CompilationUnit {
  mir::CompilationUnit unit = NamespaceUnit();
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  for (int i = 0; i < extents; ++i) {
    mir::Block body;
    fill_body(unit, mir::Builder(body, diag::SourceSpan{}));
    mir::Block cleanup;
    fill_cleanup(unit, mir::Builder(cleanup, diag::SourceSpan{}));
    mir::Builder code_body(code.Body(), diag::SourceSpan{});
    code_body.AppendFinally(std::move(body), std::move(cleanup));
  }
  AddNamedBody(unit, "g", std::move(code));
  return unit;
}

// A namespace unit whose one named body makes two calls to `callee` in one
// scope.
auto UnitCallingTwice(support::BuiltinFn callee) -> mir::CompilationUnit {
  mir::CompilationUnit unit = NamespaceUnit();
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  for (int i = 0; i < 2; ++i) {
    AppendCall(unit, mir::Builder(code.Body(), diag::SourceSpan{}), callee);
  }
  AddNamedBody(unit, "f", std::move(code));
  return unit;
}

// How many calls of `function` name a block of it that unwinding from them
// goes to.
auto InvokesIn(const lir::Function& function) -> int {
  int invokes = 0;
  for (const lir::BasicBlock& block : function.blocks) {
    if (std::holds_alternative<lir::InvokeTerm>(block.terminator.data)) {
      ++invokes;
    }
  }
  return invokes;
}

// How many blocks of `function` go on unwinding out of it.
auto UnwindResumesIn(const lir::Function& function) -> int {
  int resumes = 0;
  for (const lir::BasicBlock& block : function.blocks) {
    if (std::holds_alternative<lir::UnwindResumeTerm>(block.terminator.data)) {
      ++resumes;
    }
  }
  return resumes;
}

// A namespace unit whose body `h` can depart and answers an `int`, and whose
// body `f` keeps one answer of `h` in a local while it calls `h` again.
auto UnitHoldingAnIntAcrossACall() -> mir::CompilationUnit {
  mir::CompilationUnit unit = NamespaceUnit();
  const mir::TypeId int_type = unit.builtins.int_type;

  mir::CallableCode departing = mir::CallableCode::Defined(diag::SourceSpan{});
  departing.result_type = int_type;
  mir::Builder departing_body(departing.Body(), diag::SourceSpan{});
  AppendCall(unit, departing_body, support::BuiltinFn::kDisable);
  departing_body.AppendStmt(
      mir::ReturnStmt{.value = mir::BuildIntLiteral(unit, departing_body, 0)});
  const mir::CallableId h = unit.AddNamespaceCallable(std::move(departing));
  unit.named_callables.push_back(mir::NamedCallable{.name = "h", .body = h});

  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  const mir::LocalId held = code.locals.Add(mir::LocalDecl{.type = int_type});
  mir::Builder body(code.Body(), diag::SourceSpan{});
  const auto call_h = [&]() {
    return body.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = mir::CallableTarget{h}},
                    .arguments = {}},
            .type = int_type});
  };
  body.AppendStmt(mir::LocalDeclStmt{.target = held, .init = call_h()});
  body.AppendStmt(mir::ExprStmt{.expr = call_h()});
  AddNamedBody(unit, "f", std::move(code));
  return unit;
}

// A namespace unit whose one named body holds `regions` regions one after
// another, each taking what unwinds out of a call that can depart and handing
// on whatever it takes.
auto UnitWithRegions(int regions) -> mir::CompilationUnit {
  mir::CompilationUnit unit = NamespaceUnit();
  const mir::TypeId effect =
      mir::LibraryTypeOf(unit.types, support::LibraryDecl::kControlEffect);
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  for (int i = 0; i < regions; ++i) {
    const mir::LocalId caught = code.locals.Add(mir::LocalDecl{.type = effect});
    mir::Block body;
    AppendCall(
        unit, mir::Builder(body, diag::SourceSpan{}),
        support::BuiltinFn::kDisable);
    mir::Block handler;
    mir::Builder handler_body(handler, diag::SourceSpan{});
    handler_body.AppendStmt(
        mir::RaiseStmt{.effect = handler_body.ReadLocal(caught, effect)});
    mir::Builder code_body(code.Body(), diag::SourceSpan{});
    code_body.AppendStmt(
        mir::TryStmt{
            .body = code_body.AddScope(std::move(body)),
            .caught = caught,
            .handler = code_body.AddScope(std::move(handler))});
  }
  AddNamedBody(unit, "g", std::move(code));
  return unit;
}

// Two calls that can depart, in one scope.
void AppendTwoDepartingCalls(
    const mir::CompilationUnit& unit, mir::Builder block) {
  AppendCall(unit, block, support::BuiltinFn::kDisable);
  AppendCall(unit, block, support::BuiltinFn::kDisable);
}

// A sum computed and dropped: code that does something and cannot be unwound
// out of.
void AppendSum(const mir::CompilationUnit& unit, mir::Builder block) {
  const mir::ExprId sum = block.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kAdd,
                  .lhs = mir::BuildIntLiteral(unit, block, 1),
                  .rhs = mir::BuildIntLiteral(unit, block, 2)},
          .type = unit.builtins.int_type});
  block.AppendStmt(mir::ExprStmt{.expr = sum});
}

// A cleanup whose code is empty.
constexpr auto kAppendNothing = [](const mir::CompilationUnit&, mir::Builder) {
};

// A function that owes nothing on any way out lets unwinding leave it from
// every call, as rustc's `UnwindAction::Continue`: no call names a block of it
// to unwind to, since nothing there would run. A value whose end does nothing
// -- an `int` held in a local -- is owed no end at all, and leaving the body
// ends every byte of its frame, so the local owes nothing on that way out; a
// cleanup whose code is empty runs nothing either.
TEST(MirToLirCleanupTest, ACallOwingNothingNamesNoBlockToUnwindTo) {
  for (const mir::CompilationUnit& unit :
       {UnitCallingTwice(support::BuiltinFn::kDisable),
        UnitHoldingAnIntAcrossACall(),
        UnitWithExtents(1, AppendTwoDepartingCalls, kAppendNothing)}) {
    const diag::Result<lir::CompilationUnit> lowered = Lowered(unit);
    ASSERT_TRUE(lowered.has_value());
    EXPECT_NO_THROW(lir::Verify(*lowered));
    for (const lir::Function& function : lowered->functions) {
      EXPECT_EQ(InvokesIn(function), 0) << function.name;
      EXPECT_EQ(UnwindResumesIn(function), 0) << function.name;
    }
  }
}

// Two calls in one scope that can both depart owe the same on the way out, so
// they unwind into one cleanup block, as rustc's drop tree and clang's calls of
// one scope share one.
TEST(MirToLirCleanupTest, CallsThatOweTheSameShareACleanupBlock) {
  const diag::Result<lir::CompilationUnit> lowered =
      Lowered(UnitWithExtents(1, AppendTwoDepartingCalls, AppendSum));
  ASSERT_TRUE(lowered.has_value());
  EXPECT_NO_THROW(lir::Verify(*lowered));
  int calling = 0;
  for (const lir::Function& function : lowered->functions) {
    std::set<std::uint32_t> unwinds;
    for (const lir::BasicBlock& block : function.blocks) {
      if (const auto* call =
              std::get_if<lir::InvokeTerm>(&block.terminator.data)) {
        unwinds.insert(call->unwind.value);
        EXPECT_TRUE(function.blocks[call->unwind.value].is_cleanup)
            << function.name;
      }
    }
    if (unwinds.empty()) {
      continue;
    }
    ++calling;
    EXPECT_EQ(unwinds.size(), 1U) << function.name;
  }
  EXPECT_EQ(calling, 1);
}

// Every way unwinding goes on out of a body ends at one block -- the end of
// each chain of what is owed, and what each region with nothing owed outside it
// does not take -- as rustc's unwind drop tree ends at one resume block and
// clang's cleanups and unmatched catches at one `eh.resume`: what a target adds
// at each way out of a function is then paid once a function.
TEST(MirToLirCleanupTest, EveryWayOutByUnwindingResumesAtOneBlock) {
  for (const int count : {1, 3}) {
    for (const mir::CompilationUnit& unit :
         {UnitWithExtents(count, AppendTwoDepartingCalls, AppendSum),
          UnitWithRegions(count)}) {
      const diag::Result<lir::CompilationUnit> lowered = Lowered(unit);
      ASSERT_TRUE(lowered.has_value());
      EXPECT_NO_THROW(lir::Verify(*lowered));
      int unwinding = 0;
      for (const lir::Function& function : lowered->functions) {
        if (InvokesIn(function) == 0) {
          continue;
        }
        ++unwinding;
        EXPECT_EQ(UnwindResumesIn(function), 1) << function.name;
      }
      EXPECT_EQ(unwinding, 1);
    }
  }
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
  const auto departs = [](const mir::CompilationUnit& unit,
                          mir::Builder block) {
    AppendCall(unit, block, support::BuiltinFn::kDisable);
  };
  ExpectRefused(
      UnitWithExtents(1, kAppendNothing, departs),
      "can depart, reached by falling off its body's end");
  ExpectRefused(
      UnitWithExtents(1, departs, departs),
      "can depart, reached by a departure from its body");
  ExpectRefused(
      UnitWithExtents(
          1, departs,
          [](const mir::CompilationUnit&, mir::Builder block) {
            block.AppendStmt(mir::ReturnStmt{});
          }),
      "returns, reached by a departure from its body");
}

}  // namespace
}  // namespace lyra::lowering::mir_to_lir

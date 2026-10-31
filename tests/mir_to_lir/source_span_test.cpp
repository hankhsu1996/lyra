#include "lyra/diag/source_span.hpp"

#include <algorithm>
#include <cstddef>
#include <gtest/gtest.h>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/phase.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::mir_to_lir {
namespace {

constexpr diag::SourceSpan kFirst{.start = 1, .end = 2};
constexpr diag::SourceSpan kSecond{.start = 3, .end = 9};
constexpr diag::SourceSpan kInsideSecond{.start = 5, .end = 6};

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

// A namespace unit whose one named body declares two shared pointers, each a
// statement lowered from a piece of source of its own; the object the second
// starts out pointing at was made by a narrower piece inside it.
auto UnitOfTwoStatements() -> mir::CompilationUnit {
  mir::CompilationUnit unit;
  unit.name = "U";
  unit.phase = mir::Phase::kLegalized;
  unit.body_name = "U";
  unit.content = mir::BroughtUpNamespace{
      .install_storage = AddEmptyBody(unit),
      .initialize_storage = AddEmptyBody(unit)};
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  const mir::TypeId shared = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit.builtins.int_type,
          .ownership = mir::PointerOwnership::kShared}});
  const auto declare = [&](mir::Builder body, diag::SourceSpan made_at) {
    const mir::LocalId local = code.locals.Add(mir::LocalDecl{.type = shared});
    const mir::ExprId zero = body.Add(
        mir::Expr{
            .data = mir::MachineIntLiteral{.value = 0},
            .type = unit.builtins.int_type});
    body.AppendStmt(
        mir::LocalDeclStmt{
            .target = local,
            .init = body.Add(
                mir::Expr{
                    .data = mir::NewExpr{.initializer = zero},
                    .type = shared,
                    .span = made_at})});
  };
  mir::Builder first(code.Body(), kFirst);
  declare(first, diag::SourceSpan{});
  declare(mir::Builder(code.Body(), kSecond), kInsideSecond);
  const mir::CallableId id = unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
  unit.named_callables.push_back(mir::NamedCallable{.name = "f", .body = id});
  return unit;
}

// The pieces of source the instructions of the body named `f` were lowered
// from, in the order they run, leaving out what was lowered from none.
auto SpansOfNamedBody(const lir::CompilationUnit& unit)
    -> std::vector<diag::SourceSpan> {
  std::vector<diag::SourceSpan> spans;
  for (const lir::Function& function : unit.functions) {
    if (!function.name.ends_with(":f")) {
      continue;
    }
    for (const lir::BasicBlock& block : function.blocks) {
      for (const lir::Instr& instr : block.instrs) {
        if (instr.span != diag::SourceSpan{}) {
          spans.push_back(instr.span);
        }
      }
    }
  }
  return spans;
}

// Every instruction a statement lowers to is lowered from the piece of source
// the statement was, unless it is the lowering of an expression inside it that
// names a narrower one; nothing of one statement is attributed to the other.
TEST(MirToLirSourceSpanTest, AnInstructionNamesTheSourceOfWhatItLowers) {
  const diag::Result<lir::CompilationUnit> lowered =
      LowerUnit(UnitOfTwoStatements());
  ASSERT_TRUE(lowered.has_value());
  const std::vector<diag::SourceSpan> spans = SpansOfNamedBody(*lowered);
  ASSERT_FALSE(spans.empty());
  EXPECT_EQ(spans.front(), kFirst);
  std::size_t first_of_second = 0;
  while (first_of_second < spans.size() && spans[first_of_second] == kFirst) {
    ++first_of_second;
  }
  ASSERT_LT(first_of_second, spans.size());
  for (std::size_t i = first_of_second; i < spans.size(); ++i) {
    EXPECT_TRUE(spans[i] == kSecond || spans[i] == kInsideSecond)
        << "instruction " << i << " names " << spans[i].start;
  }
  EXPECT_NE(std::ranges::find(spans, kInsideSecond), spans.end());
}

}  // namespace
}  // namespace lyra::lowering::mir_to_lir

#include "lyra/hir/verify.hpp"

#include <cstdint>
#include <format>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::hir {
namespace {

auto AddLeaf(CompilationUnit& unit) -> ExprId {
  return unit.root_scope.exprs.Add(
      Expr{
          .type = unit.builtins.int_type,
          .data = PrimaryExpr{.data = NullLiteral{}},
          .span = {}});
}

auto AddSum(CompilationUnit& unit, ExprId lhs, ExprId rhs) -> ExprId {
  return unit.root_scope.exprs.Add(
      Expr{
          .type = unit.builtins.int_type,
          .data = BinaryExpr{.op = BinaryOp::kAdd, .lhs = lhs, .rhs = rhs},
          .span = {}});
}

// A continuous assignment is the one holder a scope needs for its expressions
// to be reached at all. `watched` is the select each entry of its sensitivity
// names.
void Assign(
    CompilationUnit& unit, ExprId lhs, ExprId rhs,
    const std::vector<ExprId>& watched) {
  std::vector<SensitivityEntry> sensitivity;
  for (const ExprId prefix : watched) {
    sensitivity.push_back(
        SensitivityEntry{
            .cell = ValueTarget{RoutedValueRef{}},
            .part = WatchedSelect{.prefix = prefix}});
  }
  unit.root_scope.continuous_assigns.Add(
      ContinuousAssign{
          .span = {},
          .lhs = lhs,
          .rhs = rhs,
          .strength = support::StrengthLevel::kStrong,
          .delay = std::nullopt,
          .sensitivity_list = std::move(sensitivity)});
}

void Assign(CompilationUnit& unit, ExprId lhs, ExprId rhs) {
  Assign(unit, lhs, rhs, {});
}

auto AddLeaf(const CompilationUnit& unit, ProceduralBody& body) -> ExprId {
  return body.exprs.Add(
      Expr{
          .type = unit.builtins.int_type,
          .data = PrimaryExpr{.data = NullLiteral{}},
          .span = {}});
}

// A unit whose one process runs a block of one expression statement per entry
// of what `fill` answers with, each holding that entry.
template <typename Fill>
auto UnitWithProcess(const Fill& fill) -> CompilationUnit {
  CompilationUnit unit("U", "U");
  Process process;
  ProceduralBody& body = process.body;
  std::vector<StmtId> statements;
  for (const ExprId expr : fill(unit, body)) {
    statements.push_back(body.stmts.Add(
        Stmt{
            .label = std::nullopt,
            .data = ExprStmt{.expr = expr},
            .span = {}}));
  }
  process.root_stmt = body.stmts.Add(
      Stmt{
          .label = std::nullopt,
          .data = BlockStmt{.statements = std::move(statements), .scope = {}},
          .span = {}});
  unit.root_scope.processes.Add(std::move(process));
  return unit;
}

// One piece of source stands at one position, so an expression reached as both
// operands of one operation is refused, and the refusal names the expression,
// where it is, and what reaches it.
TEST(HirVerifyTest, AnExpressionReachedFromTwoPlacesIsRefused) {
  CompilationUnit unit("U", "U");
  const ExprId target = AddLeaf(unit);
  const ExprId shared = AddLeaf(unit);
  Assign(unit, target, AddSum(unit, shared, shared));
  try {
    Verify(unit);
    FAIL() << "an expression reached as both operands was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(
        message.find(
            std::format("a null literal (expression {})", shared.value)),
        std::string::npos)
        << message;
    EXPECT_NE(message.find("the root scope of unit 'U'"), std::string::npos)
        << message;
    EXPECT_NE(
        message.find(
            "reached from 2 places: a binary operation, a binary operation"),
        std::string::npos)
        << message;
  }
}

TEST(HirVerifyTest, DistinctOperandsAreAccepted) {
  CompilationUnit unit("U", "U");
  const ExprId target = AddLeaf(unit);
  const ExprId lhs = AddLeaf(unit);
  const ExprId rhs = AddLeaf(unit);
  Assign(unit, target, AddSum(unit, lhs, rhs));
  EXPECT_NO_THROW(Verify(unit));
}

// An expression nothing holds is evaluated nowhere, so its operands are not
// reached through it: one of them standing under a held expression as well is
// reached once.
TEST(HirVerifyTest, AnExpressionNothingHoldsReachesNothing) {
  CompilationUnit unit("U", "U");
  const ExprId target = AddLeaf(unit);
  const ExprId lhs = AddLeaf(unit);
  const ExprId rhs = AddLeaf(unit);
  AddSum(unit, lhs, rhs);
  Assign(unit, target, AddSum(unit, lhs, rhs));
  EXPECT_NO_THROW(Verify(unit));
}

// A sensitivity describes what its holder waits on in terms of expressions the
// holder already reaches, so naming one there is no second place it stands at.
TEST(HirVerifyTest, ADescriptionIsNoPlace) {
  CompilationUnit unit("U", "U");
  const ExprId target = AddLeaf(unit);
  const ExprId lhs = AddLeaf(unit);
  const ExprId rhs = AddLeaf(unit);
  Assign(unit, target, AddSum(unit, lhs, rhs), {lhs, lhs});
  EXPECT_NO_THROW(Verify(unit));
}

// A description is still held to naming an expression its arena has, and the
// refusal says it was a description that named it.
TEST(HirVerifyTest, ADescriptionOutsideTheArenaIsRefused) {
  CompilationUnit unit("U", "U");
  const ExprId target = AddLeaf(unit);
  const ExprId source = AddLeaf(unit);
  Assign(unit, target, source, {ExprId{source.value + 7}});
  try {
    Verify(unit);
    FAIL() << "a sensitivity naming an expression outside its arena was "
              "accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(
        message.find("the sensitivity of a continuous assignment"),
        std::string::npos)
        << message;
    EXPECT_NE(
        message.find("which the arena it indexes does not hold"),
        std::string::npos)
        << message;
  }
}

// A body has an arena of its own, reached from the statements execution enters
// it at, and a refusal names the body and the statements that share.
TEST(HirVerifyTest, ABodyIsHeldToATreeThroughItsStatements) {
  EXPECT_NO_THROW(
      Verify(UnitWithProcess([](const CompilationUnit& u, ProceduralBody& b) {
        return std::vector<ExprId>{AddLeaf(u, b), AddLeaf(u, b)};
      })));

  const CompilationUnit sharing =
      UnitWithProcess([](const CompilationUnit& u, ProceduralBody& b) {
        const ExprId shared = AddLeaf(u, b);
        return std::vector<ExprId>{shared, shared};
      });
  try {
    Verify(sharing);
    FAIL() << "an expression two statements hold was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(
        message.find("process 0 of the root scope of unit 'U'"),
        std::string::npos)
        << message;
    EXPECT_NE(
        message.find(
            "reached from 2 places: an expression statement, an expression "
            "statement"),
        std::string::npos)
        << message;
  }
}

// A variable's declaration holds its declaration assignment, so one a
// statement holds as well stands at two places.
TEST(HirVerifyTest, ADeclarationAssignmentIsHeldByItsVariable) {
  const auto declaring = [](bool shared_with_a_statement) {
    return UnitWithProcess([=](const CompilationUnit& u, ProceduralBody& b) {
      const ExprId assigned = AddLeaf(u, b);
      b.procedural_vars.Add(
          ProceduralVarDecl{
              .name = std::nullopt,
              .type = u.builtins.int_type,
              .init = assigned});
      return std::vector<ExprId>{
          shared_with_a_statement ? assigned : AddLeaf(u, b)};
    });
  };
  EXPECT_NO_THROW(Verify(declaring(false)));
  try {
    Verify(declaring(true));
    FAIL() << "a declaration assignment a statement also holds was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find("a variable's declaration"), std::string::npos)
        << message;
  }
}

// A pattern holds the constant it matches against (LRM 12.6), so one that is
// an operand of the expression the pattern stands in as well stands at two
// places.
TEST(HirVerifyTest, AConstantIsHeldByItsPattern) {
  const auto matching = [](bool shared_with_an_arm) {
    return UnitWithProcess([=](const CompilationUnit& u, ProceduralBody& b) {
      const ExprId constant = AddLeaf(u, b);
      const PatternId pattern = b.patterns.Add(
          Pattern{
              .data = ConstantPattern{.value = constant},
              .subject_type = u.builtins.int_type,
              .span = {}});
      const ExprId subject = AddLeaf(u, b);
      const ExprId then_value = shared_with_an_arm ? constant : AddLeaf(u, b);
      const ExprId else_value = AddLeaf(u, b);
      return std::vector<ExprId>{b.exprs.Add(
          Expr{
              .type = u.builtins.int_type,
              .data =
                  ConditionalExpr{
                      .conditions = {ConditionClause{
                          .expr = subject, .pattern = pattern}},
                      .then_value = then_value,
                      .else_value = else_value},
              .span = {}})};
    });
  };
  EXPECT_NO_THROW(Verify(matching(false)));
  try {
    Verify(matching(true));
    FAIL() << "a pattern's constant standing as an arm as well was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find("a constant pattern"), std::string::npos) << message;
  }
}

// A unit whose root holds one loop generate of `blocks` bodies, each standing
// where `bodies` says.
auto UnitWithLoop(std::uint32_t blocks, std::vector<LoopBody> bodies)
    -> CompilationUnit {
  CompilationUnit unit("U", "U");
  Generate generate;
  for (std::uint32_t i = 0; i < blocks; ++i) {
    generate.blocks.Add(GenerateBlock{});
  }
  generate.counting = BlocksRepeat{
      .variable = {},
      .initial = AddLeaf(unit),
      .condition = AddLeaf(unit),
      .step = AddLeaf(unit),
      .arguments = {},
      .bodies = std::move(bodies)};
  unit.root_scope.generates.Add(std::move(generate));
  return unit;
}

auto BodyAt(std::uint32_t body, std::vector<std::int64_t> indices) -> LoopBody {
  return LoopBody{
      .body = StructuralScopeId{body},
      .body_arguments = std::nullopt,
      .indices = std::move(indices)};
}

// A loop of several bodies says where each one stands, every body once and no
// value under two of them, while a loop of one body builds it at every index
// and says nothing of where (LRM 27.4); each departure is refused by name.
TEST(HirVerifyTest, ALoopSaysWhereEachOfSeveralBodiesStands) {
  EXPECT_NO_THROW(Verify(UnitWithLoop(1, {BodyAt(0, {})})));
  EXPECT_NO_THROW(Verify(UnitWithLoop(0, {})));
  EXPECT_NO_THROW(Verify(UnitWithLoop(2, {BodyAt(0, {0, 2}), BodyAt(1, {1})})));

  const auto refused = [](const CompilationUnit& unit, std::string_view says) {
    try {
      Verify(unit);
      ADD_FAILURE() << "a loop that " << says << " was accepted";
    } catch (const InternalError& error) {
      const std::string message = error.what();
      EXPECT_NE(message.find(says), std::string::npos) << message;
    }
  };
  refused(
      UnitWithLoop(2, {BodyAt(0, {2, 0}), BodyAt(1, {1})}),
      "lists where body 0 stands out of order");
  refused(
      UnitWithLoop(2, {BodyAt(0, {0}), BodyAt(0, {1})}),
      "holds 2 bodies and says where 2 of them stand");
  refused(
      UnitWithLoop(2, {BodyAt(0, {0, 1}), BodyAt(1, {1})}),
      "stands two bodies at one value of its index");
  refused(
      UnitWithLoop(2, {BodyAt(0, {0}), BodyAt(1, {})}),
      "holds several bodies and says nowhere that body 1 stands");
  refused(
      UnitWithLoop(1, {BodyAt(0, {0, 1})}),
      "holds one body and lists where it stands");
}

// An id is a position in the arena of the scope that minted it, so a holder
// naming one the arena does not have is refused.
TEST(HirVerifyTest, AnExpressionOutsideTheArenaIsRefused) {
  CompilationUnit unit("U", "U");
  const ExprId target = AddLeaf(unit);
  Assign(unit, target, ExprId{target.value + 7});
  try {
    Verify(unit);
    FAIL() << "a holder naming an expression outside its arena was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(
        message.find("which the arena it indexes does not hold"),
        std::string::npos)
        << message;
  }
}

}  // namespace
}  // namespace lyra::hir

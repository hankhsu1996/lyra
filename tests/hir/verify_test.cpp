#include "lyra/hir/verify.hpp"

#include <cstdint>
#include <format>
#include <functional>
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
#include "lyra/hir/scope_construction.hpp"
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
    Verify(unit, {});
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
  EXPECT_NO_THROW(Verify(unit, {}));
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
  EXPECT_NO_THROW(Verify(unit, {}));
}

// A sensitivity describes what its holder waits on in terms of expressions the
// holder already reaches, so naming one there is no second place it stands at.
TEST(HirVerifyTest, ADescriptionIsNoPlace) {
  CompilationUnit unit("U", "U");
  const ExprId target = AddLeaf(unit);
  const ExprId lhs = AddLeaf(unit);
  const ExprId rhs = AddLeaf(unit);
  Assign(unit, target, AddSum(unit, lhs, rhs), {lhs, lhs});
  EXPECT_NO_THROW(Verify(unit, {}));
}

// A description is still held to naming an expression its arena has, and the
// refusal says it was a description that named it.
TEST(HirVerifyTest, ADescriptionOutsideTheArenaIsRefused) {
  CompilationUnit unit("U", "U");
  const ExprId target = AddLeaf(unit);
  const ExprId source = AddLeaf(unit);
  Assign(unit, target, source, {ExprId{source.value + 7}});
  try {
    Verify(unit, {});
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
  EXPECT_NO_THROW(Verify(
      UnitWithProcess([](const CompilationUnit& u, ProceduralBody& b) {
        return std::vector<ExprId>{AddLeaf(u, b), AddLeaf(u, b)};
      }),
      {}));

  const CompilationUnit sharing =
      UnitWithProcess([](const CompilationUnit& u, ProceduralBody& b) {
        const ExprId shared = AddLeaf(u, b);
        return std::vector<ExprId>{shared, shared};
      });
  try {
    Verify(sharing, {});
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
  EXPECT_NO_THROW(Verify(declaring(false), {}));
  try {
    Verify(declaring(true), {});
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
  EXPECT_NO_THROW(Verify(matching(false), {}));
  try {
    Verify(matching(true), {});
    FAIL() << "a pattern's constant standing as an arm as well was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find("a constant pattern"), std::string::npos) << message;
  }
}

// A unit holding one block body, whose root holds one generate counted as
// `counting` makes it, and a construction building at each place the body
// `places` names there, or none. The body is handed an index where
// `handed_an_index` says, and by default where a loop builds it (LRM 27.4).
struct Built {
  CompilationUnit unit;
  ScopeConstruction construction;
};

auto UnitBuilding(
    const std::function<Generate(CompilationUnit&)>& counting,
    std::vector<std::optional<std::uint32_t>> places,
    std::optional<bool> handed_an_index = std::nullopt) -> Built {
  Built built{.unit = CompilationUnit("U", "U"), .construction = {}};
  Generate generate = counting(built.unit);
  BlockBody body;
  if (handed_an_index.value_or(
          std::holds_alternative<BlocksRepeat>(generate.counting))) {
    body.scope.structural_data_objects.Add(
        StructuralDataObjectDecl{
            .name = "i",
            .type = built.unit.builtins.integer,
            .kind = StructuralConstructionValueDecl{}});
  }
  built.unit.block_bodies.Add(std::move(body));
  built.unit.root_scope.generates.Add(std::move(generate));
  std::vector<BlockConstruction>& at =
      built.construction.generates.emplace_back();
  for (const std::optional<std::uint32_t> place : places) {
    at.push_back(
        BlockConstruction{
            .body = place.has_value()
                        ? std::optional<BlockBodyId>{BlockBodyId{*place}}
                        : std::nullopt,
            .built = {}});
  }
  return built;
}

auto Single(CompilationUnit&) -> Generate {
  return Generate{.counting = SingleBlock{}};
}

auto Loop(CompilationUnit& unit) -> Generate {
  return Generate{
      .counting = BlocksRepeat{
          .variable = {},
          .initial = AddLeaf(unit),
          .condition = AddLeaf(unit),
          .step = AddLeaf(unit),
          .index = AddLeaf(unit)}};
}

// An `if` with an `else`: two alternatives.
auto Choice(CompilationUnit& unit) -> Generate {
  BlocksChoose chosen;
  chosen.root = chosen.choices.Add(
      ChoiceOnCondition{
          .condition = AddLeaf(unit),
          .holds = AlternativeStands{.position = 0},
          .fails = AlternativeStands{.position = 1}});
  chosen.alternatives = 2;
  return Generate{.counting = std::move(chosen)};
}

// What an object builds at a generate is what the generate counts out: a block
// standing alone at its one place, a loop a block at every place it counted
// (LRM 27.4), a conditional at most one of the alternatives the source wrote,
// at that alternative's place (LRM 27.5), and only bodies the unit holds, each
// handed its loop's index and nothing else. Each departure is refused by name.
TEST(HirVerifyTest, AConstructionBuildsWhatEachGenerateCountsOut) {
  const auto verify = [](const Built& built) {
    Verify(built.unit, built.construction);
  };
  EXPECT_NO_THROW(verify(UnitBuilding(Single, {0})));
  EXPECT_NO_THROW(verify(UnitBuilding(Loop, {0, 0, 0})));
  EXPECT_NO_THROW(verify(UnitBuilding(Loop, {})));
  EXPECT_NO_THROW(verify(UnitBuilding(Choice, {std::nullopt, 0})));
  EXPECT_NO_THROW(verify(UnitBuilding(Choice, {std::nullopt, std::nullopt})));

  const auto refused = [&](const Built& built, std::string_view says) {
    try {
      verify(built);
      ADD_FAILURE() << "a construction that " << says << " was accepted";
    } catch (const InternalError& error) {
      const std::string message = error.what();
      EXPECT_NE(message.find(says), std::string::npos) << message;
    }
  };
  refused(UnitBuilding(Single, {}), "builds other than its one block");
  refused(
      UnitBuilding(Single, {std::nullopt}), "builds other than its one block");
  refused(
      UnitBuilding(Loop, {0, std::nullopt}), "counts out a block of no body");
  refused(
      UnitBuilding(Choice, {0}),
      "builds other than one of the alternatives written");
  refused(
      UnitBuilding(Choice, {0, 0}),
      "builds other than one of the alternatives written");
  refused(UnitBuilding(Single, {1}), "builds a body the unit does not hold");

  refused(
      UnitBuilding(Single, {0}, true),
      "builds a body handed other than its loop's index");
  refused(
      UnitBuilding(Loop, {0}, false),
      "builds a body handed other than its loop's index");
}

// A route names a scope's own identity for a declaration only where it climbs
// nothing and steps nowhere; one that climbs to an enclosing scope names what
// that scope published instead, so a route naming the declaration by its own
// identity from there is refused.
TEST(HirVerifyTest, ARouteNamesAnotherScopesDeclarationByItsPublication) {
  const auto with_route = [](std::uint32_t hops) {
    CompilationUnit unit("U", "U");
    unit.root_scope.routes.values.Add(
        ValueRoute{
            .base = InUnitBase{.hops = StructuralHops{hops}},
            .steps = {},
            .leaf = StructuralDataObjectLeaf{
                .object = StructuralDataObjectId{0},
                .storage = {},
                .type = unit.builtins.int_type}});
    return unit;
  };
  EXPECT_NO_THROW(Verify(with_route(0), {}));
  try {
    Verify(with_route(1), {});
    FAIL() << "a route naming an enclosing scope's declaration by its own "
              "identity was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(
        message.find("names a declaration by its own scope's identity"),
        std::string::npos)
        << message;
  }
}

// An id is a position in the arena of the scope that minted it, so a holder
// naming one the arena does not have is refused.
TEST(HirVerifyTest, AnExpressionOutsideTheArenaIsRefused) {
  CompilationUnit unit("U", "U");
  const ExprId target = AddLeaf(unit);
  Assign(unit, target, ExprId{target.value + 7});
  try {
    Verify(unit, {});
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

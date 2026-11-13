#include "lyra/hir/verify.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/callee_interface.hpp"
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/coverage_library.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/declared_method.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/field_id.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/namespace.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/hir/randomization_hook.hpp"
#include "lyra/hir/scope_construction.hpp"
#include "lyra/hir/static_property_id.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine_prototype.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/def_path.hpp"
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
            .part = WatchedSelect{
                .prefix = prefix, .place = base::ComponentIndex{0}}});
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

auto Building(
    Built built, Generate generate,
    std::vector<std::optional<std::uint32_t>> places,
    std::optional<bool> handed_an_index) -> Built {
  BlockBody body;
  if (handed_an_index.value_or(
          std::holds_alternative<BlocksRepeat>(generate.counting))) {
    body.scope.structural_data_objects.Add(
        StructuralDataObjectDecl{
            .name = "i",
            .type = built.unit.builtins.integer,
            .kind = StructuralConstructionValueDecl{},
            .lexical_scope = std::nullopt});
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

// A unit whose one generate counts as `counting` makes it in that unit.
auto UnitBuilding(
    const std::function<Generate(CompilationUnit&)>& counting,
    std::vector<std::optional<std::uint32_t>> places,
    std::optional<bool> handed_an_index = std::nullopt) -> Built {
  Built built{.unit = CompilationUnit("U", "U"), .construction = {}};
  Generate generate = counting(built.unit);
  return Building(
      std::move(built), std::move(generate), std::move(places),
      handed_an_index);
}

// A unit whose one generate stands alone, which needs nothing of the unit.
auto UnitBuilding(
    SingleBlock single, std::vector<std::optional<std::uint32_t>> places,
    std::optional<bool> handed_an_index = std::nullopt) -> Built {
  return Building(
      Built{.unit = CompilationUnit("U", "U"), .construction = {}},
      Generate{.counting = single}, std::move(places), handed_an_index);
}

auto Loop(CompilationUnit& unit) -> Generate {
  return Generate{
      .counting = BlocksRepeat{
          .declared = {},
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
  chosen.alternatives = {GenerateBlockName{}, GenerateBlockName{}};
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
  EXPECT_NO_THROW(verify(UnitBuilding(SingleBlock{}, {0})));
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
  refused(UnitBuilding(SingleBlock{}, {}), "builds other than its one block");
  refused(
      UnitBuilding(SingleBlock{}, {std::nullopt}),
      "builds other than its one block");
  refused(
      UnitBuilding(Loop, {0, std::nullopt}), "counts out a block of no body");
  refused(
      UnitBuilding(Choice, {0}),
      "builds other than one of the alternatives written");
  refused(
      UnitBuilding(Choice, {0, 0}),
      "builds other than one of the alternatives written");
  refused(
      UnitBuilding(SingleBlock{}, {1}), "builds a body the unit does not hold");

  refused(
      UnitBuilding(SingleBlock{}, {0}, true),
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

auto ClassPath(std::string name) -> support::DefPath {
  return support::DefPath{
      .data = {support::ClassStep{
          .name = std::move(name), .arguments = std::nullopt}}};
}

auto DeclarationOf(std::string name, std::optional<ClassId> extends)
    -> ClassDeclaration {
  return ClassDeclaration{
      .class_path = ClassPath(std::move(name)),
      .base = extends,
      .is_interface_class = false,
      .implements = {},
      .properties = {},
      .static_properties = {},
      .constructor = std::nullopt,
      .methods = {},
      .instance_scope = std::nullopt,
      .conforming = {}};
}

auto CovergroupReading(
    std::optional<ClassId> embedding, std::vector<ClassId> counting)
    -> PublishedCovergroup {
  PublishedCovergroup published{
      .path = ClassPath("G"),
      .formals = {},
      .sample_formals = {},
      .embedding = embedding,
      .coverpoints = {},
      .cross_count = 0,
      .base = std::nullopt};
  for (const ClassId cls : counting) {
    published.coverpoints.push_back(
        PublishedCoverpoint{.value_type = TypeId{0}, .library_class = cls});
  }
  return published;
}

// Whatever a unit's lowering reads of a class to decide what a construct means,
// the unit holds what the declaring unit published of it: a method call and a
// call of a hook the class has without declaring it (LRM 18.6.2), a property
// and a static property, a construction, a cast to a class of an instance (LRM
// 8.26.5), a declaration extending or implementing it, and a covergroup
// embedded in it, counted by it or lowered through the coverage library. Each
// is refused, naming what reads the class, where the unit holds no
// declaration of class 'C' of unit 'P', and accepted where it holds one of 'C'
// and of 'Base', which 'C' extends.
TEST(HirVerifyTest, EveryReadOfAClassNeedsItsDeclaration) {
  using Reading = std::function<std::vector<ExprId>(
      CompilationUnit&, ProceduralBody&, ClassId)>;
  const auto expr = [](const CompilationUnit& u, ProceduralBody& b,
                       ExprData data) {
    return b.exprs.Add(
        Expr{.type = u.builtins.int_type, .data = std::move(data), .span = {}});
  };
  const auto call = [&](const CompilationUnit& u, ProceduralBody& b,
                        SubroutineRef callee) {
    return std::vector<ExprId>{
        expr(u, b, CallExpr{.callee = std::move(callee), .arguments = {}})};
  };
  const std::vector<std::pair<std::string_view, Reading>> readings{
      {"a method call",
       [&](CompilationUnit& u, ProceduralBody& b, ClassId cls) {
         return call(
             u, b,
             MethodCallRef{
                 .receiver = AddLeaf(u, b),
                 .callee = BuiltinHookCallee{
                     .cls = cls, .hook = RandomizationHook::kPreRandomize}});
       }},
      {"a method call",
       [&](CompilationUnit& u, ProceduralBody& b, ClassId cls) {
         return call(
             u, b,
             MethodCallRef{
                 .receiver = AddLeaf(u, b),
                 .callee =
                     ClassMethodTarget{.owner = cls, .method = MethodId{0}}});
       }},
      {"a static method call",
       [&](CompilationUnit& u, ProceduralBody& b, ClassId cls) {
         return call(
             u, b,
             StaticMethodCallRef{
                 .callee =
                     ClassMethodTarget{.owner = cls, .method = MethodId{0}},
                 .declaring_instance = std::nullopt});
       }},
      {"a property reference",
       [&](CompilationUnit& u, ProceduralBody& b, ClassId cls) {
         return std::vector<ExprId>{expr(
             u, b,
             PrimaryExpr{
                 .data = ClassPropertyRef{
                     .target = {.owner = cls, .field = FieldId{0}}}})};
       }},
      {"a static property reference",
       [&](CompilationUnit& u, ProceduralBody& b, ClassId cls) {
         return std::vector<ExprId>{expr(
             u, b,
             PrimaryExpr{
                 .data = StaticPropertyRef{
                     .target = ClassStaticPropertyTarget{
                         .owner = cls, .prop = StaticPropertyId{0}}}})};
       }},
      {"a property access",
       [&](CompilationUnit& u, ProceduralBody& b, ClassId cls) {
         return std::vector<ExprId>{expr(
             u, b,
             ClassPropertyAccessExpr{
                 .base_value = AddLeaf(u, b),
                 .target = {.owner = cls, .field = FieldId{0}}})};
       }},
      {"an object construction",
       [&](CompilationUnit& u, ProceduralBody& b, ClassId cls) {
         return std::vector<ExprId>{expr(
             u, b,
             ClassNewExpr{
                 .class_id = cls,
                 .arguments = {},
                 .declaring_instance = std::nullopt})};
       }},
      {"a dynamic cast",
       [&](CompilationUnit& u, ProceduralBody& b, ClassId cls) {
         const ExprId destination = b.exprs.Add(
             Expr{
                 .type = u.types.Intern(Type{ClassHandleType{.class_id = cls}}),
                 .data = PrimaryExpr{.data = NullLiteral{}},
                 .span = {}});
         return std::vector<ExprId>{expr(
             u, b,
             DynamicCastExpr{
                 .destination = destination,
                 .source = AddLeaf(u, b),
                 .validity =
                     AssignmentAllowed{
                         .check = RunTimeCheck::kObjectIsOfTheDestinationClass},
                 .on_invalid = InvalidAssignmentHandling::kAnswered,
                 .destination_instance = StructuralHops{0}})};
       }},
      {"the declaration of class 'D'",
       [](CompilationUnit& u, ProceduralBody&, ClassId cls) {
         const ClassId implementing = u.classes.Import(
             ClassKey{.unit_name = "P", .class_path = ClassPath("D")});
         ClassDeclaration declared = DeclarationOf("D", std::nullopt);
         declared.implements = {cls};
         u.classes.SetDeclaration(implementing, std::move(declared));
         return std::vector<ExprId>{};
       }},
      {"covergroup 'G'",
       [](CompilationUnit& u, ProceduralBody&, ClassId cls) {
         u.covergroups.Publish(
             u.covergroups.Import(
                 CovergroupKey{.unit_name = "P", .path = ClassPath("G")}),
             CovergroupReading(cls, {}));
         return std::vector<ExprId>{};
       }},
      {"covergroup 'G'",
       [](CompilationUnit& u, ProceduralBody&, ClassId cls) {
         u.covergroups.Publish(
             u.covergroups.Import(
                 CovergroupKey{.unit_name = "P", .path = ClassPath("G")}),
             CovergroupReading(std::nullopt, {cls}));
         return std::vector<ExprId>{};
       }},
      {"the coverage library",
       [](CompilationUnit& u, ProceduralBody&, ClassId cls) {
         CoverageLibraryMembers library;
         for (ClassId* member :
              {&library.registry.cls, &library.group_type.cls,
               &library.group_instance.cls, &library.item.cls,
               &library.coverpoint.cls, &library.cross.cls,
               &library.selection.cls}) {
           *member = cls;
         }
         u.coverage_library = std::move(library);
         return std::vector<ExprId>{};
       }},
  };
  const auto reading = [](const Reading& reads, bool c_declared,
                          bool base_declared) {
    return UnitWithProcess([&](CompilationUnit& u, ProceduralBody& b) {
      const ClassId cls = u.classes.Import(
          ClassKey{.unit_name = "P", .class_path = ClassPath("C")});
      const ClassId base = u.classes.Import(
          ClassKey{.unit_name = "P", .class_path = ClassPath("Base")});
      if (c_declared) u.classes.SetDeclaration(cls, DeclarationOf("C", base));
      if (base_declared) {
        u.classes.SetDeclaration(base, DeclarationOf("Base", std::nullopt));
      }
      return reads(u, b, cls);
    });
  };
  // A refusal names what reads the class first, then the class it reads.
  const auto refused = [](const CompilationUnit& unit, std::string_view reader,
                          std::string_view cls) {
    try {
      Verify(unit, {});
      ADD_FAILURE() << "a unit missing a declaration " << reader
                    << " reads was accepted";
    } catch (const InternalError& error) {
      const std::string message = error.what();
      const std::size_t named =
          message.find(std::format("hir verify: {}", reader));
      EXPECT_NE(named, std::string::npos) << message;
      EXPECT_NE(
          message.find(
              std::format(
                  "reads class '{}' of unit 'P', of which unit 'U' holds no "
                  "declaration",
                  cls),
              named),
          std::string::npos)
          << message;
    }
  };
  for (const auto& [reader, reads] : readings) {
    EXPECT_NO_THROW(Verify(reading(reads, true, true), {})) << reader;
    refused(reading(reads, false, true), reader, "C");
  }
  refused(
      reading(readings.front().second, true, false),
      "the declaration of class 'C'", "Base");
}

// A behavior is named by the introducing method's position, and a callee's
// evaluators by their positions beside its formals, so a declaration naming a
// method that introduces no behavior, or stating evaluators that do not line
// up with the formals, is refused, and the same declaration stated rightly is
// accepted.
TEST(HirVerifyTest, ADeclarationsPositionsNameWhatItsClassDeclares) {
  // A unit holding the class `C` of two methods: `base`, which introduces a
  // behavior where `introduces`, and `over`, which overrides it, taking one
  // formal and stating `stated` evaluators for it.
  const auto declaring = [](bool introduces, std::size_t stated) {
    return UnitWithProcess([&](CompilationUnit& u, ProceduralBody&) {
      const ClassId cls = u.classes.Import(
          ClassKey{.unit_name = "P", .class_path = ClassPath("C")});
      ClassDeclaration declared = DeclarationOf("C", std::nullopt);
      const auto method = [&](std::string name, std::size_t formals,
                              std::size_t defaults, MethodDispatch dispatch) {
        CalleeInterface interface;
        for (std::size_t i = 0; i < formals; ++i) {
          interface.params.push_back(
              CalleeParam{
                  .direction = ParamDirection::kInput,
                  .type = u.builtins.int_type});
        }
        declared.methods.Add(
            DeclaredMethod{
                .prototype =
                    SubroutinePrototype{
                        .name = std::move(name),
                        .interface = std::move(interface),
                        .result_type = u.builtins.void_type},
                .defaults = std::vector<std::optional<MethodId>>(defaults),
                .dispatch = std::move(dispatch),
                .replaces = std::nullopt,
                .makes = std::nullopt});
      };
      method(
          "base", 0, 0,
          introduces ? MethodDispatch{IntroducesVirtual{}}
                     : MethodDispatch{NotVirtual{}});
      method(
          "over", 1, stated,
          OverridesVirtual{
              .is_pure = false,
              .overridden =
                  ClassMethodTarget{.owner = cls, .method = MethodId{0}}});
      u.classes.SetDeclaration(cls, std::move(declared));
      return std::vector<ExprId>{};
    });
  };
  const auto refused = [](const CompilationUnit& unit, std::string_view says) {
    try {
      Verify(unit, {});
      ADD_FAILURE() << "a unit whose declaration " << says << " was accepted";
    } catch (const InternalError& error) {
      const std::string message = error.what();
      EXPECT_NE(message.find(says), std::string::npos) << message;
    }
  };
  EXPECT_NO_THROW(Verify(declaring(true, 1), {}));
  refused(
      declaring(false, 1),
      "names method 0 of class 'C' as a behavior, which introduces none");
  refused(
      declaring(true, 2),
      "method 'over' of class 'C' states 2 defaults for 1 formals");
}

// A member of a namespace is named by the unit's entry for the namespace and
// its position in what the namespace declares, so a reference naming an entry
// the unit does not hold, or a position the namespace does not declare, is
// refused, a variable's and a subroutine's alike.
TEST(HirVerifyTest, ANamespaceMemberIsNamedThroughAnEntryTheUnitHolds) {
  // A unit naming one variable and one subroutine of the namespace `P` it
  // holds when `holds`, at positions `variable` and `subroutine`.
  const auto naming = [](bool holds, std::uint32_t variable,
                         std::uint32_t subroutine) {
    return UnitWithProcess([&](CompilationUnit& u, ProceduralBody& b) {
      NamespaceId owner{0};
      if (holds) {
        owner = u.namespaces.Enter(NamespaceKey{.unit_name = "P"}).first;
        NamespaceDeclaration declared;
        declared.variables.Add(
            NamespaceVariable{
                .name = support::UnqualifiedName("v"),
                .type = u.builtins.int_type});
        declared.subroutines.Add(
            NamespaceSubroutine{
                .prototype =
                    SubroutinePrototype{
                        .name = "f",
                        .interface = {},
                        .result_type = u.builtins.void_type},
                .defaults = {}});
        u.namespaces.SetDeclaration(owner, std::move(declared));
      }
      return std::vector<ExprId>{
          b.exprs.Add(
              Expr{
                  .type = u.builtins.int_type,
                  .data =
                      PrimaryExpr{
                          .data =
                              NamespaceValueRef{
                                  .owner = owner,
                                  .variable = NamespaceVariableId{variable}}},
                  .span = {}}),
          b.exprs.Add(
              Expr{
                  .type = u.builtins.void_type,
                  .data =
                      CallExpr{
                          .callee =
                              NamespaceSubroutineRef{
                                  .owner = owner,
                                  .subroutine =
                                      NamespaceSubroutineId{subroutine}},
                          .arguments = {}},
                  .span = {}})};
    });
  };
  const auto refused = [](const CompilationUnit& unit, std::string_view names) {
    try {
      Verify(unit, {});
      ADD_FAILURE() << "a unit naming " << names << " was accepted";
    } catch (const InternalError& error) {
      const std::string message = error.what();
      EXPECT_NE(message.find(std::format("names {}", names)), std::string::npos)
          << message;
    }
  };
  EXPECT_NO_THROW(Verify(naming(true, 0, 0), {}));
  refused(
      naming(false, 0, 0), "namespace 0, which unit 'U' holds no declaration");
  refused(naming(true, 1, 0), "variable 1 of namespace 'P', which declares 1");
  refused(
      naming(true, 0, 1), "subroutine 1 of namespace 'P', which declares 1");
}

// A coverpoint or a cross is named by the covergroup declaring it and its
// place there, so a use naming a covergroup the unit holds no declaration of,
// a place that covergroup does not declare, or an item of a covergroup the one
// it is reached through neither is nor extends, is refused.
TEST(HirVerifyTest, ACoverageItemIsNamedThroughACovergroupTheUnitHolds) {
  // A unit reaching the type of the covergroup `g`, which it holds a
  // declaration of where `holds` and which declares one coverpoint, for the
  // item at place `place` of `g` where `own`, and of the covergroup `h`
  // otherwise.
  const auto naming = [](bool holds, std::uint32_t place, bool own) {
    return UnitWithProcess([&](CompilationUnit& u, ProceduralBody& b) {
      const ClassId counting = u.classes.Import(
          ClassKey{.unit_name = "P", .class_path = ClassPath("counting")});
      u.classes.SetDeclaration(
          counting, DeclarationOf("counting", std::nullopt));
      const auto covergroup = [&](std::string_view name, bool declared) {
        const CovergroupId id = u.covergroups.Import(
            CovergroupKey{
                .unit_name = "P",
                .path = support::DefPath{
                    .data = {support::ClassStep{
                        .name = std::string{name},
                        .arguments = std::nullopt}}}});
        if (declared) {
          u.covergroups.Publish(
              id, PublishedCovergroup{
                      .path = {},
                      .formals = {},
                      .sample_formals = {},
                      .embedding = std::nullopt,
                      .coverpoints = {PublishedCoverpoint{
                          .value_type = u.builtins.int_type,
                          .library_class = counting}},
                      .cross_count = 0,
                      .base = std::nullopt});
        }
        return id;
      };
      const CovergroupId g = covergroup("g", holds);
      const CovergroupId h = covergroup("h", true);
      return std::vector<ExprId>{b.exprs.Add(
          Expr{
              .type = u.builtins.void_type,
              .data =
                  CovergroupMethodCallExpr{
                      .covergroup = g,
                      .receiver =
                          CovergroupTypeReceiver{
                              .declaring_instance = std::nullopt},
                      .item =
                          CoverpointRef{
                              .covergroup = own ? g : h,
                              .coverpoint = CoverpointId{place}},
                      .method = CovergroupMethod::kGetCoverage,
                      .arguments = {}},
              .span = {}})};
    });
  };
  const auto refused = [](const CompilationUnit& unit, std::string_view says) {
    try {
      Verify(unit, {});
      ADD_FAILURE() << "a unit whose use " << says << " was accepted";
    } catch (const InternalError& error) {
      const std::string message = error.what();
      EXPECT_NE(message.find(says), std::string::npos) << message;
    }
  };
  EXPECT_NO_THROW(Verify(naming(true, 0, true), {}));
  refused(
      naming(false, 0, true),
      "names a coverpoint of covergroup 0, of which the unit holds no "
      "declaration");
  refused(
      naming(true, 1, true),
      "names coverpoint 1 of covergroup 'g', which declares no such "
      "coverpoint");
  refused(
      naming(true, 0, false),
      "names an item of covergroup 1 through covergroup 0, which neither is "
      "nor extends it");
}

// An item a covergroup declares replaces an item of a covergroup it extends
// (LRM 19.4.1), so one naming an item of a covergroup it does not extend, or
// of its own declaration, is refused.
TEST(HirVerifyTest, AnItemReplacesOneOfACovergroupItExtends) {
  // A unit declaring the covergroup `g`, which extends `h` where `extends`,
  // and whose one coverpoint replaces the coverpoint of `h`, or of `g` itself
  // where `own`.
  const auto replacing = [](bool extends, bool own) {
    CompilationUnit u("U", "U");
    const ClassId counting = u.classes.Import(
        ClassKey{.unit_name = "P", .class_path = ClassPath("counting")});
    u.classes.SetDeclaration(counting, DeclarationOf("counting", std::nullopt));
    const auto published = [&](std::optional<CovergroupId> base) {
      return PublishedCovergroup{
          .path = {},
          .formals = {},
          .sample_formals = {},
          .embedding = std::nullopt,
          .coverpoints = {PublishedCoverpoint{
              .value_type = u.builtins.int_type, .library_class = counting}},
          .cross_count = 0,
          .base = base};
    };
    const CovergroupId h = u.covergroups.Import(
        CovergroupKey{.unit_name = "P", .path = ClassPath("h")});
    u.covergroups.Publish(h, published(std::nullopt));
    const CovergroupId g = u.covergroups.DeclareOwn(ClassPath("g"));
    u.covergroups.Publish(
        g, published(extends ? std::optional{h} : std::nullopt));
    CovergroupDecl decl;
    const ExprId sampled = decl.body.exprs.Add(
        Expr{
            .type = u.builtins.int_type,
            .data = PrimaryExpr{.data = NullLiteral{}},
            .span = {}});
    decl.coverpoints.Add(
        Coverpoint{
            .name = "p",
            .expression = sampled,
            .iff = std::nullopt,
            .options = {},
            .bins = {},
            .replaces = CoverpointRef{
                .covergroup = own ? g : h, .coverpoint = CoverpointId{0}}});
    if (extends) decl.base = h;
    u.root_scope.replicated_covergroups.push_back(
        ReplicatedCovergroup{.id = g, .code = std::move(decl)});
    return u;
  };
  const auto refused = [](const CompilationUnit& unit, std::string_view says) {
    try {
      Verify(unit, {});
      ADD_FAILURE() << "a covergroup whose item " << says << " was accepted";
    } catch (const InternalError& error) {
      const std::string message = error.what();
      EXPECT_NE(message.find(says), std::string::npos) << message;
    }
  };
  EXPECT_NO_THROW(Verify(replacing(true, false), {}));
  refused(
      replacing(false, false),
      "names an item of covergroup 0 through covergroup 1, which neither is "
      "nor extends it");
  refused(replacing(true, true), "replaces an item of its own declaration");
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

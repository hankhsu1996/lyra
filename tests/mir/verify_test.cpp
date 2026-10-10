#include "lyra/mir/verify.hpp"

#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

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
  const ExprId park = inner.exprs.Add(
      Expr{
          .data = MachineBoolLiteral{.value = true},
          .type = unit.builtins.machine_bool});
  const ExprId wait = inner.exprs.Add(
      Expr{.data = WaitExpr{.park = park}, .type = unit.builtins.void_type});
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

// Awaiting an execution and stopping where a park says end differently, so
// each is refused where its operand is what the other waits on.
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
    FAIL() << "an await on a park's answer was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find("not an execution"), std::string::npos) << message;
  }
}

// A unit whose one body, `f`, is the statements `fill` writes into it.
template <typename Fill>
auto UnitWithBody(const Fill& fill) -> CompilationUnit {
  CompilationUnit unit;
  unit.name = "U";
  CallableCode code = CallableCode::Defined();
  code.result_type = unit.builtins.void_type;
  fill(unit, code.Body());
  const CallableId id = unit.callables.Add(
      CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
  unit.named_callables.push_back(NamedCallable{.name = "f", .body = id});
  return unit;
}

auto AddCall(const CompilationUnit& unit, Block& block) -> ExprId {
  return block.exprs.Add(MakeCurrentRuntimeCallExpr(unit.builtins.effects));
}

auto AddSum(const CompilationUnit& unit, Block& block, ExprId lhs, ExprId rhs)
    -> ExprId {
  return block.exprs.Add(
      Expr{
          .data = BinaryExpr{.op = BinaryOp::kAdd, .lhs = lhs, .rhs = rhs},
          .type = unit.builtins.int_type});
}

// A node that computes is evaluated at every place reaching it, so a body that
// reaches one at two places a run takes both of is refused, and the refusal
// names the body and what stands at the two places.
TEST(MirVerifyTest, AComputationOneRunReachesTwiceIsRefused) {
  const CompilationUnit unit =
      UnitWithBody([](const CompilationUnit& u, Block& body) {
        const ExprId call = AddCall(u, body);
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, call, call)});
      });
  try {
    Verify(unit);
    FAIL() << "a call reached as both operands of one operation was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find("'f' of unit 'U'"), std::string::npos) << message;
    EXPECT_NE(
        message.find("one run evaluates it at two of them"), std::string::npos)
        << message;
    EXPECT_NE(message.find("a binary operation"), std::string::npos) << message;
  }
}

// Two places under different arms of one conditional expression are not a pair
// a run can both take, so one node may stand at both; under the same arm, or
// with one of them the condition, a run takes both.
TEST(MirVerifyTest, AComputationUnderDifferentArmsIsNotReachedTwice) {
  const auto conditional = [](const CompilationUnit& u, Block& body,
                              ExprId condition, ExprId then_value,
                              ExprId else_value) {
    return body.exprs.Add(
        Expr{
            .data =
                ConditionalExpr{
                    .condition = condition,
                    .then_value = then_value,
                    .else_value = else_value},
            .type = u.builtins.int_type});
  };
  const auto flag = [](const CompilationUnit& u, Block& body) {
    return body.exprs.Add(
        Expr{
            .data = MachineBoolLiteral{.value = true},
            .type = u.builtins.machine_bool});
  };

  EXPECT_NO_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId call = AddCall(u, body);
        const ExprId inner =
            conditional(u, body, flag(u, body), call, AddCall(u, body));
        body.AppendStmt(
            ExprStmt{.expr = conditional(u, body, flag(u, body), call, inner)});
      })));

  EXPECT_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId call = AddCall(u, body);
        const ExprId sum = AddSum(u, body, call, call);
        body.AppendStmt(
            ExprStmt{
                .expr = conditional(
                    u, body, flag(u, body), sum, AddCall(u, body))});
      })),
      InternalError);

  EXPECT_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId test = body.exprs.Add(
            Expr{
                .data = CastExpr{.operand = AddCall(u, body)},
                .type = u.builtins.machine_bool});
        const ExprId as_value = body.exprs.Add(
            Expr{
                .data = CastExpr{.operand = test},
                .type = u.builtins.int_type});
        body.AppendStmt(
            ExprStmt{
                .expr =
                    conditional(u, body, test, as_value, AddCall(u, body))});
      })),
      InternalError);
}

// A node that names a thing, spells a constant or forms a place computes
// nothing itself, so it may stand at two places, and what counts is what it
// reaches: a place formed over a call reaches that call at each.
TEST(MirVerifyTest, ANodeThatComputesNothingMayStandAtSeveralPlaces) {
  EXPECT_NO_THROW(
      Verify(UnitWithBody([](const CompilationUnit& u, Block& body) {
        const ExprId literal = body.exprs.Add(
            Expr{
                .data = MachineIntLiteral{.value = 1},
                .type = u.builtins.int_type});
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, literal, literal)});
      })));

  const auto place_over = [](const CompilationUnit& u, Block& body,
                             ExprId pointer) {
    return body.exprs.Add(MakeDerefExpr(pointer, u.builtins.int_type));
  };
  EXPECT_NO_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId null = body.exprs.Add(
            Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
        const ExprId place = place_over(u, body, null);
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, place, place)});
      })));
  EXPECT_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId place = place_over(u, body, AddCall(u, body));
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, place, place)});
      })),
      InternalError);
}

// A value wanted at several places is bound among a block expression's steps
// and named at each, and the value the block yields is reached in the block of
// its steps.
TEST(MirVerifyTest, AValueBoundAmongStepsIsNamedAtEachPlace) {
  EXPECT_NO_THROW(
      Verify(UnitWithBody([](const CompilationUnit& u, Block& body) {
        Block steps;
        const ExprId call = AddCall(u, steps);
        steps.AppendStmt(ExprStmt{.expr = call});
        const ExprId literal = steps.exprs.Add(
            Expr{
                .data = MachineIntLiteral{.value = 1},
                .type = u.builtins.int_type});
        const ExprId value = AddSum(u, steps, literal, literal);
        const BlockId scope = body.child_scopes.Add(std::move(steps));
        body.AppendStmt(
            ExprStmt{
                .expr = body.exprs.Add(
                    Expr{
                        .data = BlockExpr{.scope = scope, .value = value},
                        .type = u.builtins.int_type})});
      })));

  EXPECT_THROW(
      Verify(UnitWithBody([](const CompilationUnit& u, Block& body) {
        Block steps;
        const ExprId call = AddCall(u, steps);
        steps.AppendStmt(ExprStmt{.expr = call});
        const BlockId scope = body.child_scopes.Add(std::move(steps));
        body.AppendStmt(
            ExprStmt{
                .expr = body.exprs.Add(
                    Expr{
                        .data = BlockExpr{.scope = scope, .value = call},
                        .type = u.builtins.int_type})});
      })),
      InternalError);
}

// A field is accessed on the object, and what reaches an object is dereferenced
// to it first, so an access made on the pointer itself is refused -- here in a
// nested block, where a lowering writes most statements.
TEST(MirVerifyTest, AFieldIsAccessedOnTheObjectAPointerReaches) {
  const auto access_on = [](bool dereferenced) {
    return UnitWithBody([dereferenced](CompilationUnit& u, Block& body) {
      const TypeId pointer_type = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.int_type,
              .ownership = PointerOwnership::kBorrowed}});
      Block inner;
      const ExprId pointer =
          inner.exprs.Add(Expr{.data = NullLiteral{}, .type = pointer_type});
      const ExprId receiver =
          dereferenced
              ? inner.exprs.Add(MakeDerefExpr(pointer, u.builtins.int_type))
              : pointer;
      inner.AppendStmt(
          ExprStmt{
              .expr = inner.exprs.Add(MakeFieldAccessExpr(
                  receiver,
                  ClassFieldTarget{
                      .owner = IntraUnitClassRef{ClassId{.value = 0}},
                      .slot = FieldId{.value = 0}},
                  u.builtins.int_type))});
      const BlockId scope = body.child_scopes.Add(std::move(inner));
      body.AppendStmt(BlockStmt{.scope = scope});
    });
  };

  EXPECT_NO_THROW(Verify(access_on(true)));
  try {
    Verify(access_on(false));
    FAIL() << "a field access on a pointer was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find("'f' of unit 'U'"), std::string::npos) << message;
    EXPECT_NE(
        message.find("on what reaches an object rather than on the object"),
        std::string::npos)
        << message;
  }
}

// A member function is entered on the object as well, so a call whose receiver
// is the pointer itself is refused and one on what the pointer designates is
// not.
TEST(MirVerifyTest, AMemberFunctionIsEnteredOnTheObjectAPointerReaches) {
  const auto call_on = [](bool dereferenced) {
    return UnitWithBody([dereferenced](CompilationUnit& u, Block& body) {
      const TypeId pointer_type = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.int_type,
              .ownership = PointerOwnership::kBorrowed}});
      const ExprId pointer =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = pointer_type});
      const ExprId receiver =
          dereferenced
              ? body.exprs.Add(MakeDerefExpr(pointer, u.builtins.int_type))
              : pointer;
      body.AppendStmt(
          ExprStmt{
              .expr = body.exprs.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{
                                      .target =
                                          CallableTarget{
                                              .owner = ClassId{.value = 0},
                                              .slot = CallableId{.value = 0}},
                                      .receiver = receiver},
                              .arguments = {}},
                      .type = u.builtins.void_type})});
    });
  };

  EXPECT_NO_THROW(Verify(call_on(true)));
  EXPECT_THROW(Verify(call_on(false)), InternalError);
}

// The verifier refuses `unit`, and its refusal says `why`.
void ExpectRefused(const CompilationUnit& unit, std::string_view why) {
  try {
    Verify(unit);
    ADD_FAILURE() << "accepted a unit whose refusal says \"" << why << "\"";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find(why), std::string::npos) << message;
  }
}

// An element of a numbered container is named by a position, so an index left
// in the type the source wrote it in is refused; the entry reaching an
// associative array takes a value of its index type, which is no ordinal (LRM
// 7.8).
TEST(MirVerifyTest, AnOrdinalIsStatedAsAPosition) {
  const auto element_of = [](bool keyed, bool as_position) {
    return UnitWithBody([keyed, as_position](CompilationUnit& u, Block& body) {
      const TypeId element = u.builtins.int_type;
      const TypeId container =
          keyed
              ? u.types.Intern(
                    Type{AssociativeArrayType{
                        .element_type = element,
                        .key_type = u.builtins.int_type}})
              : u.types.Intern(Type{DynamicArrayType{.element_type = element}});
      const support::BuiltinFn entry = keyed ? support::BuiltinFn::kAssocElement
                                             : support::BuiltinFn::kElement;
      const TypeId index_type =
          as_position ? PositionType(u.types) : u.builtins.int_type;
      const ExprId array =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = container});
      const ExprId at =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = index_type});
      body.AppendStmt(
          ExprStmt{
              .expr = body.exprs.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{.target = entry, .receiver = array},
                              .arguments = {at}},
                      .type = element})});
    });
  };

  EXPECT_NO_THROW(Verify(element_of(false, true)));
  EXPECT_NO_THROW(Verify(element_of(true, false)));
  ExpectRefused(element_of(false, false), "position type");
}

// A call on `entry` whose receiver is of type `container` and whose one
// argument is of type `argument`, answering a value of type `answer`.
auto UnitCalling(
    support::BuiltinFn entry, const auto& container_of, const auto& argument_of,
    const auto& answer_of) -> CompilationUnit {
  return UnitWithBody([&](CompilationUnit& u, Block& body) {
    const ExprId receiver =
        body.exprs.Add(Expr{.data = NullLiteral{}, .type = container_of(u)});
    const ExprId argument =
        body.exprs.Add(Expr{.data = NullLiteral{}, .type = argument_of(u)});
    body.AppendStmt(
        ExprStmt{
            .expr = body.exprs.Add(
                Expr{
                    .data =
                        CallExpr{
                            .callee =
                                Direct{.target = entry, .receiver = receiver},
                            .arguments = {argument}},
                    .type = answer_of(u)})});
  });
}

// An entry reaching by a position is called on something that numbers its
// parts, and one reaching by a key on an associative array (LRM 7.8).
TEST(MirVerifyTest, AnEntryIsCalledOnTheKindOfContainerItReaches) {
  const auto element = [](CompilationUnit& u) { return u.builtins.int_type; };
  const auto keyed = [](CompilationUnit& u) {
    return u.types.Intern(
        Type{AssociativeArrayType{
            .element_type = u.builtins.int_type,
            .key_type = u.builtins.int_type}});
  };
  const auto numbered = [](CompilationUnit& u) {
    return u.types.Intern(
        Type{DynamicArrayType{.element_type = u.builtins.int_type}});
  };
  const auto position = [](CompilationUnit& u) {
    return PositionType(u.types);
  };

  EXPECT_NO_THROW(Verify(
      UnitCalling(support::BuiltinFn::kElement, numbered, position, element)));
  EXPECT_NO_THROW(Verify(
      UnitCalling(support::BuiltinFn::kAssocElement, keyed, element, element)));
  ExpectRefused(
      UnitCalling(support::BuiltinFn::kElement, keyed, position, element),
      "reaches by a position into an associative array");
  ExpectRefused(
      UnitCalling(
          support::BuiltinFn::kAssocElement, numbered, element, element),
      "reaches by a key into something that is no associative array");
}

// An entry the source states over values of several kinds is not named over
// an integral value, which the entry over integral values alone carries out;
// and an entry reading an operand as a machine value is handed no integral
// one.
TEST(MirVerifyTest, ACallIsHeldToItsEntrysOneDeclaration) {
  const auto integral = [](CompilationUnit& u) { return u.builtins.int_type; };
  const auto string = [](CompilationUnit& u) { return u.builtins.string; };
  const auto bit = [](CompilationUnit& u) { return u.builtins.bit1; };

  EXPECT_NO_THROW(
      Verify(UnitCalling(support::BuiltinFn::kCaseEqual, string, string, bit)));
  EXPECT_NO_THROW(Verify(UnitCalling(
      support::BuiltinFn::kIntegralCaseEqual, integral, integral, bit)));
  ExpectRefused(
      UnitCalling(support::BuiltinFn::kCaseEqual, integral, integral, bit),
      "where its first operand is integral");

  EXPECT_NO_THROW(
      Verify(UnitCalling(support::BuiltinFn::kCompare, string, string, bit)));
  ExpectRefused(
      UnitCalling(support::BuiltinFn::kCompare, string, integral, bit),
      "reads that operand as a machine value");
}

// An operation over integral values whose operands are all constants is stated
// as the constant it evaluates to, so one left standing as an operation is
// refused; the same operation over a value the program computes stands.
TEST(MirVerifyTest, AnOperationOverConstantsIsStatedAsItsConstant) {
  const auto one = [](const CompilationUnit& u, Block& body) {
    return body.exprs.Add(
        Expr{
            .data =
                ReferenceExpr{
                    .target =
                        IntegralConstantRef{
                            .constant = u.integral_constants.Intern(
                                IntegralConstantDecl{
                                    .type = u.builtins.int_type,
                                    .value =
                                        IntegralConstant{
                                            .value_words = {1},
                                            .state_words = {}}})}},
            .type = u.builtins.int_type});
  };
  const auto computed = [](const CompilationUnit& u, Block& body) {
    return body.exprs.Add(
        Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
  };

  EXPECT_NO_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Block& body) {
        body.AppendStmt(
            ExprStmt{.expr = AddSum(u, body, one(u, body), computed(u, body))});
      })));
  ExpectRefused(
      UnitWithBody([&](const CompilationUnit& u, Block& body) {
        body.AppendStmt(
            ExprStmt{.expr = AddSum(u, body, one(u, body), one(u, body))});
      }),
      "an operation over constants");
}

}  // namespace
}  // namespace lyra::mir

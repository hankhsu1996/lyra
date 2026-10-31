#include "lyra/mir/verify.hpp"

#include <array>
#include <cstdint>
#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::mir {
namespace {

// Checks `unit` as it would be held in `phase`.
void VerifyIn(Phase phase, CompilationUnit unit) {
  unit.phase = phase;
  mir::Verify(unit, "under test");
}

// Checks `unit` as every backend reads it, which is the phase holding it to
// the most.
void Verify(const CompilationUnit& unit) {
  VerifyIn(Phase::kLegalized, unit);
}

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
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
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
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
  try {
    Verify(unit);
    FAIL() << "an await on a park's answer was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find("not an execution"), std::string::npos) << message;
  }
}

// The default of `type` stated as a body of the unit. What a verifier holds a
// stated operation to is the type it is stated for, so a value standing in for
// one the program computes is the body.
auto DefaultOf(TypeId type) -> TypeOperation {
  CallableCode code = CallableCode::Defined();
  code.result_type = type;
  Block& block = code.Body();
  block.AppendStmt(
      ReturnStmt{
          .value = block.exprs.Add(Expr{.data = NullLiteral{}, .type = type})});
  return TypeOperation{
      .type = type,
      .answers = support::BuiltinFn::kDeclaredDefault,
      .code = std::move(code)};
}

// A unit whose one body, `f`, is the code `fill` writes into it.
template <typename Fill>
auto UnitWithCode(const Fill& fill) -> CompilationUnit {
  CompilationUnit unit;
  unit.name = "U";
  CallableCode code = CallableCode::Defined();
  code.result_type = unit.builtins.void_type;
  fill(unit, code);
  // Every unit states the default of each type an array it holds has as its
  // element, so one built here does too, whatever its body reaches.
  for (const TypeId element : ArrayElementTypes(unit.types)) {
    if (FindTypeOperation(
            unit, element, support::BuiltinFn::kDeclaredDefault) == nullptr) {
      unit.type_operations.push_back(DefaultOf(element));
    }
  }
  const CallableId id = unit.callables.Add(
      CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
  unit.named_callables.push_back(NamedCallable{.name = "f", .body = id});
  return unit;
}

// A unit whose one body, `f`, is the statements `fill` writes into it.
template <typename Fill>
auto UnitWithBody(const Fill& fill) -> CompilationUnit {
  return UnitWithCode([&](CompilationUnit& unit, CallableCode& code) {
    fill(unit, code.Body());
  });
}

// The verifier refuses `unit` as every backend reads it, and its refusal says
// `why`.
void ExpectRefused(const CompilationUnit& unit, std::string_view why) {
  try {
    Verify(unit);
    ADD_FAILURE() << "accepted a unit whose refusal says \"" << why << "\"";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find(why), std::string::npos) << message;
  }
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

// A node that computes without running code, so the order of two places
// reaching it is not what a check of it asks about.
auto AddComputation(const CompilationUnit& unit, Block& block) -> ExprId {
  return block.exprs.Add(
      Expr{
          .data =
              UnaryExpr{
                  .op = UnaryOp::kMinus,
                  .operand = block.exprs.Add(
                      Expr{
                          .data = NullLiteral{},
                          .type = unit.builtins.int_type})},
          .type = unit.builtins.int_type});
}

// Whether verifying `unit` refuses it with a message holding `reason`.
auto RefusedFor(const CompilationUnit& unit, std::string_view reason) -> bool {
  try {
    Verify(unit);
  } catch (const InternalError& error) {
    return std::string_view(error.what()).find(reason) !=
           std::string_view::npos;
  }
  return false;
}

// A node that computes is evaluated at every place reaching it, so a body that
// reaches one at two places a run takes both of is refused, and the refusal
// names the body and what stands at the two places. The computation runs no
// code, so nothing about the order of the two places is what is refused.
TEST(MirVerifyTest, AComputationOneRunReachesTwiceIsRefused) {
  const CompilationUnit unit =
      UnitWithBody([](const CompilationUnit& u, Block& body) {
        const ExprId computed = AddComputation(u, body);
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, computed, computed)});
      });
  try {
    Verify(unit);
    FAIL() << "a computation reached as both operands of one operation was "
              "accepted";
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
  const auto conditional = [](Block& body, ExprId condition, ExprId then_value,
                              ExprId else_value) {
    return body.exprs.Add(
        Expr{
            .data =
                ConditionalExpr{
                    .condition = condition,
                    .then_value = then_value,
                    .else_value = else_value},
            .type = body.exprs.Get(then_value).type});
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
            conditional(body, flag(u, body), call, AddCall(u, body));
        body.AppendStmt(
            ExprStmt{.expr = conditional(body, flag(u, body), call, inner)});
      })));

  EXPECT_TRUE(RefusedFor(
      UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId computed = AddComputation(u, body);
        const ExprId sum = AddSum(u, body, computed, computed);
        body.AppendStmt(
            ExprStmt{
                .expr = conditional(
                    body, flag(u, body), sum,
                    body.exprs.Add(
                        Expr{
                            .data = NullLiteral{},
                            .type = body.exprs.Get(sum).type}))});
      }),
      "one run evaluates it at two of them"));

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
                .expr = conditional(
                    body, test, as_value,
                    body.exprs.Add(
                        Expr{
                            .data = NullLiteral{},
                            .type = u.builtins.int_type}))});
      })),
      InternalError);
}

// A node that names a thing, spells a constant, forms a place or reads one
// computes nothing itself, so it may stand at two places, and what counts is
// what it reaches: a read of a place formed over a call reaches that call at
// each.
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
        const ExprId read = body.Read(place_over(u, body, null));
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, read, read)});
      })));
  EXPECT_TRUE(RefusedFor(
      UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId read =
            body.Read(place_over(u, body, AddComputation(u, body)));
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, read, read)});
      }),
      "one run evaluates it at two of them"));
}

// Two operands that each run code can be run in the other order to another
// answer, so a legalized unit keeping both under one node is refused, and the
// refusal names the node and the operand that has to run first; two without
// side effects stay where they are.
TEST(MirVerifyTest, TwoOperandsATargetCouldReorderAreRefused) {
  EXPECT_TRUE(RefusedFor(
      UnitWithBody([](const CompilationUnit& u, Block& body) {
        body.AppendStmt(
            ExprStmt{
                .expr = AddSum(u, body, AddCall(u, body), AddCall(u, body))});
      }),
      "an operand a target could run in the other order to another answer"));
  EXPECT_NO_THROW(Verify(UnitWithBody([](const CompilationUnit& u,
                                         Block& body) {
    body.AppendStmt(
        ExprStmt{
            .expr = AddSum(
                u, body, AddComputation(u, body), AddComputation(u, body))});
  })));
}

// A place stands where a value is wanted only under a read, a read reads a
// place, and what takes the storage itself -- a borrow here -- is handed the
// place and not a read of it, which would have it act on a copy.
TEST(MirVerifyTest, StorageIsReadOnlyWhereAReadSaysSo) {
  const auto storage = [](const CompilationUnit& u, Block& body) {
    const ExprId null = body.exprs.Add(
        Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
    return body.exprs.Add(MakeDerefExpr(null, u.builtins.int_type));
  };

  EXPECT_NO_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId read = body.Read(storage(u, body));
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, read, read)});
      })));
  ExpectRefused(
      UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId place = storage(u, body);
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, place, place)});
      }),
      "a place stands as one only under a read");
  ExpectRefused(
      UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId literal = body.exprs.Add(
            Expr{
                .data = MachineIntLiteral{.value = 1},
                .type = u.builtins.int_type});
        body.AppendStmt(
            ExprStmt{
                .expr = body.exprs.Add(
                    MakeCopyExpr(literal, u.builtins.int_type))});
      }),
      "names no place");
  ExpectRefused(
      UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId read = body.Read(storage(u, body));
        body.AppendStmt(
            ExprStmt{
                .expr = body.exprs.Add(
                    Expr{
                        .data = BorrowExpr{.operand = read},
                        .type = u.builtins.int_type})});
      }),
      "would act on a copy");
}

// A dereference opens a pointer it reads, as clang reads the operand of
// builtin `*p`, and reaches through an owner as the object C++ `*unique_ptr`
// enters `operator*` on; a move moves out of a place.
TEST(MirVerifyTest, ADereferenceReadsAPointerAndNotAnOwner) {
  const auto opening = [](PointerOwnership ownership, bool read) {
    return UnitWithBody([ownership, read](CompilationUnit& u, Block& body) {
      const TypeId pointer = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.int_type, .ownership = ownership}});
      const TypeId to_pointer = u.types.Intern(
          Type{PointerType{
              .pointee = pointer, .ownership = PointerOwnership::kBorrowed}});
      const ExprId null =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = to_pointer});
      const ExprId held = body.exprs.Add(MakeDerefExpr(null, pointer));
      const ExprId opened = body.exprs.Add(
          MakeDerefExpr(read ? body.Read(held) : held, u.builtins.int_type));
      body.AppendStmt(ExprStmt{.expr = body.Read(opened)});
    });
  };

  EXPECT_NO_THROW(Verify(opening(PointerOwnership::kBorrowed, true)));
  EXPECT_NO_THROW(Verify(opening(PointerOwnership::kUnique, false)));
  ExpectRefused(
      opening(PointerOwnership::kBorrowed, false),
      "a place stands as one only under a read");
  ExpectRefused(
      opening(PointerOwnership::kUnique, true), "would act on a copy");
  ExpectRefused(
      UnitWithBody([](const CompilationUnit& u, Block& body) {
        const ExprId literal = body.exprs.Add(
            Expr{
                .data = MachineIntLiteral{.value = 1},
                .type = u.builtins.int_type});
        body.AppendStmt(
            ExprStmt{
                .expr = body.exprs.Add(
                    Expr{
                        .data = MoveExpr{.operand = literal},
                        .type = u.builtins.int_type})});
      }),
      "which names no place");
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
              .expr = inner.Read(inner.exprs.Add(MakeFieldAccessExpr(
                  receiver,
                  ClassFieldTarget{
                      .owner = ClassId{.value = 0},
                      .slot = FieldId{.value = 0}},
                  u.builtins.int_type)))});
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
      // The member function called, of a class of the unit, entered on its
      // object alone, with a body that does nothing.
      Class cls;
      CallableCode member = CallableCode::Defined();
      member.result_type = u.builtins.void_type;
      cls.callables.Add(
          CallableDecl{
              .code = std::move(member),
              .foreign = std::nullopt,
              .virtual_dispatch = std::nullopt,
              .defining_unit = std::nullopt});
      u.DefineClass(u.DeclareClass(), std::move(cls));
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

// A reference is never re-seated by a store, and only a reference held as a
// member is bound after it is created, so a store into either kind and a
// binding of a reference bound as it was created are refused.
TEST(MirVerifyTest, OnlyABindingOfAMemberMakesAReferenceNameOtherStorage) {
  const auto binding = [](bool through_store, bool of_member) {
    return UnitWithBody([=](CompilationUnit& u, Block& body) {
      const TypeId reference_type = u.types.Intern(
          Type{VariableRefType{
              .pointee = u.builtins.int_type,
              .mutability = Mutability::kMutable}});
      const TypeId target_type =
          of_member ? u.types.Intern(
                          Type{ReferenceWrapperType{
                              .pointee = u.builtins.int_type,
                              .mutability = Mutability::kMutable}})
                    : reference_type;
      // The reference's own storage, which a store or a binding writes.
      const TypeId to_target = u.types.Intern(
          Type{PointerType{
              .pointee = target_type,
              .ownership = PointerOwnership::kBorrowed}});
      const ExprId target = body.exprs.Add(MakeDerefExpr(
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = to_target}),
          target_type));
      // A store hands its target a value of the target's own type, and a
      // binding hands it the reference it binds.
      const ExprId bound = body.exprs.Add(
          Expr{
              .data = NullLiteral{},
              .type = through_store ? target_type : reference_type});
      const ExprId operation =
          through_store
              ? body.exprs.Add(MakeAssignExpr(u.builtins, target, bound))
              : body.exprs.Add(MakeCallExpr(
                    Direct{.target = support::BuiltinFn::kBindAlias},
                    {body.Read(target), bound}, u.builtins.void_type));
      body.AppendStmt(ExprStmt{.expr = operation});
    });
  };

  EXPECT_NO_THROW(Verify(binding(false, true)));
  ExpectRefused(binding(true, true), "stores into a reference");
  ExpectRefused(
      binding(false, false),
      "binds something other than a reference held as a member");
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
                      .type = u.types.Intern(
                          Type{ReferenceType{
                              .referent = element,
                              .mutability = Mutability::kReadOnly}})})});
    });
  };

  EXPECT_NO_THROW(Verify(element_of(false, true)));
  EXPECT_NO_THROW(Verify(element_of(true, false)));
  ExpectRefused(element_of(false, false), "position type");
}

// An element's read and write forms each answer a reference to the element,
// and a write lands only through the write form's: the read form's referent
// is the element default where its position names none (LRM 7.4.6), which a
// write through it would change for every read.
TEST(MirVerifyTest, AWriteLandsThroughAnElementsWriteForm) {
  const auto element_write = [](support::BuiltinFn entry, Mutability mutability,
                                bool as_reference) {
    return UnitWithBody([entry, mutability, as_reference](
                            CompilationUnit& u, Block& body) {
      const TypeId element = u.builtins.int_type;
      const TypeId container =
          u.types.Intern(Type{DynamicArrayType{.element_type = element}});
      const ExprId array =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = container});
      const ExprId at = body.exprs.Add(
          Expr{.data = NullLiteral{}, .type = PositionType(u.types)});
      const TypeId reference = u.types.Intern(
          Type{ReferenceType{.referent = element, .mutability = mutability}});
      const ExprId reached = body.exprs.Add(
          Expr{
              .data =
                  CallExpr{
                      .callee = Direct{.target = entry, .receiver = array},
                      .arguments = {at}},
              .type = as_reference ? reference : element});
      // A call typed as the element names no place, so it stands alone where
      // only its typing is what is refused.
      if (!as_reference) {
        body.AppendStmt(ExprStmt{.expr = reached});
        return;
      }
      const ExprId target = body.exprs.Add(MakeDerefExpr(reached, element));
      const ExprId value =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = element});
      body.AppendStmt(
          ExprStmt{
              .expr =
                  body.exprs.Add(MakeAssignExpr(u.builtins, target, value))});
    });
  };

  EXPECT_NO_THROW(Verify(element_write(
      support::BuiltinFn::kElementMut, Mutability::kMutable, true)));
  ExpectRefused(
      element_write(support::BuiltinFn::kElement, Mutability::kReadOnly, true),
      "writes through a read-only reference");
  ExpectRefused(
      element_write(
          support::BuiltinFn::kElementMut, Mutability::kMutable, false),
      "answers a reference and is typed as something else");
  ExpectRefused(
      element_write(support::BuiltinFn::kElement, Mutability::kMutable, true),
      "answers a reference and is typed as something else");

  // An element of an element is written through both elements' write forms,
  // the inner one included.
  const auto nested_write = [](support::BuiltinFn inner,
                               Mutability inner_mutability) {
    return UnitWithBody([inner, inner_mutability](
                            CompilationUnit& u, Block& body) {
      const TypeId element = u.builtins.int_type;
      const TypeId row =
          u.types.Intern(Type{DynamicArrayType{.element_type = element}});
      const TypeId rows =
          u.types.Intern(Type{DynamicArrayType{.element_type = row}});
      const ExprId array =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = rows});
      const auto position = [&] {
        return body.exprs.Add(
            Expr{.data = NullLiteral{}, .type = PositionType(u.types)});
      };
      const ExprId row_reference = body.exprs.Add(
          Expr{
              .data =
                  CallExpr{
                      .callee = Direct{.target = inner, .receiver = array},
                      .arguments = {position()}},
              .type = u.types.Intern(
                  Type{ReferenceType{
                      .referent = row, .mutability = inner_mutability}})});
      const ExprId row_place =
          body.exprs.Add(MakeDerefExpr(row_reference, row));
      const ExprId element_reference = body.exprs.Add(
          Expr{
              .data =
                  CallExpr{
                      .callee =
                          Direct{
                              .target = support::BuiltinFn::kElementMut,
                              .receiver = row_place},
                      .arguments = {position()}},
              .type = u.types.Intern(
                  Type{ReferenceType{
                      .referent = element,
                      .mutability = Mutability::kMutable}})});
      const ExprId target =
          body.exprs.Add(MakeDerefExpr(element_reference, element));
      const ExprId value =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = element});
      body.AppendStmt(
          ExprStmt{
              .expr =
                  body.exprs.Add(MakeAssignExpr(u.builtins, target, value))});
    });
  };

  EXPECT_NO_THROW(Verify(
      nested_write(support::BuiltinFn::kElementMut, Mutability::kMutable)));
  ExpectRefused(
      nested_write(support::BuiltinFn::kElement, Mutability::kReadOnly),
      "writes through a read-only reference");
}

// Bits are written through the reference `slice_mut` takes on the value they
// lie in, so that value is reached as a write reaches it: through an
// element's write form, never through the read form, whose referent may be
// the element default every read shares (LRM 7.4.6).
TEST(MirVerifyTest, BitsAreWrittenThroughAnElementsWriteForm) {
  const auto bits_written = [](support::BuiltinFn entry,
                               Mutability mutability) {
    return UnitWithBody([entry, mutability](CompilationUnit& u, Block& body) {
      const TypeId byte =
          PackedVectorOf(u.types, 8, IntegralStateKind::kFourState);
      const TypeId nibble =
          PackedVectorOf(u.types, 4, IntegralStateKind::kFourState);
      const TypeId container =
          u.types.Intern(Type{DynamicArrayType{.element_type = byte}});
      const ExprId array =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = container});
      const ExprId at = body.exprs.Add(
          Expr{.data = NullLiteral{}, .type = PositionType(u.types)});
      const ExprId element = body.exprs.Add(MakeDerefExpr(
          body.exprs.Add(
              Expr{
                  .data =
                      CallExpr{
                          .callee = Direct{.target = entry, .receiver = array},
                          .arguments = {at}},
                  .type = u.types.Intern(
                      Type{ReferenceType{
                          .referent = byte, .mutability = mutability}})}),
          byte));
      const ExprId bits = body.exprs.Add(
          Expr{
              .data =
                  CallExpr{
                      .callee =
                          Direct{
                              .target = support::BuiltinFn::kSliceMut,
                              .receiver = element,
                              .type_arguments = {nibble}},
                      .arguments = {at}},
              .type = u.types.Intern(Type{BitsReferenceType{.bits = nibble}})});
      const ExprId value =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = nibble});
      body.AppendStmt(
          ExprStmt{
              .expr = body.exprs.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{
                                      .target = support::BuiltinFn::kSet,
                                      .receiver = bits},
                              .arguments = {value}},
                      .type = u.builtins.void_type})});
    });
  };

  EXPECT_NO_THROW(Verify(
      bits_written(support::BuiltinFn::kElementMut, Mutability::kMutable)));
  ExpectRefused(
      bits_written(support::BuiltinFn::kElement, Mutability::kReadOnly),
      "writes through a read-only reference");
}

// A reference the program holds binds the storage it refers to, as a C++
// reference parameter binds an lvalue (LRM 13.5.2); bound to a read, every
// write through it would land on a copy.
TEST(MirVerifyTest, AReferenceBindsStorageNeverARead) {
  const auto referred_to = [](bool read_first) {
    return UnitWithBody([read_first](CompilationUnit& u, Block& body) {
      const TypeId byte =
          PackedVectorOf(u.types, 8, IntegralStateKind::kFourState);
      const TypeId pointer = u.types.Intern(
          Type{PointerType{
              .pointee = byte, .ownership = PointerOwnership::kBorrowed}});
      const ExprId place = body.exprs.Add(MakeDerefExpr(
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = pointer}), byte));
      const ExprId bound = read_first ? body.Read(place) : place;
      body.AppendStmt(
          ExprStmt{
              .expr = body.exprs.Add(
                  Expr{
                      .data =
                          CallExpr{.callee = Construct{}, .arguments = {bound}},
                      .type = u.types.Intern(
                          Type{VariableRefType{
                              .pointee = byte,
                              .mutability = Mutability::kMutable}})})});
    });
  };

  EXPECT_NO_THROW(Verify(referred_to(false)));
  ExpectRefused(referred_to(true), "takes a read");
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
  const auto read = [](CompilationUnit& u) {
    return u.types.Intern(
        Type{ReferenceType{
            .referent = u.builtins.int_type,
            .mutability = Mutability::kReadOnly}});
  };

  EXPECT_NO_THROW(Verify(
      UnitCalling(support::BuiltinFn::kElement, numbered, position, read)));
  EXPECT_NO_THROW(Verify(
      UnitCalling(support::BuiltinFn::kAssocElement, keyed, element, read)));
  ExpectRefused(
      UnitCalling(support::BuiltinFn::kElement, keyed, position, read),
      "reaches by a position into an associative array");
  ExpectRefused(
      UnitCalling(support::BuiltinFn::kAssocElement, numbered, element, read),
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

// Storage a unique pointer owns comes into existence only by a new-expression,
// and the value it is initialized from has the type allocated; a construction
// of the pointer itself is refused.
TEST(MirVerifyTest, OwnedStorageIsAllocatedOnlyByANewExpression) {
  const auto allocating = [](bool by_new, bool same_type) {
    return UnitWithBody([by_new, same_type](CompilationUnit& u, Block& body) {
      const TypeId owner = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.int_type,
              .ownership = PointerOwnership::kUnique}});
      const ExprId value = body.exprs.Add(
          Expr{
              .data = NullLiteral{},
              .type = same_type ? u.builtins.int_type : u.builtins.string});
      const Expr allocation =
          by_new
              ? MakeNewExpr(value, owner)
              : Expr{
                    .data =
                        CallExpr{.callee = Construct{}, .arguments = {value}},
                    .type = owner};
      body.AppendStmt(ExprStmt{.expr = body.exprs.Add(allocation)});
    });
  };

  EXPECT_NO_THROW(Verify(allocating(true, true)));
  ExpectRefused(
      allocating(false, true),
      "constructs what only a new-expression allocates");
  ExpectRefused(
      allocating(true, false),
      "initializes its storage from a value of another type");
}

// A fixed-size array of integers too large for a frame to hold.
auto TooLargeForAFrame(CompilationUnit& unit) -> TypeId {
  return unit.types.Intern(
      Type{UnpackedArrayType{
          .element_type = unit.builtins.int_type, .count = 4096}});
}

// A value too large for a frame is made only in storage a new-expression
// allocates, so neither a local nor a value made in the frame holds one.
TEST(MirVerifyTest, AValueTooLargeForAFrameLiesInAllocatedStorage) {
  const auto made = [](bool by_new) {
    return UnitWithBody([by_new](CompilationUnit& u, Block& body) {
      const TypeId large = TooLargeForAFrame(u);
      const ExprId value =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = large});
      const ExprId kept =
          by_new
              ? body.exprs.Add(MakeNewExpr(
                    value, u.types.Intern(
                               Type{PointerType{
                                   .pointee = large,
                                   .ownership = PointerOwnership::kUnique}})))
              : value;
      body.AppendStmt(ExprStmt{.expr = kept});
    });
  };

  EXPECT_NO_THROW(Verify(made(true)));
  ExpectRefused(made(false), "too large for a frame outside the storage");
  const CompilationUnit held_in_a_local =
      UnitWithCode([](CompilationUnit& u, CallableCode& code) {
        code.locals.Add(LocalDecl{.type = TooLargeForAFrame(u)});
      });
  ExpectRefused(held_in_a_local, "declares a local too large for a frame");

  // As built, a unit states every value plainly and none is held through a
  // box yet: only the phase the backends read holds it to frames.
  EXPECT_NO_THROW(VerifyIn(Phase::kBuilt, made(false)));
  EXPECT_NO_THROW(VerifyIn(Phase::kBuilt, held_in_a_local));
}

// An assignment writes the storage its target names, so a target that
// computes a value rather than naming storage is refused.
TEST(MirVerifyTest, AnAssignmentLandsOnAPlace) {
  const auto assigning = [](bool to_a_place) {
    return UnitWithCode([to_a_place](CompilationUnit& u, CallableCode& code) {
      const LocalId local =
          code.locals.Add(LocalDecl{.type = u.builtins.int_type});
      Block& body = code.Body();
      const ExprId named =
          body.exprs.Add(MakeLocalRefExpr(local, u.builtins.int_type));
      const ExprId target =
          to_a_place
              ? named
              : AddSum(
                    u, body,
                    body.exprs.Add(MakeCopyExpr(named, u.builtins.int_type)),
                    body.exprs.Add(MakeCopyExpr(named, u.builtins.int_type)));
      const ExprId value = body.exprs.Add(
          Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
      body.AppendStmt(
          ExprStmt{
              .expr = body.exprs.Add(
                  Expr{
                      .data = AssignExpr{.target = target, .value = value},
                      .type = u.builtins.int_type})});
    });
  };

  EXPECT_NO_THROW(Verify(assigning(true)));
  ExpectRefused(assigning(false), "which names no place");
}

// A move hands a local's value on together with the local's end, so it is the
// last use of the whole local, run exactly once on every way the body runs that
// reaches that end: nothing else names the local, and only blocks a run enters
// once whenever the one around them runs stand between the move and the
// local's declaration. A local named again after it, and one moved under an arm
// of a choice, in a branch of an `if`, in a loop's body or in its test, are
// refused; one moved in a block statement is not.
TEST(MirVerifyTest, AMoveIsTheLastUseOfAWholeLocal) {
  enum class Shape : std::uint8_t {
    kLastUse,
    kInABlockStatement,
    kNamedAgain,
    kUnderAnArm,
    kInABranch,
    kInALoopBody,
    kInALoopTest
  };
  const auto moving = [](Shape shape) {
    return UnitWithCode([shape](CompilationUnit& u, CallableCode& code) {
      const TypeId type = u.builtins.int_type;
      const LocalId moved = code.locals.Add(LocalDecl{.type = type});
      const LocalId taker = code.locals.Add(LocalDecl{.type = type});
      Block& body = code.Body();
      body.AppendStmt(
          LocalDeclStmt{
              .target = moved,
              .init =
                  body.exprs.Add(Expr{.data = NullLiteral{}, .type = type})});
      const bool inside = shape == Shape::kInABlockStatement ||
                          shape == Shape::kInABranch ||
                          shape == Shape::kInALoopBody;
      Block inner;
      Block& at = inside ? inner : body;
      ExprId taken = at.exprs.Add(
          Expr{
              .data =
                  MoveExpr{
                      .operand = at.exprs.Add(MakeLocalRefExpr(moved, type))},
              .type = type});
      if (shape == Shape::kUnderAnArm) {
        taken = at.exprs.Add(
            Expr{
                .data =
                    ConditionalExpr{
                        .condition = at.exprs.Add(
                            Expr{
                                .data = NullLiteral{},
                                .type = u.builtins.machine_bool}),
                        .then_value = taken,
                        .else_value = at.exprs.Add(
                            Expr{.data = NullLiteral{}, .type = type})},
                .type = type});
      }
      if (shape == Shape::kInALoopTest) {
        body.AppendStmt(
            WhileStmt{
                .condition = taken, .scope = body.child_scopes.Add(Block{})});
        return;
      }
      at.AppendStmt(LocalDeclStmt{.target = taker, .init = taken});
      if (shape == Shape::kInALoopBody) {
        const ExprId condition = body.exprs.Add(
            Expr{.data = NullLiteral{}, .type = u.builtins.machine_bool});
        body.AppendStmt(
            WhileStmt{
                .condition = condition,
                .scope = body.child_scopes.Add(std::move(inner))});
      }
      if (shape == Shape::kInABlockStatement) {
        body.AppendStmt(
            BlockStmt{.scope = body.child_scopes.Add(std::move(inner))});
      }
      if (shape == Shape::kInABranch) {
        const ExprId condition = body.exprs.Add(
            Expr{.data = NullLiteral{}, .type = u.builtins.machine_bool});
        body.AppendStmt(
            IfStmt{
                .condition = condition,
                .then_scope = body.child_scopes.Add(std::move(inner)),
                .else_scope = std::nullopt});
      }
      if (shape == Shape::kNamedAgain) {
        body.AppendStmt(
            ExprStmt{
                .expr = body.exprs.Add(MakeCopyExpr(
                    body.exprs.Add(MakeLocalRefExpr(moved, type)), type))});
      }
    });
  };

  EXPECT_NO_THROW(Verify(moving(Shape::kLastUse)));
  EXPECT_NO_THROW(Verify(moving(Shape::kInABlockStatement)));
  ExpectRefused(
      moving(Shape::kNamedAgain), "a move of a local named elsewhere");
  for (const Shape not_once :
       {Shape::kUnderAnArm, Shape::kInABranch, Shape::kInALoopBody,
        Shape::kInALoopTest}) {
    ExpectRefused(
        moving(not_once),
        "a move not run exactly once through its local's block");
  }
}

// A value handed to something of a type it states is of that type: a local's
// initializer, what a body answers, what an assignment writes, and what a whole
// write through a wrapper writes.
TEST(MirVerifyTest, AValueHandedMeetsTheTypeItIsHandedTo) {
  enum class HandedTo : std::uint8_t {
    kLocal,
    kResult,
    kAssignment,
    kWrapper,
  };
  const auto handing = [](HandedTo to, bool of_its_type) {
    return UnitWithCode([=](CompilationUnit& u, CallableCode& code) {
      const TypeId held = u.builtins.int_type;
      code.result_type = held;
      Block& body = code.Body();
      const ExprId value = body.exprs.Add(
          Expr{
              .data = NullLiteral{},
              .type = of_its_type ? held : u.builtins.integer});
      const auto written = [&](TypeId local_type) {
        const LocalId local = code.locals.Add(LocalDecl{.type = local_type});
        return body.exprs.Add(MakeLocalRefExpr(local, local_type));
      };
      switch (to) {
        case HandedTo::kLocal:
          body.AppendStmt(
              LocalDeclStmt{
                  .target = code.locals.Add(LocalDecl{.type = held}),
                  .init = value});
          return;
        case HandedTo::kResult:
          body.AppendStmt(ReturnStmt{.value = value});
          return;
        case HandedTo::kAssignment:
          body.AppendStmt(
              ExprStmt{
                  .expr = body.exprs.Add(
                      MakeAssignExpr(u.builtins, written(held), value))});
          return;
        case HandedTo::kWrapper:
          body.AppendStmt(
              ExprStmt{
                  .expr = body.exprs.Add(MakeCallExpr(
                      Direct{
                          .target = support::BuiltinFn::kSet,
                          .receiver = written(u.types.Intern(
                              Type{ObservableType{.value = held}}))},
                      {value}, u.builtins.void_type))});
          return;
      }
    });
  };

  for (const auto& [to, refusal] : std::array{
           std::pair{HandedTo::kLocal, "hands a local's initializer"},
           std::pair{HandedTo::kResult, "hands what it answers"},
           std::pair{HandedTo::kAssignment, "hands an assignment's target"},
           std::pair{HandedTo::kWrapper, "hands a whole write's storage"}}) {
    EXPECT_NO_THROW(Verify(handing(to, true)));
    ExpectRefused(handing(to, false), refusal);
  }
}

// A composite is its parts and nothing more, so each part is a value of the
// type of the position it lands in -- a product's component, an element list's
// element -- and there is one part for each position. A type with no positions
// is built some other way.
TEST(MirVerifyTest, ACompositesPartsAreOfTheirPositionsTypes) {
  enum class Built : std::uint8_t { kProduct, kElementList, kNeither };
  const auto composing = [](Built built, std::vector<bool> of_position_type) {
    return UnitWithCode([=](CompilationUnit& u, CallableCode& code) {
      const TypeId held = u.builtins.int_type;
      TypeId type = held;
      switch (built) {
        case Built::kProduct:
          type = u.types.Intern(
              Type{TupleType{.elements = std::vector<TypeId>(2, held)}});
          break;
        case Built::kElementList:
          type = MachineArrayOf(u.types, held, 2);
          break;
        case Built::kNeither:
          break;
      }
      Block& body = code.Body();
      std::vector<ExprId> values;
      for (const bool of_its_type : of_position_type) {
        values.push_back(body.exprs.Add(
            Expr{
                .data = NullLiteral{},
                .type = of_its_type ? held : u.builtins.integer}));
      }
      const ExprId composite = body.exprs.Add(
          Expr{
              .data = CompositeExpr{.parts = std::move(values)}, .type = type});
      body.AppendStmt(
          LocalDeclStmt{
              .target = code.locals.Add(LocalDecl{.type = type}),
              .init = composite});
    });
  };

  for (const Built built : {Built::kProduct, Built::kElementList}) {
    EXPECT_NO_THROW(Verify(composing(built, {true, true})));
    ExpectRefused(
        composing(built, {true, false}), "hands a part of a composite");
    ExpectRefused(composing(built, {true}), "parts for 2 positions");
  }
  ExpectRefused(
      composing(Built::kNeither, {true}), "a composite of a type with no");
}

// A cast joins only types every target converts between in its own syntax: an
// integral value at a type laying it out alike, a value read as the machine
// boolean a condition tests, and a machine integer at another. A value moved
// between representations is a call.
TEST(MirVerifyTest, ACastReadsAValueAsATypeEveryTargetConvertsItTo) {
  const auto casting = [](auto from, auto to) {
    return UnitWithCode([=](CompilationUnit& u, CallableCode& code) {
      Block& body = code.Body();
      const ExprId value =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = from(u)});
      const ExprId cast = body.exprs.Add(
          Expr{.data = CastExpr{.operand = value}, .type = to(u)});
      body.AppendStmt(
          LocalDeclStmt{
              .target = code.locals.Add(LocalDecl{.type = to(u)}),
              .init = cast});
    });
  };
  const auto int_type = [](CompilationUnit& u) { return u.builtins.int_type; };
  const auto integer = [](CompilationUnit& u) { return u.builtins.integer; };
  const auto string = [](CompilationUnit& u) { return u.builtins.string; };
  const auto machine_bool = [](CompilationUnit& u) {
    return u.builtins.machine_bool;
  };
  const auto machine_int64 = [](CompilationUnit& u) {
    return u.builtins.machine_int64;
  };

  EXPECT_NO_THROW(Verify(casting(int_type, machine_bool)));
  EXPECT_NO_THROW(Verify(casting(machine_int64, machine_bool)));
  ExpectRefused(casting(int_type, integer), "which a cast does not join");
  ExpectRefused(casting(int_type, string), "which a cast does not join");
  ExpectRefused(casting(int_type, machine_int64), "which a cast does not join");
}

// A unit states operations only for a type it states them for per unit, and a
// default for each type an array of it holds, each once, and a call names one
// only where the unit states it.
TEST(MirVerifyTest, AUnitStatesEachTypeOperationOnceAndCallsOnlyThose) {
  const auto calling_the_default = [](bool stated) {
    return UnitWithBody([stated](CompilationUnit& u, Block& body) {
      const TypeId array = u.types.Intern(
          Type{UnpackedArrayType{
              .element_type = u.builtins.int_type, .count = 4}});
      if (stated) {
        u.type_operations.push_back(DefaultOf(array));
      }
      body.AppendStmt(
          ExprStmt{
              .expr = body.exprs.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{
                                      .target =
                                          TypeOperationTarget{
                                              .type = array,
                                              .answers = support::BuiltinFn::
                                                  kDeclaredDefault}},
                              .arguments = {}},
                      .type = array})});
    });
  };

  EXPECT_NO_THROW(Verify(calling_the_default(true)));
  ExpectRefused(
      calling_the_default(false), "which its unit states no body for");

  // A type no array holds has no default stated, and an element type whose
  // other operations are the library's has no other.
  CompilationUnit unasked = calling_the_default(true);
  unasked.type_operations.push_back(DefaultOf(unasked.builtins.string));
  ExpectRefused(
      unasked,
      "an operation is stated for a type whose operations are not stated per "
      "unit, or a default for a type no array holds");
  CompilationUnit compared = calling_the_default(true);
  TypeOperation equality = DefaultOf(compared.builtins.int_type);
  equality.answers = support::ValueOperator::kEquality;
  compared.type_operations.push_back(std::move(equality));
  ExpectRefused(
      compared,
      "an operation is stated for a type whose operations are not stated per "
      "unit, or a default for a type no array holds");

  CompilationUnit unanswered = calling_the_default(true);
  std::erase_if(unanswered.type_operations, [&](const TypeOperation& stated) {
    return stated.type == unanswered.builtins.int_type;
  });
  ExpectRefused(
      unanswered, "the element type of an array has no default stated");

  CompilationUnit twice = calling_the_default(true);
  twice.type_operations.push_back(
      DefaultOf(twice.type_operations.front().type));
  ExpectRefused(twice, "a type has one operation stated twice");
}

// A block expression's scope ends when it yields, so what it yields refers to
// nothing that ends there -- not a local its steps declare, not a value it
// makes, not an element of a container it declares or makes, not what a
// reference its steps hold was bound to there -- while it may refer to
// storage that outlives it, reached by a position one of its locals holds.
TEST(MirVerifyTest, ABlockExpressionYieldsNothingThatEndsWithIt) {
  // What a yield is built from: the steps, a local they declare and one the
  // body declares outside them, both arrays of `int`, and a position the steps
  // declare.
  struct Yielding {
    CompilationUnit* unit = nullptr;
    CallableCode* code = nullptr;
    Block* steps = nullptr;
    TypeId array;
    LocalId outer;
    LocalId own;
    LocalId at;
  };
  const auto yielding = [](const auto& yield) {
    return UnitWithCode([&](CompilationUnit& u, CallableCode& code) {
      const TypeId array = u.types.Intern(
          Type{DynamicArrayType{.element_type = u.builtins.int_type}});
      Block& body = code.Body();
      const LocalId outer = code.locals.Add(LocalDecl{.type = array});
      body.AppendStmt(
          LocalDeclStmt{
              .target = outer,
              .init =
                  body.exprs.Add(Expr{.data = NullLiteral{}, .type = array})});
      Block steps;
      const LocalId own = code.locals.Add(LocalDecl{.type = array});
      steps.AppendStmt(
          LocalDeclStmt{
              .target = own,
              .init =
                  steps.exprs.Add(Expr{.data = NullLiteral{}, .type = array})});
      const LocalId at =
          code.locals.Add(LocalDecl{.type = PositionType(u.types)});
      steps.AppendStmt(
          LocalDeclStmt{
              .target = at,
              .init = steps.exprs.Add(
                  Expr{.data = NullLiteral{}, .type = PositionType(u.types)})});
      Yielding given{
          .unit = &u,
          .code = &code,
          .steps = &steps,
          .array = array,
          .outer = outer,
          .own = own,
          .at = at};
      const ExprId value = yield(given);
      const TypeId type = steps.exprs.Get(value).type;
      const BlockId scope = body.child_scopes.Add(std::move(steps));
      body.AppendStmt(
          ExprStmt{
              .expr = body.exprs.Add(
                  Expr{
                      .data = BlockExpr{.scope = scope, .value = value},
                      .type = type})});
    });
  };
  const auto reference_to = [](CompilationUnit& u, TypeId referent) {
    return u.types.Intern(
        Type{ReferenceType{
            .referent = referent, .mutability = Mutability::kReadOnly}});
  };
  const auto borrow = [&](const Yielding& y, ExprId operand) {
    return y.steps->exprs.Add(
        Expr{
            .data = BorrowExpr{.operand = operand},
            .type = reference_to(*y.unit, y.steps->exprs.Get(operand).type)});
  };
  // An element of what `receiver` names, at the position the steps hold.
  const auto element_of = [&](const Yielding& y, ExprId receiver) {
    return y.steps->exprs.Add(
        Expr{
            .data =
                CallExpr{
                    .callee =
                        Direct{
                            .target = support::BuiltinFn::kElement,
                            .receiver = receiver},
                    .arguments = {y.steps->ReadLocal(
                        y.at, PositionType(y.unit->types))}},
            .type = reference_to(*y.unit, y.unit->builtins.int_type)});
  };
  // A reference to `bound`, held by a local of the steps and read or moved
  // out of it.
  const auto held = [&](const Yielding& y, LocalId bound, bool moved) {
    const ExprId reference = borrow(y, y.steps->LocalPlace(bound, y.array));
    const TypeId type = y.steps->exprs.Get(reference).type;
    const LocalId holder = y.code->locals.Add(LocalDecl{.type = type});
    y.steps->AppendStmt(LocalDeclStmt{.target = holder, .init = reference});
    const ExprId place = y.steps->LocalPlace(holder, type);
    return moved ? y.steps->exprs.Add(
                       Expr{.data = MoveExpr{.operand = place}, .type = type})
                 : y.steps->Read(place);
  };
  constexpr std::string_view kWhy =
      "referring to storage that ends with that block";

  ExpectRefused(
      yielding([&](const Yielding& y) {
        return borrow(y, y.steps->LocalPlace(y.own, y.array));
      }),
      kWhy);
  ExpectRefused(
      yielding([&](const Yielding& y) {
        const ExprId literal = y.steps->exprs.Add(
            Expr{
                .data = MachineIntLiteral{.value = 1},
                .type = y.unit->builtins.int_type});
        return borrow(y, AddSum(*y.unit, *y.steps, literal, literal));
      }),
      kWhy);
  ExpectRefused(
      yielding([&](const Yielding& y) {
        const ExprId reference = borrow(y, y.steps->LocalPlace(y.own, y.array));
        return y.steps->exprs.Add(
            Expr{
                .data = CompositeExpr{.parts = {reference}},
                .type = y.unit->types.Intern(
                    Type{TupleType{
                        .elements = {y.steps->exprs.Get(reference).type}}})});
      }),
      kWhy);
  ExpectRefused(
      yielding([&](const Yielding& y) {
        return element_of(y, y.steps->LocalPlace(y.own, y.array));
      }),
      kWhy);
  ExpectRefused(
      yielding([&](const Yielding& y) {
        return element_of(
            y,
            y.steps->exprs.Add(Expr{.data = NullLiteral{}, .type = y.array}));
      }),
      kWhy);
  ExpectRefused(
      yielding([&](const Yielding& y) { return held(y, y.own, false); }), kWhy);
  ExpectRefused(
      yielding([&](const Yielding& y) { return held(y, y.own, true); }), kWhy);

  EXPECT_NO_THROW(Verify(yielding([&](const Yielding& y) {
    return borrow(y, y.steps->LocalPlace(y.outer, y.array));
  })));
  EXPECT_NO_THROW(Verify(yielding([&](const Yielding& y) {
    return element_of(y, y.steps->LocalPlace(y.outer, y.array));
  })));
  EXPECT_NO_THROW(Verify(
      yielding([&](const Yielding& y) { return held(y, y.outer, false); })));
  EXPECT_NO_THROW(Verify(
      yielding([&](const Yielding& y) { return held(y, y.outer, true); })));
  EXPECT_NO_THROW(Verify(yielding(
      [&](const Yielding& y) { return y.steps->ReadLocal(y.own, y.array); })));
}

// A call of a callable of the unit's namespace names no type, since only a
// library entry is generic over one, and answers what the callable returns.
TEST(MirVerifyTest, ACallableOfTheUnitIsCalledAtNoTypesAndAnswersItsResult) {
  const auto calling_itself = [](bool at_a_type, bool answering_void) {
    return UnitWithBody([=](CompilationUnit& u, Block& body) {
      body.AppendStmt(
          ExprStmt{
              .expr = body.exprs.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{
                                      .target =
                                          UnitCallableTarget{
                                              .slot = CallableId{0}},
                                      .type_arguments =
                                          at_a_type
                                              ? std::vector{u.builtins.int_type}
                                              : std::vector<TypeId>{}},
                              .arguments = {}},
                      .type = answering_void ? u.builtins.void_type
                                             : u.builtins.int_type})});
    });
  };

  EXPECT_NO_THROW(Verify(calling_itself(false, true)));
  ExpectRefused(
      calling_itself(true, true),
      "states a type argument to a callee generic over none");
  ExpectRefused(
      calling_itself(false, false), "calls a callable that answers type");
}

// A placement new-expression makes its value in storage its allocation
// function hands out and owns, so it answers a pointer owning nothing, from a
// function that hands storage out -- the one its argument finds, as C++ finds
// `operator new` by the types of the placement arguments -- and nothing but
// such an expression reaches that function.
TEST(MirVerifyTest, APlacementMakesItsValueInStorageItDoesNotOwn) {
  const auto placing = [](support::BuiltinFn allocate,
                          PointerOwnership answered) {
    return UnitWithBody([=](CompilationUnit& u, Block& body) {
      const ExprId keeper = body.exprs.Add(
          Expr{.data = NullLiteral{}, .type = u.builtins.deferred_effects_ptr});
      const ExprId value = body.exprs.Add(
          Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
      body.AppendStmt(
          ExprStmt{
              .expr = body.exprs.Add(MakePlacementNewExpr(
                  value,
                  PlacementAllocation{
                      .allocate = allocate, .arguments = {keeper}},
                  u.types.Intern(
                      Type{PointerType{
                          .pointee = u.builtins.int_type,
                          .ownership = answered}})))});
    });
  };

  EXPECT_NO_THROW(Verify(placing(
      support::BuiltinFn::kDeferredEffectsAllocate,
      PointerOwnership::kBorrowed)));
  ExpectRefused(
      placing(
          support::BuiltinFn::kDeferredEffectsAllocate,
          PointerOwnership::kUnique),
      "places a value but answers other than a pointer owning nothing");
  ExpectRefused(
      placing(support::BuiltinFn::kNbaEffects, PointerOwnership::kBorrowed),
      "names an allocation function other than the one its argument "
      "allocates by");
  ExpectRefused(
      placing(
          support::BuiltinFn::kViolationReportsAllocate,
          PointerOwnership::kBorrowed),
      "names an allocation function other than the one its argument "
      "allocates by");

  const CompilationUnit calling =
      UnitWithBody([](CompilationUnit& u, Block& body) {
        body.AppendStmt(
            ExprStmt{
                .expr = body.exprs.Add(
                    Expr{
                        .data =
                            CallExpr{
                                .callee =
                                    Direct{
                                        .target = support::BuiltinFn::
                                            kDeferredEffectsAllocate},
                                .arguments = {}},
                        .type = u.builtins.void_type})});
      });
  ExpectRefused(
      calling,
      "calls an allocation function, which only a new-expression reaches");
}

// A keeper runs the effect committed to it once the frame that made the effect
// has gone on, so an effect made in a keeper's storage holds no reference to
// storage of that frame, where a copy of the same value is what it may hold.
TEST(MirVerifyTest, AnEffectMadeInAKeeperRefersToNothingItsFrameEnds) {
  const auto committing = [](bool captures_a_reference) {
    return UnitWithCode([=](CompilationUnit& u, CallableCode& code) {
      const TypeId value_type = u.builtins.int_type;
      const TypeId captured =
          captures_a_reference ? u.types.Intern(
                                     Type{ReferenceType{
                                         .referent = value_type,
                                         .mutability = Mutability::kReadOnly}})
                               : value_type;
      const ClosureId id = u.DeclareClosure();
      const TypeId effect_type =
          u.types.Intern(Type{ClosureType{.closure_id = id}});
      ClosureDecl effect;
      const FieldId field = effect.fields.Add(FieldDecl{.type = captured});
      effect.field_order = {field};
      effect.invoke.receiver = effect.invoke.AddLocal(u.types.Intern(
          Type{PointerType{
              .pointee = effect_type,
              .ownership = PointerOwnership::kUnique,
              .mutability = Mutability::kMutable}}));
      effect.invoke.result_type = u.builtins.void_type;
      effect.invoke.Body().AppendStmt(ReturnStmt{.value = std::nullopt});
      u.DefineClosure(id, std::move(effect));

      Block& body = code.Body();
      const LocalId value = code.AddLocal(value_type);
      body.AppendStmt(
          LocalDeclStmt{
              .target = value,
              .init = body.exprs.Add(
                  Expr{.data = NullLiteral{}, .type = value_type})});
      const TypeId keeper_pointer = u.builtins.deferred_effects_ptr;
      const LocalId keeper = code.AddLocal(keeper_pointer);
      body.AppendStmt(
          LocalDeclStmt{
              .target = keeper,
              .init = body.exprs.Add(
                  Expr{.data = NullLiteral{}, .type = keeper_pointer})});
      const ExprId capture =
          captures_a_reference
              ? body.exprs.Add(
                    Expr{
                        .data =
                            BorrowExpr{
                                .operand = body.LocalPlace(value, value_type)},
                        .type = captured})
              : body.ReadLocal(value, value_type);
      const ExprId made = body.exprs.Add(MakePlacementNewExpr(
          body.exprs.Add(
              Expr{
                  .data =
                      ClosureExpr{
                          .closure = id,
                          .field_inits = {{.target = field, .value = capture}}},
                  .type = effect_type}),
          PlacementAllocation{
              .allocate = support::BuiltinFn::kDeferredEffectsAllocate,
              .arguments = {body.ReadLocal(keeper, keeper_pointer)}},
          u.types.Intern(
              Type{PointerType{
                  .pointee = effect_type,
                  .ownership = PointerOwnership::kBorrowed,
                  .mutability = Mutability::kMutable}})));
      body.AppendStmt(
          ExprStmt{
              .expr = body.exprs.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{
                                      .target = support::BuiltinFn::
                                          kDeferredEffectsCommit,
                                      .receiver = body.Deref(
                                          u.types,
                                          body.LocalPlace(
                                              keeper, keeper_pointer),
                                          u.types.Get(keeper_pointer)
                                              .Get<PointerType>()
                                              .pointee)},
                              .arguments = {made}},
                      .type = u.builtins.void_type})});
    });
  };

  EXPECT_NO_THROW(Verify(committing(false)));
  ExpectRefused(
      committing(true),
      "is kept past its maker's end yet captures in field 0 a reference to "
      "storage of the frame that made it");
}

// An entry another unit defines is declared here and holds nothing this unit
// emits: a record states none of its methods, a subroutine has no body, and
// each is named as that unit published it, by a unit other than this one.
TEST(MirVerifyTest, AnEntryAnotherUnitDefinesHoldsNothingOfItsOwn) {
  struct Declared {
    bool own_unit = false;
    bool record_methods = false;
    bool subroutine_body = false;
    bool named = true;
  };
  const auto unit_declaring = [](Declared declared) {
    return UnitWithBody([&](CompilationUnit& u, Block&) {
      const std::string defining = declared.own_unit ? u.name : "Pkg";
      u.records.Add(
          RecordDecl{
              .defining_unit = defining,
              .path = {},
              .kind = RecordKind::kStruct,
              .elements = {}});
      u.record_methods.Declare();
      u.record_methods.Define(
          RecordId{0}, declared.record_methods
                           ? RecordMethods{RecordMethod{
                                 .answers = support::ValueOperator::kEquality,
                                 .code = CallableCode::Defined()}}
                           : RecordMethods{});
      const CallableId subroutine = u.callables.Add(
          CallableDecl{
              .code = declared.subroutine_body ? CallableCode::Defined()
                                               : CallableCode{},
              .foreign = std::nullopt,
              .virtual_dispatch = std::nullopt,
              .defining_unit = defining});
      const StaticVariableId variable = u.static_variables.Add(
          StaticVariableDecl{
              .type = u.builtins.int_type, .defining_unit = defining});
      if (declared.named) {
        u.named_callables.push_back(
            NamedCallable{.name = "g", .body = subroutine});
        u.named_static_variables.push_back(
            NamedStaticVariable{.name = "v", .variable = variable});
      }
    });
  };

  EXPECT_NO_THROW(Verify(unit_declaring({})));
  ExpectRefused(
      unit_declaring({.own_unit = true}),
      "declares as another unit's its own record");
  ExpectRefused(
      unit_declaring({.record_methods = true}),
      "states methods of its own for the record");
  ExpectRefused(
      unit_declaring({.subroutine_body = true}),
      "holds a body for a subroutine another unit defines");
  ExpectRefused(
      unit_declaring({.named = false}),
      "declares with no name a subroutine another unit defines");
}

// A class another unit defines is declared here once, under that unit's name,
// and holds nothing this unit emits; a method of a class this unit defines and
// a subroutine of its own namespace have their bodies here, since no other
// unit defines one.
TEST(MirVerifyTest, ADeclaredClassHoldsNothingAndAnOwnBodyIsHere) {
  struct Declared {
    bool own_unit = false;
    bool twice = false;
    bool method_body = false;
    bool object_record = false;
    bool own_method_without_body = false;
    bool own_subroutine_without_body = false;
  };
  const auto unit_declaring = [](Declared declared) {
    return UnitWithBody([&](CompilationUnit& u, Block&) {
      const auto declare = [&] {
        Class cls;
        cls.defining_unit = declared.own_unit ? u.name : "Pkg";
        cls.identity = support::DefPath{
            .data = {
                support::ClassStep{.name = "C", .arguments = std::nullopt}}};
        cls.base = ClassRef{ManagedObjectRootRef{}};
        if (declared.method_body) {
          cls.callables.Add(
              CallableDecl{
                  .code = CallableCode::Defined(),
                  .foreign = std::nullopt,
                  .virtual_dispatch = std::nullopt,
                  .defining_unit = std::nullopt});
        }
        if (declared.object_record) {
          cls.object_definition_initializer.value =
              cls.object_definition_initializer.body.exprs.Add(
                  Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
        }
        u.DefineClass(u.DeclareClass(), std::move(cls));
      };
      declare();
      if (declared.twice) declare();
      if (declared.own_method_without_body) {
        Class own;
        own.base = ClassRef{ManagedObjectRootRef{}};
        own.callables.Add(
            CallableDecl{
                .code = CallableCode{},
                .foreign = std::nullopt,
                .virtual_dispatch = std::nullopt,
                .defining_unit = std::nullopt});
        u.DefineClass(u.DeclareClass(), std::move(own));
      }
      if (declared.own_subroutine_without_body) {
        u.callables.Add(
            CallableDecl{
                .code = CallableCode{},
                .foreign = std::nullopt,
                .virtual_dispatch = std::nullopt,
                .defining_unit = std::nullopt});
      }
    });
  };

  EXPECT_NO_THROW(Verify(unit_declaring({})));
  ExpectRefused(
      unit_declaring({.own_unit = true}),
      "declares as another unit's its own class");
  ExpectRefused(unit_declaring({.twice = true}), "declares twice the class");
  ExpectRefused(
      unit_declaring({.method_body = true}),
      "holds code or data of its own for the class");
  ExpectRefused(
      unit_declaring({.object_record = true}),
      "holds code or data of its own for the class");
  ExpectRefused(
      unit_declaring({.own_method_without_body = true}),
      "leaves with no body a method of a class it defines");
  ExpectRefused(
      unit_declaring({.own_subroutine_without_body = true}),
      "leaves with no body its own subroutine");
}

}  // namespace
}  // namespace lyra::mir

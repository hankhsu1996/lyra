#include "lyra/mir/verify.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/abi/library_records.hpp"
#include "lyra/abi/records.hpp"
#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/builder.hpp"
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
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/constant_value.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/library_record_builder.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/class_name.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/library_decl.hpp"
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
  CallableCode code = CallableCode::Defined(diag::SourceSpan{});
  code.result_type = result;
  Block inner;
  Builder inner_at(inner, diag::SourceSpan{});
  const ExprId park = inner_at.Add(
      Expr{
          .data = MachineBoolLiteral{.value = true},
          .type = unit.builtins.machine_bool});
  const ExprId wait = inner_at.Add(
      Expr{.data = WaitExpr{.park = park}, .type = unit.builtins.void_type});
  inner_at.AppendStmt(ExprStmt{.expr = wait});
  const BlockId scope = code.Body().child_scopes.Add(std::move(inner));
  Builder code_body(code.Body(), diag::SourceSpan{});
  code_body.AppendStmt(BlockStmt{.scope = scope});
  const CallableId id = unit.AddNamespaceCallable(std::move(code));
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
  CallableCode code = CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.coroutine_void;
  Builder code_body(code.Body(), diag::SourceSpan{});
  const ExprId answer = code_body.Add(
      Expr{
          .data = MachineBoolLiteral{.value = true},
          .type = unit.builtins.machine_bool});
  const ExprId await = code_body.Add(
      Expr{
          .data = AwaitExpr{.execution = answer},
          .type = unit.builtins.void_type});
  code_body.AppendStmt(ExprStmt{.expr = await});
  unit.AddNamespaceCallable(std::move(code));
  try {
    Verify(unit);
    FAIL() << "an await on a park's answer was accepted";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(message.find("not an execution"), std::string::npos) << message;
  }
}

// A unit whose one body, `f`, is the code `fill` writes into it.
template <typename Fill>
auto UnitWithCode(const Fill& fill) -> CompilationUnit {
  CompilationUnit unit;
  unit.name = "U";
  CallableCode code = CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  fill(unit, code);
  const CallableId id = unit.AddNamespaceCallable(std::move(code));
  unit.named_callables.push_back(NamedCallable{.name = "f", .body = id});
  return unit;
}

// A fixed-size array of four `element`s, whose element is reached by a step
// its layout places rather than by a body of its type.
auto FourOf(const TypePool& types, TypeId element) -> TypeId {
  return types.Intern(
      Type{UnpackedArrayType{.element_type = element, .count = 4}});
}

// A unit whose one body, `f`, is the statements `fill` writes into it.
template <typename Fill>
auto UnitWithBody(const Fill& fill) -> CompilationUnit {
  return UnitWithCode([&](CompilationUnit& unit, CallableCode& code) {
    fill(unit, Builder(code.Body(), diag::SourceSpan{}));
  });
}

// Declares, as the record `record`'s one method, the one answering equality,
// whose code is `code`.
void DeclareEquality(
    CompilationUnit& unit, RecordId record, CallableCode code) {
  unit.AddRecordMethod(record, support::EqualityItem::kEqual, std::move(code));
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

auto AddCall(const CompilationUnit& unit, Builder block) -> ExprId {
  return block.Add(MakeCurrentRuntimeCallExpr(unit.builtins.effects));
}

auto AddSum(const CompilationUnit& unit, Builder block, ExprId lhs, ExprId rhs)
    -> ExprId {
  return block.Add(
      Expr{
          .data = BinaryExpr{.op = BinaryOp::kAdd, .lhs = lhs, .rhs = rhs},
          .type = unit.builtins.int_type});
}

// A node that computes without running code, so the order of two places
// reaching it is not what a check of it asks about.
auto AddComputation(const CompilationUnit& unit, Builder block) -> ExprId {
  return block.Add(
      Expr{
          .data =
              UnaryExpr{
                  .op = UnaryOp::kMinus,
                  .operand = block.Add(
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
      UnitWithBody([](const CompilationUnit& u, Builder body) {
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
  const auto conditional = [](Builder body, ExprId condition, ExprId then_value,
                              ExprId else_value) {
    return body.Add(
        Expr{
            .data =
                ConditionalExpr{
                    .condition = condition,
                    .then_value = then_value,
                    .else_value = else_value},
            .type = body.Get(then_value).type});
  };
  const auto flag = [](const CompilationUnit& u, Builder body) {
    return body.Add(
        Expr{
            .data = MachineBoolLiteral{.value = true},
            .type = u.builtins.machine_bool});
  };

  EXPECT_NO_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Builder body) {
        const ExprId call = AddCall(u, body);
        const ExprId inner =
            conditional(body, flag(u, body), call, AddCall(u, body));
        body.AppendStmt(
            ExprStmt{.expr = conditional(body, flag(u, body), call, inner)});
      })));

  EXPECT_TRUE(RefusedFor(
      UnitWithBody([&](const CompilationUnit& u, Builder body) {
        const ExprId computed = AddComputation(u, body);
        const ExprId sum = AddSum(u, body, computed, computed);
        body.AppendStmt(
            ExprStmt{
                .expr = conditional(
                    body, flag(u, body), sum,
                    body.Add(
                        Expr{
                            .data = NullLiteral{},
                            .type = body.Get(sum).type}))});
      }),
      "one run evaluates it at two of them"));

  EXPECT_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Builder body) {
        const ExprId test = body.Add(
            Expr{
                .data = CastExpr{.operand = AddCall(u, body)},
                .type = u.builtins.machine_bool});
        const ExprId as_value = body.Add(
            Expr{
                .data = CastExpr{.operand = test},
                .type = u.builtins.int_type});
        body.AppendStmt(
            ExprStmt{
                .expr = conditional(
                    body, test, as_value,
                    body.Add(
                        Expr{
                            .data = NullLiteral{},
                            .type = u.builtins.int_type}))});
      })),
      InternalError);
}

// A node that names a thing, spells a constant, forms a place or reads a value
// where it lies in storage only a binding of the body reaches -- a local, a
// member of one, what a box a local holds owns -- computes nothing itself, so
// it may stand at two places, and what counts is what it reaches: two reads of
// a place formed over a computation reach that computation at each. A read of
// any other storage computes, since a write between two of them would make
// them two values, such as what a pointer owning nothing reaches. A field step
// on an untagged union reaches its storage, which is storage of the record's
// own like a structure's member, so a read of it computes nothing either.
TEST(MirVerifyTest, ANodeThatComputesNothingMayStandAtSeveralPlaces) {
  EXPECT_NO_THROW(
      Verify(UnitWithBody([](const CompilationUnit& u, Builder body) {
        const ExprId literal = body.Add(
            Expr{
                .data = MachineIntLiteral{.value = 1},
                .type = u.builtins.int_type});
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, literal, literal)});
      })));

  EXPECT_NO_THROW(
      Verify(UnitWithCode([](CompilationUnit& u, CallableCode& code) {
        const LocalId local =
            code.locals.Add(LocalDecl{.type = u.builtins.int_type});
        Builder body(code.Body(), diag::SourceSpan{});
        const ExprId read = body.ReadLocal(local, u.builtins.int_type);
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, read, read)});
      })));

  const auto place_over = [](const CompilationUnit& u, Builder body,
                             ExprId pointer) {
    return body.Add(MakeDerefExpr(pointer, u.builtins.int_type));
  };
  const auto null = [](const CompilationUnit& u, Builder body) {
    return body.Add(Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
  };
  EXPECT_NO_THROW(Verify(UnitWithBody([&](const CompilationUnit& u,
                                          Builder body) {
    const ExprId place = place_over(u, body, null(u, body));
    body.AppendStmt(
        ExprStmt{.expr = AddSum(u, body, body.Read(place), body.Read(place))});
  })));
  EXPECT_TRUE(RefusedFor(
      UnitWithBody([&](const CompilationUnit& u, Builder body) {
        const ExprId read = body.Read(place_over(u, body, null(u, body)));
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, read, read)});
      }),
      "one run evaluates it at two of them"));
  EXPECT_TRUE(RefusedFor(
      UnitWithBody([&](const CompilationUnit& u, Builder body) {
        const ExprId place = place_over(u, body, AddComputation(u, body));
        body.AppendStmt(
            ExprStmt{
                .expr = AddSum(u, body, body.Read(place), body.Read(place))});
      }),
      "one run evaluates it at two of them"));

  const auto read_through_local = [](PointerOwnership ownership) {
    return UnitWithCode([ownership](CompilationUnit& u, CallableCode& code) {
      const TypeId pointer = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.int_type, .ownership = ownership}});
      const LocalId local = code.locals.Add(LocalDecl{.type = pointer});
      Builder body(code.Body(), diag::SourceSpan{});
      const ExprId read = body.Read(body.Deref(
          u.types, body.LocalPlace(local, pointer), u.builtins.int_type));
      body.AppendStmt(ExprStmt{.expr = AddSum(u, body, read, read)});
    });
  };
  EXPECT_NO_THROW(Verify(read_through_local(PointerOwnership::kUnique)));
  EXPECT_TRUE(RefusedFor(
      read_through_local(PointerOwnership::kBorrowed),
      "one run evaluates it at two of them"));

  const auto member_of_local = [](bool untagged_union) {
    return UnitWithCode([untagged_union](
                            CompilationUnit& u, CallableCode& code) {
      const TypeId held = u.builtins.int_type;
      const RecordId record = u.records.Add(
          RecordDecl{
              .defining_unit = std::nullopt,
              .path = {},
              .shape = untagged_union ? decltype(RecordDecl::shape){UnionRecord{
                                            .storage = held, .members = {held}}}
                                      : decltype(RecordDecl::shape){
                                            StructRecord{.members = {held}}}});
      const TypeId type =
          u.types.Intern(Type{RecordType{.declaration = record}});
      const LocalId local = code.locals.Add(LocalDecl{.type = type});
      Builder body(code.Body(), diag::SourceSpan{});
      const ExprId member = body.Add(
          Expr{
              .data =
                  FieldAccessExpr{
                      .receiver = body.LocalPlace(local, type),
                      .field =
                          ComponentFieldTarget{
                              .position = base::ComponentIndex{0}}},
              .type = u.builtins.int_type});
      const ExprId read = body.Read(member);
      body.AppendStmt(ExprStmt{.expr = AddSum(u, body, read, read)});
    });
  };
  EXPECT_NO_THROW(Verify(member_of_local(false)));
  EXPECT_NO_THROW(Verify(member_of_local(true)));
}

// Two operands that each run code can be run in the other order to another
// answer, so a legalized unit keeping both under one node is refused, and the
// refusal names the node and the operand that has to run first; two without
// side effects stay where they are.
TEST(MirVerifyTest, TwoOperandsATargetCouldReorderAreRefused) {
  EXPECT_TRUE(RefusedFor(
      UnitWithBody([](const CompilationUnit& u, Builder body) {
        body.AppendStmt(
            ExprStmt{
                .expr = AddSum(u, body, AddCall(u, body), AddCall(u, body))});
      }),
      "an operand a target could run in the other order to another answer"));
  EXPECT_NO_THROW(Verify(UnitWithBody([](const CompilationUnit& u,
                                         Builder body) {
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
  const auto storage = [](const CompilationUnit& u, Builder body) {
    const ExprId null =
        body.Add(Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
    return body.Add(MakeDerefExpr(null, u.builtins.int_type));
  };

  EXPECT_NO_THROW(Verify(UnitWithBody([&](const CompilationUnit& u,
                                          Builder body) {
    const ExprId place = storage(u, body);
    body.AppendStmt(
        ExprStmt{.expr = AddSum(u, body, body.Read(place), body.Read(place))});
  })));
  ExpectRefused(
      UnitWithBody([&](const CompilationUnit& u, Builder body) {
        const ExprId place = storage(u, body);
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, place, place)});
      }),
      "a place stands as one only under a read");
  ExpectRefused(
      UnitWithBody([&](const CompilationUnit& u, Builder body) {
        const ExprId literal = body.Add(
            Expr{
                .data = MachineIntLiteral{.value = 1},
                .type = u.builtins.int_type});
        body.AppendStmt(
            ExprStmt{
                .expr = body.Add(MakeCopyExpr(literal, u.builtins.int_type))});
      }),
      "names no place");
  ExpectRefused(
      UnitWithBody([&](const CompilationUnit& u, Builder body) {
        const ExprId read = body.Read(storage(u, body));
        body.AppendStmt(
            ExprStmt{
                .expr = body.Add(
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
    return UnitWithBody([ownership, read](CompilationUnit& u, Builder body) {
      const TypeId pointer = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.int_type, .ownership = ownership}});
      const TypeId to_pointer = u.types.Intern(
          Type{PointerType{
              .pointee = pointer, .ownership = PointerOwnership::kBorrowed}});
      const ExprId null =
          body.Add(Expr{.data = NullLiteral{}, .type = to_pointer});
      const ExprId held = body.Add(MakeDerefExpr(null, pointer));
      const ExprId opened = body.Add(
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
      UnitWithBody([](const CompilationUnit& u, Builder body) {
        const ExprId literal = body.Add(
            Expr{
                .data = MachineIntLiteral{.value = 1},
                .type = u.builtins.int_type});
        body.AppendStmt(
            ExprStmt{
                .expr = body.Add(
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
      Verify(UnitWithBody([](const CompilationUnit& u, Builder body) {
        Block steps;
        Builder steps_at(steps, diag::SourceSpan{});
        const ExprId call = AddCall(u, steps_at);
        steps_at.AppendStmt(ExprStmt{.expr = call});
        const ExprId literal = steps_at.Add(
            Expr{
                .data = MachineIntLiteral{.value = 1},
                .type = u.builtins.int_type});
        const ExprId value = AddSum(u, steps_at, literal, literal);
        const BlockId scope = body.AddScope(std::move(steps));
        body.AppendStmt(
            ExprStmt{
                .expr = body.Add(
                    Expr{
                        .data = BlockExpr{.scope = scope, .value = value},
                        .type = u.builtins.int_type})});
      })));

  EXPECT_THROW(
      Verify(UnitWithBody([](const CompilationUnit& u, Builder body) {
        Block steps;
        Builder steps_at(steps, diag::SourceSpan{});
        const ExprId call = AddCall(u, steps_at);
        steps_at.AppendStmt(ExprStmt{.expr = call});
        const BlockId scope = body.AddScope(std::move(steps));
        body.AppendStmt(
            ExprStmt{
                .expr = body.Add(
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
    return UnitWithBody([dereferenced](CompilationUnit& u, Builder body) {
      const TypeId pointer_type = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.int_type,
              .ownership = PointerOwnership::kBorrowed}});
      Block inner;
      Builder inner_at(inner, diag::SourceSpan{});
      const ExprId pointer =
          inner_at.Add(Expr{.data = NullLiteral{}, .type = pointer_type});
      const ExprId receiver =
          dereferenced
              ? inner_at.Add(MakeDerefExpr(pointer, u.builtins.int_type))
              : pointer;
      inner_at.AppendStmt(
          ExprStmt{
              .expr = inner_at.Read(inner_at.Add(MakeFieldAccessExpr(
                  receiver,
                  ClassFieldTarget{
                      .owner = ClassId{.value = 0},
                      .slot = FieldId{.value = 0}},
                  u.builtins.int_type)))});
      const BlockId scope = body.AddScope(std::move(inner));
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
    return UnitWithBody([dereferenced](CompilationUnit& u, Builder body) {
      const TypeId pointer_type = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.int_type,
              .ownership = PointerOwnership::kBorrowed}});
      const ExprId pointer =
          body.Add(Expr{.data = NullLiteral{}, .type = pointer_type});
      const ExprId receiver =
          dereferenced ? body.Add(MakeDerefExpr(pointer, u.builtins.int_type))
                       : pointer;
      // The member function called, of a class of the unit, entered on its
      // object alone, with a body that does nothing.
      Class cls;
      CallableCode member = CallableCode::Defined(diag::SourceSpan{});
      member.result_type = u.builtins.void_type;
      const ClassId owner = u.DeclareClass();
      const CallableId called =
          u.AddClassCallable(owner, std::nullopt, std::move(member));
      u.DefineClass(owner, std::move(cls));
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{
                                      .target =
                                          CallableTarget{.callable = called},
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
    return UnitWithBody([=](CompilationUnit& u, Builder body) {
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
      const ExprId target = body.Add(MakeDerefExpr(
          body.Add(Expr{.data = NullLiteral{}, .type = to_target}),
          target_type));
      // A store hands its target a value of the target's own type, and a
      // binding hands it the reference it binds.
      const ExprId bound = body.Add(
          Expr{
              .data = NullLiteral{},
              .type = through_store ? target_type : reference_type});
      const ExprId operation =
          through_store
              ? body.Add(MakeAssignExpr(u.builtins, target, bound))
              : body.Add(MakeCallExpr(
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

// A local referring to storage it does not own refers to what its declaration
// bound it to, so a store re-seating one is refused, except where it is a
// loop's cursor that the loop's step advances, as an iterator is.
TEST(MirVerifyTest, ALocalBorrowingStorageIsReseatedOnlyAsALoopsCursor) {
  const auto reseated = [](bool by_the_step) {
    return UnitWithCode([=](CompilationUnit& u, CallableCode& code) {
      const TypeId borrowed = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.int_type,
              .ownership = PointerOwnership::kBorrowed}});
      const LocalId cursor = code.AddLocal(borrowed);
      Builder body(code.Body(), diag::SourceSpan{});
      body.AppendStmt(
          LocalDeclStmt{
              .target = cursor,
              .init = body.Add(Expr{.data = NullLiteral{}, .type = borrowed})});
      const ExprId advance = body.Add(MakeAssignExpr(
          u.builtins, body.LocalPlace(cursor, borrowed),
          body.Add(Expr{.data = NullLiteral{}, .type = borrowed})));
      if (by_the_step) {
        body.AppendStmt(
            ForStmt{
                .init = {},
                .condition = std::nullopt,
                .step = {advance},
                .scope = body.AddScope(Block{})});
        return;
      }
      body.AppendStmt(ExprStmt{.expr = advance});
    });
  };

  EXPECT_NO_THROW(Verify(reseated(true)));
  ExpectRefused(
      reseated(false), "re-seats a local referring to storage it does not own");
}

// Whether a call of an entry reporting where it was written is handed that.
enum class WrittenAt : std::uint8_t { kHanded, kWithheld };

// The arguments a call of `entry` is handed: `argument`, and where the call
// is written where the entry reports there and `written_at` hands it.
auto ArgumentsOf(
    const CompilationUnit& u, Builder body, support::BuiltinFn entry,
    ExprId argument, WrittenAt written_at = WrittenAt::kHanded)
    -> std::vector<ExprId> {
  std::vector<ExprId> arguments{argument};
  if (support::RuntimeEntryOf(entry).takes_the_location &&
      written_at == WrittenAt::kHanded) {
    arguments.push_back(body.Add(
        Expr{.data = NullLiteral{}, .type = u.builtins.source_location}));
  }
  return arguments;
}

// An element of a numbered container is named by a position, so an index left
// in the type the source wrote it in is refused; the entry reaching an
// associative array takes a value of its index type, which is no ordinal (LRM
// 7.8).
TEST(MirVerifyTest, AnOrdinalIsStatedAsAPosition) {
  const auto element_of = [](bool keyed, bool as_position) {
    return UnitWithBody([keyed, as_position](CompilationUnit& u, Builder body) {
      const TypeId element = u.builtins.int_type;
      const TypeId container =
          keyed ? AssociativeArrayOf(u.types, u.builtins.int_type, element)
                : FourOf(u.types, element);
      const support::BuiltinFn entry =
          keyed ? support::BuiltinFn::kAssocReferElement
                : support::BuiltinFn::kElement;
      const TypeId index_type =
          as_position ? PositionType(u.types) : u.builtins.int_type;
      const ExprId array =
          body.Add(Expr{.data = NullLiteral{}, .type = container});
      const ExprId at =
          body.Add(Expr{.data = NullLiteral{}, .type = index_type});
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{.target = entry, .receiver = array},
                              .arguments = ArgumentsOf(u, body, entry, at)},
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
// is the element default where its position names none (LRM 7.4.5), which a
// write through it would change for every read.
TEST(MirVerifyTest, AWriteLandsThroughAnElementsWriteForm) {
  const auto element_write = [](support::BuiltinFn entry, Mutability mutability,
                                bool as_reference) {
    return UnitWithBody([entry, mutability, as_reference](
                            CompilationUnit& u, Builder body) {
      const TypeId element = u.builtins.int_type;
      const TypeId container = FourOf(u.types, element);
      const ExprId array =
          body.Add(Expr{.data = NullLiteral{}, .type = container});
      const ExprId at =
          body.Add(Expr{.data = NullLiteral{}, .type = PositionType(u.types)});
      const TypeId reference = u.types.Intern(
          Type{ReferenceType{.referent = element, .mutability = mutability}});
      const ExprId reached = body.Add(
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
      const ExprId target = body.Add(MakeDerefExpr(reached, element));
      const ExprId value =
          body.Add(Expr{.data = NullLiteral{}, .type = element});
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(MakeAssignExpr(u.builtins, target, value))});
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
                            CompilationUnit& u, Builder body) {
      const TypeId element = u.builtins.int_type;
      const TypeId row = FourOf(u.types, element);
      const TypeId rows = FourOf(u.types, row);
      const ExprId array = body.Add(Expr{.data = NullLiteral{}, .type = rows});
      const auto position = [&] {
        return body.Add(
            Expr{.data = NullLiteral{}, .type = PositionType(u.types)});
      };
      const ExprId row_reference = body.Add(
          Expr{
              .data =
                  CallExpr{
                      .callee = Direct{.target = inner, .receiver = array},
                      .arguments = {position()}},
              .type = u.types.Intern(
                  Type{ReferenceType{
                      .referent = row, .mutability = inner_mutability}})});
      const ExprId row_place = body.Add(MakeDerefExpr(row_reference, row));
      const ExprId element_reference = body.Add(
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
      const ExprId target = body.Add(MakeDerefExpr(element_reference, element));
      const ExprId value =
          body.Add(Expr{.data = NullLiteral{}, .type = element});
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(MakeAssignExpr(u.builtins, target, value))});
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
// the element default every read shares (LRM 7.4.5).
TEST(MirVerifyTest, BitsAreWrittenThroughAnElementsWriteForm) {
  const auto bits_written = [](support::BuiltinFn entry,
                               Mutability mutability) {
    return UnitWithBody([entry, mutability](CompilationUnit& u, Builder body) {
      const TypeId byte =
          PackedVectorOf(u.types, 8, IntegralStateKind::kFourState);
      const TypeId nibble =
          PackedVectorOf(u.types, 4, IntegralStateKind::kFourState);
      const TypeId container = FourOf(u.types, byte);
      const ExprId array =
          body.Add(Expr{.data = NullLiteral{}, .type = container});
      const ExprId at =
          body.Add(Expr{.data = NullLiteral{}, .type = PositionType(u.types)});
      const ExprId element = body.Add(MakeDerefExpr(
          body.Add(
              Expr{
                  .data =
                      CallExpr{
                          .callee = Direct{.target = entry, .receiver = array},
                          .arguments = {at}},
                  .type = u.types.Intern(
                      Type{ReferenceType{
                          .referent = byte, .mutability = mutability}})}),
          byte));
      const ExprId bits = body.Add(
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
          body.Add(Expr{.data = NullLiteral{}, .type = nibble});
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
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
    return UnitWithBody([read_first](CompilationUnit& u, Builder body) {
      const TypeId byte =
          PackedVectorOf(u.types, 8, IntegralStateKind::kFourState);
      const TypeId pointer = u.types.Intern(
          Type{PointerType{
              .pointee = byte, .ownership = PointerOwnership::kBorrowed}});
      const ExprId place = body.Add(MakeDerefExpr(
          body.Add(Expr{.data = NullLiteral{}, .type = pointer}), byte));
      const ExprId bound = read_first ? body.Read(place) : place;
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
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
    const auto& answer_of, WrittenAt written_at = WrittenAt::kHanded)
    -> CompilationUnit {
  return UnitWithBody([&](CompilationUnit& u, Builder body) {
    const ExprId receiver =
        body.Add(Expr{.data = NullLiteral{}, .type = container_of(u)});
    const ExprId argument =
        body.Add(Expr{.data = NullLiteral{}, .type = argument_of(u)});
    body.AppendStmt(
        ExprStmt{
            .expr = body.Add(
                Expr{
                    .data =
                        CallExpr{
                            .callee =
                                Direct{.target = entry, .receiver = receiver},
                            .arguments = ArgumentsOf(
                                u, body, entry, argument, written_at)},
                    .type = answer_of(u)})});
  });
}

// An entry reaching by a position is called on something that numbers its
// parts, and one reaching by a key on an associative array (LRM 7.8).
TEST(MirVerifyTest, AnEntryIsCalledOnTheKindOfContainerItReaches) {
  const auto element = [](CompilationUnit& u) { return u.builtins.int_type; };
  const auto keyed = [](CompilationUnit& u) {
    return AssociativeArrayOf(
        u.types, u.builtins.int_type, u.builtins.int_type);
  };
  const auto numbered = [](CompilationUnit& u) {
    return FourOf(u.types, u.builtins.int_type);
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
  EXPECT_NO_THROW(Verify(UnitCalling(
      support::BuiltinFn::kAssocReferElement, keyed, element, read)));
  ExpectRefused(
      UnitCalling(support::BuiltinFn::kElement, keyed, position, read),
      "reaches by a position into an associative array");
  ExpectRefused(
      UnitCalling(
          support::BuiltinFn::kAssocReferElement, numbered, element, read),
      "reaches by a key into something that is no associative array");
}

// A run-time-sized container's own operations are its type's bodies, so a call
// of the library's entry naming one on such a container is refused, whether the
// entry names an item of a trait the type implements or one of the type's own
// methods; the same entries on a fixed-size array, whose operations no core
// keeps, are the library's to answer.
TEST(MirVerifyTest, TheLibraryIsNotAskedWhatAContainerStatesAsItsOwn) {
  const auto queue = [](CompilationUnit& u) {
    return UnboundedQueueOf(u.types, u.builtins.int_type);
  };
  const auto numbered = [](CompilationUnit& u) {
    return FourOf(u.types, u.builtins.int_type);
  };
  const auto index = [](CompilationUnit& u) {
    return u.builtins.machine_int64;
  };
  const auto read = [](CompilationUnit& u) {
    return u.types.Intern(
        Type{ReferenceType{
            .referent = u.builtins.int_type,
            .mutability = Mutability::kReadOnly}});
  };
  const auto count = [](CompilationUnit& u) { return u.builtins.int_type; };

  EXPECT_NO_THROW(Verify(
      UnitCalling(support::BuiltinFn::kGetUnchecked, numbered, index, read)));
  ExpectRefused(
      UnitCalling(support::BuiltinFn::kGetUnchecked, queue, index, read),
      "an operation that type states as its own");
  ExpectRefused(
      UnitCalling(support::BuiltinFn::kSize, queue, index, count),
      "an operation that type states as its own");
}

// A body declared not to unwind lets no unwind leave it: one calling only
// what cannot unwind itself is accepted -- an element read of a fixed-size
// array -- and one calling what may unwind -- a read through an associative
// array's key -- is refused, as the same call is accepted in a body declaring
// no such thing; and so is one placing a value in storage that an entry which
// may unwind hands out, where the reserved placement form, which cannot, is
// accepted.
TEST(MirVerifyTest, ABodyDeclaredNotToUnwindCallsOnlyWhatCannotUnwind) {
  const auto calling = [](support::BuiltinFn entry, bool keyed,
                          bool can_unwind) {
    return UnitWithCode([=](CompilationUnit& u, CallableCode& code) {
      code.can_unwind = can_unwind;
      Builder body(code.Body(), diag::SourceSpan{});
      const TypeId container =
          keyed ? AssociativeArrayOf(
                      u.types, u.builtins.int_type, u.builtins.int_type)
                : FourOf(u.types, u.builtins.int_type);
      const TypeId coordinate =
          keyed ? u.builtins.int_type : PositionType(u.types);
      const TypeId read = u.types.Intern(
          Type{ReferenceType{
              .referent = u.builtins.int_type,
              .mutability = Mutability::kReadOnly}});
      const ExprId receiver =
          body.Add(Expr{.data = NullLiteral{}, .type = container});
      const ExprId argument =
          body.Add(Expr{.data = NullLiteral{}, .type = coordinate});
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{.target = entry, .receiver = receiver},
                              .arguments =
                                  ArgumentsOf(u, body, entry, argument)},
                      .type = read})});
    });
  };
  using support::BuiltinFn;
  EXPECT_NO_THROW(Verify(calling(BuiltinFn::kElement, false, false)));
  EXPECT_NO_THROW(Verify(calling(BuiltinFn::kAssocReferElement, true, true)));
  ExpectRefused(
      calling(BuiltinFn::kAssocReferElement, true, false),
      "is declared not to unwind and makes");

  const auto placing = [](support::BuiltinFn allocate) {
    return UnitWithCode([=](CompilationUnit& u, CallableCode& code) {
      code.can_unwind = false;
      Builder body(code.Body(), diag::SourceSpan{});
      const TypeId held = u.builtins.int_type;
      const TypeId place = u.types.Intern(
          Type{PointerType{
              .pointee = held, .ownership = PointerOwnership::kBorrowed}});
      const ExprId address =
          body.Add(Expr{.data = NullLiteral{}, .type = place});
      const ExprId value = body.Add(Expr{.data = NullLiteral{}, .type = held});
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(MakePlacementNewExpr(
                  value,
                  PlacementAllocation{
                      .allocate = allocate, .arguments = {address}},
                  place))});
    });
  };
  EXPECT_NO_THROW(Verify(placing(BuiltinFn::kDefaultPlacementNew)));
  ExpectRefused(
      placing(BuiltinFn::kAssocElement),
      "places a value in storage 'assoc_element' hands out, which may "
      "unwind");
}

// An entry the source states over values of several kinds is not named where
// another is stated for its first operand -- an integral value, or a queue,
// whose element write can grow it past its bound; an entry that can report is
// handed where it was written; and an entry reading an operand as a machine
// value is handed no integral one.
TEST(MirVerifyTest, ACallIsHeldToItsEntrysOneDeclaration) {
  const auto integral = [](CompilationUnit& u) { return u.builtins.int_type; };
  const auto string = [](CompilationUnit& u) { return u.builtins.string; };
  const auto bit = [](CompilationUnit& u) { return u.builtins.bit1; };
  const auto queue = [](CompilationUnit& u) {
    return QueueOf(u.types, u.builtins.int_type, support::UnboundedQueue{});
  };
  const auto keyed = [](CompilationUnit& u) {
    return AssociativeArrayOf(
        u.types, u.builtins.int_type, u.builtins.int_type);
  };
  const auto position = [](CompilationUnit& u) {
    return PositionType(u.types);
  };
  const auto write = [](CompilationUnit& u) {
    return u.types.Intern(
        Type{ReferenceType{
            .referent = u.builtins.int_type,
            .mutability = Mutability::kMutable}});
  };

  EXPECT_NO_THROW(
      Verify(UnitCalling(support::BuiltinFn::kCaseEqual, string, string, bit)));
  EXPECT_NO_THROW(Verify(UnitCalling(
      support::BuiltinFn::kIntegralCaseEqual, integral, integral, bit)));
  ExpectRefused(
      UnitCalling(support::BuiltinFn::kCaseEqual, integral, integral, bit),
      "where another is stated for its first operand");

  ExpectRefused(
      UnitCalling(support::BuiltinFn::kElementMut, queue, position, write),
      "where another is stated for its first operand");
  ExpectRefused(
      UnitCalling(
          support::BuiltinFn::kAssocReferElement, keyed, integral, write,
          WrittenAt::kWithheld),
      "is not handed where that is");

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
  const auto one = [](const CompilationUnit& u, Builder body) {
    return body.Add(MakeIntegralLiteral(
        u, u.builtins.int_type,
        IntegralConstant{.value_words = {1}, .state_words = {}}));
  };
  const auto computed = [](const CompilationUnit& u, Builder body) {
    return body.Add(Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
  };

  EXPECT_NO_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Builder body) {
        body.AppendStmt(
            ExprStmt{.expr = AddSum(u, body, one(u, body), computed(u, body))});
      })));
  ExpectRefused(
      UnitWithBody([&](const CompilationUnit& u, Builder body) {
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
    return UnitWithBody([by_new, same_type](CompilationUnit& u, Builder body) {
      const TypeId owner = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.int_type,
              .ownership = PointerOwnership::kUnique}});
      const ExprId value = body.Add(
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
      body.AppendStmt(ExprStmt{.expr = body.Add(allocation)});
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
    return UnitWithBody([by_new](CompilationUnit& u, Builder body) {
      const TypeId large = TooLargeForAFrame(u);
      const ExprId value = body.Add(Expr{.data = NullLiteral{}, .type = large});
      const ExprId kept =
          by_new
              ? body.Add(MakeNewExpr(
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
      Builder body(code.Body(), diag::SourceSpan{});
      const ExprId named =
          body.Add(MakeLocalRefExpr(local, u.builtins.int_type));
      const ExprId target =
          to_a_place
              ? named
              : AddSum(
                    u, body, body.Add(MakeCopyExpr(named, u.builtins.int_type)),
                    body.Add(MakeCopyExpr(named, u.builtins.int_type)));
      const ExprId value =
          body.Add(Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
                  Expr{
                      .data = AssignExpr{.target = target, .value = value},
                      .type = u.builtins.int_type})});
    });
  };

  EXPECT_NO_THROW(Verify(assigning(true)));
  ExpectRefused(assigning(false), "which names no place");
}

// A move hands a local's value on together with the local's end, so it is the
// last use of the whole local, as in Rust, run exactly once on every way the
// body runs that reaches that end: what names the local before it in the order
// the block runs is fine, nothing names it in the statement that moves it or
// after it, no value that can hold an address is built from it, and only
// blocks a run enters once whenever the one around them runs stand between the
// move and the local's declaration. A local named before the move -- one a
// statement the block holds last but runs first included -- and one moved in a
// block statement are accepted, and so is one whose address a local of a block
// ended before the move keeps; one named in the moving statement or after it,
// one a block expression declares and names again in the value it yields,
// one whose address a pointer local, a tuple local or an assignment keeps,
// and one moved under an arm of a choice, in a branch of an `if`, in a loop's
// body or in its test, are refused.
TEST(MirVerifyTest, AMoveIsTheLastUseOfAWholeLocal) {
  enum class Shape : std::uint8_t {
    kLastUse,
    kNamedBefore,
    kNamedFirstInRunOrder,
    kInABlockStatement,
    kNamedInTheMovingStatement,
    kNamedInItsBlocksValue,
    kNamedAfter,
    kAddressKept,
    kAddressKeptInANestedBlock,
    kAddressKeptInATuple,
    kAddressAssigned,
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
      Builder body(code.Body(), diag::SourceSpan{});
      // Declared by a block expression, whose value moves it and then names
      // it.
      if (shape == Shape::kNamedInItsBlocksValue) {
        const TypeId pair =
            u.types.Intern(Type{TupleType{.elements = {type, type}}});
        Block steps;
        Builder steps_at(steps, diag::SourceSpan{});
        steps_at.AppendStmt(
            LocalDeclStmt{
                .target = moved,
                .init =
                    steps_at.Add(Expr{.data = NullLiteral{}, .type = type})});
        const ExprId value = steps_at.Add(
            Expr{
                .data =
                    CompositeExpr{
                        .parts =
                            {steps_at.Add(
                                 Expr{
                                     .data =
                                         MoveExpr{
                                             .operand =
                                                 steps_at.Add(MakeLocalRefExpr(
                                                     moved, type))},
                                     .type = type}),
                             steps_at.Add(MakeCopyExpr(
                                 steps_at.Add(MakeLocalRefExpr(moved, type)),
                                 type))}},
                .type = pair});
        const BlockId scope = body.AddScope(std::move(steps));
        body.AppendStmt(
            LocalDeclStmt{
                .target = code.locals.Add(LocalDecl{.type = pair}),
                .init = body.Add(
                    Expr{
                        .data = BlockExpr{.scope = scope, .value = value},
                        .type = pair})});
        return;
      }
      body.AppendStmt(
          LocalDeclStmt{
              .target = moved,
              .init = body.Add(Expr{.data = NullLiteral{}, .type = type})});
      const auto copy_of_moved = [&](Builder at) {
        return at.Add(
            MakeCopyExpr(at.Add(MakeLocalRefExpr(moved, type)), type));
      };
      if (shape == Shape::kNamedBefore) {
        body.AppendStmt(ExprStmt{.expr = copy_of_moved(body)});
      }
      const TypeId pointer =
          BorrowedPointerTo(u.types, type, Mutability::kMutable);
      const auto address_of_moved = [&] {
        return body.Add(
            Expr{
                .data =
                    AddressOfExpr{
                        .operand = body.Add(MakeLocalRefExpr(moved, type))},
                .type = pointer});
      };
      if (shape == Shape::kAddressKept) {
        body.AppendStmt(
            LocalDeclStmt{
                .target = code.locals.Add(LocalDecl{.type = pointer}),
                .init = address_of_moved()});
      }
      if (shape == Shape::kAddressKeptInANestedBlock) {
        Block nested;
        Builder nested_at(nested, diag::SourceSpan{});
        nested_at.AppendStmt(
            LocalDeclStmt{
                .target = code.locals.Add(LocalDecl{.type = pointer}),
                .init = nested_at.Add(
                    Expr{
                        .data =
                            AddressOfExpr{
                                .operand = nested_at.Add(
                                    MakeLocalRefExpr(moved, type))},
                        .type = pointer})});
        body.AppendStmt(BlockStmt{.scope = body.AddScope(std::move(nested))});
      }
      if (shape == Shape::kAddressKeptInATuple) {
        const TypeId pair = u.types.Intern(
            Type{TupleType{.elements = {pointer, u.builtins.machine_bool}}});
        body.AppendStmt(
            LocalDeclStmt{
                .target = code.locals.Add(LocalDecl{.type = pair}),
                .init = body.Add(
                    Expr{
                        .data =
                            CompositeExpr{
                                .parts =
                                    {address_of_moved(),
                                     body.Add(
                                         Expr{
                                             .data = NullLiteral{},
                                             .type =
                                                 u.builtins.machine_bool})}},
                        .type = pair})});
      }
      if (shape == Shape::kAddressAssigned) {
        const LocalId kept = code.locals.Add(LocalDecl{.type = pointer});
        body.AppendStmt(
            LocalDeclStmt{
                .target = kept,
                .init =
                    body.Add(Expr{.data = NullLiteral{}, .type = pointer})});
        body.AppendStmt(
            ExprStmt{
                .expr = body.Add(
                    Expr{
                        .data =
                            AssignExpr{
                                .target =
                                    body.Add(MakeLocalRefExpr(kept, pointer)),
                                .value = address_of_moved()},
                        .type = u.builtins.void_type})});
      }
      if (shape == Shape::kNamedInTheMovingStatement) {
        Block steps;
        Builder steps_at(steps, diag::SourceSpan{});
        steps_at.AppendStmt(ExprStmt{.expr = copy_of_moved(steps_at)});
        const ExprId value = steps_at.Add(
            Expr{
                .data =
                    MoveExpr{
                        .operand = steps_at.Add(MakeLocalRefExpr(moved, type))},
                .type = type});
        const BlockId scope = body.AddScope(std::move(steps));
        body.AppendStmt(
            LocalDeclStmt{
                .target = taker,
                .init = body.Add(
                    Expr{
                        .data = BlockExpr{.scope = scope, .value = value},
                        .type = type})});
        return;
      }
      const bool inside = shape == Shape::kInABlockStatement ||
                          shape == Shape::kInABranch ||
                          shape == Shape::kInALoopBody;
      Block inner;
      Builder inner_at(inner, diag::SourceSpan{});
      Builder at = inside ? inner_at : body;
      ExprId taken = at.Add(
          Expr{
              .data =
                  MoveExpr{.operand = at.Add(MakeLocalRefExpr(moved, type))},
              .type = type});
      if (shape == Shape::kUnderAnArm) {
        taken = at.Add(
            Expr{
                .data =
                    ConditionalExpr{
                        .condition = at.Add(
                            Expr{
                                .data = NullLiteral{},
                                .type = u.builtins.machine_bool}),
                        .then_value = taken,
                        .else_value =
                            at.Add(Expr{.data = NullLiteral{}, .type = type})},
                .type = type});
      }
      if (shape == Shape::kInALoopTest) {
        body.AppendStmt(
            WhileStmt{.condition = taken, .scope = body.AddScope(Block{})});
        return;
      }
      at.AppendStmt(LocalDeclStmt{.target = taker, .init = taken});
      if (shape == Shape::kInALoopBody) {
        const ExprId condition = body.Add(
            Expr{.data = NullLiteral{}, .type = u.builtins.machine_bool});
        body.AppendStmt(
            WhileStmt{
                .condition = condition,
                .scope = body.AddScope(std::move(inner))});
      }
      if (shape == Shape::kInABlockStatement) {
        body.AppendStmt(BlockStmt{.scope = body.AddScope(std::move(inner))});
      }
      if (shape == Shape::kInABranch) {
        const ExprId condition = body.Add(
            Expr{.data = NullLiteral{}, .type = u.builtins.machine_bool});
        body.AppendStmt(
            IfStmt{
                .condition = condition,
                .then_scope = body.AddScope(std::move(inner)),
                .else_scope = std::nullopt});
      }
      if (shape == Shape::kNamedAfter) {
        body.AppendStmt(ExprStmt{.expr = copy_of_moved(body)});
      }
      // Held last by the block, run second, before the move.
      if (shape == Shape::kNamedFirstInRunOrder) {
        body.AppendStmt(ExprStmt{.expr = copy_of_moved(body)});
        std::vector<StmtId>& order = code.Body().root_stmts;
        std::rotate(order.begin() + 1, order.end() - 1, order.end());
      }
    });
  };

  EXPECT_NO_THROW(Verify(moving(Shape::kLastUse)));
  EXPECT_NO_THROW(Verify(moving(Shape::kNamedBefore)));
  EXPECT_NO_THROW(Verify(moving(Shape::kNamedFirstInRunOrder)));
  EXPECT_NO_THROW(Verify(moving(Shape::kAddressKeptInANestedBlock)));
  EXPECT_NO_THROW(Verify(moving(Shape::kInABlockStatement)));
  ExpectRefused(
      moving(Shape::kNamedInTheMovingStatement),
      "names it again in the statement that moves it");
  ExpectRefused(moving(Shape::kNamedInItsBlocksValue), "names it again");
  ExpectRefused(moving(Shape::kNamedAfter), "names it again after the move");
  for (const Shape kept :
       {Shape::kAddressKept, Shape::kAddressKeptInATuple,
        Shape::kAddressAssigned}) {
    ExpectRefused(moving(kept), "stores an address of its storage in a value");
  }
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
      Builder body(code.Body(), diag::SourceSpan{});
      const ExprId value = body.Add(
          Expr{
              .data = NullLiteral{},
              .type = of_its_type ? held : u.builtins.integer});
      const auto written = [&](TypeId local_type) {
        const LocalId local = code.locals.Add(LocalDecl{.type = local_type});
        return body.Add(MakeLocalRefExpr(local, local_type));
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
                  .expr = body.Add(
                      MakeAssignExpr(u.builtins, written(held), value))});
          return;
        case HandedTo::kWrapper:
          body.AppendStmt(
              ExprStmt{
                  .expr = body.Add(MakeCallExpr(
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

// The reserved placement form makes a value in the storage its one argument
// points at, so the argument points at a value of the type placed there.
TEST(MirVerifyTest, ThePlacementAtAnAddressPlacesWhatTheAddressPointsAt) {
  const auto placing = [](bool of_pointee_type) {
    return UnitWithBody([=](CompilationUnit& u, Builder body) {
      const TypeId held = u.builtins.int_type;
      const TypeId place = u.types.Intern(
          Type{PointerType{
              .pointee = held, .ownership = PointerOwnership::kBorrowed}});
      const TypeId other = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.integer,
              .ownership = PointerOwnership::kBorrowed}});
      const ExprId address = body.Add(
          Expr{.data = NullLiteral{}, .type = of_pointee_type ? place : other});
      const ExprId value = body.Add(Expr{.data = NullLiteral{}, .type = held});
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(MakePlacementNewExpr(
                  value,
                  PlacementAllocation{
                      .allocate = support::BuiltinFn::kDefaultPlacementNew,
                      .arguments = {address}},
                  place))});
    });
  };

  EXPECT_NO_THROW(Verify(placing(true)));
  ExpectRefused(placing(false), "names an allocation function other than");
}

// A local is named at the type it is declared, so that a view of it at
// another type -- a read-only reference to a local declared writable -- is a
// conversion of what is read, never a second spelling of the local.
TEST(MirVerifyTest, ALocalIsNamedAtTheTypeItIsDeclared) {
  const auto naming = [](Mutability named_as) {
    return UnitWithCode([=](CompilationUnit& u, CallableCode& code) {
      const auto reference = [&](Mutability mutability) {
        return u.types.Intern(
            Type{ReferenceType{
                .referent = u.builtins.int_type, .mutability = mutability}});
      };
      const LocalId local =
          code.locals.Add(LocalDecl{.type = reference(Mutability::kMutable)});
      code.params.push_back(local);
      Builder body(code.Body(), diag::SourceSpan{});
      const ExprId read = body.Read(body.Deref(
          u.types, body.LocalPlace(local, reference(named_as)),
          u.builtins.int_type));
      body.AppendStmt(
          LocalDeclStmt{
              .target = code.locals.Add(LocalDecl{.type = u.builtins.int_type}),
              .init = read});
    });
  };

  EXPECT_NO_THROW(Verify(naming(Mutability::kMutable)));
  ExpectRefused(naming(Mutability::kReadOnly), "hands a local it names");
}

// An address moved by a count stays an address of what it pointed at, moved by
// a machine integer, and a value taken out of storage through an address owning
// nothing is of the type that storage held.
TEST(MirVerifyTest, AnAddressMovesByACountAndYieldsWhatItPointsAt) {
  enum class Wrong : std::uint8_t {
    kNothing,
    kCountNotAMachineInteger,
    kMovedToAnotherType,
    kTakenAsAnotherType,
    kTakenThroughAnOwner,
  };
  const auto taking = [](Wrong wrong) {
    return UnitWithCode([=](CompilationUnit& u, CallableCode& code) {
      const TypeId held = u.builtins.int_type;
      const auto pointer_to = [&](TypeId pointee, PointerOwnership ownership) {
        return u.types.Intern(
            Type{PointerType{.pointee = pointee, .ownership = ownership}});
      };
      const TypeId address = pointer_to(
          held, wrong == Wrong::kTakenThroughAnOwner
                    ? PointerOwnership::kUnique
                    : PointerOwnership::kBorrowed);
      Builder body(code.Body(), diag::SourceSpan{});
      const ExprId start =
          body.Add(Expr{.data = NullLiteral{}, .type = address});
      const ExprId count = body.Add(
          Expr{
              .data = MachineIntLiteral{.value = 1},
              .type = wrong == Wrong::kCountNotAMachineInteger
                          ? held
                          : u.builtins.machine_int64});
      const ExprId moved = body.Add(
          Expr{
              .data =
                  BinaryExpr{.op = BinaryOp::kAdd, .lhs = start, .rhs = count},
              .type = wrong == Wrong::kMovedToAnotherType
                          ? pointer_to(
                                u.builtins.integer, PointerOwnership::kBorrowed)
                          : address});
      const TypeId taken_type =
          wrong == Wrong::kTakenAsAnotherType ? u.builtins.integer : held;
      const ExprId taken = body.Add(MakeCallExpr(
          Direct{.target = support::BuiltinFn::kPtrRead}, {moved}, taken_type));
      body.AppendStmt(
          LocalDeclStmt{
              .target = code.locals.Add(LocalDecl{.type = taken_type}),
              .init = taken});
    });
  };

  EXPECT_NO_THROW(Verify(taking(Wrong::kNothing)));
  ExpectRefused(
      taking(Wrong::kCountNotAMachineInteger), "which is no machine integer");
  ExpectRefused(taking(Wrong::kMovedToAnotherType), "hands a moved address");
  ExpectRefused(
      taking(Wrong::kTakenAsAnotherType), "hands a value taken out of storage");
  ExpectRefused(
      taking(Wrong::kTakenThroughAnOwner), "one address owning nothing");
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
      Builder body(code.Body(), diag::SourceSpan{});
      std::vector<ExprId> values;
      for (const bool of_its_type : of_position_type) {
        values.push_back(body.Add(
            Expr{
                .data = NullLiteral{},
                .type = of_its_type ? held : u.builtins.integer}));
      }
      const ExprId composite = body.Add(
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

// A constant of the unit is a value of its type: each part of a structure of
// the library is a value of the type the library declares its member at, a
// structure has a part for each member, a view is of an array of its own
// entries, an integral's bits are canonical for its width, an array is as long
// as its type, and a constant is named, by a pointer or by a body, at its own
// type.
TEST(MirVerifyTest, AConstantIsAValueOfItsType) {
  enum class Wrong : std::uint8_t {
    kNothing,
    kEntryMember,
    kViewOfOtherEntries,
    kTooFewParts,
    kPointerAtOtherType,
    kBitsNotCanonical,
    kArrayOfOtherLength,
  };
  const auto stating = [](Wrong wrong) {
    return UnitWithBody([wrong](CompilationUnit& u, Builder) {
      const auto library = [&](support::LibraryDecl decl) {
        return u.types.Intern(Type{LibraryType{.decl = decl, .args = {}}});
      };
      const TypeId instantiation =
          library(support::LibraryDecl::kInstantiation);
      // `{entry, table}`, an instantiation that makes nothing.
      ConstantValue entry{RecordValue{
          .parts = {
              ConstantValue{NullValue{}},
              wrong == Wrong::kEntryMember
                  ? ConstantValue{CStringValue{.text = "not an address"}}
                  : ConstantValue{NullValue{}}}}};
      if (wrong == Wrong::kTooFewParts) {
        entry = ConstantValue{RecordValue{.parts = {}}};
      }
      const TypeId listed = wrong == Wrong::kViewOfOtherEntries
                                ? library(support::LibraryDecl::kScopeCallable)
                                : instantiation;
      const TypeId array = MachineArrayOf(u.types, listed, 1);
      ArrayValue entries;
      entries.elements.push_back(
          wrong == Wrong::kViewOfOtherEntries
              ? ConstantValue{RecordValue{
                    .parts =
                        {ConstantValue{CStringValue{.text = "f"}},
                         ConstantValue{NullValue{}}}}}
              : std::move(entry));
      const ConstantId places = u.constants.Intern(
          ConstantDecl{
              .type = array, .value = ConstantValue{std::move(entries)}});
      // `{places}`: the generate's places, a view of that array.
      u.constants.Intern(
          ConstantDecl{
              .type = library(support::LibraryDecl::kGeneratePlaces),
              .value = ConstantValue{RecordValue{
                  .parts = {ConstantValue{SpanValue{.array = places}}}}}});
      if (wrong == Wrong::kPointerAtOtherType) {
        u.constants.Intern(
            ConstantDecl{
                .type = u.types.Intern(
                    Type{PointerType{
                        .pointee = u.builtins.source_location_record,
                        .ownership = PointerOwnership::kBorrowed,
                        .mutability = Mutability::kReadOnly}}),
                .value = ConstantValue{PointerValue{
                    .to = ConstantRef{.constant = places},
                    .type = u.builtins.source_location_record}}});
      }
      // A one-bit value whose word sets a bit above its width.
      if (wrong == Wrong::kBitsNotCanonical) {
        u.constants.Intern(
            ConstantDecl{
                .type = u.builtins.bit1,
                .value = ConstantValue{IntegralValue{
                    .planes = {.value_words = {2}, .state_words = {}}}}});
      }
      if (wrong == Wrong::kArrayOfOtherLength) {
        u.constants.Intern(
            ConstantDecl{
                .type = MachineArrayOf(u.types, u.builtins.machine_word, 2),
                .value = ConstantValue{ArrayValue{
                    .elements = {
                        ConstantValue{MachineIntValue{.value = 1}}}}}});
      }
    });
  };
  // A body naming a constant at another type than the constant's own.
  const auto naming_at_other_type = [] {
    return UnitWithBody([](CompilationUnit& u, Builder body) {
      const ConstantId held = u.constants.Intern(
          ConstantDecl{
              .type = u.builtins.int_type,
              .value = ConstantValue{IntegralValue{
                  .planes = {.value_words = {1}, .state_words = {}}}}});
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
                  Expr{
                      .data =
                          ReferenceExpr{
                              .target = ConstantRef{.constant = held}},
                      .type = u.builtins.int_unsigned})});
    });
  };

  EXPECT_NO_THROW(Verify(stating(Wrong::kNothing)));
  ExpectRefused(stating(Wrong::kEntryMember), "states a text");
  ExpectRefused(
      stating(Wrong::kViewOfOtherEntries),
      "a view of a constant that is no array of its element");
  ExpectRefused(stating(Wrong::kTooFewParts), "a structure of 0 parts");
  ExpectRefused(
      stating(Wrong::kPointerAtOtherType),
      "the address of a constant at another type than its own");
  ExpectRefused(stating(Wrong::kBitsNotCanonical), "bits not canonical");
  ExpectRefused(stating(Wrong::kArrayOfOtherLength), "an array of 1 elements");
  ExpectRefused(naming_at_other_type(), "hands a constant it names");
}

// Code named as a value is of the type the code has -- every parameter it
// takes and what it results in -- whatever type the place it is read into
// states, which a cast reaches: a body named as itself, and a method named by
// the entry its class gives it, which is that entry's type. A method its class
// gives no entry is entered through none.
TEST(MirVerifyTest, CodeNamedAsAValueIsOfTheTypeTheCodeHas) {
  enum class Named : std::uint8_t { kBody, kEntry, kMethodWithoutEntry };
  const auto naming = [](Named named_as, bool at_its_own_type) {
    return UnitWithCode(
        [named_as, at_its_own_type](CompilationUnit& u, CallableCode& code) {
          // A body of a class of the unit taking an `int` and doing nothing,
          // which is also the entry of itself where the class gives it one.
          CallableCode named = CallableCode::Defined(diag::SourceSpan{});
          named.result_type = u.builtins.void_type;
          named.params.push_back(named.AddLocal(u.builtins.int_type));
          const TypeId own = FunctionTypeOf(u.types, named);
          Class cls;
          const ClassId owner = u.DeclareClass();
          const CallableId slot =
              u.AddClassCallable(owner, std::nullopt, std::move(named));
          if (named_as == Named::kEntry) {
            cls.entries.push_back(MethodEntry{.method = slot, .entry = slot});
          }
          u.DefineClass(owner, std::move(cls));
          const TypeId stated =
              at_its_own_type
                  ? own
                  : u.types.Intern(
                        Type{MachineFunctionType{
                            .params = {}, .result = u.builtins.void_type}});
          const ReferenceTarget target =
              named_as == Named::kBody
                  ? ReferenceTarget{FunctionRef{.callable = slot}}
                  : ReferenceTarget{DispatchEntryRef{.method = slot}};
          Builder body(code.Body(), diag::SourceSpan{});
          const ExprId reference = body.Add(
              Expr{.data = ReferenceExpr{.target = target}, .type = stated});
          body.AppendStmt(
              LocalDeclStmt{
                  .target = code.locals.Add(LocalDecl{.type = stated}),
                  .init = reference});
        });
  };

  for (const Named named_as : {Named::kBody, Named::kEntry}) {
    EXPECT_NO_THROW(Verify(naming(named_as, true)));
    ExpectRefused(naming(named_as, false), "names code of type");
  }
  ExpectRefused(
      naming(Named::kMethodWithoutEntry, true),
      "names the entry of a method its class gives none");
}

// A cast joins only types every target converts between in its own syntax: an
// integral value at a type laying it out alike, a value read as the machine
// boolean a condition tests, a machine integer at another, and a code address
// as one the target enters alike -- erased for a table to hold, or handed a
// handle of another class, or a reference to one that is as writable -- and
// never as one handed something else. A value moved between representations
// is a call.
TEST(MirVerifyTest, ACastReadsAValueAsATypeEveryTargetConvertsItTo) {
  const auto casting = [](auto from, auto to) {
    return UnitWithCode([=](CompilationUnit& u, CallableCode& code) {
      Builder body(code.Body(), diag::SourceSpan{});
      const ExprId value =
          body.Add(Expr{.data = NullLiteral{}, .type = from(u)});
      const ExprId cast =
          body.Add(Expr{.data = CastExpr{.operand = value}, .type = to(u)});
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

  const auto code_handed = [](auto param) {
    return [=](CompilationUnit& u) {
      return u.types.Intern(
          Type{MachineFunctionType{
              .params = {param(u)}, .result = u.builtins.void_type}});
    };
  };
  const auto handle_of = [](support::LibraryDecl decl) {
    return [=](CompilationUnit& u) {
      return u.types.Intern(
          Type{ManagedRefType{
              .pointee = u.types.Intern(
                  Type{LibraryType{.decl = decl, .args = {}}})}});
    };
  };
  const auto erased = [](CompilationUnit& u) {
    return ErasedFunction(u.types);
  };
  const auto reference_to = [](auto referent, Mutability mutability) {
    return [=](CompilationUnit& u) {
      return u.types.Intern(
          Type{ReferenceType{
              .referent = referent(u), .mutability = mutability}});
    };
  };
  const auto problem = handle_of(support::LibraryDecl::kConstraintProblem);
  const auto root = handle_of(support::LibraryDecl::kObjectRoot);

  EXPECT_NO_THROW(Verify(casting(int_type, machine_bool)));
  EXPECT_NO_THROW(Verify(casting(machine_int64, machine_bool)));
  EXPECT_NO_THROW(Verify(casting(code_handed(int_type), erased)));
  EXPECT_NO_THROW(Verify(casting(erased, code_handed(int_type))));
  EXPECT_NO_THROW(Verify(casting(code_handed(problem), code_handed(root))));
  EXPECT_NO_THROW(Verify(casting(
      code_handed(reference_to(problem, Mutability::kReadOnly)),
      code_handed(reference_to(root, Mutability::kReadOnly)))));
  ExpectRefused(
      casting(
          code_handed(reference_to(problem, Mutability::kMutable)),
          code_handed(reference_to(root, Mutability::kReadOnly))),
      "which a cast does not join");
  ExpectRefused(casting(int_type, integer), "which a cast does not join");
  ExpectRefused(casting(int_type, string), "which a cast does not join");
  ExpectRefused(casting(int_type, machine_int64), "which a cast does not join");
  ExpectRefused(
      casting(code_handed(int_type), code_handed(machine_int64)),
      "which a cast does not join");
}

// A call names an operation of a type no declaration states it for only where
// the type can have one, and no body has to exist for the call to be whole.
TEST(MirVerifyTest, ACallNamesOnlyAnOperationItsTypeCanHave) {
  const auto calling = [](const auto& type_of,
                          support::ValueOperation answers) {
    return UnitWithBody([&](CompilationUnit& u, Builder body) {
      const TypeId type = type_of(u);
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{
                                      .target =
                                          TypeOperationTarget{
                                              .type = type,
                                              .answers = answers}},
                              .arguments = {}},
                      .type = type})});
    });
  };
  const auto array = [](CompilationUnit& u) {
    return u.types.Intern(
        Type{UnpackedArrayType{
            .element_type = u.builtins.int_type, .count = 4}});
  };
  const auto string_type = [](CompilationUnit& u) { return u.builtins.string; };
  const auto machine_bool = [](CompilationUnit& u) {
    return u.builtins.machine_bool;
  };
  const auto int_type = [](CompilationUnit& u) { return u.builtins.int_type; };
  const support::ValueOperation default_value =
      support::DefaultItem::kDeclaredDefault;

  EXPECT_NO_THROW(Verify(calling(array, default_value)));
  EXPECT_NO_THROW(Verify(calling(string_type, default_value)));
  // What is no data type has no default, and a type whose other operations
  // are the library's has no other.
  ExpectRefused(
      calling(machine_bool, default_value), "which that type cannot have");
  ExpectRefused(
      calling(int_type, support::EqualityItem::kEqual),
      "which that type cannot have");
}

// A call of an operation MIR declares for a type hands it what the declaration
// takes: a receiver exactly where it takes one, then an operand of each
// parameter's type in order, at the type arguments it is taken at; and a run
// written on a designation hands the slice write it carries out what that
// write takes after the array.
TEST(MirVerifyTest, ACallOfADeclaredOperationHandsWhatItsDeclarationTakes) {
  // What a call is handed: its receiver, where it has one, and its arguments.
  struct Handed {
    std::optional<ExprId> receiver;
    std::vector<ExprId> arguments;
  };
  // A unit calling `target` of `type_of(u)`, handed what `hand(u, body,
  // value, other)` builds from a local of the type and one of `int`.
  const auto calling = [](const auto& type_of, const auto& target_of,
                          const auto& hand) {
    return UnitWithCode([&](CompilationUnit& u, CallableCode& code) {
      const TypeId type = type_of(u);
      const LocalId value = code.AddLocal(type);
      const LocalId other = code.AddLocal(u.builtins.int_type);
      Builder body(code.Body(), diag::SourceSpan{});
      Handed handed = hand(
          u, body, body.LocalPlace(value, type),
          body.LocalPlace(other, u.builtins.int_type));
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{
                                      .target = target_of(u, type),
                                      .receiver = handed.receiver,
                                      .type_arguments = {}},
                              .arguments = std::move(handed.arguments)},
                      .type = u.builtins.int_type})});
    });
  };
  const auto dynamic = [](CompilationUnit& u) {
    return DynamicArrayOf(u.types, u.builtins.int_type);
  };
  const auto fixed = [](CompilationUnit& u) {
    return u.types.Intern(
        Type{UnpackedArrayType{
            .element_type = u.builtins.int_type, .count = 4}});
  };
  const auto run_type = [](CompilationUnit& u) {
    return u.types.Intern(
        Type{UnpackedArrayType{
            .element_type = u.builtins.int_type, .count = 2}});
  };
  const auto operation = [](support::ValueOperation answers,
                            const auto& arguments_of) {
    return [=](CompilationUnit& u, TypeId type) -> DirectTarget {
      return TypeOperationTarget{
          .type = type, .answers = answers, .arguments = arguments_of(u)};
    };
  };
  const auto at_none = [](CompilationUnit&) { return std::vector<TypeId>{}; };
  const auto at_int = [](CompilationUnit& u) {
    return std::vector<TypeId>{u.builtins.int_type};
  };
  const auto receiving = [](CompilationUnit& u, Builder body, ExprId value,
                            ExprId) {
    return Handed{.receiver = Borrow(u, body, value), .arguments = {}};
  };
  const support::ValueOperation size = support::BuiltinFn::kSize;

  EXPECT_NO_THROW(
      Verify(calling(dynamic, operation(size, at_none), receiving)));
  ExpectRefused(
      calling(
          dynamic, operation(size, at_none),
          [](CompilationUnit& u, Builder body, ExprId, ExprId other) {
            return Handed{.receiver = Borrow(u, body, other), .arguments = {}};
          }),
      "operands of other types than its declaration takes");
  // The object handed as an argument is not the object the call is asked of.
  ExpectRefused(
      calling(
          dynamic, operation(size, at_none),
          [](CompilationUnit& u, Builder body, ExprId value, ExprId) {
            return Handed{
                .receiver = std::nullopt,
                .arguments = {Borrow(u, body, value)}};
          }),
      "with no receiver");
  // An operation taken at a type is asked at one, and one taken at none at
  // none, a fixed-size array's run as a dynamic array's.
  ExpectRefused(
      calling(
          dynamic, operation(support::BuiltinFn::kElementSlice, at_none),
          receiving),
      "type arguments");
  ExpectRefused(
      calling(
          fixed, operation(support::BuiltinFn::kReverse, at_int), receiving),
      "type arguments");

  // A fixed-size array's and a tuple's operations on a whole value are
  // declared as a dynamic array's are, each of its own type.
  const auto tuple = [](CompilationUnit& u) {
    return u.types.Intern(
        Type{
            TupleType{.elements = {u.builtins.int_type, u.builtins.int_type}}});
  };
  const support::ValueOperation equality = support::EqualityItem::kEqual;
  const auto compared_with_itself = [](CompilationUnit& u, Builder body,
                                       ExprId value, ExprId) {
    return Handed{
        .receiver = Borrow(u, body, value),
        .arguments = {Borrow(u, body, value)}};
  };
  const auto compared_with_an_int = [](CompilationUnit& u, Builder body,
                                       ExprId value, ExprId other) {
    return Handed{
        .receiver = Borrow(u, body, value),
        .arguments = {Borrow(u, body, other)}};
  };
  EXPECT_NO_THROW(Verify(
      calling(fixed, operation(equality, at_none), compared_with_itself)));
  ExpectRefused(
      calling(fixed, operation(equality, at_none), compared_with_an_int),
      "operands of other types than its declaration takes");
  EXPECT_NO_THROW(Verify(
      calling(tuple, operation(equality, at_none), compared_with_itself)));
  ExpectRefused(
      calling(tuple, operation(equality, at_none), compared_with_an_int),
      "operands of other types than its declaration takes");
  // A fixed-size array's interface states no count of its own to ask.
  ExpectRefused(
      calling(fixed, operation(size, at_none), receiving),
      "which that type cannot have");

  // A run written on a designation of a dynamic array within a write: where
  // it starts, the run, of `run`'s type, and where it was written, answering
  // the positions it changed.
  const auto designated = [&](const auto& run_of) {
    return UnitWithCode([&](CompilationUnit& u, CallableCode& code) {
      const TypeId array = dynamic(u);
      const TypeId designation =
          u.types.Intern(Type{DesignationType{.value = array}});
      const TypeId position = PositionType(u.types);
      const LocalId designating = code.AddLocal(designation);
      const LocalId start = code.AddLocal(position);
      const LocalId run = code.AddLocal(run_of(u));
      const LocalId written_at = code.AddLocal(u.builtins.source_location);
      Builder body(code.Body(), diag::SourceSpan{});
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{
                                      .target = support::BuiltinFn::
                                          kAssignDesignatedRun,
                                      .receiver = body.LocalPlace(
                                          designating, designation),
                                      .type_arguments = {run_type(u)}},
                              .arguments =
                                  {Borrow(
                                       u, body,
                                       body.LocalPlace(start, position)),
                                   Borrow(
                                       u, body,
                                       body.LocalPlace(run, run_of(u))),
                                   body.ReadLocal(
                                       written_at,
                                       u.builtins.source_location)}},
                      .type = ChangedRangeType(u.types)})});
    });
  };
  EXPECT_NO_THROW(Verify(designated(run_type)));
  ExpectRefused(
      designated([](CompilationUnit& u) { return u.builtins.int_type; }),
      "operands of other types than its declaration takes");
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
    Builder* steps = nullptr;
    TypeId array;
    LocalId outer;
    LocalId own;
    LocalId at;
  };
  const auto yielding = [](const auto& yield) {
    return UnitWithCode([&](CompilationUnit& u, CallableCode& code) {
      const TypeId array = FourOf(u.types, u.builtins.int_type);
      Builder body(code.Body(), diag::SourceSpan{});
      const LocalId outer = code.locals.Add(LocalDecl{.type = array});
      body.AppendStmt(
          LocalDeclStmt{
              .target = outer,
              .init = body.Add(Expr{.data = NullLiteral{}, .type = array})});
      Block steps;
      Builder steps_at(steps, diag::SourceSpan{});
      const LocalId own = code.locals.Add(LocalDecl{.type = array});
      steps_at.AppendStmt(
          LocalDeclStmt{
              .target = own,
              .init =
                  steps_at.Add(Expr{.data = NullLiteral{}, .type = array})});
      const LocalId at =
          code.locals.Add(LocalDecl{.type = PositionType(u.types)});
      steps_at.AppendStmt(
          LocalDeclStmt{
              .target = at,
              .init = steps_at.Add(
                  Expr{.data = NullLiteral{}, .type = PositionType(u.types)})});
      Yielding given{
          .unit = &u,
          .code = &code,
          .steps = &steps_at,
          .array = array,
          .outer = outer,
          .own = own,
          .at = at};
      const ExprId value = yield(given);
      const TypeId type = steps.exprs.Get(value).type;
      const BlockId scope = body.AddScope(std::move(steps));
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
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
    return y.steps->Add(
        Expr{
            .data = BorrowExpr{.operand = operand},
            .type = reference_to(*y.unit, y.steps->Get(operand).type)});
  };
  // An element of what `receiver` names, at the position the steps hold.
  const auto element_of = [&](const Yielding& y, ExprId receiver) {
    return y.steps->Add(
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
    const TypeId type = y.steps->Get(reference).type;
    const LocalId holder = y.code->locals.Add(LocalDecl{.type = type});
    y.steps->AppendStmt(LocalDeclStmt{.target = holder, .init = reference});
    const ExprId place = y.steps->LocalPlace(holder, type);
    return moved ? y.steps->Add(
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
        const ExprId literal = y.steps->Add(
            Expr{
                .data = MachineIntLiteral{.value = 1},
                .type = y.unit->builtins.int_type});
        return borrow(y, AddSum(*y.unit, *y.steps, literal, literal));
      }),
      kWhy);
  ExpectRefused(
      yielding([&](const Yielding& y) {
        const ExprId reference = borrow(y, y.steps->LocalPlace(y.own, y.array));
        return y.steps->Add(
            Expr{
                .data = CompositeExpr{.parts = {reference}},
                .type = y.unit->types.Intern(
                    Type{TupleType{
                        .elements = {y.steps->Get(reference).type}}})});
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
            y, y.steps->Add(Expr{.data = NullLiteral{}, .type = y.array}));
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
    return UnitWithBody([=](CompilationUnit& u, Builder body) {
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{
                                      .target =
                                          CallableTarget{
                                              .callable = CallableId{0}},
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
    return UnitWithBody([=](CompilationUnit& u, Builder body) {
      const ExprId keeper = body.Add(
          Expr{.data = NullLiteral{}, .type = u.builtins.deferred_effects_ptr});
      const ExprId value =
          body.Add(Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(MakePlacementNewExpr(
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
          support::BuiltinFn::kAssertionControlAllocate,
          PointerOwnership::kBorrowed),
      "names an allocation function other than the one its argument "
      "allocates by");

  const auto calling = [](support::BuiltinFn entry) {
    return UnitWithBody([=](CompilationUnit& u, Builder body) {
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee = Direct{.target = entry},
                              .arguments = {}},
                      .type = u.builtins.void_type})});
    });
  };
  ExpectRefused(
      calling(support::BuiltinFn::kDeferredEffectsAllocate),
      "calls an allocation function, which only a new-expression reaches");
  ExpectRefused(
      calling(support::BuiltinFn::kDefaultPlacementNew),
      "calls the reserved placement form, which only a new-expression "
      "reaches");
}

// A keeper runs the effect committed to it once the frame that made the effect
// has gone on, so an effect made in a keeper's storage holds no reference to
// storage of that frame, where a copy of the same value is what it may hold.
// A body committing to a keeper an effect that captures one field, of the type
// `captured_type` answers, built by `capture` in the body.
template <typename CapturedType, typename Capture>
auto UnitCommittingAnEffect(
    const CapturedType& captured_type, const Capture& capture)
    -> CompilationUnit {
  return UnitWithCode([&](CompilationUnit& u, CallableCode& code) {
    const TypeId captured = captured_type(u);
    const ClosureId id = u.DeclareClosure();
    const TypeId effect_type =
        u.types.Intern(Type{ClosureType{.closure_id = id}});
    ClosureDecl effect;
    const FieldId field = effect.fields.Add(FieldDecl{.type = captured});
    effect.field_order = {field};
    CallableCode invoke = CallableCode::Defined(diag::SourceSpan{});
    invoke.receiver = invoke.AddLocal(u.types.Intern(
        Type{PointerType{
            .pointee = effect_type,
            .ownership = PointerOwnership::kUnique,
            .mutability = Mutability::kMutable}}));
    invoke.result_type = u.builtins.void_type;
    Builder(invoke.Body(), diag::SourceSpan{})
        .AppendStmt(ReturnStmt{.value = std::nullopt});
    u.AddClosureBody(id, std::move(invoke));
    u.DefineClosure(id, std::move(effect));

    Builder body(code.Body(), diag::SourceSpan{});
    const TypeId keeper_pointer = u.builtins.deferred_effects_ptr;
    const LocalId keeper = code.AddLocal(keeper_pointer);
    body.AppendStmt(
        LocalDeclStmt{
            .target = keeper,
            .init =
                body.Add(Expr{.data = NullLiteral{}, .type = keeper_pointer})});
    const ExprId value = capture(u, code, body, captured);
    const ExprId made = body.Add(MakePlacementNewExpr(
        body.Add(
            Expr{
                .data =
                    ClosureExpr{
                        .closure = id,
                        .field_inits = {{.target = field, .value = value}}},
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
            .expr = body.Add(
                Expr{
                    .data =
                        CallExpr{
                            .callee =
                                Direct{
                                    .target = support::BuiltinFn::
                                        kDeferredEffectsCommit,
                                    .receiver = body.Deref(
                                        u.types,
                                        body.LocalPlace(keeper, keeper_pointer),
                                        u.types.Get(keeper_pointer)
                                            .Get<PointerType>()
                                            .pointee)},
                            .arguments = {made}},
                    .type = u.builtins.void_type})});
  });
}

auto ReadOnlyReferenceTo(CompilationUnit& u, TypeId referent) -> TypeId {
  return u.types.Intern(
      Type{ReferenceType{
          .referent = referent, .mutability = Mutability::kReadOnly}});
}

auto DeclaredNull(CallableCode& code, Builder body, TypeId type) -> LocalId {
  const LocalId local = code.AddLocal(type);
  body.AppendStmt(
      LocalDeclStmt{
          .target = local,
          .init = body.Add(Expr{.data = NullLiteral{}, .type = type})});
  return local;
}

TEST(MirVerifyTest, AnEffectMadeInAKeeperRefersToNothingItsFrameEnds) {
  const auto a_value = [](CompilationUnit& u) { return u.builtins.int_type; };
  EXPECT_NO_THROW(Verify(UnitCommittingAnEffect(
      a_value,
      [](CompilationUnit&, CallableCode& code, Builder body, TypeId type) {
        return body.ReadLocal(DeclaredNull(code, body, type), type);
      })));
  ExpectRefused(
      UnitCommittingAnEffect(
          [&](CompilationUnit& u) {
            return ReadOnlyReferenceTo(u, a_value(u));
          },
          [&](CompilationUnit& u, CallableCode& code, Builder body,
              TypeId type) {
            const TypeId value = a_value(u);
            return body.Add(
                Expr{
                    .data =
                        BorrowExpr{
                            .operand = body.LocalPlace(
                                DeclaredNull(code, body, value), value)},
                    .type = type});
          }),
      "is kept past its maker's end yet captures in field 0 a reference to "
      "storage of the frame that made it");
}

// A reference an object lives through says how long what it reaches lives, and
// nothing about the variable holding it: a local holding one is the frame's,
// and the object reached by dereferencing it outlives the frame.
TEST(MirVerifyTest, AnEffectMayReachAnObjectThroughALocalAndNotTheLocal) {
  const auto scope = [](CompilationUnit& u) {
    return u.types.Get(u.builtins.scope_ptr).Get<PointerType>().pointee;
  };
  const auto handle = [&](CompilationUnit& u) {
    return u.types.Intern(Type{UnmanagedRefType{.pointee = scope(u)}});
  };
  EXPECT_NO_THROW(Verify(UnitCommittingAnEffect(
      [&](CompilationUnit& u) { return ReadOnlyReferenceTo(u, scope(u)); },
      [&](CompilationUnit& u, CallableCode& code, Builder body, TypeId type) {
        const TypeId held = handle(u);
        const LocalId local = DeclaredNull(code, body, held);
        return body.Add(
            Expr{
                .data =
                    BorrowExpr{
                        .operand = body.Deref(
                            u.types, body.LocalPlace(local, held), scope(u))},
                .type = type});
      })));
  ExpectRefused(
      UnitCommittingAnEffect(
          [&](CompilationUnit& u) { return ReadOnlyReferenceTo(u, handle(u)); },
          [&](CompilationUnit& u, CallableCode& code, Builder body,
              TypeId type) {
            const TypeId held = handle(u);
            return body.Add(
                Expr{
                    .data =
                        BorrowExpr{
                            .operand = body.LocalPlace(
                                DeclaredNull(code, body, held), held)},
                    .type = type});
          }),
      "is kept past its maker's end yet captures in field 0 a reference to "
      "storage of the frame that made it");
}

// An entry another unit defines is declared here and holds nothing this unit
// emits: a record states none of its methods, and each is named as that unit
// published it, by a unit other than this one.
TEST(MirVerifyTest, AnEntryAnotherUnitDefinesHoldsNothingOfItsOwn) {
  struct Declared {
    bool own_unit = false;
    bool record_methods = false;
    bool named = true;
  };
  const auto unit_declaring = [](Declared declared) {
    return UnitWithBody([&](CompilationUnit& u, Builder) {
      const std::string defining = declared.own_unit ? u.name : "Pkg";
      u.records.Add(
          RecordDecl{
              .defining_unit = defining, .path = {}, .shape = StructRecord{}});
      if (declared.record_methods) {
        DeclareEquality(
            u, RecordId{0}, CallableCode::Defined(diag::SourceSpan{}));
      }
      const CallableId subroutine = u.AddSubroutineOfUnit(defining);
      const StaticVariableId variable = u.static_variables.Add(
          StaticVariableDecl{
              .type = u.builtins.int_type, .defining_unit = defining});
      if (declared.named) {
        u.named_callables.push_back(
            NamedCallable{.name = "g", .body = subroutine});
        u.named_static_variables.push_back(
            NamedStaticVariable{
                .name = support::UnqualifiedName("v"), .variable = variable});
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
      unit_declaring({.named = false}),
      "declares with no name a subroutine another unit defines");
}

// A definition every body of a unit states alike -- a record's method here --
// stands in for the others where the program is linked, so it names nothing a
// body has for itself alone: a call of a function only its position reaches is
// refused, and the same call of one reached by its name stands.
TEST(MirVerifyTest, ADefinitionEveryBodyStatesNamesNothingOneBodyHas) {
  const auto unit_whose_method_calls = [](bool a_named_function) {
    return UnitWithCode([&](CompilationUnit& u, CallableCode&) {
      CallableCode callee = CallableCode::Defined(diag::SourceSpan{});
      callee.result_type = u.builtins.void_type;
      const CallableId called = u.AddNamespaceCallable(std::move(callee));
      if (a_named_function) {
        u.named_callables.push_back(NamedCallable{.name = "g", .body = called});
      }
      CallableCode method = CallableCode::Defined(diag::SourceSpan{});
      method.result_type = u.builtins.void_type;
      Builder body(method.Body(), diag::SourceSpan{});
      const ExprId call = body.Add(
          Expr{
              .data =
                  CallExpr{
                      .callee =
                          Direct{.target = CallableTarget{.callable = called}},
                      .arguments = {}},
              .type = u.builtins.void_type});
      body.AppendStmt(ExprStmt{.expr = call});
      u.records.Add(
          RecordDecl{
              .defining_unit = std::nullopt,
              .path = {},
              .shape = StructRecord{}});
      DeclareEquality(u, RecordId{0}, std::move(method));
    });
  };

  EXPECT_NO_THROW(Verify(unit_whose_method_calls(true)));
  ExpectRefused(
      unit_whose_method_calls(false), "names a function only one body has");
}

// What a class every body states holds for the library is stated alike with
// the rest of it, and a method reaches another class by a member as well as by
// a call: a class only one body has is refused in each, and the class itself
// stands in the first.
TEST(MirVerifyTest, WhatEveryBodyStatesReachesNoClassOneBodyHas) {
  const auto class_named = [](support::ClassName name) {
    Class cls;
    cls.identity = std::move(name);
    cls.base = ClassRef{ClassObjectRootRef{}};
    return cls;
  };
  const auto stated_by_every_body = [] {
    return support::DefPath{
        .data = {support::ClassStep{.name = "C", .arguments = std::nullopt}}};
  };

  const auto unit_whose_definition_names = [&](bool its_own_class) {
    return UnitWithBody([&](CompilationUnit& u, Builder) {
      const ClassId own = u.DeclareClass();
      const ClassId shared = u.DeclareClass();
      u.DefineClass(
          own, class_named(support::UnnamedClass{.disambiguator = 0}));
      Class stated = class_named(stated_by_every_body());
      // What its definition names is reached through a constant of the unit,
      // the table of the interfaces it implements, which every body stating
      // the class states alike.
      const ConstantValue null{NullValue{}};
      LibraryRecordBuilder<abi::InterfaceRow> row(u);
      row.Set(
          &abi::InterfaceRow::of,
          ConstantValue{PointerValue{
              .to = DefinitionRef{.of = its_own_class ? shared : own},
              .type = LibraryTypeOf(
                  u.types, support::LibraryDecl::kObjectDefinition)}});
      row.Set(&abi::InterfaceRow::table, null);
      LibraryRecordBuilder<abi::ObjectDefinition> definition(u);
      definition.Set(&abi::ObjectDefinition::base, null);
      definition.Set(&abi::ObjectDefinition::scope, null);
      definition.Set(&abi::ObjectDefinition::class_object, null);
      definition.Set(&abi::ObjectDefinition::end, null);
      std::vector<ConstantValue> rows;
      rows.push_back(std::move(row).Build());
      definition.Set(
          &abi::ObjectDefinition::interfaces,
          ViewOf(
              u,
              LibraryTypeOf(u.types, abi::LibraryDeclOf<abi::InterfaceRow>()),
              std::move(rows)));
      definition.Set(&abi::ObjectDefinition::methods, null);
      definition.Set(&abi::ObjectDefinition::declaring_instance, null);
      definition.Set(
          &abi::ObjectDefinition::interface_reaches,
          ViewOf(
              u,
              LibraryTypeOf(u.types, abi::LibraryDeclOf<abi::InterfaceReach>()),
              {}));
      stated.definition = std::move(definition).Build();
      u.DefineClass(shared, std::move(stated));
    });
  };
  const auto refusal_of = [](const CompilationUnit& unit) {
    try {
      Verify(unit);
    } catch (const InternalError& error) {
      return std::string(error.what());
    }
    return std::string{};
  };
  EXPECT_EQ(refusal_of(unit_whose_definition_names(true)), "");
  ExpectRefused(
      unit_whose_definition_names(false), "names a class only one body has");

  ExpectRefused(
      UnitWithBody([&](CompilationUnit& u, Builder) {
        const ClassId own = u.DeclareClass();
        u.DefineClass(
            own, class_named(support::UnnamedClass{.disambiguator = 0}));
        CallableCode method = CallableCode::Defined(diag::SourceSpan{});
        method.result_type = u.builtins.void_type;
        Builder body(method.Body(), diag::SourceSpan{});
        const ExprId object =
            body.Add(Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
        const ExprId member = body.Add(
            Expr{
                .data =
                    FieldAccessExpr{
                        .receiver = object,
                        .field =
                            ClassFieldTarget{.owner = own, .slot = FieldId{0}}},
                .type = u.builtins.int_type});
        body.AppendStmt(ExprStmt{.expr = member});
        u.records.Add(
            RecordDecl{
                .defining_unit = std::nullopt,
                .path = {},
                .shape = StructRecord{}});
        DeclareEquality(u, RecordId{0}, std::move(method));
      }),
      "names a class only one body has");
}

// A class another unit defines is declared here once, under that unit's name,
// and holds nothing this unit emits; a method of a class this unit defines, its
// construction and a subroutine of its own namespace have their bodies here,
// since no other unit defines one.
TEST(MirVerifyTest, ADeclaredClassHoldsNothingAndAnOwnBodyIsHere) {
  struct Declared {
    bool own_unit = false;
    bool twice = false;
    bool method_body = false;
    bool object_record = false;
    bool own_method_without_body = false;
    bool own_construction_without_body = false;
    bool own_subroutine_without_body = false;
  };
  const auto unit_declaring = [](Declared declared) {
    return UnitWithBody([&](CompilationUnit& u, Builder) {
      const auto declare = [&] {
        Class cls;
        cls.defining_unit = declared.own_unit ? u.name : "Pkg";
        cls.identity = support::DefPath{
            .data = {
                support::ClassStep{.name = "C", .arguments = std::nullopt}}};
        cls.base = ClassRef{ClassObjectRootRef{}};
        const ClassId id = u.DeclareClass();
        if (declared.method_body) {
          u.AddClassCallable(
              id, std::nullopt, CallableCode::Defined(diag::SourceSpan{}));
        }
        if (declared.object_record) {
          cls.definition = ConstantValue{NullValue{}};
        }
        u.DefineClass(id, std::move(cls));
      };
      declare();
      if (declared.twice) declare();
      if (declared.own_method_without_body) {
        Class own;
        own.base = ClassRef{ClassObjectRootRef{}};
        const ClassId id = u.DeclareClass();
        u.AddClassCallable(id, std::nullopt, CallableCode{});
        u.DefineClass(id, std::move(own));
      }
      if (declared.own_construction_without_body) {
        Class own;
        own.base = ClassRef{ClassObjectRootRef{}};
        const ClassId id = u.DeclareClass();
        u.AddClassConstructor(id, CallableCode{});
        u.DefineClass(id, std::move(own));
      }
      if (declared.own_subroutine_without_body) {
        u.AddNamespaceCallable(CallableCode{});
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
      unit_declaring({.own_construction_without_body = true}),
      "leaves with no body the construction of a class it defines");
  ExpectRefused(
      unit_declaring({.own_subroutine_without_body = true}),
      "leaves with no body its own subroutine");
}

// Each callable's owner states who declares it and where it stands, and what
// the take functions cannot rule out is refused: an identity taken and never
// stated, a second construction of a class, a record answering one operation
// twice, and a closure with no body to invoke. Where a callable stands is
// handed out by its owner's take, so none can stand where another does.
TEST(MirVerifyTest, TheOwnersOfAUnitsCallablesCohere) {
  enum class Keyed : std::uint8_t {
    kByTheUnit,
    kNeverStated,
    kASecondConstruction,
    kOneOperationTwice,
    kAClosureWithNoBody,
  };
  const auto unit_keyed = [](Keyed keyed) {
    return UnitWithBody([keyed](CompilationUnit& u, Builder) {
      const ClassId id = u.DeclareClass();
      Class cls;
      cls.base = ClassRef{ClassObjectRootRef{}};
      u.DefineClass(id, std::move(cls));
      const auto member = [&] {
        CallableCode code = CallableCode::Defined(diag::SourceSpan{});
        code.result_type = u.builtins.void_type;
        return code;
      };
      u.AddClassCallable(id, std::nullopt, member());
      u.AddClassCallable(id, std::nullopt, member());
      u.AddClassConstructor(id, member());
      switch (keyed) {
        case Keyed::kByTheUnit:
          break;
        case Keyed::kNeverStated:
          u.TakeClassCallable(id);
          break;
        case Keyed::kASecondConstruction:
          u.AddClassConstructor(id, member());
          break;
        case Keyed::kAClosureWithNoBody:
          u.DefineClosure(u.DeclareClosure(), ClosureDecl{});
          break;
        case Keyed::kOneOperationTwice: {
          const RecordId record = u.records.Add(
              RecordDecl{
                  .defining_unit = std::nullopt,
                  .path = {},
                  .shape = StructRecord{}});
          const support::ValueOperation equality =
              support::EqualityItem::kEqual;
          u.AddRecordMethod(record, equality, member());
          u.AddRecordMethod(record, equality, member());
          break;
        }
      }
    });
  };

  EXPECT_NO_THROW(Verify(unit_keyed(Keyed::kByTheUnit)));
  ExpectRefused(unit_keyed(Keyed::kNeverStated), "never states the taken");
  ExpectRefused(
      unit_keyed(Keyed::kASecondConstruction),
      "gives a class a second construction");
  ExpectRefused(
      unit_keyed(Keyed::kOneOperationTwice),
      "answers one operation of a record twice");
  ExpectRefused(
      unit_keyed(Keyed::kAClosureWithNoBody),
      "declares closure 0 with no body to invoke");
}

// A record this unit defines states the body of every method it declares;
// one it leaves bodiless is refused whether or not anything calls it.
TEST(MirVerifyTest, ARecordThisUnitDefinesStatesEveryMethod) {
  const auto unit_with_method = [](bool with_body) {
    return UnitWithBody([with_body](CompilationUnit& u, Builder) {
      u.records.Add(
          RecordDecl{
              .defining_unit = std::nullopt,
              .path = {},
              .shape = StructRecord{}});
      CallableCode code = with_body ? CallableCode::Defined(diag::SourceSpan{})
                                    : CallableCode{};
      code.result_type = u.builtins.void_type;
      DeclareEquality(u, RecordId{0}, std::move(code));
    });
  };
  EXPECT_NO_THROW(Verify(unit_with_method(true)));
  ExpectRefused(
      unit_with_method(false), "leaves with no body a method of a record");
}

// A behavior a class leaves to what extends it has no body anywhere (LRM
// 8.21), so a direct call of it is refused, as one of a method with a body is
// not.
TEST(MirVerifyTest, OnlyDispatchReachesAMethodLeftAbstract) {
  const auto calling = [](bool left_abstract) {
    return UnitWithBody([left_abstract](CompilationUnit& u, Builder body) {
      const TypeId pointer_type = u.types.Intern(
          Type{PointerType{
              .pointee = u.builtins.int_type,
              .ownership = PointerOwnership::kBorrowed}});
      const ExprId pointer =
          body.Add(Expr{.data = NullLiteral{}, .type = pointer_type});
      const ExprId receiver =
          body.Add(MakeDerefExpr(pointer, u.builtins.int_type));
      Class cls;
      CallableCode member = left_abstract
                                ? CallableCode{}
                                : CallableCode::Defined(diag::SourceSpan{});
      member.result_type = u.builtins.void_type;
      const ClassId owner = u.DeclareClass();
      const CallableId called = u.AddClassCallable(
          owner,
          left_abstract
              ? std::optional<VirtualDispatchRole>{IntroducesVirtualSlot{
                    .is_pure = true}}
              : std::nullopt,
          std::move(member));
      u.DefineClass(owner, std::move(cls));
      body.AppendStmt(
          ExprStmt{
              .expr = body.Add(
                  Expr{
                      .data =
                          CallExpr{
                              .callee =
                                  Direct{
                                      .target =
                                          CallableTarget{.callable = called},
                                      .receiver = receiver},
                              .arguments = {}},
                      .type = u.builtins.void_type})});
    });
  };
  EXPECT_NO_THROW(Verify(calling(false)));
  ExpectRefused(
      calling(true),
      "calls directly a method its class leaves to what extends it");
}

// Code named as a value is a body a class this unit defines declares, or one of
// a unit's namespace: a construction, a closure's body and a record's method
// are entered only by what they are for, a function the foreign program
// defines is no code of the program's own, a method its class leaves to what
// extends it has no code anywhere, and a dispatch entry is of a method.
TEST(MirVerifyTest, CodeNamedAsAValueIsAMethodOrANamespaceBody) {
  enum class Named : std::uint8_t {
    kMethod,
    kNamespaceBody,
    kConstruction,
    kClosureBody,
    kRecordMethod,
    kForeignFunction,
    kAbstractMethod,
  };
  const auto naming = [](Named named, bool as_entry) {
    return UnitWithCode([named, as_entry](
                            CompilationUnit& u, CallableCode& code) {
      const ClassId owner = u.DeclareClass();
      Class cls;
      cls.base = ClassRef{ClassObjectRootRef{}};
      CallableCode body = CallableCode::Defined(diag::SourceSpan{});
      body.result_type = u.builtins.void_type;
      const TypeId type = FunctionTypeOf(u.types, body);
      CallableCode decl = std::move(body);
      CallableId callable{};
      switch (named) {
        case Named::kMethod:
          callable = u.AddClassCallable(owner, std::nullopt, std::move(decl));
          break;
        case Named::kAbstractMethod: {
          CallableCode prototype;
          prototype.result_type = u.builtins.void_type;
          callable = u.AddClassCallable(
              owner, IntroducesVirtualSlot{.is_pure = true},
              std::move(prototype));
          break;
        }
        case Named::kNamespaceBody:
          callable = u.AddNamespaceCallable(std::move(decl));
          break;
        case Named::kConstruction:
          callable = u.AddClassConstructor(owner, std::move(decl));
          break;
        case Named::kClosureBody: {
          const ClosureId closure = u.DeclareClosure();
          callable = u.AddClosureBody(closure, std::move(decl));
          u.DefineClosure(closure, ClosureDecl{});
          break;
        }
        case Named::kRecordMethod: {
          const RecordId record = u.records.Add(
              RecordDecl{
                  .defining_unit = std::nullopt,
                  .path = {},
                  .shape = StructRecord{}});
          callable = u.AddRecordMethod(
              record, support::EqualityItem::kEqual, std::move(decl));
          break;
        }
        case Named::kForeignFunction: {
          CallableCode prototype;
          prototype.result_type = u.builtins.void_type;
          callable = u.AddForeignCallable(
              ForeignLinkage{.foreign_name = "c_function"},
              std::move(prototype));
          break;
        }
      }
      if (as_entry) {
        cls.entries.push_back(
            MethodEntry{.method = callable, .entry = callable});
      }
      u.DefineClass(owner, std::move(cls));
      const ReferenceTarget target =
          as_entry ? ReferenceTarget{DispatchEntryRef{.method = callable}}
                   : ReferenceTarget{FunctionRef{.callable = callable}};
      Builder at(code.Body(), diag::SourceSpan{});
      at.AppendStmt(
          LocalDeclStmt{
              .target = code.locals.Add(LocalDecl{.type = type}),
              .init = at.Add(
                  Expr{
                      .data = ReferenceExpr{.target = target}, .type = type})});
    });
  };
  EXPECT_NO_THROW(Verify(naming(Named::kMethod, false)));
  EXPECT_NO_THROW(Verify(naming(Named::kNamespaceBody, false)));
  EXPECT_NO_THROW(Verify(naming(Named::kMethod, true)));
  ExpectRefused(
      naming(Named::kConstruction, false),
      "names as code a callable that is no method nor namespace body");
  ExpectRefused(
      naming(Named::kClosureBody, false),
      "names as code a callable that is no method nor namespace body");
  ExpectRefused(
      naming(Named::kRecordMethod, false),
      "names as code a callable that is no method nor namespace body");
  ExpectRefused(
      naming(Named::kForeignFunction, false),
      "names as code a function the foreign program defines");
  ExpectRefused(
      naming(Named::kAbstractMethod, false),
      "names as code a method its class leaves to what extends it");
  ExpectRefused(
      naming(Named::kNamespaceBody, true),
      "names the entry of a callable no class declares as a method");
}

// A call of something that reports a failure of the design at its caller -- a
// runtime entry whose declaration says it is handed where it was called -- is
// handed that location as its last argument once the unit is legalized: the
// unit's constant for where the call starts, or, in a body that reports at its
// own caller, the location that body was handed, which then states none of its
// own.
TEST(MirVerifyTest, ACallReportingAtItsCallerIsHandedWhereThatIs) {
  const auto place_of_its_own = [](const CompilationUnit& u, Builder body) {
    return BuildSourceLocation(
        u, diag::SourcePosition{.file = "test.sv", .line = 1, .column = 2},
        body);
  };
  const auto finish = [](const CompilationUnit& u, Builder body,
                         std::optional<ExprId> place) {
    std::vector<ExprId> arguments{
        AddCall(u, body), BuildMachineIntLiteral(u, body, 1)};
    if (place.has_value()) {
      arguments.push_back(*place);
    }
    body.AppendStmt(
        ExprStmt{
            .expr = body.Add(MakeCallExpr(
                Direct{.target = support::BuiltinFn::kFinish},
                std::move(arguments), u.builtins.void_type))});
  };
  // A body handed where it was called, whose call to `$finish` hands on that
  // place, and which may state a place of its own beside it.
  const auto reporting_at_its_caller = [&](bool states_its_own) {
    return UnitWithCode([&](CompilationUnit& u, CallableCode& code) {
      const LocalId handed =
          code.locals.Add(LocalDecl{.type = u.builtins.source_location});
      code.params.push_back(handed);
      code.caller_location = handed;
      Builder body(code.Body(), diag::SourceSpan{});
      finish(
          u, body,
          body.Add(MakeCopyExpr(
              body.Add(MakeLocalRefExpr(handed, u.builtins.source_location)),
              u.builtins.source_location)));
      if (states_its_own) {
        body.AppendStmt(ExprStmt{.expr = place_of_its_own(u, body)});
      }
    });
  };

  EXPECT_NO_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Builder body) {
        finish(u, body, place_of_its_own(u, body));
      })));
  const auto handed_nothing =
      UnitWithBody([&](const CompilationUnit& u, Builder body) {
        finish(u, body, std::nullopt);
      });
  ExpectRefused(handed_nothing, "is not handed where that is");
  EXPECT_NO_THROW(VerifyIn(Phase::kBuilt, handed_nothing));
  EXPECT_NO_THROW(Verify(reporting_at_its_caller(false)));
  ExpectRefused(
      reporting_at_its_caller(true),
      "states a location of its own, though it reports where it was called");
}

// Once a unit is legalized a class handle is opened by the library's call,
// which answers a pointer the dereference then reads through, so nothing
// reaches an object through a handle itself.
TEST(MirVerifyTest, AHandleIsOpenedByTheLibrary) {
  const auto dereferenced = UnitWithBody([](const CompilationUnit& u,
                                            Builder body) {
    const TypeId object =
        LibraryTypeOf(u.types, support::LibraryDecl::kObjectRoot);
    const ExprId handle = body.Add(
        Expr{
            .data = NullLiteral{},
            .type = u.types.Intern(Type{ManagedRefType{.pointee = object}})});
    body.AppendStmt(
        ExprStmt{
            .expr = body.Add(
                Expr{.data = DerefExpr{.pointer = handle}, .type = object})});
  });
  ExpectRefused(
      dereferenced, "reaches an object through a class handle itself");
  EXPECT_NO_THROW(VerifyIn(Phase::kBuilt, dereferenced));
}

// Reaching an object through a class handle opens the handle, which can fail
// the design, so it is an evaluation in the unit as it is built as much as
// once the library's call makes the opening, and one reached at two places is
// refused before the conversion.
TEST(MirVerifyTest, AnObjectReachedThroughAHandleIsAnEvaluation) {
  const auto twice = UnitWithBody([](const CompilationUnit& u, Builder body) {
    const TypeId object =
        LibraryTypeOf(u.types, support::LibraryDecl::kObjectRoot);
    const ExprId handle = body.Add(
        Expr{
            .data = NullLiteral{},
            .type = u.types.Intern(Type{ManagedRefType{.pointee = object}})});
    const ExprId opened =
        body.Add(Expr{.data = DerefExpr{.pointer = handle}, .type = object});
    body.AppendStmt(ExprStmt{.expr = opened});
    body.AppendStmt(ExprStmt{.expr = opened});
  });
  try {
    VerifyIn(Phase::kBuilt, twice);
    ADD_FAILURE() << "accepted an object reached through a handle twice";
  } catch (const InternalError& error) {
    const std::string message = error.what();
    EXPECT_NE(
        message.find("one run evaluates it at two of them"), std::string::npos)
        << message;
  }
}

void AppendCall(const CompilationUnit& unit, Builder block, Callee callee) {
  const ExprId call = block.Add(
      Expr{
          .data = CallExpr{.callee = std::move(callee), .arguments = {}},
          .type = unit.builtins.void_type});
  block.AppendStmt(ExprStmt{.expr = call});
}

// A unit whose one body is a statement nested in a block labelled 0, that
// statement an empty body paired with the cleanup `fill` writes.
template <typename Fill>
auto UnitWhoseCleanupHolds(const Fill& fill) -> CompilationUnit {
  return UnitWithCode([&](CompilationUnit& unit, CallableCode& code) {
    Block labelled;
    Builder labelled_at(labelled, diag::SourceSpan{});
    Block cleanup;
    fill(unit, code, labelled_at.Into(cleanup));
    labelled_at.AppendFinally(Block{}, std::move(cleanup));
    Builder body(code.Body(), diag::SourceSpan{});
    body.AppendStmt(
        BlockStmt{
            .scope = body.AddScope(std::move(labelled)),
            .break_label = LabelId{.value = 0}});
  });
}

// A cleanup runs on every way out of its body, a departure on its way out
// included, so a second way out starting inside it would have nowhere to go:
// it leaves only by running to its end. Every other way out is refused, and the
// refusal says the cleanup is what holds it.
TEST(MirVerifyTest, ACleanupLeavesOnlyByRunningToItsEnd) {
  EXPECT_NO_THROW(Verify(UnitWhoseCleanupHolds(
      [](const CompilationUnit& u, CallableCode&, Builder cleanup) {
        AppendCall(
            u, cleanup, Direct{.target = support::BuiltinFn::kLeaveStaticInit});
      })));
  ExpectRefused(
      UnitWhoseCleanupHolds([](const CompilationUnit& u, CallableCode&,
                               Builder cleanup) {
        AppendCall(u, cleanup, Direct{.target = support::BuiltinFn::kDisable});
      }),
      "a cleanup holds a call of the library entry 'disable', which is not "
      "stated not to leave it");
  ExpectRefused(
      UnitWhoseCleanupHolds([](CompilationUnit& u, CallableCode&,
                               Builder cleanup) {
        CallableCode callee = CallableCode::Defined(diag::SourceSpan{});
        callee.result_type = u.builtins.void_type;
        const CallableId called = u.AddNamespaceCallable(std::move(callee));
        AppendCall(
            u, cleanup, Direct{.target = CallableTarget{.callable = called}});
      }),
      "a cleanup holds a call of a body, which is not stated not to leave it");
  ExpectRefused(
      UnitWhoseCleanupHolds([](const CompilationUnit& u, CallableCode&,
                               Builder cleanup) {
        cleanup.AppendStmt(
            RaiseStmt{
                .effect = cleanup.Add(
                    Expr{.data = NullLiteral{}, .type = u.builtins.int_type})});
      }),
      "a cleanup raises");
  ExpectRefused(
      UnitWhoseCleanupHolds(
          [](const CompilationUnit& u, CallableCode&, Builder cleanup) {
            const auto literal = [&] {
              return cleanup.Add(
                  Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
            };
            cleanup.AppendStmt(
                ExprStmt{.expr = AddSum(u, cleanup, literal(), literal())});
          }),
      "a cleanup holds a binary operation, which is not stated not to leave "
      "it");
  ExpectRefused(
      UnitWhoseCleanupHolds(
          [](const CompilationUnit& u, CallableCode& code, Builder cleanup) {
            const LocalId caught =
                code.locals.Add(LocalDecl{.type = u.builtins.int_type});
            cleanup.AppendStmt(
                TryStmt{
                    .body = cleanup.AddScope(Block{}),
                    .caught = caught,
                    .handler = cleanup.AddScope(Block{})});
          }),
      "a cleanup holds a region, whose handler asks what arrived");
  ExpectRefused(
      UnitWhoseCleanupHolds(
          [](const CompilationUnit&, CallableCode&, Builder cleanup) {
            cleanup.AppendStmt(ReturnStmt{});
          }),
      "a cleanup returns");
  ExpectRefused(
      UnitWhoseCleanupHolds(
          [](const CompilationUnit&, CallableCode&, Builder cleanup) {
            cleanup.AppendStmt(BreakStmt{.target = LabelId{.value = 0}});
          }),
      "a cleanup breaks out of label 0, which stands around it");
  ExpectRefused(
      UnitWhoseCleanupHolds(
          [](const CompilationUnit&, CallableCode&, Builder cleanup) {
            cleanup.AppendStmt(ContinueStmt{});
          }),
      "a cleanup continues a loop that stands around it");
  ExpectRefused(
      UnitWhoseCleanupHolds(
          [](const CompilationUnit& u, CallableCode& code, Builder cleanup) {
            code.result_type = u.builtins.coroutine_void;
            const ExprId park = cleanup.Add(
                Expr{
                    .data = MachineBoolLiteral{.value = true},
                    .type = u.builtins.machine_bool});
            cleanup.AppendStmt(
                ExprStmt{
                    .expr = cleanup.Add(
                        Expr{
                            .data = WaitExpr{.park = park},
                            .type = u.builtins.void_type})});
          }),
      "a cleanup holds a wait, which is not stated not to leave it");
  EXPECT_NO_THROW(Verify(UnitWhoseCleanupHolds(
      [](const CompilationUnit& u, CallableCode&, Builder cleanup) {
        Block inner;
        Builder inner_at = cleanup.Into(inner);
        inner_at.AppendStmt(BreakStmt{.target = LabelId{.value = 1}});
        Block loop;
        Builder loop_at = cleanup.Into(loop);
        loop_at.AppendStmt(ContinueStmt{});
        const ExprId condition = cleanup.Add(
            Expr{
                .data = MachineBoolLiteral{.value = false},
                .type = u.builtins.machine_bool});
        cleanup.AppendStmt(
            BlockStmt{
                .scope = cleanup.AddScope(std::move(inner)),
                .break_label = LabelId{.value = 1}});
        cleanup.AppendStmt(
            WhileStmt{
                .condition = condition,
                .scope = cleanup.AddScope(std::move(loop))});
      })));
}

}  // namespace
}  // namespace lyra::mir

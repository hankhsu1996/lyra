#include "lyra/mir/verify.hpp"

#include <gtest/gtest.h>
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
#include "lyra/mir/local.hpp"
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
          .virtual_dispatch = std::nullopt});
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
  EXPECT_THROW(
      Verify(UnitWithBody([&](const CompilationUnit& u, Block& body) {
        const ExprId read = body.Read(place_over(u, body, AddCall(u, body)));
        body.AppendStmt(ExprStmt{.expr = AddSum(u, body, read, read)});
      })),
      InternalError);
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
                      .owner = IntraUnitClassRef{ClassId{.value = 0}},
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
      const ExprId bound =
          body.exprs.Add(Expr{.data = NullLiteral{}, .type = reference_type});
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
                              .type_argument = nibble},
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
          .element_type = unit.builtins.int_type,
          .dim = UnpackedRange{.left = 0, .right = 4095}}});
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
  ExpectRefused(
      UnitWithCode([](CompilationUnit& u, CallableCode& code) {
        code.locals.Add(LocalDecl{.type = TooLargeForAFrame(u)});
      }),
      "declares a local too large for a frame");
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

// A unit states operations only for a type it states them for per unit, and a
// default for each type an array of it holds, each once, and a call names one
// only where the unit states it.
TEST(MirVerifyTest, AUnitStatesEachTypeOperationOnceAndCallsOnlyThose) {
  const auto calling_the_default = [](bool stated) {
    return UnitWithBody([stated](CompilationUnit& u, Block& body) {
      const TypeId array = u.types.Intern(
          Type{UnpackedArrayType{
              .element_type = u.builtins.int_type,
              .dim = UnpackedRange{.left = 0, .right = 3}}});
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

}  // namespace
}  // namespace lyra::mir

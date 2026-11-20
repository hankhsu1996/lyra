#include "lyra/lir/verify.hpp"

#include <cstdint>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/record_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/linkage.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lir {
namespace {

// A unit holding one body, which takes a machine integer and has two slots of
// a pair of them, and runs `instr` before it returns.
struct PairBody {
  CompilationUnit unit;
  TypeId integer;
  TypeId pair;
  Function fn;
  ValueId x;
  ValueId slot;
  ValueId other;

  PairBody() {
    integer = unit.types.Intern(
        Type{MachineIntType{
            .width = MachineIntWidth::k32, .signedness = Signedness::kSigned}});
    pair = unit.types.Intern(Type{TupleType{.elements = {integer, integer}}});
    fn.name = "f";
    fn.result_type = unit.types.Intern(Type{VoidType{}});
    x = fn.values.Add(
        Local{.name = "x", .type = integer, .kind = LocalKind::kParam});
    fn.params.push_back(x);
    slot = fn.values.Add(
        Local{.name = "slot", .type = pair, .kind = LocalKind::kPlace});
    other = fn.values.Add(
        Local{.name = "other", .type = pair, .kind = LocalKind::kPlace});
  }

  // A record of the aggregate `kind` declares, holding two machine integers.
  auto RecordOfTwo(RecordKind kind) -> TypeId {
    const RecordId id = unit.records.Add(
        RecordDecl{
            .defining_unit = std::nullopt,
            .path = {},
            .linkage = support::Linkage::kExternal,
            .kind = kind,
            .elements = {integer, integer},
            .methods = {}});
    return unit.types.Intern(Type{RecordType{.declaration = id}});
  }

  // A value of `type` the body defines once.
  auto Transient(TypeId type) -> Destination {
    return lir::Transient{
        .value = fn.values.Add(
            Local{.name = "t", .type = type, .kind = LocalKind::kTemp})};
  }

  void VerifyRunning(Instr instr) {
    fn.blocks.push_back(
        BasicBlock{
            .instrs = {std::move(instr)},
            .terminator = Terminator{.data = ReturnTerm{}}});
    unit.functions.Add(FunctionAttributes{}, std::move(fn));
    Verify(unit);
  }
};

auto PlaceOf(ValueId local) -> Place {
  return Place{.base = Use{.value = local}, .chain = {}};
}

void ExpectRefused(PairBody& body, Instr instr, const std::string& because) {
  try {
    body.VerifyRunning(std::move(instr));
    FAIL() << "the verifier accepted it";
  } catch (const InternalError& error) {
    EXPECT_NE(std::string(error.what()).find(because), std::string::npos)
        << error.what();
  }
}

// A value made where it is put initializes storage holding nothing yet.
TEST(LirVerifyTest, AValueMadeIntoAnEmptySlotInitializesIt) {
  PairBody body;
  const Operand x = Use{.value = body.x};
  EXPECT_NO_THROW(body.VerifyRunning(
      Instr{
          .data = AssignInstr{
              .destination = PlaceOf(body.slot),
              .value = ProductInstr{.components = {x, x}}}}));
}

// A read makes no value: what it answers is owned by the storage it read, so
// putting it in storage would leave two owners of one value.
TEST(LirVerifyTest, AReadIsNeverPutIntoStorage) {
  PairBody body;
  ExpectRefused(
      body,
      Instr{
          .data =
              AssignInstr{
                  .destination = PlaceOf(body.slot),
                  .value = LoadInstr{.place = PlaceOf(body.other)}}},
      "where nothing is made");
}

// A part of a slot lies in a value the slot already holds, so a value put
// there would overwrite one without ending it; that is a store's work.
TEST(LirVerifyTest, AValueIsNeverPutIntoAPartOfAHeldValue) {
  PairBody body;
  Place part = PlaceOf(body.slot);
  part.chain.emplace_back(
      ComponentProjection{.index = base::ComponentIndex{.value = 0}});
  ExpectRefused(
      body,
      Instr{
          .data =
              AssignInstr{
                  .destination = std::move(part),
                  .value = UseInstr{.operand = Use{.value = body.x}}}},
      "already holds one");
}

// The return place holds nothing until the body makes its answer there, so the
// answer may be built in it part by part, as rustc's clone shim builds `Self`.
TEST(LirVerifyTest, AnAnswerIsBuiltPartByPartInItsReturnPlace) {
  PairBody body;
  body.fn.result_type = body.pair;
  body.fn.return_place = body.slot;
  Place first = PlaceOf(body.slot);
  first.chain.emplace_back(
      ComponentProjection{.index = base::ComponentIndex{.value = 0}});
  EXPECT_NO_THROW(body.VerifyRunning(
      Instr{
          .data = AssignInstr{
              .destination = std::move(first),
              .value = UseInstr{.operand = Use{.value = body.x}}}}));
}

// What a parameter points at holds whatever its caller left there, so nothing
// is initialized there; a value is built there anew only by ending the one it
// holds in the same statement.
TEST(LirVerifyTest, AValueIsBuiltWhereAParameterPointsOnlyInPlaceOfOne) {
  const auto pointed = [](PairBody& body) {
    const TypeId to_pair = body.unit.types.Intern(
        Type{PointerType{
            .pointee = body.pair,
            .ownership = PointerOwnership::kBorrowed,
            .mutability = Mutability::kMutable}});
    const ValueId at = body.fn.values.Add(
        Local{.name = "at", .type = to_pair, .kind = LocalKind::kParam});
    body.fn.params.push_back(at);
    return Place{.base = Use{.value = at}, .chain = {DerefProjection{}}};
  };

  PairBody initialized;
  const Operand x = Use{.value = initialized.x};
  const Place into = pointed(initialized);
  ExpectRefused(
      initialized,
      Instr{
          .data =
              AssignInstr{
                  .destination = into,
                  .value = ProductInstr{.components = {x, x}}}},
      "already holds one");

  PairBody replaced;
  const Operand y = Use{.value = replaced.x};
  const Place over = pointed(replaced);
  EXPECT_NO_THROW(replaced.VerifyRunning(
      Instr{
          .data = EndAndReplaceInstr{
              .place = over, .value = ProductInstr{.components = {y, y}}}}));
}

// A tagged union is built holding one of its own members, so a position past
// the last names storage the type does not have.
TEST(LirVerifyTest, ATaggedUnionIsBuiltOnlyHoldingAMemberItHas) {
  PairBody body;
  const TypeId union_type = body.RecordOfTwo(RecordKind::kTaggedUnion);
  ExpectRefused(
      body,
      Instr{
          .data =
              AssignInstr{
                  .destination = body.Transient(union_type),
                  .value =
                      TaggedUnionInstr{
                          .member = base::ComponentIndex{.value = 2},
                          .value = Use{.value = body.x}}}},
      "a member its type does not have");
}

// Only a tagged union keeps a tag (LRM 7.3.2), so a read of one in anything
// else reads bytes that hold some other value.
TEST(LirVerifyTest, ATagIsReadOnlyOfATaggedUnion) {
  PairBody body;
  const TypeId structure = body.RecordOfTwo(RecordKind::kStruct);
  const ValueId held = body.fn.values.Add(
      Local{.name = "s", .type = structure, .kind = LocalKind::kPlace});
  ExpectRefused(
      body,
      Instr{
          .data =
              AssignInstr{
                  .destination = body.Transient(body.integer),
                  .value = DiscriminantInstr{.place = PlaceOf(held)}}},
      "no tagged union");
}

// An element taken by value is a copy no write reaches, which is right only of
// a sequence of handles; any other element is storage of its own.
TEST(LirVerifyTest, AnElementIsTakenByValueOnlyOfASequenceOfHandles) {
  PairBody body;
  ExpectRefused(
      body,
      Instr{
          .data =
              AssignInstr{
                  .destination = body.Transient(body.integer),
                  .value =
                      VectorGetInstr{
                          .vector = Use{.value = body.x},
                          .index = Use{.value = body.x}}}},
      "a sequence of handles");
}

// A call of a function of the unit states what the function takes and answers,
// since below this layer two references are one machine address and a call at
// the wrong type would still link.
TEST(LirVerifyTest, ACallStatesWhatTheFunctionItEntersTakesAndAnswers) {
  // A body calling a function that takes a machine integer and answers a pair
  // with `args`, keeping the answer at `answered`.
  const auto calling = [](auto&& args_of, auto&& answered_of,
                          const std::string& because) {
    PairBody body;
    Function callee;
    callee.name = "callee";
    callee.result_type = body.pair;
    callee.params.push_back(callee.values.Add(
        Local{.name = "p", .type = body.integer, .kind = LocalKind::kParam}));
    const FunctionId called = body.unit.functions.Add(
        FunctionAttributes{.can_unwind = false}, std::move(callee));
    const Instr call{
        .data = AssignInstr{
            .destination = body.Transient(answered_of(body)),
            .value = CallInstr{
                .target = FunctionTarget{.function = called},
                .args = args_of(body)}}};
    if (because.empty()) {
      EXPECT_NO_THROW(body.VerifyRunning(call));
    } else {
      ExpectRefused(body, call, because);
    }
  };
  const auto one_integer = [](PairBody& body) {
    return std::vector<Operand>{Use{.value = body.x}};
  };
  const auto a_pair = [](PairBody& body) { return body.pair; };
  calling(one_integer, a_pair, "");
  calling(
      [](PairBody&) { return std::vector<Operand>{}; }, a_pair,
      "states 0 arguments where it takes 1");
  calling(
      [](PairBody& body) {
        return std::vector<Operand>{Use{.value = body.slot}};
      },
      a_pair, "of another type than its parameter");
  calling(
      one_integer, [](PairBody& body) { return body.integer; },
      "at another type than it answers");
}

// A body of a class hands its own object to what a class it derives from
// declares, so a receiver meets its parameter as an address of the same kind of
// any object, and nothing else does.
TEST(LirVerifyTest, OnlyAReceiverMeetsAnAddressOfAnotherClass) {
  // A body calling a function of two parameters, `first` and `second`, with
  // arguments of `first_handed` and `second_handed`.
  const auto calling = [](auto&& types_of, const std::string& because) {
    PairBody body;
    const auto [first, second, first_handed, second_handed] = types_of(body);
    Function callee;
    callee.name = "callee";
    callee.result_type = body.unit.types.Intern(Type{VoidType{}});
    callee.params.push_back(callee.values.Add(
        Local{.name = "a", .type = first, .kind = LocalKind::kParam}));
    callee.params.push_back(callee.values.Add(
        Local{.name = "b", .type = second, .kind = LocalKind::kParam}));
    const FunctionId called = body.unit.functions.Add(
        FunctionAttributes{.can_unwind = false}, std::move(callee));
    const ValueId one = body.fn.values.Add(
        Local{.name = "p", .type = first_handed, .kind = LocalKind::kParam});
    const ValueId other = body.fn.values.Add(
        Local{.name = "q", .type = second_handed, .kind = LocalKind::kParam});
    const Instr call{
        .data = AssignInstr{
            .destination = body.Transient(body.fn.result_type),
            .value = CallInstr{
                .target = FunctionTarget{.function = called},
                .args = {Use{.value = one}, Use{.value = other}}}}};
    if (because.empty()) {
      EXPECT_NO_THROW(body.VerifyRunning(call));
    } else {
      ExpectRefused(body, call, because);
    }
  };
  const auto object = [](PairBody& body, std::uint32_t id) {
    return body.unit.types.Intern(
        Type{ObjectType{.class_id = ClassId{.value = id}}});
  };
  const auto address = [](PairBody& body, TypeId pointee,
                          PointerOwnership ownership) {
    return body.unit.types.Intern(
        Type{PointerType{
            .pointee = pointee,
            .ownership = ownership,
            .mutability = Mutability::kReadOnly}});
  };
  struct Types {
    TypeId first;
    TypeId second;
    TypeId first_handed;
    TypeId second_handed;
  };
  calling(
      [&](PairBody& body) {
        const TypeId base =
            address(body, object(body, 0), PointerOwnership::kBorrowed);
        const TypeId derived =
            address(body, object(body, 1), PointerOwnership::kBorrowed);
        return Types{
            .first = base,
            .second = body.integer,
            .first_handed = derived,
            .second_handed = body.integer};
      },
      "");
  calling(
      [&](PairBody& body) {
        const TypeId base =
            address(body, object(body, 0), PointerOwnership::kBorrowed);
        const TypeId shared =
            address(body, object(body, 1), PointerOwnership::kShared);
        return Types{
            .first = base,
            .second = body.integer,
            .first_handed = shared,
            .second_handed = body.integer};
      },
      "of another type than its parameter");
  calling(
      [&](PairBody& body) {
        const TypeId base =
            address(body, object(body, 0), PointerOwnership::kBorrowed);
        const TypeId derived =
            address(body, object(body, 1), PointerOwnership::kBorrowed);
        return Types{
            .first = base,
            .second = base,
            .first_handed = base,
            .second_handed = derived};
      },
      "of another type than its parameter");
}

// A unit holding a body of `blocks`, declared as `caller` says, and a callee
// with no body of its own declared as `callee_attributes` says, whose function
// the body's calls name.
struct UnwindingBody {
  CompilationUnit unit;
  TypeId void_type;
  FunctionId callee;
  Function body;

  explicit UnwindingBody(FunctionAttributes callee_attributes) {
    void_type = unit.types.Intern(Type{VoidType{}});
    Function declared;
    declared.name = "callee";
    declared.result_type = void_type;
    callee = unit.functions.Add(callee_attributes, std::move(declared));
  }

  [[nodiscard]] auto Call() const -> CallInstr {
    return CallInstr{.target = FunctionTarget{.function = callee}, .args = {}};
  }

  // A value answering nothing the body defines once.
  auto Transient() -> Destination {
    return lir::Transient{
        .value = body.values.Add(
            Local{.name = "t", .type = void_type, .kind = LocalKind::kTemp})};
  }

  // A call as an instruction, answering nothing.
  auto CallInstruction() -> Instr {
    return Instr{
        .data = AssignInstr{.destination = Transient(), .value = Call()}};
  }

  // A block ending in a call that returns to `returned` and unwinds to
  // `unwind`.
  auto Invoking(BlockId returned, BlockId unwind) -> BasicBlock {
    return BasicBlock{
        .instrs = {},
        .terminator =
            Terminator{
                .data =
                    InvokeTerm{
                        .destination = Transient(),
                        .call = Call(),
                        .returned = returned,
                        .unwind = unwind}},
        .is_cleanup = false};
  }

  void Verify(FunctionAttributes caller, std::vector<BasicBlock> blocks) {
    body.name = "f";
    body.result_type = void_type;
    body.blocks = std::move(blocks);
    unit.functions.Add(caller, std::move(body));
    lir::Verify(unit);
  }
};

void ExpectRefusedUnwinding(
    UnwindingBody& unwinding, FunctionAttributes caller,
    std::vector<BasicBlock> blocks, std::string_view because) {
  try {
    unwinding.Verify(caller, std::move(blocks));
    FAIL() << "the verifier accepted it";
  } catch (const InternalError& error) {
    EXPECT_NE(
        std::string_view{error.what()}.find(because), std::string_view::npos)
        << error.what();
  }
}

auto Returning(std::vector<Instr> instrs) -> BasicBlock {
  return BasicBlock{
      .instrs = std::move(instrs),
      .terminator = Terminator{.data = ReturnTerm{}},
      .is_cleanup = false};
}

auto Resuming() -> BasicBlock {
  return BasicBlock{
      .instrs = {},
      .terminator = Terminator{.data = UnwindResumeTerm{}},
      .is_cleanup = true};
}

// A function declared not to unwind lets no unwinding out of it: a call that
// can be unwound out of, as an instruction, leaves with nothing to take it, and
// going on unwinding leaves too. In a function that can unwind, the same call
// is unwinding passing through a body that owes nothing.
TEST(LirVerifyTest, AFunctionDeclaredNotToUnwindLetsNoUnwindingOut) {
  {
    UnwindingBody unwinding(FunctionAttributes{.can_unwind = true});
    EXPECT_NO_THROW(unwinding.Verify(
        FunctionAttributes{.can_unwind = true},
        {Returning({unwinding.CallInstruction()})}));
  }
  {
    UnwindingBody unwinding(FunctionAttributes{.can_unwind = true});
    std::vector<BasicBlock> blocks{Returning({unwinding.CallInstruction()})};
    ExpectRefusedUnwinding(
        unwinding, FunctionAttributes{.can_unwind = false}, std::move(blocks),
        "declared not to unwind");
  }
  {
    UnwindingBody unwinding(FunctionAttributes{.can_unwind = true});
    std::vector<BasicBlock> blocks{
        unwinding.Invoking(BlockId{1}, BlockId{2}), Returning({}), Resuming()};
    ExpectRefusedUnwinding(
        unwinding, FunctionAttributes{.can_unwind = false}, std::move(blocks),
        "declared not to unwind");
  }
}

// A cleanup leaves only by running to its end: a block reached only by
// unwinding calls nothing that can be unwound out of, and control reaches one
// only by unwinding. A call names a block to unwind to only where its callee
// can be unwound out of.
TEST(LirVerifyTest, ACleanupLeavesOnlyByRunningToItsEnd) {
  {
    UnwindingBody unwinding(FunctionAttributes{.can_unwind = true});
    Instr call = unwinding.CallInstruction();
    std::vector<BasicBlock> blocks{BasicBlock{
        .instrs = {std::move(call)},
        .terminator = Terminator{.data = UnwindResumeTerm{}},
        .is_cleanup = true}};
    ExpectRefusedUnwinding(
        unwinding, FunctionAttributes{.can_unwind = true}, std::move(blocks),
        "reached only by unwinding calls");
  }
  {
    UnwindingBody unwinding(FunctionAttributes{.can_unwind = true});
    std::vector<BasicBlock> blocks{
        BasicBlock{
            .instrs = {},
            .terminator = Terminator{.data = BranchTerm{.target = BlockId{1}}},
            .is_cleanup = false},
        Resuming()};
    ExpectRefusedUnwinding(
        unwinding, FunctionAttributes{.can_unwind = true}, std::move(blocks),
        "control passes between");
  }
  {
    UnwindingBody unwinding(FunctionAttributes{.can_unwind = false});
    std::vector<BasicBlock> blocks{
        unwinding.Invoking(BlockId{1}, BlockId{2}), Returning({}), Resuming()};
    ExpectRefusedUnwinding(
        unwinding, FunctionAttributes{.can_unwind = true}, std::move(blocks),
        "its callee cannot");
  }
}

// Unwinding is taken only on a way out by unwinding, and goes on from there in
// an ordinary block, while what is not taken goes on in a block reached only by
// unwinding.
TEST(LirVerifyTest, UnwindingIsTakenOnlyOnAWayOutByUnwinding) {
  const FunctionAttributes may_unwind{.can_unwind = true};
  // Block 0 calls, returning to 1 and unwinding to `unwind`; block 2 is the
  // taking point, marked as `taken_in_cleanup` says; block 3 goes on
  // unwinding out of the body.
  const auto verifying = [&](bool taken_in_cleanup, BlockId unwind,
                             BlockId then, std::optional<BlockId> otherwise,
                             std::string_view because) {
    UnwindingBody unwinding(may_unwind);
    std::vector<BasicBlock> blocks;
    blocks.push_back(unwinding.Invoking(BlockId{1}, unwind));
    blocks.push_back(Returning({}));
    blocks.push_back(
        BasicBlock{
            .instrs = {},
            .terminator =
                Terminator{
                    .data =
                        ReceiveDepartureTerm{
                            .departure = unwinding.Transient(),
                            .then = then,
                            .otherwise = otherwise}},
            .is_cleanup = taken_in_cleanup});
    blocks.push_back(Resuming());
    if (because.empty()) {
      EXPECT_NO_THROW(unwinding.Verify(may_unwind, std::move(blocks)));
    } else {
      ExpectRefusedUnwinding(unwinding, may_unwind, std::move(blocks), because);
    }
  };
  verifying(true, BlockId{2}, BlockId{1}, BlockId{3}, "");
  verifying(true, BlockId{2}, BlockId{1}, std::nullopt, "");
  verifying(
      true, BlockId{2}, BlockId{3}, std::nullopt, "what is taken goes on");
  verifying(
      true, BlockId{2}, BlockId{1}, BlockId{1}, "what is not taken goes on");
  verifying(
      false, BlockId{3}, BlockId{1}, std::nullopt,
      "taken somewhere other than");
}

// A type's default is reached through what the type is told to the library by,
// so no call states what it answers.
TEST(LirVerifyTest, ATypesDefaultAnswersAValueOfThatType) {
  const support::ValueOperation default_value =
      support::DefaultItem::kDeclaredDefault;
  CompilationUnit unit;
  const TypeId integer = unit.types.Intern(
      Type{MachineIntType{
          .width = MachineIntWidth::k32, .signedness = Signedness::kSigned}});
  const TypeId other = unit.types.Intern(
      Type{MachineIntType{
          .width = MachineIntWidth::k64, .signedness = Signedness::kSigned}});
  Function fn;
  fn.name = "default";
  fn.result_type = other;
  fn.blocks.push_back(
      BasicBlock{.instrs = {}, .terminator = Terminator{.data = ReturnTerm{}}});
  const FunctionId function =
      unit.functions.Add(FunctionAttributes{}, std::move(fn));
  unit.type_operations.emplace(
      TypeOperation{.type = integer, .answers = default_value}, function);
  try {
    Verify(unit);
    FAIL() << "a default of another type passed";
  } catch (const InternalError& refused) {
    EXPECT_NE(
        std::string_view{refused.what()}.find("a value of another type"),
        std::string_view::npos);
  }
}

// A unit leaves here complete, so a call naming a function reaches a body.
TEST(LirVerifyTest, EveryFunctionDeclaredIsDefined) {
  CompilationUnit unit;
  unit.functions.Declare(FunctionAttributes{});
  try {
    Verify(unit);
    FAIL() << "a declared function with no body passed";
  } catch (const InternalError& refused) {
    EXPECT_NE(
        std::string_view{refused.what()}.find("defines nothing"),
        std::string_view::npos);
  }
}

}  // namespace
}  // namespace lyra::lir

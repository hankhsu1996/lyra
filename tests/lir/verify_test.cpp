#include "lyra/lir/verify.hpp"

#include <gtest/gtest.h>
#include <string>
#include <utility>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/linkage.hpp"
#include "lyra/lir/record_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

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
            .path = {},
            .linkage = Linkage::kExternal,
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
    unit.functions.Add(std::move(fn));
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

// A union is built holding one of its own members, so a position past the last
// names storage the type does not have.
TEST(LirVerifyTest, AUnionIsBuiltOnlyHoldingAMemberItHas) {
  PairBody body;
  const TypeId union_type = body.RecordOfTwo(RecordKind::kUnion);
  ExpectRefused(
      body,
      Instr{
          .data =
              AssignInstr{
                  .destination = body.Transient(union_type),
                  .value =
                      UnionInstr{
                          .member = base::ComponentIndex{.value = 2},
                          .value = Use{.value = body.x}}}},
      "a member its type does not have");
}

// Only a tagged union keeps a tag (LRM 7.3.2), so a read of one in anything
// else reads bytes that hold some other value.
TEST(LirVerifyTest, ATagIsReadOnlyOfATaggedUnion) {
  PairBody body;
  const TypeId untagged = body.RecordOfTwo(RecordKind::kUnion);
  const ValueId held = body.fn.values.Add(
      Local{.name = "u", .type = untagged, .kind = LocalKind::kPlace});
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

}  // namespace
}  // namespace lyra::lir

#include "lyra/lir/verify.hpp"

#include <gtest/gtest.h>
#include <string>
#include <utility>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
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

}  // namespace
}  // namespace lyra::lir

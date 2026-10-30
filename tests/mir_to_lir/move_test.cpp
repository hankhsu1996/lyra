#include <cstdint>
#include <gtest/gtest.h>
#include <map>
#include <optional>
#include <set>
#include <utility>
#include <variant>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/verify.hpp"
#include "lyra/lowering/mir_to_lir/lower.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/phase.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::mir_to_lir {
namespace {

auto AddEmptyBody(mir::CompilationUnit& unit) -> mir::CallableId {
  mir::CallableCode code = mir::CallableCode::Defined();
  code.result_type = unit.builtins.void_type;
  return unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
}

// A namespace unit whose one named body declares a shared pointer and then a
// second one starting as the first, taken either by a move or by a read. A
// shared pointer owns what it points at and has a copy, and a local holding
// one is a slot of its own rather than a variable.
auto UnitTakingASharedPointer(bool moved) -> mir::CompilationUnit {
  mir::CompilationUnit unit;
  unit.name = "U";
  unit.phase = mir::Phase::kLegalized;
  unit.body_name = "U";
  unit.content = mir::BroughtUpNamespace{
      .install_storage = AddEmptyBody(unit),
      .initialize_storage = AddEmptyBody(unit)};
  mir::CallableCode code = mir::CallableCode::Defined();
  code.result_type = unit.builtins.void_type;
  const mir::TypeId shared = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit.builtins.int_type,
          .ownership = mir::PointerOwnership::kShared}});
  const mir::LocalId source = code.locals.Add(mir::LocalDecl{.type = shared});
  const mir::LocalId taker = code.locals.Add(mir::LocalDecl{.type = shared});
  mir::Block& body = code.Body();
  const mir::ExprId zero = body.exprs.Add(
      mir::Expr{
          .data = mir::MachineIntLiteral{.value = 0},
          .type = unit.builtins.int_type});
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = source,
          .init = body.exprs.Add(
              mir::Expr{
                  .data = mir::NewExpr{.initializer = zero}, .type = shared})});
  const mir::ExprId named =
      body.exprs.Add(mir::MakeLocalRefExpr(source, shared));
  const mir::ExprId taken =
      moved ? body.exprs.Add(
                  mir::Expr{
                      .data = mir::MoveExpr{.operand = named}, .type = shared})
            : body.exprs.Add(mir::MakeCopyExpr(named, shared));
  body.AppendStmt(mir::LocalDeclStmt{.target = taker, .init = taken});
  const mir::CallableId id = unit.callables.Add(
      mir::CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
  unit.named_callables.push_back(mir::NamedCallable{.name = "f", .body = id});
  return unit;
}

// The slot a place names when it is a slot itself, with no step into it.
auto SlotOf(const lir::Place& place) -> std::optional<std::uint32_t> {
  const auto* slot = std::get_if<lir::Use>(&place.base);
  if (slot == nullptr || !place.chain.empty()) {
    return std::nullopt;
  }
  return slot->value.value;
}

// Which slots the two locals of the one named body are, and which slots that
// body ends the value of: the source is the slot initialized from no other
// slot, the taker the one initialized from what was read out of the source,
// and an end is of the slot its operand was read out of.
struct Slots {
  std::optional<std::uint32_t> source;
  std::optional<std::uint32_t> taker;
  std::set<std::uint32_t> ended;
};

auto SlotsOf(const lir::Function& function) -> Slots {
  std::map<std::uint32_t, std::uint32_t> read_from;
  Slots slots;
  for (const lir::BasicBlock& block : function.blocks) {
    for (const lir::Instr& instr : block.instrs) {
      const auto* assign = std::get_if<lir::AssignInstr>(&instr.data);
      if (assign == nullptr) {
        continue;
      }
      const auto* defined = std::get_if<lir::Transient>(&assign->destination);
      const auto* load = std::get_if<lir::LoadInstr>(&assign->value);
      if (defined != nullptr && load != nullptr) {
        if (const std::optional<std::uint32_t> slot = SlotOf(load->place)) {
          read_from[defined->value.value] = *slot;
        }
      }
      const auto* call = std::get_if<lir::CallInstr>(&assign->value);
      const auto* argument = call != nullptr && !call->args.empty()
                                 ? std::get_if<lir::Use>(&call->args.front())
                                 : nullptr;
      const bool of_a_slot =
          argument != nullptr && read_from.contains(argument->value.value);
      // A copy of what was read out of a slot stands for that slot's value.
      if (defined != nullptr && of_a_slot &&
          std::holds_alternative<lir::CopyValueTarget>(call->target)) {
        read_from[defined->value.value] = read_from.at(argument->value.value);
      }
      if (of_a_slot &&
          std::holds_alternative<lir::EndValueTarget>(call->target)) {
        slots.ended.insert(read_from.at(argument->value.value));
      }
      // A slot made from what another slot holds, used as it is or copied,
      // is the taker; one made from anything else is the source.
      const auto* stored = std::get_if<lir::Place>(&assign->destination);
      const std::optional<std::uint32_t> slot =
          stored != nullptr ? SlotOf(*stored) : std::nullopt;
      if (slot.has_value()) {
        const auto* used = std::get_if<lir::UseInstr>(&assign->value);
        const auto* value =
            used != nullptr ? std::get_if<lir::Use>(&used->operand) : nullptr;
        const bool from_a_slot =
            (value != nullptr && read_from.contains(value->value.value)) ||
            (of_a_slot &&
             std::holds_alternative<lir::CopyValueTarget>(call->target));
        (from_a_slot ? slots.taker : slots.source) = slot;
      }
    }
  }
  return slots;
}

// The function the body named `f` was lowered to, or none.
auto NamedBody(const lir::CompilationUnit& unit) -> const lir::Function* {
  for (const lir::Function& function : unit.functions) {
    if (function.name.ends_with(":f")) {
      return &function;
    }
  }
  return nullptr;
}

// A move hands the value on together with the source's end, whether or not
// its type has a copy: the taker owns the one value and ends it, and the
// source, ending, releases nothing. A read leaves two values, each ended.
TEST(MirToLirMoveTest, AMovedValueIsEndedByWhatTookItAndNotByItsSource) {
  const diag::Result<lir::CompilationUnit> moved =
      LowerUnit(UnitTakingASharedPointer(true));
  ASSERT_TRUE(moved.has_value());
  EXPECT_NO_THROW(lir::Verify(*moved));
  const lir::Function* moving = NamedBody(*moved);
  ASSERT_NE(moving, nullptr);
  const Slots after_move = SlotsOf(*moving);
  ASSERT_TRUE(after_move.source.has_value());
  ASSERT_TRUE(after_move.taker.has_value());
  EXPECT_EQ(after_move.ended, std::set{*after_move.taker});

  const diag::Result<lir::CompilationUnit> read =
      LowerUnit(UnitTakingASharedPointer(false));
  ASSERT_TRUE(read.has_value());
  const lir::Function* reading = NamedBody(*read);
  ASSERT_NE(reading, nullptr);
  const Slots after_read = SlotsOf(*reading);
  ASSERT_TRUE(after_read.source.has_value());
  ASSERT_TRUE(after_read.taker.has_value());
  EXPECT_EQ(
      after_read.ended, (std::set{*after_read.source, *after_read.taker}));
}

}  // namespace
}  // namespace lyra::lowering::mir_to_lir

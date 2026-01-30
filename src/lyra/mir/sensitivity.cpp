#include "lyra/mir/sensitivity.hpp"

#include <cstdint>
#include <unordered_set>
#include <variant>
#include <vector>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

namespace {

void CollectFromOperand(
    const Operand& op, const Arena& arena, std::unordered_set<uint32_t>& seen);

void CollectFromPlace(
    const Place& place, const Arena& arena,
    std::unordered_set<uint32_t>& seen) {
  if (place.root.kind != PlaceRoot::Kind::kDesign) {
    return;
  }
  seen.insert(static_cast<uint32_t>(place.root.id));

  for (const auto& proj : place.projections) {
    std::visit(
        common::Overloaded{
            [&](const IndexProjection& idx) {
              CollectFromOperand(idx.index, arena, seen);
            },
            [&](const BitRangeProjection& br) {
              CollectFromOperand(br.bit_offset, arena, seen);
            },
            [](const FieldProjection&) {},
            [](const SliceProjection&) {},
            [](const DerefProjection&) {},
            [](const UnionMemberProjection&) {},
        },
        proj.info);
  }
}

void CollectFromOperand(
    const Operand& op, const Arena& arena, std::unordered_set<uint32_t>& seen) {
  if (op.kind != Operand::Kind::kUse) {
    return;
  }
  auto place_id = std::get<PlaceId>(op.payload);
  CollectFromPlace(arena[place_id], arena, seen);
}

void CollectFromRhs(
    const RightHandSide& rhs, const Arena& arena,
    std::unordered_set<uint32_t>& seen);

void CollectFromRvalue(
    const Rvalue& rvalue, const Arena& arena,
    std::unordered_set<uint32_t>& seen) {
  for (const auto& op : rvalue.operands) {
    CollectFromOperand(op, arena, seen);
  }

  std::visit(
      common::Overloaded{
          [&](const GuardedUseRvalueInfo& info) {
            CollectFromPlace(arena[info.place], arena, seen);
          },
          [&](const BuiltinCallRvalueInfo& info) {
            if (info.receiver.has_value()) {
              CollectFromPlace(arena[*info.receiver], arena, seen);
            }
          },
          [](const UnaryRvalueInfo&) {},
          [](const BinaryRvalueInfo&) {},
          [](const CastRvalueInfo&) {},
          [](const BitCastRvalueInfo&) {},
          [](const AggregateRvalueInfo&) {},
          [](const IndexValidityRvalueInfo&) {},
          [](const ConcatRvalueInfo&) {},
          [](const SFormatRvalueInfo&) {},
          [](const TestPlusargsRvalueInfo&) {},
          [](const RuntimeQueryRvalueInfo&) {},
          [](const MathCallRvalueInfo&) {},
          [](const SystemTfRvalueInfo&) {},
      },
      rvalue.info);
}

void CollectFromRhs(
    const RightHandSide& rhs, const Arena& arena,
    std::unordered_set<uint32_t>& seen) {
  std::visit(
      common::Overloaded{
          [&](const Operand& op) { CollectFromOperand(op, arena, seen); },
          [&](const Rvalue& rv) { CollectFromRvalue(rv, arena, seen); },
      },
      rhs);
}

void CollectFromInstruction(
    const Instruction& instr, const Arena& arena,
    std::unordered_set<uint32_t>& seen) {
  std::visit(
      common::Overloaded{
          [&](const Assign& assign) {
            CollectFromRhs(assign.rhs, arena, seen);
          },
          [&](const GuardedAssign& ga) {
            CollectFromRhs(ga.rhs, arena, seen);
            CollectFromOperand(ga.guard, arena, seen);
          },
          [](const Effect&) {},
          [&](const DeferredAssign& da) {
            CollectFromRhs(da.rhs, arena, seen);
          },
          [&](const Call& call) {
            for (const auto& arg : call.args) {
              CollectFromOperand(arg, arena, seen);
            }
          },
          [&](const BuiltinCall& bcall) {
            for (const auto& arg : bcall.args) {
              CollectFromOperand(arg, arena, seen);
            }
          },
          [&](const ValuePlusargs& vp) {
            CollectFromOperand(vp.query, arena, seen);
          },
      },
      instr.data);
}

void CollectFromTerminator(
    const Terminator& term, const Arena& arena,
    std::unordered_set<uint32_t>& seen) {
  std::visit(
      common::Overloaded{
          [&](const Branch& branch) {
            CollectFromPlace(arena[branch.condition], arena, seen);
          },
          [](const Jump&) {},
          [](const Switch&) {},
          [](const QualifiedDispatch&) {},
          [](const Delay&) {},
          [](const Wait&) {},
          [](const Return&) {},
          [](const Finish&) {},
          [](const Repeat&) {},
      },
      term.data);
}

}  // namespace

auto CollectSensitivity(const Process& process, const Arena& arena)
    -> std::vector<WaitTrigger> {
  std::unordered_set<uint32_t> seen;

  for (const auto& block : process.blocks) {
    for (const auto& instr : block.instructions) {
      CollectFromInstruction(instr, arena, seen);
    }
    CollectFromTerminator(block.terminator, arena, seen);
  }

  std::vector<WaitTrigger> triggers;
  triggers.reserve(seen.size());
  for (uint32_t slot : seen) {
    triggers.push_back(
        {.signal = SlotId{slot}, .edge = common::EdgeKind::kAnyChange});
  }
  return triggers;
}

}  // namespace lyra::mir

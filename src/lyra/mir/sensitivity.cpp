#include "lyra/mir/sensitivity.hpp"

#include <cstdint>
#include <optional>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/assoc_op.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

namespace {

// Observation key: signal scope + id. Distinguishes module-local and
// design-global signals for deduplication and observation tracking.
struct SignalKey {
  SignalRef::Scope scope;
  uint32_t id;
  auto operator==(const SignalKey&) const -> bool = default;
};

struct SignalKeyHash {
  auto operator()(const SignalKey& k) const noexcept -> size_t {
    return std::hash<uint64_t>{}((static_cast<uint64_t>(k.scope) << 32) | k.id);
  }
};

// Per-signal observation tracking. For each signal, we track all distinct
// observed places. A nullopt entry means "full slot" and subsumes all sub-range
// observations.
struct ObservationSet {
  std::unordered_map<
      SignalKey, std::vector<std::optional<PlaceId>>, SignalKeyHash>
      entries;

  void Add(
      SignalRef::Scope scope, uint32_t id, std::optional<PlaceId> place_id) {
    SignalKey key{scope, id};
    auto& vec = entries[key];

    // If we already have a full-slot observation, no need to add more.
    for (const auto& entry : vec) {
      if (!entry.has_value()) return;
    }

    // If adding a full-slot observation, clear all sub-range entries.
    if (!place_id.has_value()) {
      vec.clear();
      vec.push_back(std::nullopt);
      return;
    }

    // Dedup identical PlaceIds.
    for (const auto& entry : vec) {
      if (entry.has_value() && entry->value == place_id->value) return;
    }

    vec.push_back(place_id);
  }
};

// Check if all projections in a place are potentially resolvable for
// observation narrowing. Accepts FieldProjection and IndexProjection with
// constant or temp indices (temps may resolve to constants at LLVM lowering
// time). The LLVM backend's ResolveByteRange determines actual constness;
// non-constant temps safely fall back to full-slot observation.
auto HasStaticObservation(const Place& place) -> bool {
  if (place.projections.empty()) return false;

  for (const auto& proj : place.projections) {
    bool is_static = std::visit(
        common::Overloaded{
            [](const FieldProjection&) { return true; },
            [](const IndexProjection& idx) {
              return idx.index.kind == Operand::Kind::kConst ||
                     idx.index.kind == Operand::Kind::kUseTemp;
            },
            [](const BitRangeProjection&) { return false; },
            [](const SliceProjection&) { return false; },
            [](const DerefProjection&) { return false; },
            [](const UnionMemberProjection&) { return false; },
        },
        proj.info);
    if (!is_static) return false;
  }
  return true;
}

void CollectFromOperand(
    const Operand& op, const Arena& arena, ObservationSet& obs);

void CollectFromPlace(
    PlaceId place_id, const Place& place, const Arena& arena,
    ObservationSet& obs) {
  // Only module-local and design-global roots produce sensitivity triggers.
  SignalRef::Scope scope;
  if (place.root.kind == PlaceRoot::Kind::kModuleSlot) {
    scope = SignalRef::Scope::kModuleLocal;
  } else if (place.root.kind == PlaceRoot::Kind::kDesignGlobal) {
    scope = SignalRef::Scope::kDesignGlobal;
  } else {
    return;
  }
  auto slot_id = static_cast<uint32_t>(place.root.id);

  if (HasStaticObservation(place)) {
    obs.Add(scope, slot_id, place_id);
  } else {
    obs.Add(scope, slot_id, std::nullopt);
  }

  // Continue collecting sub-reads from projection operands (e.g., dynamic
  // index expressions reference other design variables that are also
  // sensitivity triggers).
  for (const auto& proj : place.projections) {
    std::visit(
        common::Overloaded{
            [&](const IndexProjection& idx) {
              CollectFromOperand(idx.index, arena, obs);
            },
            [&](const BitRangeProjection& br) {
              CollectFromOperand(br.bit_offset, arena, obs);
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
    const Operand& op, const Arena& arena, ObservationSet& obs) {
  if (op.kind != Operand::Kind::kUse) {
    return;
  }
  auto place_id = std::get<PlaceId>(op.payload);
  CollectFromPlace(place_id, arena[place_id], arena, obs);
}

void CollectFromRhs(
    const RightHandSide& rhs, const Arena& arena, ObservationSet& obs);

void CollectFromRvalue(
    const Rvalue& rvalue, const Arena& arena, ObservationSet& obs) {
  for (const auto& op : rvalue.operands) {
    CollectFromOperand(op, arena, obs);
  }

  std::visit(
      common::Overloaded{
          [&](const GuardedUseRvalueInfo& info) {
            CollectFromPlace(info.place, arena[info.place], arena, obs);
          },
          [&](const BuiltinCallRvalueInfo& info) {
            if (info.receiver.has_value()) {
              CollectFromPlace(
                  *info.receiver, arena[*info.receiver], arena, obs);
            }
          },
          [](const UnaryRvalueInfo&) {},
          [](const BinaryRvalueInfo&) {},
          [](const CastRvalueInfo&) {},
          [](const BitCastRvalueInfo&) {},
          [](const AggregateRvalueInfo&) {},
          [](const IndexValidityRvalueInfo&) {},
          [](const ConcatRvalueInfo&) {},
          [](const ReplicateRvalueInfo&) {},
          [](const SFormatRvalueInfo&) {},
          [&](const TestPlusargsRvalueInfo& info) {
            CollectFromOperand(info.query.operand, arena, obs);
          },
          [&](const FopenRvalueInfo& info) {
            CollectFromOperand(info.filename.operand, arena, obs);
            if (info.mode) {
              CollectFromOperand(info.mode->operand, arena, obs);
            }
          },
          [](const RuntimeQueryRvalueInfo&) {},
          [](const MathCallRvalueInfo&) {},
          [](const SystemTfRvalueInfo&) {},
          [](const ArrayQueryRvalueInfo&) {},
          [&](const SystemCmdRvalueInfo& info) {
            if (info.command) {
              CollectFromOperand(info.command->operand, arena, obs);
            }
          },
      },
      rvalue.info);
}

void CollectFromRhs(
    const RightHandSide& rhs, const Arena& arena, ObservationSet& obs) {
  std::visit(
      common::Overloaded{
          [&](const Operand& op) { CollectFromOperand(op, arena, obs); },
          [&](const Rvalue& rv) { CollectFromRvalue(rv, arena, obs); },
      },
      rhs);
}

void CollectFromStatement(
    const Statement& stmt, const Arena& arena, ObservationSet& obs) {
  std::visit(
      common::Overloaded{
          [&](const Assign& assign) { CollectFromRhs(assign.rhs, arena, obs); },
          [&](const GuardedAssign& ga) {
            CollectFromRhs(ga.rhs, arena, obs);
            CollectFromOperand(ga.guard, arena, obs);
          },
          [](const Effect&) {},
          [&](const DeferredAssign& da) { CollectFromRhs(da.rhs, arena, obs); },
          [&](const Call& call) {
            for (const auto& arg : call.in_args) {
              CollectFromOperand(arg, arena, obs);
            }
          },
          [&](const BuiltinCall& bcall) {
            for (const auto& arg : bcall.args) {
              CollectFromOperand(arg, arena, obs);
            }
          },
          [&](const DefineTemp& dt) { CollectFromRhs(dt.rhs, arena, obs); },
          [&](const AssocOp& aop) {
            std::visit(
                [&](const auto& op) {
                  using T = std::decay_t<decltype(op)>;
                  if constexpr (requires { op.key; }) {
                    CollectFromOperand(op.key, arena, obs);
                  }
                  if constexpr (requires { op.value; }) {
                    CollectFromOperand(op.value, arena, obs);
                  }
                },
                aop.data);
          },
      },
      stmt.data);
}

void CollectFromTerminator(
    const Terminator& term, const Arena& arena, ObservationSet& obs) {
  auto collect = [&](const Operand& op) { CollectFromOperand(op, arena, obs); };
  ForEachLocalOperand(term, collect);
  ForEachSuccessor(term, [&](const TerminatorSuccessor& succ) {
    for (const auto& arg : succ.args) {
      collect(arg);
    }
  });
}

}  // namespace

auto CollectSensitivity(const Process& process, const Arena& arena)
    -> std::vector<WaitTrigger> {
  ObservationSet obs;

  for (const auto& block : process.blocks) {
    for (const auto& stmt : block.statements) {
      CollectFromStatement(stmt, arena, obs);
    }
    CollectFromTerminator(block.terminator, arena, obs);
  }

  std::vector<WaitTrigger> triggers;
  for (const auto& [key, places] : obs.entries) {
    for (const auto& place : places) {
      triggers.push_back(
          {.signal = SignalRef{.scope = key.scope, .id = key.id},
           .edge = common::EdgeKind::kAnyChange,
           .observed_place = place});
    }
  }
  return triggers;
}

}  // namespace lyra::mir

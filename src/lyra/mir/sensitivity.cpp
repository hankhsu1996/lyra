#include "lyra/mir/sensitivity.hpp"

#include <cstdint>
#include <optional>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/mir/access_path.hpp"
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

// Read collection with must-def filtering.
// These functions thread a MustDefSet to suppress reads of whole signals
// that are already must-defined (written on all paths to this point).

void CollectReadsFromOperand(
    const Operand& op, const Arena& arena, const MustDefSet& must_def,
    ObservationSet& obs);

void CollectReadsFromPlace(
    PlaceId place_id, const Place& place, const Arena& arena,
    const MustDefSet& must_def, ObservationSet& obs) {
  // Only module-local and design-global roots produce sensitivity triggers.
  SignalRef::Scope scope{};
  if (place.root.kind == PlaceRoot::Kind::kModuleSlot) {
    scope = SignalRef::Scope::kModuleLocal;
  } else if (place.root.kind == PlaceRoot::Kind::kDesignGlobal) {
    scope = SignalRef::Scope::kDesignGlobal;
  } else {
    return;
  }
  auto slot_id = static_cast<uint32_t>(place.root.id);

  // Must-def filter: suppress reads covered by a prior must-def write.
  // Phase 1: only whole-signal reads can be suppressed (no projections).
  auto path = TryExtractWholeSignalPath(place);
  if (path.has_value() && must_def.Covers(*path)) {
    return;
  }

  if (HasStaticObservation(place)) {
    obs.Add(scope, slot_id, place_id);
  } else {
    obs.Add(scope, slot_id, std::nullopt);
  }

  // Continue collecting sub-reads from projection operands (e.g., dynamic
  // index expressions reference other design variables that are also
  // sensitivity triggers). These are collected independently of the
  // must-def filter on the signal itself.
  for (const auto& proj : place.projections) {
    std::visit(
        common::Overloaded{
            [&](const IndexProjection& idx) {
              CollectReadsFromOperand(idx.index, arena, must_def, obs);
            },
            [&](const BitRangeProjection& br) {
              CollectReadsFromOperand(br.bit_offset, arena, must_def, obs);
            },
            [](const FieldProjection&) {},
            [](const SliceProjection&) {},
            [](const DerefProjection&) {},
            [](const UnionMemberProjection&) {},
        },
        proj.info);
  }
}

void CollectReadsFromOperand(
    const Operand& op, const Arena& arena, const MustDefSet& must_def,
    ObservationSet& obs) {
  if (op.kind != Operand::Kind::kUse) {
    return;
  }
  auto place_id = std::get<PlaceId>(op.payload);
  CollectReadsFromPlace(place_id, arena[place_id], arena, must_def, obs);
}

void CollectReadsFromRhs(
    const RightHandSide& rhs, const Arena& arena, const MustDefSet& must_def,
    ObservationSet& obs);

void CollectReadsFromRvalue(
    const Rvalue& rvalue, const Arena& arena, const MustDefSet& must_def,
    ObservationSet& obs) {
  for (const auto& op : rvalue.operands) {
    CollectReadsFromOperand(op, arena, must_def, obs);
  }

  std::visit(
      common::Overloaded{
          [&](const GuardedUseRvalueInfo& info) {
            CollectReadsFromPlace(
                info.place, arena[info.place], arena, must_def, obs);
          },
          [&](const BuiltinCallRvalueInfo& info) {
            if (info.receiver.has_value()) {
              CollectReadsFromPlace(
                  *info.receiver, arena[*info.receiver], arena, must_def, obs);
            }
          },
          [](const UnaryRvalueInfo&) {},
          [](const BinaryRvalueInfo&) {},
          [](const CastRvalueInfo&) {},
          [](const BitCastRvalueInfo&) {},
          [](const AggregateRvalueInfo&) {},
          [](const IsKnownRvalueInfo&) {},
          [](const IndexInRangeRvalueInfo&) {},
          [](const ConcatRvalueInfo&) {},
          [](const ReplicateRvalueInfo&) {},
          [](const SFormatRvalueInfo&) {},
          [&](const TestPlusargsRvalueInfo& info) {
            CollectReadsFromOperand(info.query.operand, arena, must_def, obs);
          },
          [&](const FopenRvalueInfo& info) {
            CollectReadsFromOperand(
                info.filename.operand, arena, must_def, obs);
            if (info.mode) {
              CollectReadsFromOperand(info.mode->operand, arena, must_def, obs);
            }
          },
          [](const RuntimeQueryRvalueInfo&) {},
          [](const MathCallRvalueInfo&) {},
          [](const SystemTfRvalueInfo&) {},
          [](const ArrayQueryRvalueInfo&) {},
          [&](const SystemCmdRvalueInfo& info) {
            if (info.command) {
              CollectReadsFromOperand(
                  info.command->operand, arena, must_def, obs);
            }
          },
      },
      rvalue.info);
}

void CollectReadsFromRhs(
    const RightHandSide& rhs, const Arena& arena, const MustDefSet& must_def,
    ObservationSet& obs) {
  std::visit(
      common::Overloaded{
          [&](const Operand& op) {
            CollectReadsFromOperand(op, arena, must_def, obs);
          },
          [&](const Rvalue& rv) {
            CollectReadsFromRvalue(rv, arena, must_def, obs);
          },
      },
      rhs);
}

// Collect reads from a statement and advance must-def state.
// Order: reads first (against current must_def), then writes update must_def.
void TransferStatement(
    const Statement& stmt, const Arena& arena, MustDefSet& must_def,
    ObservationSet& obs) {
  std::visit(
      common::Overloaded{
          [&](const Assign& assign) {
            // 1. Collect reads from RHS
            CollectReadsFromRhs(assign.rhs, arena, must_def, obs);
            // 2. Transfer: unconditional whole-signal write extends must-def.
            // Phase 1: only plain Assign to an unprojected signal root.
            // Projected, guarded, and deferred writes are conservative.
            const auto& dest = arena[assign.dest];
            auto path = TryExtractWholeSignalPath(dest);
            if (path.has_value()) {
              must_def.Insert(*path);
            }
          },
          [&](const GuardedAssign& ga) {
            // Conditional write: collect reads but do NOT extend must-def
            CollectReadsFromRhs(ga.rhs, arena, must_def, obs);
            CollectReadsFromOperand(ga.guard, arena, must_def, obs);
          },
          [](const Effect&) {},
          [&](const DeferredAssign& da) {
            // NBA: collect reads but do NOT extend must-def
            // (write happens in a later region, not before subsequent reads)
            CollectReadsFromRhs(da.rhs, arena, must_def, obs);
          },
          [&](const Call& call) {
            for (const auto& arg : call.in_args) {
              CollectReadsFromOperand(arg, arena, must_def, obs);
            }
            // Conservative: calls may have side effects, do not extend must-def
          },
          [&](const DpiCall& dpi_call) {
            for (const auto& binding : dpi_call.args) {
              if (binding.input_value) {
                CollectReadsFromOperand(
                    *binding.input_value, arena, must_def, obs);
              }
            }
            // Conservative: DPI calls may have side effects
          },
          [&](const BuiltinCall& bcall) {
            for (const auto& arg : bcall.args) {
              CollectReadsFromOperand(arg, arena, must_def, obs);
            }
            // Conservative: builtin calls do not extend must-def
          },
          [&](const DefineTemp& dt) {
            CollectReadsFromRhs(dt.rhs, arena, must_def, obs);
            // Temps are not signals, no must-def effect
          },
          [&](const AssocOp& aop) {
            std::visit(
                [&](const auto& op) {
                  using T = std::decay_t<decltype(op)>;
                  if constexpr (requires { op.key; }) {
                    CollectReadsFromOperand(op.key, arena, must_def, obs);
                  }
                  if constexpr (requires { op.value; }) {
                    CollectReadsFromOperand(op.value, arena, must_def, obs);
                  }
                },
                aop.data);
            // Conservative: assoc ops do not extend must-def
          },
      },
      stmt.data);
}

// Collect reads from a terminator (conditions, selectors, edge args).
void CollectReadsFromTerminator(
    const Terminator& term, const Arena& arena, const MustDefSet& must_def,
    ObservationSet& obs) {
  auto collect = [&](const Operand& op) {
    CollectReadsFromOperand(op, arena, must_def, obs);
  };
  ForEachLocalOperand(term, collect);
  ForEachSuccessor(term, [&](const TerminatorSuccessor& succ) {
    for (const auto& arg : succ.args) {
      collect(arg);
    }
  });
}

// Compute reverse postorder of the CFG.
void ComputeRPO(
    uint32_t block_idx, const std::vector<BasicBlock>& blocks,
    std::vector<bool>& visited, std::vector<uint32_t>& rpo) {
  visited[block_idx] = true;
  ForEachSuccessor(blocks[block_idx].terminator, [&](const auto& succ) {
    auto target = succ.target.value;
    if (target < blocks.size() && !visited[target]) {
      ComputeRPO(target, blocks, visited, rpo);
    }
  });
  rpo.push_back(block_idx);
}

}  // namespace

auto CollectSensitivity(const Process& process, const Arena& arena)
    -> std::vector<WaitTrigger> {
  const auto& blocks = process.blocks;
  if (blocks.empty()) return {};

  auto num_blocks = static_cast<uint32_t>(blocks.size());
  auto entry_idx = process.entry.value;

  // Build predecessor lists for the meet operation.
  std::vector<std::vector<uint32_t>> predecessors(num_blocks);
  for (uint32_t i = 0; i < num_blocks; ++i) {
    ForEachSuccessor(blocks[i].terminator, [&](const auto& succ) {
      auto target = succ.target.value;
      if (target < num_blocks) {
        predecessors[target].push_back(i);
      }
    });
  }

  // Compute reverse postorder for efficient iteration.
  std::vector<bool> visited(num_blocks, false);
  std::vector<uint32_t> rpo;
  rpo.reserve(num_blocks);
  ComputeRPO(entry_idx, blocks, visited, rpo);
  std::reverse(rpo.begin(), rpo.end());

  // Initialize dataflow state.
  // Entry block starts with Empty (nothing is must-defined).
  // All other blocks start with Top (optimistic -- refined by meet).
  std::vector<MustDefSet> block_in(num_blocks, MustDefSet::Top());
  std::vector<MustDefSet> block_out(num_blocks, MustDefSet::Top());
  block_in[entry_idx] = MustDefSet::Empty();

  // Observations are accumulated monotonically across fixpoint iterations.
  // The must-def lattice starts optimistic (Top for non-entry blocks) and
  // can only become more conservative (smaller sets) on later iterations.
  // More conservative must-def means fewer reads are suppressed, so later
  // iterations may only ADD observations, never require removing earlier ones.
  // This makes union-accumulation into a single ObservationSet safe.
  ObservationSet obs;

  // Forward dataflow fixpoint iteration.
  // Reads are collected during transfer so that each read sees the
  // actual must-def state at its program point, not a summary.
  bool changed = true;
  while (changed) {
    changed = false;
    for (auto block_idx : rpo) {
      // Meet: intersect all predecessor out states.
      MustDefSet in_state =
          block_idx == entry_idx ? MustDefSet::Empty() : MustDefSet::Top();
      for (auto pred : predecessors[block_idx]) {
        in_state.IntersectWith(block_out[pred]);
      }

      if (!(in_state == block_in[block_idx])) {
        block_in[block_idx] = in_state;
      }

      // Transfer: walk statements with running must-def, collecting reads.
      MustDefSet state = block_in[block_idx];
      for (const auto& stmt : blocks[block_idx].statements) {
        TransferStatement(stmt, arena, state, obs);
      }

      // Collect reads from the terminator with the final block state.
      CollectReadsFromTerminator(
          blocks[block_idx].terminator, arena, state, obs);

      // Update out state and check for convergence.
      if (!(state == block_out[block_idx])) {
        block_out[block_idx] = state;
        changed = true;
      }
    }
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

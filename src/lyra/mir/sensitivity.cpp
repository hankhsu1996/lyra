#include "lyra/mir/sensitivity.hpp"

#include <cstdint>
#include <optional>
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
    SignalKey key{.scope = scope, .id = id};
    auto& vec = entries[key];

    // If we already have a full-slot observation, no need to add more.
    for (const auto& entry : vec) {
      if (!entry.has_value()) return;
    }

    // If adding a full-slot observation, clear all sub-range entries.
    if (!place_id.has_value()) {
      vec.clear();
      vec.emplace_back(std::nullopt);
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
    ObservationSet& obs,
    const std::optional<SensitivityExternalRefEnv>& ext_ref_env = {});

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
  auto path = TryExtractWholeSignalPath(place);
  if (path.has_value() && must_def.Covers(*path)) {
    return;
  }

  if (HasStaticObservation(place)) {
    obs.Add(scope, slot_id, place_id);
  } else {
    obs.Add(scope, slot_id, std::nullopt);
  }

  // Continue collecting sub-reads from projection operands.
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

// Resolve ExternalRefId to the same canonical SignalRef form used by
// ordinary place-root reads. Both PlaceId and ExternalRefId reads converge
// on the same signal-owner path before triggers are emitted.
auto ResolveExternalRefSignal(
    ExternalRefId ref_id, const SensitivityExternalRefEnv& env) -> SignalRef {
  return std::visit(
      common::Overloaded{
          [&](const PreBindingSensitivityEnv& pre) -> SignalRef {
            if (ref_id.value >= pre.resolved_slots.size()) {
              throw common::InternalError(
                  "ResolveExternalRefSignal",
                  std::format(
                      "external ref {} out of range (pre-resolved size {})",
                      ref_id.value, pre.resolved_slots.size()));
            }
            const auto& slot = pre.resolved_slots[ref_id.value];
            if (!slot.has_value()) {
              throw common::InternalError(
                  "ResolveExternalRefSignal",
                  std::format(
                      "external ref {} has no pre-resolved sensitivity slot",
                      ref_id.value));
            }
            return SignalRef{
                .scope = SignalRef::Scope::kDesignGlobal, .id = *slot};
          },
          [&](const PostBindingSensitivityEnv& post) -> SignalRef {
            if (post.bindings == nullptr || post.construction == nullptr) {
              throw common::InternalError(
                  "ResolveExternalRefSignal",
                  "post-binding env has null bindings or construction");
            }
            if (ref_id.value >= post.bindings->size()) {
              throw common::InternalError(
                  "ResolveExternalRefSignal",
                  std::format(
                      "external ref {} out of range (bindings size {})",
                      ref_id.value, post.bindings->size()));
            }
            const auto& binding = (*post.bindings)[ref_id.value];
            const auto& obj =
                post.construction->objects.at(binding.target_object.value);
            auto global_slot =
                obj.design_state_base_slot + binding.target_local_slot.value;
            return SignalRef{
                .scope = SignalRef::Scope::kDesignGlobal, .id = global_slot};
          },
      },
      env);
}

void CollectReadsFromOperand(
    const Operand& op, const Arena& arena, const MustDefSet& must_def,
    ObservationSet& obs,
    const std::optional<SensitivityExternalRefEnv>& ext_ref_env) {
  if (op.kind == Operand::Kind::kUse) {
    auto place_id = std::get<PlaceId>(op.payload);
    CollectReadsFromPlace(place_id, arena[place_id], arena, must_def, obs);
    return;
  }
  if (op.kind == Operand::Kind::kExternalRef) {
    if (!ext_ref_env.has_value()) {
      throw common::InternalError(
          "CollectReadsFromOperand",
          "ExternalRefId in process MIR requires sensitivity resolution env");
    }
    auto ref_id = std::get<ExternalRefId>(op.payload);
    auto signal = ResolveExternalRefSignal(ref_id, *ext_ref_env);
    obs.Add(signal.scope, signal.id, std::nullopt);
    return;
  }
}

void CollectReadsFromRhs(
    const RightHandSide& rhs, const Arena& arena, const MustDefSet& must_def,
    ObservationSet& obs,
    const std::optional<SensitivityExternalRefEnv>& ext_ref_env = {});

void CollectReadsFromRvalue(
    const Rvalue& rvalue, const Arena& arena, const MustDefSet& must_def,
    ObservationSet& obs,
    const std::optional<SensitivityExternalRefEnv>& ext_ref_env = {}) {
  for (const auto& op : rvalue.operands) {
    CollectReadsFromOperand(op, arena, must_def, obs, ext_ref_env);
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
          [](const SelectRvalueInfo&) {},
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
    ObservationSet& obs,
    const std::optional<SensitivityExternalRefEnv>& ext_ref_env) {
  std::visit(
      common::Overloaded{
          [&](const Operand& op) {
            CollectReadsFromOperand(op, arena, must_def, obs, ext_ref_env);
          },
          [&](const Rvalue& rv) {
            CollectReadsFromRvalue(rv, arena, must_def, obs, ext_ref_env);
          },
      },
      rhs);
}

// Collect reads from a statement and advance must-def state.
// Order: reads first (against current must_def), then writes update must_def.
void TransferStatement(
    const Statement& stmt, const Arena& arena, MustDefSet& must_def,
    ObservationSet& obs,
    const std::optional<SensitivityExternalRefEnv>& ext_ref_env = {}) {
  std::visit(
      common::Overloaded{
          [&](const Assign& assign) {
            // 1. Collect reads from RHS
            CollectReadsFromRhs(assign.rhs, arena, must_def, obs, ext_ref_env);
            // 2. Transfer: unconditional whole-signal write extends must-def.
            // Phase 1: only plain Assign to an unprojected signal root.
            // Projected, guarded, and deferred writes are conservative.
            // ExternalRefId targets have no local place -- skip must-def.
            const auto* dest_pid = std::get_if<PlaceId>(&assign.dest);
            if (!dest_pid) return;
            const auto& dest = arena[*dest_pid];
            auto path = TryExtractWholeSignalPath(dest);
            if (path.has_value()) {
              must_def.Insert(*path);
            }
          },
          [&](const GuardedAssign& ga) {
            CollectReadsFromRhs(ga.rhs, arena, must_def, obs, ext_ref_env);
            CollectReadsFromOperand(
                ga.guard, arena, must_def, obs, ext_ref_env);
          },
          [](const Effect&) {},
          [&](const DeferredAssign& da) {
            CollectReadsFromRhs(da.rhs, arena, must_def, obs, ext_ref_env);
          },
          [&](const Call& call) {
            for (const auto& arg : call.in_args) {
              CollectReadsFromOperand(arg, arena, must_def, obs, ext_ref_env);
            }
            // Conservative: calls may have side effects, do not extend must-def
          },
          [&](const DpiCall& dpi_call) {
            for (const auto& binding : dpi_call.args) {
              if (binding.input_value) {
                CollectReadsFromOperand(
                    *binding.input_value, arena, must_def, obs, ext_ref_env);
              }
            }
            // Conservative: DPI calls may have side effects
          },
          [&](const BuiltinCall& bcall) {
            for (const auto& arg : bcall.args) {
              CollectReadsFromOperand(arg, arena, must_def, obs, ext_ref_env);
            }
            // Conservative: builtin calls do not extend must-def
          },
          [&](const DefineTemp& dt) {
            CollectReadsFromRhs(dt.rhs, arena, must_def, obs, ext_ref_env);
            // Temps are not signals, no must-def effect
          },
          [&](const AssocOp& aop) {
            std::visit(
                [&](const auto& op) {
                  if constexpr (requires { op.key; }) {
                    CollectReadsFromOperand(
                        op.key, arena, must_def, obs, ext_ref_env);
                  }
                  if constexpr (requires { op.value; }) {
                    CollectReadsFromOperand(
                        op.value, arena, must_def, obs, ext_ref_env);
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
    ObservationSet& obs,
    const std::optional<SensitivityExternalRefEnv>& ext_ref_env = {}) {
  auto collect = [&](const Operand& op) {
    CollectReadsFromOperand(op, arena, must_def, obs, ext_ref_env);
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

auto CollectSensitivity(
    const Process& process, const Arena& arena,
    const std::optional<SensitivityExternalRefEnv>& ext_ref_env)
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
  std::ranges::reverse(rpo);

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
        TransferStatement(stmt, arena, state, obs, ext_ref_env);
      }

      // Collect reads from the terminator with the final block state.
      CollectReadsFromTerminator(
          blocks[block_idx].terminator, arena, state, obs, ext_ref_env);

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
           .observed_place = place,
           .late_bound = std::nullopt});
    }
  }
  return triggers;
}

}  // namespace lyra::mir

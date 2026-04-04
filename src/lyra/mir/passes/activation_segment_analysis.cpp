#include "lyra/mir/passes/activation_segment_analysis.hpp"

#include <algorithm>
#include <cstdint>
#include <queue>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir::passes {

namespace {

auto IsSegmentExitTerminator(const Terminator& term) -> bool {
  return std::visit(
      [](const auto& t) -> bool {
        using T = std::decay_t<decltype(t)>;
        return std::is_same_v<T, Delay> || std::is_same_v<T, Wait> ||
               std::is_same_v<T, Return> || std::is_same_v<T, Finish> ||
               std::is_same_v<T, Repeat>;
      },
      term.data);
}

void ForEachIntraSegmentSuccessor(const Terminator& term, auto&& func) {
  std::visit(
      common::Overloaded{
          [&](const Jump& j) { func(j.target.value); },
          [&](const Branch& b) {
            func(b.then_target.value);
            func(b.else_target.value);
          },
          [&](const Switch& s) {
            for (auto t : s.targets) {
              func(t.value);
            }
          },
          [](const Delay&) {},
          [](const Wait&) {},
          [](const Return&) {},
          [](const Finish&) {},
          [](const Repeat&) {},
      },
      term.data);
}

struct ModuleSlotInfo {
  SignalRef slot;
  TypeId root_type;
};

auto ExtractModuleSlotInfo(const Place& place)
    -> std::optional<ModuleSlotInfo> {
  if (place.root.kind == PlaceRoot::Kind::kModuleSlot) {
    return ModuleSlotInfo{
        .slot =
            SignalRef{
                .scope = SignalRef::Scope::kModuleLocal,
                .id = static_cast<uint32_t>(place.root.id),
            },
        .root_type = place.root.type,
    };
  }
  if (place.root.kind == PlaceRoot::Kind::kDesignGlobal) {
    return ModuleSlotInfo{
        .slot =
            SignalRef{
                .scope = SignalRef::Scope::kDesignGlobal,
                .id = static_cast<uint32_t>(place.root.id),
            },
        .root_type = place.root.type,
    };
  }
  return std::nullopt;
}

void CollectOperandReads(
    const Operand& op, const Arena& arena, uint32_t block_index,
    uint32_t stmt_index, std::vector<SlotAccessRecord>& accesses) {
  if (op.kind != Operand::Kind::kUse) return;
  auto place_id = std::get<PlaceId>(op.payload);
  const auto& place = arena[place_id];
  auto info = ExtractModuleSlotInfo(place);
  if (!info) return;
  auto shape = place.projections.empty() ? AccessShape::kWholeSlotRead
                                         : AccessShape::kProjectedRead;
  accesses.push_back(
      SlotAccessRecord{
          .slot = info->slot,
          .shape = shape,
          .root_type = info->root_type,
          .block_index = block_index,
          .statement_index = stmt_index,
      });
}

void CollectRhsReads(
    const RightHandSide& rhs, const Arena& arena, uint32_t block_index,
    uint32_t stmt_index, std::vector<SlotAccessRecord>& accesses) {
  if (const auto* op = std::get_if<Operand>(&rhs)) {
    CollectOperandReads(*op, arena, block_index, stmt_index, accesses);
  } else if (const auto* rv = std::get_if<Rvalue>(&rhs)) {
    for (const auto& operand : rv->operands) {
      CollectOperandReads(operand, arena, block_index, stmt_index, accesses);
    }
  }
}

void CollectWriteAccess(
    PlaceId dest, const Arena& arena, uint32_t block_index, uint32_t stmt_index,
    std::vector<SlotAccessRecord>& accesses) {
  const auto& place = arena[dest];
  auto info = ExtractModuleSlotInfo(place);
  if (!info) return;
  auto shape = place.projections.empty() ? AccessShape::kWholeSlotWrite
                                         : AccessShape::kProjectedWrite;
  accesses.push_back(
      SlotAccessRecord{
          .slot = info->slot,
          .shape = shape,
          .root_type = info->root_type,
          .block_index = block_index,
          .statement_index = stmt_index,
      });
}

void CollectFormatOpReads(
    const std::vector<FormatOp>& ops, const Arena& arena, uint32_t block_index,
    uint32_t stmt_index, std::vector<SlotAccessRecord>& accesses) {
  for (const auto& fop : ops) {
    if (fop.value) {
      CollectOperandReads(*fop.value, arena, block_index, stmt_index, accesses);
    }
  }
}

void CollectStatementAccesses(
    const Statement& stmt, const Arena& arena, uint32_t block_index,
    uint32_t stmt_index, std::vector<SlotAccessRecord>& accesses) {
  std::visit(
      common::Overloaded{
          [&](const Assign& a) {
            CollectWriteAccess(
                a.dest, arena, block_index, stmt_index, accesses);
            CollectRhsReads(a.rhs, arena, block_index, stmt_index, accesses);
          },
          [&](const GuardedAssign& ga) {
            CollectWriteAccess(
                ga.dest, arena, block_index, stmt_index, accesses);
            CollectRhsReads(ga.rhs, arena, block_index, stmt_index, accesses);
            CollectOperandReads(
                ga.guard, arena, block_index, stmt_index, accesses);
          },
          [&](const DeferredAssign& da) {
            const auto& place = arena[da.dest];
            auto info = ExtractModuleSlotInfo(place);
            if (info) {
              accesses.push_back(
                  SlotAccessRecord{
                      .slot = info->slot,
                      .shape = AccessShape::kDeferredWrite,
                      .root_type = info->root_type,
                      .block_index = block_index,
                      .statement_index = stmt_index,
                  });
            }
            CollectRhsReads(da.rhs, arena, block_index, stmt_index, accesses);
          },
          [&](const Effect& e) {
            std::visit(
                common::Overloaded{
                    [&](const DisplayEffect& d) {
                      CollectFormatOpReads(
                          d.ops, arena, block_index, stmt_index, accesses);
                      if (d.descriptor) {
                        CollectOperandReads(
                            *d.descriptor, arena, block_index, stmt_index,
                            accesses);
                      }
                    },
                    [&](const ReportEffect& s) {
                      CollectFormatOpReads(
                          s.ops, arena, block_index, stmt_index, accesses);
                    },
                    [&](const SystemTfEffect& stf) {
                      for (const auto& arg : stf.args) {
                        CollectOperandReads(
                            arg, arena, block_index, stmt_index, accesses);
                      }
                    },
                    [&](const MemIOEffect& mio) {
                      if (mio.filename.operand.kind == Operand::Kind::kUse) {
                        CollectOperandReads(
                            mio.filename.operand, arena, block_index,
                            stmt_index, accesses);
                      }
                      if (mio.start_addr) {
                        CollectOperandReads(
                            *mio.start_addr, arena, block_index, stmt_index,
                            accesses);
                      }
                      if (mio.end_addr) {
                        CollectOperandReads(
                            *mio.end_addr, arena, block_index, stmt_index,
                            accesses);
                      }
                      if (mio.is_read) {
                        const auto& place = arena[mio.target];
                        auto info = ExtractModuleSlotInfo(place);
                        if (info) {
                          accesses.push_back(
                              SlotAccessRecord{
                                  .slot = info->slot,
                                  .shape = AccessShape::kExternalWriteTarget,
                                  .root_type = info->root_type,
                                  .block_index = block_index,
                                  .statement_index = stmt_index,
                              });
                        }
                      }
                    },
                    [&](const FillPackedEffect& fp) {
                      CollectWriteAccess(
                          fp.target, arena, block_index, stmt_index, accesses);
                      CollectOperandReads(
                          fp.fill_value, arena, block_index, stmt_index,
                          accesses);
                    },
                    [](const StrobeEffect&) {},
                    [](const MonitorEffect&) {},
                    [](const MonitorControlEffect&) {},
                    [](const TimeFormatEffect&) {},
                    [](const RecordDecisionObservation&) {},
                    [](const RecordDecisionObservationDynamic&) {},
                    [](const CoverHitEffect&) {},
                },
                e.op);
          },
          [&](const Call& c) {
            for (const auto& arg : c.in_args) {
              CollectOperandReads(
                  arg, arena, block_index, stmt_index, accesses);
            }
            for (const auto& wb : c.writebacks) {
              const auto& place = arena[wb.dest];
              auto info = ExtractModuleSlotInfo(place);
              if (info) {
                accesses.push_back(
                    SlotAccessRecord{
                        .slot = info->slot,
                        .shape = AccessShape::kExternalWriteTarget,
                        .root_type = info->root_type,
                        .block_index = block_index,
                        .statement_index = stmt_index,
                    });
              }
            }
            // Call return dest: the call writes the return value to this
            // destination through CommitValue inside LowerCall. This is
            // an external write through the canonical call path.
            if (c.ret && c.ret->dest) {
              const auto& place = arena[*c.ret->dest];
              auto info = ExtractModuleSlotInfo(place);
              if (info) {
                accesses.push_back(
                    SlotAccessRecord{
                        .slot = info->slot,
                        .shape = AccessShape::kExternalWriteTarget,
                        .root_type = info->root_type,
                        .block_index = block_index,
                        .statement_index = stmt_index,
                    });
              }
            }
          },
          [&](const DpiCall& dc) {
            for (const auto& binding : dc.args) {
              if (binding.input_value) {
                CollectOperandReads(
                    *binding.input_value, arena, block_index, stmt_index,
                    accesses);
              }
              if (binding.writeback_dest) {
                const auto& place = arena[*binding.writeback_dest];
                auto info = ExtractModuleSlotInfo(place);
                if (info) {
                  accesses.push_back(
                      SlotAccessRecord{
                          .slot = info->slot,
                          .shape = AccessShape::kExternalWriteTarget,
                          .root_type = info->root_type,
                          .block_index = block_index,
                          .statement_index = stmt_index,
                      });
                }
              }
            }
            if (dc.ret && dc.ret->dest) {
              const auto& place = arena[*dc.ret->dest];
              auto info = ExtractModuleSlotInfo(place);
              if (info) {
                accesses.push_back(
                    SlotAccessRecord{
                        .slot = info->slot,
                        .shape = AccessShape::kExternalWriteTarget,
                        .root_type = info->root_type,
                        .block_index = block_index,
                        .statement_index = stmt_index,
                    });
              }
            }
          },
          [&](const BuiltinCall& bc) {
            for (const auto& arg : bc.args) {
              CollectOperandReads(
                  arg, arena, block_index, stmt_index, accesses);
            }
          },
          [&](const DefineTemp& dt) {
            CollectRhsReads(dt.rhs, arena, block_index, stmt_index, accesses);
          },
          [&](const AssocOp&) {},
      },
      stmt.data);
}

// Semantic analysis of a statement's interaction with canonical state.
//
// This function answers factual questions about what the statement does
// to canonical storage. It does NOT decide sync/reload policy -- that
// is the contract planner's job.
//
// Classification rules by family:
//
//   Assign/GuardedAssign to module slot:
//     Managed writes go to shadow (no canonical interaction). Non-managed
//     direct writes go to canonical but do not require prior publication
//     of managed state. Dirty-marks (LyraMarkDirty) only mark the
//     runtime update_set; they do not trigger synchronous subscriber
//     callbacks or canonical reads.
//
//   DisplayEffect/ReportEffect:
//     Values come through the resolver/value lowering path. The runtime
//     print functions read the provided data pointer, not canonical
//     slot storage independently.
//
//   $fclose/$fflush:
//     File I/O operations with no canonical slot interaction.
//
//   $writemem:
//     Reads canonical array storage, but in v1 managed slots are
//     whole-slot scalars that cannot overlap the target array.
//     If managed slots later include arrays, this must be revisited.
//
//   $readmem*:
//     Runtime writes directly to canonical target storage.
//
//   FillPackedEffect:
//     Writes directly to canonical target storage.
//
//   Call/BuiltinCall/AssocOp:
//     Callee/runtime can read and write any canonical slot via
//     design_ptr or direct container access.
//
//   Unknown SystemTfEffect:
//     Conservative fallback for unrecognized opcodes.
void AnalyzeStatementSemantics(
    const Statement& stmt, const Arena& arena, uint32_t block_index,
    uint32_t stmt_index, std::vector<StatementSemanticFact>& facts) {
  std::visit(
      common::Overloaded{
          // Assign/GuardedAssign to module slot: no canonical-state
          // interaction relevant to activation-local coherence.
          //
          // Managed-slot writes go to the shadow alloca. Non-managed-
          // slot writes commit directly to canonical and emit dirty-
          // marks, but dirty-marks do not trigger synchronous
          // subscriber callbacks or canonical reads in the current
          // runtime. State updates become visible to other processes
          // only after the process body returns and the fixpoint runs,
          // at which point segment-exit sync has already published all
          // managed values to canonical.
          [](const Assign&) {},
          [](const GuardedAssign&) {},
          [](const DeferredAssign&) {},
          [](const DefineTemp&) {},
          [&](const Effect& e) {
            std::visit(
                common::Overloaded{
                    // Display/severity: resolver-only consumer.
                    [](const DisplayEffect&) {},
                    [](const ReportEffect&) {},
                    [&](const SystemTfEffect& stf) {
                      switch (stf.opcode) {
                        // $fclose/$fflush: file I/O, no canonical
                        // slot interaction.
                        case SystemTfOpcode::kFclose:
                        case SystemTfOpcode::kFflush:
                          break;
                        default:
                          // Unrecognized system TF: conservative.
                          facts.push_back(
                              StatementSemanticFact{
                                  .block_index = block_index,
                                  .statement_index = stmt_index,
                                  .reads_canonical_state = false,
                                  .writes_canonical_state = false,
                                  .write_footprint =
                                      CanonicalWriteFootprint::kNone,
                                  .write_targets = {},
                                  .conservative_unknown = true,
                              });
                          break;
                      }
                    },
                    [&](const MemIOEffect& mio) {
                      if (mio.is_read) {
                        // $readmem*: runtime writes to canonical target.
                        // Reload target is slot-granular (SignalRef).
                        // In v1 this is correct because $readmem targets
                        // unpacked arrays which are ineligible for managed
                        // status. If eligibility broadens, verify reload
                        // granularity still matches.
                        const auto& place = arena[mio.target];
                        auto info = ExtractModuleSlotInfo(place);
                        if (info) {
                          facts.push_back(
                              StatementSemanticFact{
                                  .block_index = block_index,
                                  .statement_index = stmt_index,
                                  .reads_canonical_state = false,
                                  .writes_canonical_state = true,
                                  .write_footprint =
                                      CanonicalWriteFootprint::kSpecific,
                                  .write_targets = {info->slot},
                                  .conservative_unknown = false,
                              });
                        } else {
                          // Target is not a module slot (e.g., design-
                          // global or unresolvable). Conservative.
                          facts.push_back(
                              StatementSemanticFact{
                                  .block_index = block_index,
                                  .statement_index = stmt_index,
                                  .reads_canonical_state = false,
                                  .writes_canonical_state = false,
                                  .write_footprint =
                                      CanonicalWriteFootprint::kNone,
                                  .write_targets = {},
                                  .conservative_unknown = true,
                              });
                        }
                      }
                      // $writemem: reads canonical array, but in v1
                      // managed slots are whole-slot scalars. No overlap
                      // with the array target is possible. No fact needed.
                    },
                    [&](const FillPackedEffect& fp) {
                      // Writes directly to canonical target storage.
                      // Reload target is slot-granular. In v1 packed
                      // targets are ineligible for managed status. If
                      // eligibility broadens, verify granularity.
                      const auto& place = arena[fp.target];
                      auto info = ExtractModuleSlotInfo(place);
                      if (info) {
                        facts.push_back(
                            StatementSemanticFact{
                                .block_index = block_index,
                                .statement_index = stmt_index,
                                .reads_canonical_state = false,
                                .writes_canonical_state = true,
                                .write_footprint =
                                    CanonicalWriteFootprint::kSpecific,
                                .write_targets = {info->slot},
                                .conservative_unknown = false,
                            });
                      } else {
                        facts.push_back(
                            StatementSemanticFact{
                                .block_index = block_index,
                                .statement_index = stmt_index,
                                .reads_canonical_state = false,
                                .writes_canonical_state = false,
                                .write_footprint =
                                    CanonicalWriteFootprint::kNone,
                                .write_targets = {},
                                .conservative_unknown = true,
                            });
                      }
                    },
                    [](const StrobeEffect&) {},
                    [](const MonitorEffect&) {},
                    [](const MonitorControlEffect&) {},
                    [](const TimeFormatEffect&) {},
                    [](const RecordDecisionObservation&) {},
                    [](const RecordDecisionObservationDynamic&) {},
                    [](const CoverHitEffect&) {},
                },
                e.op);
          },
          // Call: callee reads/writes canonical via design_ptr.
          [&](const Call&) {
            facts.push_back(
                StatementSemanticFact{
                    .block_index = block_index,
                    .statement_index = stmt_index,
                    .reads_canonical_state = true,
                    .writes_canonical_state = true,
                    .write_footprint = CanonicalWriteFootprint::kAll,
                    .write_targets = {},
                    .conservative_unknown = false,
                });
          },
          // DpiCall: foreign call may read/write canonical state.
          [&](const DpiCall&) {
            facts.push_back(
                StatementSemanticFact{
                    .block_index = block_index,
                    .statement_index = stmt_index,
                    .reads_canonical_state = true,
                    .writes_canonical_state = true,
                    .write_footprint = CanonicalWriteFootprint::kAll,
                    .write_targets = {},
                    .conservative_unknown = false,
                });
          },
          // BuiltinCall: runtime modifies canonical container storage.
          [&](const BuiltinCall&) {
            facts.push_back(
                StatementSemanticFact{
                    .block_index = block_index,
                    .statement_index = stmt_index,
                    .reads_canonical_state = true,
                    .writes_canonical_state = true,
                    .write_footprint = CanonicalWriteFootprint::kAll,
                    .write_targets = {},
                    .conservative_unknown = false,
                });
          },
          // AssocOp: runtime modifies canonical associative storage.
          [&](const AssocOp&) {
            facts.push_back(
                StatementSemanticFact{
                    .block_index = block_index,
                    .statement_index = stmt_index,
                    .reads_canonical_state = true,
                    .writes_canonical_state = true,
                    .write_footprint = CanonicalWriteFootprint::kAll,
                    .write_targets = {},
                    .conservative_unknown = false,
                });
          },
      },
      stmt.data);
}

void CollectObserverRegistrations(
    const Statement& stmt, uint32_t block_index, uint32_t stmt_index,
    std::vector<ObserverRegistration>& registrations) {
  const auto* effect = std::get_if<Effect>(&stmt.data);
  if (effect == nullptr) return;
  bool is_observer = std::visit(
      [](const auto& e) -> bool {
        using T = std::decay_t<decltype(e)>;
        return std::is_same_v<T, StrobeEffect> ||
               std::is_same_v<T, MonitorEffect>;
      },
      effect->op);
  if (is_observer) {
    registrations.push_back(
        ObserverRegistration{
            .block_index = block_index,
            .statement_index = stmt_index,
        });
  }
}

auto CollectResumeEntryBlocks(const Process& process) -> std::vector<uint32_t> {
  std::vector<uint32_t> entries;
  entries.push_back(process.entry.value);
  for (const auto& block : process.blocks) {
    std::visit(
        common::Overloaded{
            [&](const Delay& d) { entries.push_back(d.resume.value); },
            [&](const Wait& w) { entries.push_back(w.resume.value); },
            [](const auto&) {},
        },
        block.terminator.data);
  }
  std::ranges::sort(entries);
  auto [first, last] = std::ranges::unique(entries);
  entries.erase(first, last);
  return entries;
}

auto ComputeSegmentBlocks(uint32_t entry, const std::vector<BasicBlock>& blocks)
    -> std::vector<uint32_t> {
  std::vector<bool> visited(blocks.size(), false);
  std::queue<uint32_t> worklist;
  worklist.push(entry);
  visited[entry] = true;

  std::vector<uint32_t> result;
  while (!worklist.empty()) {
    uint32_t bi = worklist.front();
    worklist.pop();
    result.push_back(bi);

    if (IsSegmentExitTerminator(blocks[bi].terminator)) continue;

    ForEachIntraSegmentSuccessor(blocks[bi].terminator, [&](uint32_t succ) {
      if (succ < blocks.size() && !visited[succ]) {
        visited[succ] = true;
        worklist.push(succ);
      }
    });
  }

  std::ranges::sort(result);
  return result;
}

auto IsEligibleScalarType(const Type& type, const TypeArena& types) -> bool {
  switch (type.Kind()) {
    case TypeKind::kIntegral:
    case TypeKind::kReal:
    case TypeKind::kShortReal:
      return true;
    case TypeKind::kEnum: {
      auto base_type_id = type.AsEnum().base_type;
      const auto& base_type = types[base_type_id];
      return base_type.Kind() == TypeKind::kIntegral;
    }
    default:
      return false;
  }
}

}  // namespace

auto AnalyzeActivationSegments(const Process& process, const Arena& arena)
    -> std::vector<ActivationSegment> {
  auto entry_blocks = CollectResumeEntryBlocks(process);
  std::vector<ActivationSegment> segments;

  for (uint32_t entry : entry_blocks) {
    auto segment_blocks = ComputeSegmentBlocks(entry, process.blocks);

    std::vector<SlotAccessRecord> accesses;
    std::vector<StatementSemanticFact> facts;
    std::vector<ObserverRegistration> observer_regs;

    for (uint32_t bi : segment_blocks) {
      const auto& block = process.blocks[bi];
      for (uint32_t si = 0; si < block.statements.size(); ++si) {
        CollectStatementAccesses(block.statements[si], arena, bi, si, accesses);
        AnalyzeStatementSemantics(block.statements[si], arena, bi, si, facts);
        CollectObserverRegistrations(
            block.statements[si], bi, si, observer_regs);
      }
    }

    segments.push_back(
        ActivationSegment{
            .entry_block = entry,
            .blocks = std::move(segment_blocks),
            .semantic_facts = std::move(facts),
            .slot_accesses = std::move(accesses),
            .observer_registrations = std::move(observer_regs),
        });
  }

  return segments;
}

auto EvaluateSegmentEligibility(const ActivationSegment& segment)
    -> SegmentEligibility {
  // v1 allowed segment grammar: no persistent observer registrations.
  // Segments with strobe/monitor effects install observers that read
  // slot state at future time points outside the current activation,
  // which breaks the single-writer exclusivity assumption.
  if (!segment.observer_registrations.empty()) {
    return SegmentEligibility{
        .eligible = false,
        .reason = SegmentBlocker::kPersistentObserverRegistration,
    };
  }
  return SegmentEligibility{.eligible = true, .reason = std::nullopt};
}

namespace {

// v1 allowed access grammar for activation-local eligibility.
// A slot is eligible ONLY IF every access matches this whitelist exactly.
// Anything not explicitly recognized stays on the canonical path.
auto IsAllowedAccessShape(AccessShape shape) -> bool {
  switch (shape) {
    case AccessShape::kWholeSlotRead:
    case AccessShape::kWholeSlotWrite:
      return true;
    case AccessShape::kProjectedRead:
    case AccessShape::kProjectedWrite:
    case AccessShape::kDeferredWrite:
    case AccessShape::kAddressEscape:
    case AccessShape::kExternalWriteTarget:
      return false;
  }
  return false;
}

// Map a non-allowed access shape to its ineligibility reason.
// Only called for shapes where IsAllowedAccessShape returned false.
auto ReasonForDisallowedShape(AccessShape shape) -> SlotIneligibilityReason {
  switch (shape) {
    case AccessShape::kProjectedRead:
    case AccessShape::kProjectedWrite:
      return SlotIneligibilityReason::kProjectedAccess;
    case AccessShape::kDeferredWrite:
      return SlotIneligibilityReason::kDeferredWrite;
    case AccessShape::kAddressEscape:
      return SlotIneligibilityReason::kAddressEscape;
    case AccessShape::kExternalWriteTarget:
      return SlotIneligibilityReason::kExternalWriteTarget;
    case AccessShape::kWholeSlotRead:
    case AccessShape::kWholeSlotWrite:
      // These are allowed shapes -- this function should never be called
      // for them. If it is, that is a bug in the caller.
      break;
  }
  throw common::InternalError(
      "ReasonForDisallowedShape",
      "called for an allowed access shape (should be unreachable)");
}

}  // namespace

auto EvaluateSlotEligibility(
    const ActivationSegment& segment, const TypeArena& types)
    -> std::vector<SlotEligibility> {
  // Whitelist model: a slot is eligible only if it fully matches the
  // allowed v1 grammar. Every access is checked against the whitelist.
  // Anything not explicitly allowed -> ineligible, stays canonical.

  struct SlotProof {
    SignalRef slot;
    TypeId root_type;
    bool has_whole_slot_write = false;
    // First disallowed access shape, if any.
    std::optional<SlotIneligibilityReason> rejection;
  };

  std::vector<SlotProof> proofs;
  auto find_or_add = [&](SignalRef slot, TypeId root_type) -> SlotProof& {
    for (auto& p : proofs) {
      if (p.slot == slot) return p;
    }
    proofs.push_back(
        SlotProof{.slot = slot, .root_type = root_type, .rejection = {}});
    return proofs.back();
  };

  for (const auto& access : segment.slot_accesses) {
    auto& proof = find_or_add(access.slot, access.root_type);
    if (proof.rejection) continue;

    if (!IsAllowedAccessShape(access.shape)) {
      proof.rejection = ReasonForDisallowedShape(access.shape);
      continue;
    }

    if (access.shape == AccessShape::kWholeSlotWrite) {
      proof.has_whole_slot_write = true;
    }
  }

  std::vector<SlotEligibility> results;
  for (const auto& proof : proofs) {
    auto reject = [&](SlotIneligibilityReason reason) {
      results.push_back(
          SlotEligibility{
              .slot = proof.slot,
              .root_type = proof.root_type,
              .eligible = false,
              .reason = reason,
          });
    };

    // v1 allowed slot grammar: prove each requirement.

    // 1. Must be module-local (not design-global).
    if (proof.slot.scope != SignalRef::Scope::kModuleLocal) {
      reject(SlotIneligibilityReason::kDesignGlobal);
      continue;
    }

    // 2. Must be a plain scalar type (integral, real, shortreal,
    //    or enum with integral base).
    const auto& type = types[proof.root_type];
    if (!IsEligibleScalarType(type, types)) {
      auto reason = (type.Kind() == TypeKind::kEnum)
                        ? SlotIneligibilityReason::kEnumNonIntegralBase
                        : SlotIneligibilityReason::kUnsupportedType;
      reject(reason);
      continue;
    }

    // 3. All accesses must be in the allowed grammar.
    if (proof.rejection) {
      reject(*proof.rejection);
      continue;
    }

    // 4. Must have at least one whole-slot write (read-only slots
    //    gain nothing from activation-local treatment).
    if (!proof.has_whole_slot_write) {
      reject(SlotIneligibilityReason::kReadOnly);
      continue;
    }

    // All proofs passed. Slot is eligible.
    results.push_back(
        SlotEligibility{
            .slot = proof.slot,
            .root_type = proof.root_type,
            .eligible = true,
            .reason = std::nullopt,
        });
  }

  return results;
}

}  // namespace lyra::mir::passes

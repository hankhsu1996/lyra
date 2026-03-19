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
          [&](const QualifiedDispatch& qd) {
            for (auto t : qd.targets) {
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
                    [&](const SeverityEffect& s) {
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

void ClassifyStatementBoundary(
    const Statement& stmt, const Arena& arena, uint32_t block_index,
    uint32_t stmt_index, std::vector<BoundaryRecord>& boundaries) {
  std::visit(
      common::Overloaded{
          [&](const Assign& a) {
            // Writes to non-managed module slots emit dirty-marks that
            // can trigger runtime subscriptions reading other (managed)
            // slots. Treat these as observation boundaries so managed
            // slots are synced before the dirty-mark fires.
            const auto& place = arena[a.dest];
            if (place.root.kind == PlaceRoot::Kind::kModuleSlot ||
                place.root.kind == PlaceRoot::Kind::kDesignGlobal) {
              boundaries.push_back(
                  BoundaryRecord{
                      .block_index = block_index,
                      .statement_index = stmt_index,
                      .kind = BoundaryKind::kObservation,
                      .affected_slots = {},
                  });
            }
          },
          [&](const GuardedAssign& ga) {
            const auto& place = arena[ga.dest];
            if (place.root.kind == PlaceRoot::Kind::kModuleSlot ||
                place.root.kind == PlaceRoot::Kind::kDesignGlobal) {
              boundaries.push_back(
                  BoundaryRecord{
                      .block_index = block_index,
                      .statement_index = stmt_index,
                      .kind = BoundaryKind::kObservation,
                      .affected_slots = {},
                  });
            }
          },
          [](const DeferredAssign&) {},
          [](const DefineTemp&) {},
          [&](const Effect& e) {
            std::visit(
                common::Overloaded{
                    [&](const DisplayEffect&) {
                      boundaries.push_back(
                          BoundaryRecord{
                              .block_index = block_index,
                              .statement_index = stmt_index,
                              .kind = BoundaryKind::kObservation,
                              .affected_slots = {},
                          });
                    },
                    [&](const SeverityEffect&) {
                      boundaries.push_back(
                          BoundaryRecord{
                              .block_index = block_index,
                              .statement_index = stmt_index,
                              .kind = BoundaryKind::kObservation,
                              .affected_slots = {},
                          });
                    },
                    [&](const SystemTfEffect& stf) {
                      // All SystemTfEffect opcodes in current MIR are
                      // observation-only (kFclose, kFflush). Writeback-
                      // capable system TFs ($fscanf, $fgets, $fread) are
                      // lowered through the Call path, not SystemTfEffect.
                      switch (stf.opcode) {
                        case SystemTfOpcode::kFclose:
                        case SystemTfOpcode::kFflush:
                          boundaries.push_back(
                              BoundaryRecord{
                                  .block_index = block_index,
                                  .statement_index = stmt_index,
                                  .kind = BoundaryKind::kObservation,
                                  .affected_slots = {},
                              });
                          break;
                        default:
                          // Conservative: unknown system TF effect treated
                          // as may-write-any.
                          boundaries.push_back(
                              BoundaryRecord{
                                  .block_index = block_index,
                                  .statement_index = stmt_index,
                                  .kind = BoundaryKind::kMayWriteAny,
                                  .affected_slots = {},
                              });
                          break;
                      }
                    },
                    [&](const MemIOEffect& mio) {
                      if (mio.is_read) {
                        const auto& place = arena[mio.target];
                        auto info = ExtractModuleSlotInfo(place);
                        std::vector<SignalRef> affected;
                        if (info) affected.push_back(info->slot);
                        boundaries.push_back(
                            BoundaryRecord{
                                .block_index = block_index,
                                .statement_index = stmt_index,
                                .kind = BoundaryKind::kWritebackSpecific,
                                .affected_slots = std::move(affected),
                            });
                      } else {
                        boundaries.push_back(
                            BoundaryRecord{
                                .block_index = block_index,
                                .statement_index = stmt_index,
                                .kind = BoundaryKind::kObservation,
                                .affected_slots = {},
                            });
                      }
                    },
                    [&](const FillPackedEffect& fp) {
                      const auto& place = arena[fp.target];
                      auto info = ExtractModuleSlotInfo(place);
                      std::vector<SignalRef> affected;
                      if (info) affected.push_back(info->slot);
                      boundaries.push_back(
                          BoundaryRecord{
                              .block_index = block_index,
                              .statement_index = stmt_index,
                              .kind = BoundaryKind::kWritebackSpecific,
                              .affected_slots = std::move(affected),
                          });
                    },
                    [](const StrobeEffect&) {},
                    [](const MonitorEffect&) {},
                    [](const MonitorControlEffect&) {},
                    [](const TimeFormatEffect&) {},
                },
                e.op);
          },
          [&](const Call&) {
            boundaries.push_back(
                BoundaryRecord{
                    .block_index = block_index,
                    .statement_index = stmt_index,
                    .kind = BoundaryKind::kMayWriteAny,
                    .affected_slots = {},
                });
          },
          [&](const BuiltinCall&) {
            boundaries.push_back(
                BoundaryRecord{
                    .block_index = block_index,
                    .statement_index = stmt_index,
                    .kind = BoundaryKind::kMayWriteAny,
                    .affected_slots = {},
                });
          },
          [&](const AssocOp&) {
            boundaries.push_back(
                BoundaryRecord{
                    .block_index = block_index,
                    .statement_index = stmt_index,
                    .kind = BoundaryKind::kMayWriteAny,
                    .affected_slots = {},
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
    std::vector<BoundaryRecord> boundaries;
    std::vector<ObserverRegistration> observer_regs;

    for (uint32_t bi : segment_blocks) {
      const auto& block = process.blocks[bi];
      for (uint32_t si = 0; si < block.statements.size(); ++si) {
        CollectStatementAccesses(block.statements[si], arena, bi, si, accesses);
        ClassifyStatementBoundary(
            block.statements[si], arena, bi, si, boundaries);
        CollectObserverRegistrations(
            block.statements[si], bi, si, observer_regs);
      }
    }

    segments.push_back(
        ActivationSegment{
            .entry_block = entry,
            .blocks = std::move(segment_blocks),
            .boundaries = std::move(boundaries),
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
    proofs.push_back(SlotProof{.slot = slot, .root_type = root_type});
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

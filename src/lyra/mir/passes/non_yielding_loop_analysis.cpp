#include "lyra/mir/passes/non_yielding_loop_analysis.hpp"

#include <algorithm>
#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/passes/canonical_loop_analysis.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir::passes {

namespace {

// Check that a terminator stays entirely within local CFG execution.
// Only pure control-flow terminators qualify. Anything that can suspend,
// finish, or otherwise hand control to the scheduler prevents qualification.
auto IsAllowedTerminator(const Terminator& term) -> bool {
  return std::visit(
      [](const auto& t) -> bool {
        using T = std::decay_t<decltype(t)>;
        return std::is_same_v<T, Jump> || std::is_same_v<T, Branch> ||
               std::is_same_v<T, Switch>;
      },
      term.data);
}

// Check that an effect is semantically local to the current activation:
// it reads DesignState but does not create scheduler-visible observation
// or register programs with the runtime.
auto IsAllowedEffect(const EffectOp& op) -> bool {
  return std::visit(
      [](const auto& e) -> bool {
        using T = std::decay_t<decltype(e)>;
        return std::is_same_v<T, DisplayEffect> ||
               std::is_same_v<T, ReportEffect> ||
               std::is_same_v<T, MonitorControlEffect> ||
               std::is_same_v<T, TimeFormatEffect>;
      },
      op);
}

// Check that a statement belongs to the stage-1 positive semantic subset.
auto IsAllowedStatement(const Statement& stmt) -> bool {
  return std::visit(
      [](const auto& s) -> bool {
        using T = std::decay_t<decltype(s)>;
        if constexpr (
            std::is_same_v<T, DefineTemp> || kIsDirectAssign<T> ||
            std::is_same_v<T, GuardedAssign>) {
          return true;
        } else if constexpr (std::is_same_v<T, Effect>) {
          return IsAllowedEffect(s.op);
        } else {
          return false;
        }
      },
      stmt.data);
}

// Check that a design-slot root type uses a packed/scalar commit path
// whose notification timing can be split (immediate store, deferred mark).
// Container, string, and managed types bundle commit+notify and are
// excluded from stage 1.
auto IsSupportedDeferredRootType(TypeKind kind) -> bool {
  switch (kind) {
    case TypeKind::kIntegral:
    case TypeKind::kEnum:
    case TypeKind::kReal:
    case TypeKind::kShortReal:
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kUnpackedArray:
    case TypeKind::kUnpackedStruct:
    case TypeKind::kUnpackedUnion:
      return true;
    default:
      return false;
  }
}

// Extract the canonical notified root for a design-slot place.
// Returns nullopt for locals, temps, or non-design roots.
auto ExtractNotifiedRoot(const Place& place) -> std::optional<SignalRef> {
  if (place.root.kind == PlaceRoot::Kind::kModuleSlot) {
    return SignalRef{
        .scope = SignalRef::Scope::kModuleLocal,
        .id = static_cast<uint32_t>(place.root.id),
    };
  }
  if (place.root.kind == PlaceRoot::Kind::kDesignGlobal) {
    return SignalRef{
        .scope = SignalRef::Scope::kDesignGlobal,
        .id = static_cast<uint32_t>(place.root.id),
    };
  }
  if (place.root.kind == PlaceRoot::Kind::kObjectLocal) {
    return SignalRef{
        .scope = SignalRef::Scope::kObjectLocal,
        .id = static_cast<uint32_t>(place.root.id),
        .object_index = place.root.object_index,
    };
  }
  if (place.root.kind == PlaceRoot::Kind::kBoundChildDest) {
    return std::nullopt;
  }
  return std::nullopt;
}

// Add a notified root to the set if not already present.
void InsertRoot(std::vector<SignalRef>& roots, SignalRef ref) {
  for (const auto& existing : roots) {
    if (existing == ref) return;
  }
  roots.push_back(ref);
}

// Collect canonical notified roots from design-slot writes in a block.
// Returns false if any write uses an unsupported deferred-store path.
auto CollectWrittenRoots(
    const BasicBlock& block, const Arena& arena, const TypeArena& types,
    std::vector<SignalRef>& roots) -> bool {
  for (const auto& stmt : block.statements) {
    std::optional<PlaceId> dest;
    if (auto ref = TryGetDirectAssign(stmt.data); ref) {
      if (const auto* pid = std::get_if<PlaceId>(ref.dest)) {
        dest = *pid;
      }
    } else if (const auto* ga = std::get_if<GuardedAssign>(&stmt.data)) {
      dest = RequireLocalDest(ga->dest, "NonYieldingLoopAnalysis");
    }
    if (!dest) continue;

    const auto& place = arena[*dest];
    auto root = ExtractNotifiedRoot(place);
    if (!root) continue;

    const Type& root_type = types[place.root.type];
    if (!IsSupportedDeferredRootType(root_type.Kind())) {
      return false;
    }

    // Whole-aggregate writes to unpacked containers go through the
    // aggregate notify path (CommitNotifyAggregateIfDesignSlot), which
    // is not a supported deferred-store path. Only element-level writes
    // (with at least one projection) use the packed inline store path.
    bool is_unpacked_aggregate =
        root_type.Kind() == TypeKind::kUnpackedArray ||
        root_type.Kind() == TypeKind::kUnpackedStruct ||
        root_type.Kind() == TypeKind::kUnpackedUnion;
    if (is_unpacked_aggregate && place.projections.empty()) {
      return false;
    }

    InsertRoot(roots, *root);
  }
  return true;
}

// Check that all blocks in a loop qualify for deferred notification.
// Populates notified_roots with canonical roots of all design-slot writes.
// Returns false if any block violates the stage-1 semantic subset.
auto QualifyLoop(
    const CanonicalInductionLoop& loop, const std::vector<BasicBlock>& blocks,
    const Arena& arena, const TypeArena& types,
    std::vector<SignalRef>& notified_roots) -> bool {
  // Check header
  if (!IsAllowedTerminator(blocks[loop.header].terminator)) return false;
  for (const auto& stmt : blocks[loop.header].statements) {
    if (!IsAllowedStatement(stmt)) return false;
  }
  if (!CollectWrittenRoots(blocks[loop.header], arena, types, notified_roots)) {
    return false;
  }

  // Check body blocks
  for (uint32_t bi : loop.body_blocks) {
    if (!IsAllowedTerminator(blocks[bi].terminator)) return false;
    for (const auto& stmt : blocks[bi].statements) {
      if (!IsAllowedStatement(stmt)) return false;
    }
    if (!CollectWrittenRoots(blocks[bi], arena, types, notified_roots)) {
      return false;
    }
  }

  // Check latch
  if (!IsAllowedTerminator(blocks[loop.latch].terminator)) return false;
  for (const auto& stmt : blocks[loop.latch].statements) {
    if (!IsAllowedStatement(stmt)) return false;
  }
  return CollectWrittenRoots(blocks[loop.latch], arena, types, notified_roots);
}

// Determine the exit block for a canonical loop.
// The exit is the header's Branch false-target (the block reached when
// the loop condition is false).
auto GetExitBlock(
    const CanonicalInductionLoop& loop, const std::vector<BasicBlock>& blocks)
    -> uint32_t {
  const auto& branch = std::get<Branch>(blocks[loop.header].terminator.data);
  return branch.else_target.value;
}

}  // namespace

auto FindDeferredNotificationLoops(
    const std::vector<BasicBlock>& blocks, const Arena& arena,
    const TypeArena& types) -> std::vector<DeferredNotificationLoop> {
  auto loops = FindCanonicalInductionLoops(blocks, arena);
  if (loops.empty()) return {};

  // Sort by header ascending so outermost loops (lower header index)
  // are processed first and can subsume inner loops.
  std::ranges::sort(
      loops, [](const auto& a, const auto& b) { return a.header < b.header; });

  std::vector<bool> covered(blocks.size(), false);
  std::vector<DeferredNotificationLoop> result;

  for (const auto& loop : loops) {
    // Skip if any block in this loop is already covered by an outer region.
    bool already_covered = false;
    if (covered[loop.header]) already_covered = true;
    if (!already_covered) {
      for (uint32_t bi : loop.body_blocks) {
        if (covered[bi]) {
          already_covered = true;
          break;
        }
      }
    }
    if (!already_covered && covered[loop.latch]) already_covered = true;
    if (already_covered) continue;

    std::vector<SignalRef> notified_roots;
    if (!QualifyLoop(loop, blocks, arena, types, notified_roots)) continue;

    // Only emit a region if there are actual design-slot writes to defer.
    if (notified_roots.empty()) continue;

    uint32_t exit_block = GetExitBlock(loop, blocks);

    // region_blocks contains only the loop interior (header, body, latch).
    // The exit_block is NOT included: it is the successor that receives
    // the loop-exit edge effect, not part of the deferred-notification
    // region itself. This invariant is critical -- if the exit block were
    // in region_blocks, codegen would both suppress notifications in that
    // block AND emit deferred marks there, which is contradictory.
    std::vector<uint32_t> region_blocks;
    region_blocks.push_back(loop.header);
    for (uint32_t bi : loop.body_blocks) {
      region_blocks.push_back(bi);
    }
    region_blocks.push_back(loop.latch);
    std::ranges::sort(region_blocks);

    // Reject if the exit block is a member of the loop region set.
    // Nested control flow (e.g., break in an inner loop) can produce
    // layouts where the exit block is also a body block of the outer
    // loop. Test against the actual region set, not a numeric range.
    if (std::ranges::binary_search(region_blocks, exit_block)) continue;

    // Mark all region blocks as covered so inner loops are subsumed.
    for (uint32_t bi : region_blocks) {
      covered[bi] = true;
    }

    result.push_back(
        DeferredNotificationLoop{
            .header = loop.header,
            .latch = loop.latch,
            .exit_block = exit_block,
            .region_blocks = std::move(region_blocks),
            .notified_roots = std::move(notified_roots),
        });
  }

  return result;
}

}  // namespace lyra::mir::passes

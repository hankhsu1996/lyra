#include "lyra/lowering/hir_to_mir/process.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/statement.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/sensitivity.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ConvertProcessKind(hir::ProcessKind hir_kind) -> mir::ProcessKind {
  switch (hir_kind) {
    case hir::ProcessKind::kInitial:
      return mir::ProcessKind::kOnce;
    case hir::ProcessKind::kFinal:
      return mir::ProcessKind::kFinal;
    case hir::ProcessKind::kAlways:
    case hir::ProcessKind::kAlwaysComb:
    case hir::ProcessKind::kAlwaysFf:
    case hir::ProcessKind::kAlwaysLatch:
      return mir::ProcessKind::kLooping;
  }
  __builtin_unreachable();
}

// Rewrite the epilogue back-edge of a static event-controlled process so the
// runtime can reuse the wait-plan (refresh path) instead of clear+reinstall
// every cycle.
//
// Canonical input shape (produced by LowerEventWait + EmitProcessEpilogue):
//   bb0 (entry): Wait{triggers=T, resume=bb1}   [no statements]
//   bb1..N:      <body>  ...  Repeat
//
// Rewritten shape:
//   bb0 (entry): Wait{triggers=T, resume=bb1}   [initial arm, runs once]
//   bb1..N:      <body>  ...  Wait{triggers=T, resume=bb1}
//
// Returns true if the rewrite was applied.
auto RewriteStaticEventLoopBackedge(
    std::vector<mir::BasicBlock>& blocks, BlockIndex entry_idx) -> bool {
  auto& entry_block = blocks[entry_idx.value];
  const auto* entry_wait = std::get_if<mir::Wait>(&entry_block.terminator.data);

  // Guard 1: entry must be a pure wait-arm (no statements, Wait terminator).
  if (entry_wait == nullptr || !entry_block.statements.empty()) {
    return false;
  }

  // Guard 2: entry Wait must resume into a body block, not itself.
  if (entry_wait->resume.value == entry_idx.value) {
    return false;
  }

  // Guard 3: exactly one Repeat must exist. mir::Repeat semantics are
  // "restart from process entry" (see terminator.hpp), so every Repeat is
  // structurally an epilogue back-edge. Multiple Repeats indicate
  // non-canonical control flow where the rewrite may not be safe.
  mir::BasicBlock* epilogue_block = nullptr;
  for (auto& block : blocks) {
    if (std::holds_alternative<mir::Repeat>(block.terminator.data)) {
      if (epilogue_block != nullptr) {
        return false;  // Multiple Repeats -- skip.
      }
      epilogue_block = &block;
    }
  }

  if (epilogue_block == nullptr) {
    return false;
  }

  // Clone the entry Wait unchanged. The cloned Wait re-arms the same
  // triggers and resumes into the same body entry block.
  epilogue_block->terminator.data = mir::Wait{*entry_wait};
  return true;
}

}  // namespace

auto LowerProcess(
    hir::ProcessId hir_proc_id, const hir::Process& process,
    const LoweringInput& input, mir::Arena& mir_arena,
    const DeclView& decl_view, OriginMap* origin_map,
    std::vector<mir::FunctionId>* generated_functions,
    hir::ModuleBodyId body_id) -> Result<mir::ProcessId> {
  Context ctx{
      .mir_arena = &mir_arena,
      .hir_arena = input.hir_arena,
      .type_arena = input.type_arena,
      .constant_arena = input.constant_arena,
      .symbol_table = input.symbol_table,
      .body_places = decl_view.body_places,
      .design_places = decl_view.design_places,
      .local_places = {},
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .temp_types = {},
      .builtin_types = input.builtin_types,
      .symbol_to_mir_function = decl_view.functions,
      .generated_functions = generated_functions,
      .return_slot = std::nullopt,
      .return_type = input.builtin_types.void_type,
      .design_slots = decl_view.slots,
      .body_slots = decl_view.body_slots,
  };

  MirBuilder builder(&mir_arena, &ctx, origin_map, body_id);

  BlockIndex entry_idx = builder.CreateBlock();
  builder.SetCurrentBlock(entry_idx);

  Result<void> stmt_result = LowerStatement(process.body, builder);
  if (!stmt_result) {
    return std::unexpected(stmt_result.error());
  }

  mir::ProcessKind mir_kind = ConvertProcessKind(process.kind);
  builder.EmitProcessEpilogue(mir_kind);

  std::vector<mir::BasicBlock> blocks = builder.Finish();

  // For always_comb/always_latch: replace the Repeat terminator with
  // Wait(sensitivity_triggers, resume=entry). This makes the process
  // re-execute when any read design variable changes.
  if (process.kind == hir::ProcessKind::kAlwaysComb ||
      process.kind == hir::ProcessKind::kAlwaysLatch) {
    mir::Process temp_process{
        .kind = mir_kind,
        .entry = mir::BasicBlockId{entry_idx.value},
        .blocks = blocks,
    };
    auto triggers = mir::CollectSensitivity(temp_process, mir_arena);

    // Find the block with Repeat terminator and replace
    for (auto& block : blocks) {
      if (std::holds_alternative<mir::Repeat>(block.terminator.data)) {
        block.terminator.data = mir::Wait{
            .triggers = std::move(triggers),
            .resume = mir::BasicBlockId{entry_idx.value},
        };
        break;
      }
    }
  }

  // For always_ff and static event-controlled always: rewrite the epilogue
  // Repeat into a cloned Wait so the runtime stays on the refresh path.
  if (process.kind == hir::ProcessKind::kAlwaysFf ||
      process.kind == hir::ProcessKind::kAlways) {
    RewriteStaticEventLoopBackedge(blocks, entry_idx);
  }

  // Pre-allocate the ProcessId so we can record origin before adding
  mir::ProcessId mir_proc_id = mir_arena.ReserveProcess();

  // Record process origin (must be done before creating Process struct due to
  // designated initializer ordering requirements)
  common::OriginId origin = common::OriginId::Invalid();
  if (origin_map != nullptr) {
    origin = origin_map->Record(mir_proc_id, hir_proc_id, body_id);
  }

  mir::Process mir_process{
      .kind = mir_kind,
      .entry = mir::BasicBlockId{entry_idx.value},  // Local index
      .blocks = std::move(blocks),
      .origin = origin,
      .temp_metadata = std::move(ctx.temp_metadata),
      .materialize_count = ctx.materialize_count,
  };

  mir_arena.SetProcessBody(mir_proc_id, std::move(mir_process));
  return mir_proc_id;
}

}  // namespace lyra::lowering::hir_to_mir

#include "lyra/lowering/hir_to_mir/process.hpp"

#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/statement.hpp"
#include "lyra/mir/routine.hpp"

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

}  // namespace

auto LowerProcess(
    const hir::Process& process, const LoweringInput& input,
    mir::Arena& mir_arena, const PlaceMap& module_places,
    const SymbolToMirFunctionMap& symbol_to_mir_function) -> mir::ProcessId {
  Context ctx{
      .mir_arena = &mir_arena,
      .hir_arena = input.hir_arena,
      .type_arena = input.type_arena,
      .constant_arena = input.constant_arena,
      .symbol_table = input.symbol_table,
      .module_places = &module_places,
      .local_places = {},
      .next_local_id = 0,
      .next_temp_id = 0,
      .builtin_types = input.builtin_types,
      .symbol_to_mir_function = &symbol_to_mir_function,
      .return_place = mir::kInvalidPlaceId,
  };

  MirBuilder builder(&mir_arena, &ctx);

  BlockIndex entry_idx = builder.CreateBlock();
  builder.SetCurrentBlock(entry_idx);

  LowerStatement(process.body, builder);

  mir::ProcessKind mir_kind = ConvertProcessKind(process.kind);
  if (mir_kind == mir::ProcessKind::kLooping) {
    builder.EmitRepeat();
  } else {
    // kOnce (initial) and kFinal both terminate after running
    builder.EmitTerminate();
  }

  std::vector<mir::BasicBlock> blocks = builder.Finish();

  mir::Process mir_process{
      .kind = mir_kind,
      .entry = mir::BasicBlockId{entry_idx.value},  // Local index
      .blocks = std::move(blocks),
  };

  return mir_arena.AddProcess(std::move(mir_process));
}

}  // namespace lyra::lowering::hir_to_mir

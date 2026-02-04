#include "lyra/lowering/hir_to_mir/lower.hpp"

#include <expected>
#include <memory>
#include <utility>
#include <variant>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/design.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/package.hpp"
#include "lyra/mir/verify.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Verify all MIR produced by lowering. Called once at the end of the pipeline.
// Failures throw InternalError (compiler bug - we emitted invalid MIR).
void VerifyLoweredMir(
    const mir::Design& design, const mir::Arena& arena,
    const TypeArena& type_arena) {
  for (const auto& element : design.elements) {
    std::visit(
        common::Overloaded{
            [&](const mir::Module& mod) {
              for (mir::FunctionId fid : mod.functions) {
                mir::VerifyFunction(arena[fid], arena, type_arena);
              }
              for (mir::ProcessId pid : mod.processes) {
                mir::VerifyProcess(arena[pid], arena, type_arena);
              }
            },
            [&](const mir::Package& pkg) {
              for (mir::FunctionId fid : pkg.functions) {
                mir::VerifyFunction(arena[fid], arena, type_arena);
              }
            },
        },
        element);
  }
}

// Compute MIR statistics for --stats output.
auto ComputeMirStats(const mir::Design& design, const mir::Arena& arena)
    -> LoweringStats {
  LoweringStats stats;

  auto count_routine = [&](const auto& routine) {
    for (const auto& meta : routine.temp_metadata) {
      if (meta.kind == mir::TempKind::kPlace) {
        ++stats.place_temps;
      } else {
        ++stats.value_temps;
      }
    }
    for (const auto& block : routine.blocks) {
      stats.mir_stmts += block.statements.size();
    }
    stats.materialize_to_place += routine.materialize_count;
  };

  for (const auto& element : design.elements) {
    std::visit(
        common::Overloaded{
            [&](const mir::Module& mod) {
              for (auto fid : mod.functions) {
                count_routine(arena[fid]);
              }
              for (auto pid : mod.processes) {
                count_routine(arena[pid]);
              }
            },
            [&](const mir::Package& pkg) {
              for (auto fid : pkg.functions) {
                count_routine(arena[fid]);
              }
            },
        },
        element);
  }

  for (auto fid : design.generated_functions) {
    count_routine(arena[fid]);
  }
  for (auto pid : design.init_processes) {
    count_routine(arena[pid]);
  }

  return stats;
}

}  // namespace

auto LowerHirToMir(const LoweringInput& input) -> Result<LoweringResult> {
  auto mir_arena = std::make_unique<mir::Arena>();
  OriginMap origin_map;

  LoweringInput full_input = input;
  full_input.builtin_types = InternBuiltinTypes(*input.type_arena);

  auto design_result =
      LowerDesign(*input.design, full_input, *mir_arena, &origin_map);
  if (!design_result) return std::unexpected(design_result.error());
  mir::Design design = std::move(*design_result);

  // Single verification gate at the end of lowering
  VerifyLoweredMir(design, *mir_arena, *input.type_arena);

  LoweringStats stats = ComputeMirStats(design, *mir_arena);

  return LoweringResult{
      .design = std::move(design),
      .mir_arena = std::move(mir_arena),
      .origin_map = std::move(origin_map),
      .stats = stats,
  };
}

}  // namespace lyra::lowering::hir_to_mir

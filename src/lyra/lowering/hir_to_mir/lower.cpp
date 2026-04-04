#include "lyra/lowering/hir_to_mir/lower.hpp"

#include <cstddef>
#include <expected>
#include <format>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/design.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/optimize.hpp"
#include "lyra/mir/package.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/verify.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ProcessKindStr(mir::ProcessKind kind) -> std::string_view {
  switch (kind) {
    case mir::ProcessKind::kOnce:
      return "initial";
    case mir::ProcessKind::kLooping:
      return "always";
    case mir::ProcessKind::kFinal:
      return "final";
  }
  return "process";
}

// Verify all MIR produced by lowering. Dispatches to the correct arena
// by ownership domain: body routines use body.arena, design-global
// routines use the design arena.
void VerifyLoweredMir(
    const mir::Design& design, const mir::InstanceTable& instance_table,
    const mir::Arena& design_arena, const TypeArena& type_arena) {
  for (size_t ei = 0; ei < design.elements.size(); ++ei) {
    const auto& element = design.elements[ei];
    std::visit(
        common::Overloaded{
            [&](const mir::Module& mod) {
              const auto& body = mir::GetModuleBody(design, mod);
              const auto& arena = body.arena;
              std::string_view module_path =
                  instance_table.GetPathBySymbol(mod.instance_sym);
              for (size_t fi = 0; fi < body.functions.size(); ++fi) {
                std::string label =
                    module_path.empty()
                        ? std::format("element[{}]: function[{}]", ei, fi)
                        : std::format("{}: function[{}]", module_path, fi);
                mir::VerifyFunction(
                    arena[body.functions[fi]], arena, type_arena, label);
              }
              for (size_t pi = 0; pi < body.processes.size(); ++pi) {
                const auto& proc = arena[body.processes[pi]];
                std::string label = module_path.empty()
                                        ? std::format(
                                              "element[{}]: {}[{}]", ei,
                                              ProcessKindStr(proc.kind), pi)
                                        : std::format(
                                              "{}: {}[{}]", module_path,
                                              ProcessKindStr(proc.kind), pi);
                mir::VerifyProcess(proc, arena, type_arena, label);
              }
            },
            [&](const mir::Package& pkg) {
              for (size_t fi = 0; fi < pkg.functions.size(); ++fi) {
                std::string label =
                    std::format("element[{}]: function[{}]", ei, fi);
                mir::VerifyFunction(
                    design_arena[pkg.functions[fi]], design_arena, type_arena,
                    label);
              }
            },
        },
        element);
  }
}

// Compute MIR statistics for --stats output.
// Dispatches to body arenas for body routines, design arena for
// design-global routines.
auto ComputeMirStats(const mir::Design& design, const mir::Arena& design_arena)
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
              const auto& body = mir::GetModuleBody(design, mod);
              for (auto fid : body.functions) {
                count_routine(body.arena[fid]);
              }
              for (auto pid : body.processes) {
                count_routine(body.arena[pid]);
              }
            },
            [&](const mir::Package& pkg) {
              for (auto fid : pkg.functions) {
                count_routine(design_arena[fid]);
              }
            },
        },
        element);
  }

  for (auto fid : design.generated_functions) {
    count_routine(design_arena[fid]);
  }
  for (auto pid : design.init_processes) {
    count_routine(design_arena[pid]);
  }

  return stats;
}

}  // namespace

auto LowerHirToMir(const LoweringInput& input) -> Result<LoweringResult> {
  if (input.specialization_map == nullptr) {
    throw common::InternalError(
        "LowerHirToMir", "specialization_map must not be null");
  }

  auto design_arena = std::make_unique<mir::Arena>();
  OriginMap design_origins;

  LoweringInput full_input = input;
  full_input.builtin_types = InternBuiltinTypes(*input.type_arena);

  auto design_result =
      LowerDesign(*input.design, full_input, *design_arena, &design_origins);
  if (!design_result) return std::unexpected(design_result.error());
  mir::Design design = std::move(design_result->design);
  mir::ConstructionInput construction = std::move(design_result->construction);
  auto resolved_bindings = std::move(design_result->resolved_bindings);
  auto body_origins = std::move(design_result->body_origins);
  auto dpi_export_wrappers = std::move(design_result->dpi_export_wrappers);

  // Pre-optimization verification: lowered MIR must be structurally valid
  VerifyLoweredMir(
      design, construction.instance_table, *design_arena, *input.type_arena);

  // MIR optimization stage: routine-local transforms on verified MIR
  mir::RunMirOptimizations(design, *design_arena);

  // Post-optimization verification: optimized MIR must remain valid
  VerifyLoweredMir(
      design, construction.instance_table, *design_arena, *input.type_arena);

  LoweringStats stats = ComputeMirStats(design, *design_arena);

  return LoweringResult{
      .design = std::move(design),
      .construction = std::move(construction),
      .design_arena = std::move(design_arena),
      .design_origins = std::move(design_origins),
      .body_origins = std::move(body_origins),
      .stats = stats,
      .resolved_bindings = std::move(resolved_bindings),
      .dpi_export_wrappers = std::move(dpi_export_wrappers),
      .dpi_header = std::move(design_result->dpi_header),
  };
}

}  // namespace lyra::lowering::hir_to_mir

#include <cstdint>
#include <expected>
#include <utility>
#include <variant>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/design.hpp"
#include "lyra/lowering/hir_to_mir/design_internal.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/module.hpp"
#include "lyra/lowering/hir_to_mir/package.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {
template <class>
inline constexpr bool kAlwaysFalse = false;
}  // namespace

auto LowerDesign(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map) -> Result<mir::Design> {
  const DesignDeclarations decls =
      CollectDeclarations(design, input, mir_arena);

  mir::Design result;
  result.num_design_slots = decls.num_design_slots;
  result.slots = decls.slots;
  result.global_precision_power = input.global_precision_power;
  // Thread slot ranges and def keys for process template grouping in LLVM
  // backend
  result.instance_slot_ranges.reserve(decls.instance_slot_ranges.size());
  for (const auto& range : decls.instance_slot_ranges) {
    result.instance_slot_ranges.push_back({range.slot_begin, range.slot_count});
  }
  result.module_def_ids = decls.module_def_ids;
  result.instance_param_inits = decls.instance_param_inits;
  if (input.instance_table != nullptr) {
    result.instance_table = *input.instance_table;
  }

  // Lower package init processes
  // Collect dynamically generated functions (e.g., strobe thunks)
  DeclView init_view{
      .design_places = &decls.design_places,
      .functions = &decls.functions,
      .slots = &decls.slots};
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      if (pkg->init_process) {
        hir::ProcessId hir_proc_id = pkg->init_process;
        const hir::Process& proc = (*input.hir_arena)[hir_proc_id];
        // Package init processes have no owning instance (UINT32_MAX sentinel)
        Result<mir::ProcessId> mir_proc_result = LowerProcess(
            hir_proc_id, proc, input, mir_arena, init_view, origin_map,
            UINT32_MAX, &result.generated_functions);
        if (!mir_proc_result) {
          return std::unexpected(mir_proc_result.error());
        }
        result.init_processes.push_back(*mir_proc_result);
      }
    }
  }

  // Lower design elements (modules and packages)
  uint32_t module_index = 0;
  for (const auto& element : design.elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      Result<mir::ModuleBody> body_result =
          LowerModule(*mod, input, mir_arena, origin_map, decls, module_index);
      ++module_index;
      if (!body_result) {
        return std::unexpected(body_result.error());
      }
      mir::ModuleBodyId body_id{
          static_cast<uint32_t>(result.module_bodies.size())};
      result.module_bodies.push_back(std::move(*body_result));
      result.elements.emplace_back(
          mir::Module{.instance_sym = mod->symbol, .body_id = body_id});
    } else if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      Result<mir::Package> pkg_result =
          LowerPackage(*pkg, input, mir_arena, origin_map, decls);
      if (!pkg_result) {
        return std::unexpected(pkg_result.error());
      }
      result.elements.emplace_back(std::move(*pkg_result));
    }
  }

  // Apply port drive bindings (creates synthetic always_comb processes)
  if (input.binding_plan != nullptr) {
    auto binding_result =
        ApplyBindings(*input.binding_plan, decls, input, mir_arena, result);
    if (!binding_result) return std::unexpected(binding_result.error());
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir

#include "lyra/lowering/hir_to_mir/design.hpp"

#include <cstddef>
#include <type_traits>
#include <utility>
#include <variant>

#include "absl/container/flat_hash_map.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/module.hpp"
#include "lyra/lowering/hir_to_mir/package.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/lowering/hir_to_mir/routine.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/sensitivity.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {
template <class>
inline constexpr bool kAlwaysFalse = false;

// Create a synthetic always_comb process for a port drive binding.
// This implements input port semantics: parent expression drives child
// variable.
auto CreateDriveProcess(
    const ast_to_hir::DriveBinding& binding, const DesignDeclarations& decls,
    const LoweringInput& input, mir::Arena& mir_arena)
    -> Result<mir::ProcessId> {
  // Set up context for lowering the rvalue expression.
  // Port bindings only reference design-level symbols, so module_places
  // points to design_places.
  Context ctx{
      .mir_arena = &mir_arena,
      .hir_arena = input.hir_arena,
      .type_arena = input.type_arena,
      .constant_arena = input.constant_arena,
      .symbol_table = input.symbol_table,
      .module_places = &decls.design_places,
      .local_places = {},
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .temp_types = {},
      .builtin_types = input.builtin_types,
      .symbol_to_mir_function = &decls.functions,
  };

  MirBuilder builder(&mir_arena, &ctx);

  BlockIndex entry_idx = builder.CreateBlock();
  builder.SetCurrentBlock(entry_idx);

  // Lower the rvalue expression to MIR operand
  auto source_result = LowerExpression(binding.rvalue, builder);
  if (!source_result) {
    return std::unexpected(source_result.error());
  }
  mir::Operand source = std::move(*source_result);

  // Get the target place (child port variable)
  mir::PlaceId target = decls.design_places.at(binding.child_port_sym);

  // Emit assignment: child_port = rvalue
  builder.EmitAssign(target, source);

  // Emit Repeat terminator (will be replaced with Wait)
  builder.EmitRepeat();

  std::vector<mir::BasicBlock> blocks = builder.Finish();

  // Build temp process to collect sensitivity
  mir::Process temp_process{
      .kind = mir::ProcessKind::kLooping,
      .entry = mir::BasicBlockId{entry_idx.value},
      .blocks = blocks,
  };
  auto triggers = mir::CollectSensitivity(temp_process, mir_arena);

  // Replace Repeat terminator with Wait (same as always_comb in process.cpp)
  for (auto& block : blocks) {
    if (std::holds_alternative<mir::Repeat>(block.terminator.data)) {
      block.terminator.data = mir::Wait{
          .triggers = std::move(triggers),
          .resume = mir::BasicBlockId{entry_idx.value},
      };
      break;
    }
  }

  mir::Process mir_process{
      .kind = mir::ProcessKind::kLooping,
      .entry = mir::BasicBlockId{entry_idx.value},
      .blocks = std::move(blocks),
  };

  return mir_arena.AddProcess(std::move(mir_process));
}

// Apply port bindings: drives become synthetic processes, aliases become
// entries in the alias_map.
auto ApplyBindings(
    const ast_to_hir::DesignBindingPlan& plan, const DesignDeclarations& decls,
    const LoweringInput& input, mir::Arena& mir_arena, mir::Design& design)
    -> Result<void> {
  if (plan.drives.empty() && plan.aliases.empty()) {
    return {};
  }

  // Build map from instance_sym -> element index for finding parent modules
  absl::flat_hash_map<SymbolId, size_t> instance_to_elem;
  for (size_t i = 0; i < design.elements.size(); ++i) {
    if (auto* mod = std::get_if<mir::Module>(&design.elements[i])) {
      instance_to_elem[mod->instance_sym] = i;
    }
  }

  // Create drive processes and attach to parent modules
  for (const auto& drive : plan.drives) {
    auto parent_it = instance_to_elem.find(drive.parent_instance_sym);
    if (parent_it == instance_to_elem.end()) {
      const Symbol& sym = (*input.symbol_table)[drive.parent_instance_sym];
      throw common::InternalError(
          "ApplyBindings",
          std::format(
              "parent instance '{}' not found in design elements", sym.name));
    }
    size_t parent_idx = parent_it->second;

    auto proc_result = CreateDriveProcess(drive, decls, input, mir_arena);
    if (!proc_result) {
      return std::unexpected(proc_result.error());
    }
    mir::ProcessId proc_id = *proc_result;

    // Attach process to parent module
    auto& module = std::get<mir::Module>(design.elements[parent_idx]);
    module.processes.push_back(proc_id);
  }

  // Process alias bindings (output/inout ports)
  for (const auto& alias : plan.aliases) {
    // Get the child port's place
    auto child_it = decls.design_places.find(alias.child_port_sym);
    if (child_it == decls.design_places.end()) {
      const Symbol& sym = (*input.symbol_table)[alias.child_port_sym];
      throw common::InternalError(
          "ApplyBindings",
          std::format("child port '{}' not found in design places", sym.name));
    }
    mir::PlaceId child_place_id = child_it->second;

    // Validate child place is a simple design slot (no projections)
    const mir::Place& child_place = mir_arena[child_place_id];
    if (child_place.root.kind != mir::PlaceRoot::Kind::kDesign) {
      throw common::InternalError(
          "ApplyBindings", "alias child port must be a design slot");
    }
    if (!child_place.projections.empty()) {
      throw common::InternalError(
          "ApplyBindings", "alias child port must have no projections");
    }

    mir::SlotId child_slot{static_cast<uint32_t>(child_place.root.id)};

    // Lower the parent lvalue expression to get the target PlaceId.
    // Note: We use a fresh Context with no current block, so any lvalue
    // that requires emitting instructions will fail. This is intentional:
    // alias targets must be pure places (no runtime computation).
    Context ctx{
        .mir_arena = &mir_arena,
        .hir_arena = input.hir_arena,
        .type_arena = input.type_arena,
        .constant_arena = input.constant_arena,
        .symbol_table = input.symbol_table,
        .module_places = &decls.design_places,
        .local_places = {},
        .next_local_id = 0,
        .next_temp_id = 0,
        .local_types = {},
        .temp_types = {},
        .builtin_types = input.builtin_types,
        .symbol_to_mir_function = &decls.functions,
    };

    MirBuilder builder(&mir_arena, &ctx);
    Result<LvalueResult> parent_lvalue_result =
        LowerLvalue(alias.lvalue, builder);
    if (!parent_lvalue_result) {
      return std::unexpected(parent_lvalue_result.error());
    }
    LvalueResult parent_lvalue = *parent_lvalue_result;

    // Validate parent place resolves to kDesign (may have projections)
    const mir::Place& parent_place = mir_arena[parent_lvalue.place];
    if (parent_place.root.kind != mir::PlaceRoot::Kind::kDesign) {
      throw common::InternalError(
          "ApplyBindings", "alias target must be a design slot");
    }

    // V1 invariant: alias targets must NOT contain BitRange projections.
    // This keeps alias composition trivial: child accesses just use the
    // resolved target place. If target had BitRange, composing with child's
    // own projections would be complex/invalid.
    for (const auto& proj : parent_place.projections) {
      if (mir::IsBitRange(proj)) {
        throw common::InternalError(
            "ApplyBindings",
            "alias target must not contain bit-range projections");
      }
    }

    // Record alias: child_slot -> parent_place
    design.alias_map[child_slot] = parent_lvalue.place;
  }
  return {};
}

}  // namespace

auto CollectDeclarations(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena) -> DesignDeclarations {
  DesignDeclarations decls;
  int next_slot = 0;

  // Ordering contract: packages first (in element order), then all module
  // instances (in BFS elaboration order from LowerDesign). This order is ABI —
  // do not change without updating all consumers (LLVM layout, MIR interpreter,
  // dump).

  // Allocate package variable design places + pre-allocate function IDs
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      for (SymbolId var : pkg->variables) {
        const Symbol& sym = (*input.symbol_table)[var];
        mir::Place place{
            .root =
                mir::PlaceRoot{
                    .kind = mir::PlaceRoot::Kind::kDesign,
                    .id = next_slot++,
                    .type = sym.type,
                },
            .projections = {},
        };
        decls.design_places[var] = mir_arena.AddPlace(std::move(place));
        decls.slot_table.push_back(sym.type);
      }

      // Pre-allocate MIR function IDs with frozen signatures
      for (hir::FunctionId hir_func_id : pkg->functions) {
        const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];
        mir::FunctionSignature sig =
            BuildFunctionSignature(hir_func, *input.symbol_table);
        mir::FunctionId mir_func_id = mir_arena.ReserveFunction(std::move(sig));
        decls.functions[hir_func.symbol] = mir_func_id;
      }
    }
  }

  // Allocate module variable design places
  for (const auto& element : design.elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      for (SymbolId var : mod->variables) {
        const Symbol& sym = (*input.symbol_table)[var];
        mir::Place place{
            .root =
                mir::PlaceRoot{
                    .kind = mir::PlaceRoot::Kind::kDesign,
                    .id = next_slot++,
                    .type = sym.type,
                },
            .projections = {},
        };
        decls.design_places[var] = mir_arena.AddPlace(std::move(place));
        decls.slot_table.push_back(sym.type);
      }
    }
  }

  decls.num_design_slots = static_cast<size_t>(next_slot);
  return decls;
}

auto LowerDesign(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map) -> Result<mir::Design> {
  const DesignDeclarations decls =
      CollectDeclarations(design, input, mir_arena);

  mir::Design result;
  result.num_design_slots = decls.num_design_slots;
  result.slot_table = decls.slot_table;

  // Lower package init processes
  DeclView init_view{
      .places = &decls.design_places, .functions = &decls.functions};
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      if (pkg->init_process) {
        hir::ProcessId hir_proc_id = pkg->init_process;
        const hir::Process& proc = (*input.hir_arena)[hir_proc_id];
        mir::ProcessId mir_proc = LowerProcess(
            hir_proc_id, proc, input, mir_arena, init_view, origin_map);
        result.init_processes.push_back(mir_proc);
      }
    }
  }

  // Lower design elements (modules and packages)
  for (const auto& element : design.elements) {
    std::visit(
        [&](const auto& e) {
          using T = std::decay_t<decltype(e)>;
          if constexpr (std::is_same_v<T, hir::Module>) {
            result.elements.emplace_back(
                LowerModule(e, input, mir_arena, origin_map, decls));
          } else if constexpr (std::is_same_v<T, hir::Package>) {
            result.elements.emplace_back(
                LowerPackage(e, input, mir_arena, origin_map, decls));
          } else {
            static_assert(kAlwaysFalse<T>, "unhandled hir::DesignElement");
          }
        },
        element);
  }

  // Apply port drive bindings (creates synthetic always_comb processes)
  if (input.binding_plan != nullptr) {
    auto binding_result =
        ApplyBindings(*input.binding_plan, decls, input, mir_arena, result);
    if (!binding_result) {
      return std::unexpected(binding_result.error());
    }
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir

#include "lyra/lowering/hir_to_mir/module.hpp"

#include <utility>
#include <vector>

#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/lowering/hir_to_mir/statement.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerFunctionBody(
    const hir::Function& function, const LoweringInput& input,
    mir::Arena& mir_arena, const PlaceMap& module_places,
    const SymbolToMirFunctionMap& symbol_to_mir_function) -> mir::Function {
  Context ctx{
      .mir_arena = &mir_arena,
      .hir_arena = &input.hir_arena,
      .type_arena = &input.type_arena,
      .constant_arena = &input.constant_arena,
      .symbol_table = &input.symbol_table,
      .module_places = &module_places,
      .local_places = {},
      .next_local_id = 0,
      .next_temp_id = 0,
      .bit_type = input.bit_type,
      .offset_type = input.offset_type,
      .symbol_to_mir_function = &symbol_to_mir_function,
      .return_place = mir::kInvalidPlaceId,
  };

  MirBuilder builder(&mir_arena, &ctx);
  BlockIndex entry_idx = builder.CreateBlock();
  builder.SetCurrentBlock(entry_idx);

  // Reserve local 0 for return value (non-void only)
  const Type& ret_type = input.type_arena[function.return_type];
  if (ret_type.Kind() != TypeKind::kVoid) {
    ctx.return_place = ctx.AllocLocal(function.symbol, function.return_type);
  }

  // Allocate parameters as locals
  for (SymbolId param : function.parameters) {
    const Symbol& sym = input.symbol_table[param];
    ctx.AllocLocal(param, sym.type);
  }

  // Lower function body
  LowerStatement(function.body, builder);

  // Implicit return at end
  builder.EmitReturn();

  std::vector<mir::BasicBlock> blocks = builder.Finish();

  // Collect storage types for interpreter
  std::vector<TypeId> local_types(static_cast<size_t>(ctx.next_local_id));
  std::vector<TypeId> temp_types(static_cast<size_t>(ctx.next_temp_id));

  // Fill in types from local_places
  for (const auto& [sym, place_id] : ctx.local_places) {
    const auto& place = mir_arena[place_id];
    if (place.root.kind == mir::PlaceRoot::Kind::kLocal) {
      auto idx = static_cast<size_t>(place.root.id);
      if (idx < local_types.size()) {
        local_types[idx] = place.root.type;
      }
    }
  }

  // Fill in temps from blocks (temps don't have symbols)
  for (const mir::BasicBlock& block : blocks) {
    for (const auto& inst : block.instructions) {
      std::visit(
          [&](const auto& i) {
            using T = std::decay_t<decltype(i)>;
            if constexpr (std::is_same_v<T, mir::Assign>) {
              const auto& place = mir_arena[i.target];
              if (place.root.kind == mir::PlaceRoot::Kind::kTemp) {
                auto idx = static_cast<size_t>(place.root.id);
                if (idx < temp_types.size()) {
                  temp_types[idx] = place.root.type;
                }
              }
            } else if constexpr (std::is_same_v<T, mir::Compute>) {
              const auto& place = mir_arena[i.target];
              if (place.root.kind == mir::PlaceRoot::Kind::kTemp) {
                auto idx = static_cast<size_t>(place.root.id);
                if (idx < temp_types.size()) {
                  temp_types[idx] = place.root.type;
                }
              }
            }
          },
          inst);
    }
  }

  return mir::Function{
      .entry = mir::BasicBlockId{entry_idx.value},  // Local index
      .blocks = std::move(blocks),
      .local_types = std::move(local_types),
      .temp_types = std::move(temp_types),
  };
}

}  // namespace

auto LowerModule(
    const hir::Module& module, const LoweringInput& input,
    mir::Arena& mir_arena) -> mir::Module {
  mir::Module result;

  // Allocate storage for module-level variables
  PlaceMap module_places;
  int next_module_slot = 0;

  for (SymbolId var_sym : module.variables) {
    const Symbol& sym = input.symbol_table[var_sym];
    mir::Place place{
        .root =
            mir::PlaceRoot{
                .kind = mir::PlaceRoot::Kind::kDesign,
                .id = next_module_slot++,
                .type = sym.type,
            },
        .projections = {},
    };
    mir::PlaceId place_id = mir_arena.AddPlace(std::move(place));
    module_places[var_sym] = place_id;
  }

  // Phase 1: Pre-allocate mir::FunctionIds and build symbol map
  SymbolToMirFunctionMap symbol_to_mir_function;
  std::vector<std::pair<hir::FunctionId, mir::FunctionId>> function_pairs;

  for (hir::FunctionId hir_func_id : module.functions) {
    const hir::Function& hir_func = input.hir_arena[hir_func_id];

    // Reserve the mir::FunctionId
    mir::FunctionId mir_func_id = mir_arena.ReserveFunction();
    symbol_to_mir_function[hir_func.symbol] = mir_func_id;
    function_pairs.emplace_back(hir_func_id, mir_func_id);
    result.functions.push_back(mir_func_id);
  }

  // Phase 2: Lower function bodies (map is complete, recursion works)
  for (auto [hir_func_id, mir_func_id] : function_pairs) {
    const hir::Function& hir_func = input.hir_arena[hir_func_id];

    mir::Function mir_func = LowerFunctionBody(
        hir_func, input, mir_arena, module_places, symbol_to_mir_function);

    mir_arena.SetFunction(mir_func_id, std::move(mir_func));
  }

  // Phase 3: Lower processes (can reference functions)
  for (hir::ProcessId proc_id : module.processes) {
    const hir::Process& hir_process = input.hir_arena[proc_id];
    mir::ProcessId mir_proc_id = LowerProcess(
        hir_process, input, mir_arena, module_places, symbol_to_mir_function);
    result.processes.push_back(mir_proc_id);
  }

  // Note: Tasks are lowered similarly to functions
  for (hir::TaskId task_id : module.tasks) {
    (void)task_id;
  }

  result.num_module_slots = static_cast<size_t>(next_module_slot);
  return result;
}

}  // namespace lyra::lowering::hir_to_mir

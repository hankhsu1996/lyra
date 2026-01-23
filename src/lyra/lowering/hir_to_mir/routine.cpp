#include "lyra/lowering/hir_to_mir/routine.hpp"

#include <cstddef>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/statement.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildFunctionSignature(
    const hir::Function& function, const SymbolTable& symbol_table)
    -> mir::FunctionSignature {
  mir::FunctionSignature sig;
  sig.return_type = function.return_type;
  sig.return_policy = mir::ReturnPolicy::kDirect;

  sig.params.reserve(function.parameters.size());
  for (SymbolId param : function.parameters) {
    const Symbol& sym = symbol_table[param];
    sig.params.push_back({.type = sym.type, .kind = mir::PassingKind::kValue});
  }

  return sig;
}

auto LowerFunctionBody(
    const hir::Function& function, const LoweringInput& input,
    mir::Arena& mir_arena, const PlaceMap& design_places,
    const SymbolToMirFunctionMap& symbol_to_mir_function, OriginMap* origin_map)
    -> mir::Function {
  Context ctx{
      .mir_arena = &mir_arena,
      .hir_arena = input.hir_arena,
      .type_arena = input.type_arena,
      .constant_arena = input.constant_arena,
      .symbol_table = input.symbol_table,
      .module_places = &design_places,
      .local_places = {},
      .next_local_id = 0,
      .next_temp_id = 0,
      .builtin_types = input.builtin_types,
      .symbol_to_mir_function = &symbol_to_mir_function,
      .return_place = mir::kInvalidPlaceId,
  };

  MirBuilder builder(&mir_arena, &ctx, origin_map);
  BlockIndex entry_idx = builder.CreateBlock();
  builder.SetCurrentBlock(entry_idx);

  // Reserve local 0 for return value (non-void only)
  const Type& ret_type = (*input.type_arena)[function.return_type];
  if (ret_type.Kind() != TypeKind::kVoid) {
    ctx.return_place = ctx.AllocLocal(function.symbol, function.return_type);
  }

  // Allocate parameters as locals
  for (SymbolId param : function.parameters) {
    const Symbol& sym = (*input.symbol_table)[param];
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
          inst.data);
    }
  }

  return mir::Function{
      .signature = {},
      .entry = mir::BasicBlockId{entry_idx.value},
      .blocks = std::move(blocks),
      .local_types = std::move(local_types),
      .temp_types = std::move(temp_types),
  };
}

}  // namespace lyra::lowering::hir_to_mir

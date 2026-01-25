#include "lyra/lowering/hir_to_mir/routine.hpp"

#include <utility>
#include <vector>

#include "lyra/common/symbol.hpp"
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
    mir::Arena& mir_arena, const DeclView& decl_view, OriginMap* origin_map)
    -> mir::Function {
  Context ctx{
      .mir_arena = &mir_arena,
      .hir_arena = input.hir_arena,
      .type_arena = input.type_arena,
      .constant_arena = input.constant_arena,
      .symbol_table = input.symbol_table,
      .module_places = decl_view.places,
      .local_places = {},
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .temp_types = {},
      .builtin_types = input.builtin_types,
      .symbol_to_mir_function = decl_view.functions,
  };

  MirBuilder builder(&mir_arena, &ctx, origin_map);
  BlockIndex entry_idx = builder.CreateBlock();
  builder.SetCurrentBlock(entry_idx);

  // Allocate parameters as locals and record their slot indices
  std::vector<uint32_t> param_local_slots;
  param_local_slots.reserve(function.parameters.size());
  for (SymbolId param : function.parameters) {
    const Symbol& sym = (*input.symbol_table)[param];
    auto alloc = ctx.AllocLocal(param, sym.type);
    param_local_slots.push_back(alloc.local_slot);
  }

  // Lower function body
  LowerStatement(function.body, builder);

  // Handle implicit return for void functions (non-void throws InternalError)
  builder.EmitImplicitReturn(function.return_type);

  std::vector<mir::BasicBlock> blocks = builder.Finish();

  // Signature is set by caller (SetFunctionBody); use empty placeholder here
  return mir::Function{
      .signature = {},
      .entry = mir::BasicBlockId{entry_idx.value},
      .blocks = std::move(blocks),
      .local_types = std::move(ctx.local_types),
      .temp_types = std::move(ctx.temp_types),
      .param_local_slots = std::move(param_local_slots),
  };
}

}  // namespace lyra::lowering::hir_to_mir

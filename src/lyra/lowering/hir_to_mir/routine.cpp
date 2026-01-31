#include "lyra/lowering/hir_to_mir/routine.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/statement.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Determine return policy based on return type.
// This is frozen here - backends consume it, don't recompute it.
//
// Policy rules:
// - kVoid: void functions
// - kSretOutParam: all aggregates (unpacked struct/array/union) and managed
// - kDirect: scalars only (integral, real, packed types)
//
// All aggregates and managed types use sret for ABI stability.
auto ComputeReturnPolicy(TypeId return_type, const TypeArena& types)
    -> mir::ReturnPolicy {
  const Type& type = types[return_type];

  switch (type.Kind()) {
    case TypeKind::kVoid:
      return mir::ReturnPolicy::kVoid;

    // Sret: all aggregates and managed types
    case TypeKind::kUnpackedStruct:
    case TypeKind::kUnpackedArray:
    case TypeKind::kUnpackedUnion:
    case TypeKind::kString:
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
      return mir::ReturnPolicy::kSretOutParam;

    // Direct return: scalars (fit in registers)
    case TypeKind::kIntegral:
    case TypeKind::kReal:
    case TypeKind::kShortReal:
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum:
      return mir::ReturnPolicy::kDirect;
  }
  return mir::ReturnPolicy::kDirect;
}

}  // namespace

auto BuildFunctionSignature(
    const hir::Function& function, const SymbolTable& symbol_table,
    const TypeArena& type_arena) -> mir::FunctionSignature {
  mir::FunctionSignature sig;
  sig.return_type = function.return_type;
  sig.return_policy = ComputeReturnPolicy(function.return_type, type_arena);

  sig.params.reserve(function.parameters.size());
  for (const hir::FunctionParam& param : function.parameters) {
    const Symbol& sym = symbol_table[param.symbol];
    mir::PassingKind kind = mir::PassingKind::kValue;
    switch (param.direction) {
      case hir::ParameterDirection::kInput:
        kind = mir::PassingKind::kValue;
        break;
      case hir::ParameterDirection::kOutput:
        kind = mir::PassingKind::kOut;
        break;
      case hir::ParameterDirection::kInOut:
        kind = mir::PassingKind::kInOut;
        break;
      case hir::ParameterDirection::kRef:
        // Should have been rejected at HIR lowering
        throw common::InternalError(
            "BuildFunctionSignature", "ref parameters not supported");
    }
    sig.params.push_back({.type = sym.type, .kind = kind});
  }

  return sig;
}

auto LowerFunctionBody(
    const hir::Function& function, const LoweringInput& input,
    mir::Arena& mir_arena, const DeclView& decl_view, OriginMap* origin_map)
    -> Result<mir::Function> {
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
      .return_slot = std::nullopt,
      .return_type = function.return_type,
  };

  MirBuilder builder(&mir_arena, &ctx, origin_map);
  BlockIndex entry_idx = builder.CreateBlock();
  BlockIndex exit_idx = builder.CreateBlock();
  builder.SetExitBlock(exit_idx);
  builder.SetCurrentBlock(entry_idx);

  // Allocate parameters as locals and record their slot indices
  std::vector<uint32_t> param_local_slots;
  param_local_slots.reserve(function.parameters.size());
  for (const hir::FunctionParam& param : function.parameters) {
    const Symbol& sym = (*input.symbol_table)[param.symbol];
    auto alloc = ctx.AllocLocal(param.symbol, sym.type);
    param_local_slots.push_back(alloc.local_slot);
  }

  // For non-void functions, allocate return_slot (the implicit return
  // variable). This must be done AFTER parameters since HIR return_var is
  // already registered as a local with the function name in scope.
  if (function.return_var.has_value()) {
    auto alloc = ctx.AllocLocal(*function.return_var, function.return_type);
    ctx.return_slot = alloc.place;
  }

  // Lower function body
  Result<void> stmt_result = LowerStatement(function.body, builder);
  if (!stmt_result) {
    return std::unexpected(stmt_result.error());
  }

  // If body falls through, branch to exit
  if (builder.IsReachable()) {
    builder.EmitJump(exit_idx);
  }

  // Emit exit block: load from return_slot and return, or void return
  builder.SetCurrentBlock(exit_idx);
  if (ctx.return_slot.has_value()) {
    builder.EmitReturn(mir::Operand::Use(*ctx.return_slot));
  } else {
    builder.EmitTerminate(std::nullopt);
  }

  std::vector<mir::BasicBlock> blocks = builder.Finish();

  // Signature and origins are set by caller (SetFunctionBody); placeholders
  // here
  return mir::Function{
      .signature = {},
      .entry = mir::BasicBlockId{entry_idx.value},
      .blocks = std::move(blocks),
      .local_types = std::move(ctx.local_types),
      .temp_types = std::move(ctx.temp_types),
      .param_local_slots = std::move(param_local_slots),
      .param_origins = {},
      .origin = common::OriginId::Invalid(),
  };
}

}  // namespace lyra::lowering::hir_to_mir

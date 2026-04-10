#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/design_internal.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/connection_endpoint.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

// Recursively find any NameRef symbol in an expression tree.
auto FindAnyNameRef(hir::ExpressionId expr_id, const hir::Arena& arena)
    -> std::optional<SymbolId> {
  const auto& expr = arena[expr_id];
  if (expr.kind == hir::ExpressionKind::kNameRef) {
    return std::get<hir::NameRefExpressionData>(expr.data).symbol;
  }
  // Walk children for common expression kinds.
  return std::visit(
      [&](const auto& data) -> std::optional<SymbolId> {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::UnaryExpressionData>) {
          return FindAnyNameRef(data.operand, arena);
        } else if constexpr (std::is_same_v<T, hir::BinaryExpressionData>) {
          auto lhs = FindAnyNameRef(data.lhs, arena);
          if (lhs) return lhs;
          return FindAnyNameRef(data.rhs, arena);
        } else if constexpr (std::is_same_v<T, hir::CastExpressionData>) {
          return FindAnyNameRef(data.operand, arena);
        } else if constexpr (std::is_same_v<
                                 T, hir::ConditionalExpressionData>) {
          auto c = FindAnyNameRef(data.condition, arena);
          if (c) return c;
          auto t = FindAnyNameRef(data.then_expr, arena);
          if (t) return t;
          return FindAnyNameRef(data.else_expr, arena);
        } else if constexpr (std::is_same_v<T, hir::BitCastExpressionData>) {
          return FindAnyNameRef(data.operand, arena);
        } else {
          return std::nullopt;
        }
      },
      expr.data);
}

// Build a per-instance PlaceMap for expression lowering in a body-local
// context. Maps per-instance variable symbols to kModuleSlot places, deriving
// LocalSlotId from canonical body-local slot data (not manual ordering).
auto BuildPerInstancePlaces(
    const hir::Module& inst_mod, const hir::Module& rep_mod,
    const std::vector<BodyLocalSlotEntry>& body_slots, mir::Arena& body_arena)
    -> PlaceMap {
  // Build representative symbol -> LocalSlotId from canonical body-local data.
  std::unordered_map<SymbolId, common::LocalSlotId, SymbolIdHash>
      rep_sym_to_slot;
  std::unordered_map<SymbolId, TypeId, SymbolIdHash> rep_sym_to_type;
  for (const auto& entry : body_slots) {
    rep_sym_to_slot[entry.sym] = entry.local_slot;
    rep_sym_to_type[entry.sym] = entry.type;
  }

  PlaceMap places;

  auto map_by_position = [&](const std::vector<SymbolId>& inst_syms,
                             const std::vector<SymbolId>& rep_syms) {
    for (size_t i = 0; i < inst_syms.size(); ++i) {
      auto slot_it = rep_sym_to_slot.find(rep_syms[i]);
      if (slot_it == rep_sym_to_slot.end()) continue;
      auto type_it = rep_sym_to_type.find(rep_syms[i]);
      auto place_id = body_arena.AddPlace(
          mir::Place{
              .root =
                  mir::PlaceRoot{
                      .kind = mir::PlaceRoot::Kind::kModuleSlot,
                      .id = static_cast<int>(slot_it->second.value),
                      .type = type_it->second,
                  },
              .projections = {}});
      places[inst_syms[i]] = place_id;
    }
  };

  map_by_position(inst_mod.variables, rep_mod.variables);
  map_by_position(inst_mod.nets, rep_mod.nets);
  map_by_position(inst_mod.param_slots, rep_mod.param_slots);

  return places;
}

// Lower a parent expression as a body-local function in the parent body.
// The function reads parent values through kModuleSlot and returns the
// computed value. The child endpoint is NOT part of the function body --
// it's metadata on the CompiledConnectionExpr.
auto LowerExprAsBodyFunction(
    hir::ExpressionId expr_id, TypeId result_type, const LoweringInput& input,
    const DesignDeclarations& decls, mir::Arena& body_arena,
    const PlaceMap& per_instance_places) -> Result<mir::FunctionId> {
  // Set up a hybrid context: design-level HIR arena (has the expression)
  // + body-local MIR arena (for emitting instructions) + body-local places.
  Context ctx{
      .mir_arena = &body_arena,
      .hir_arena = input.hir_arena,
      .type_arena = input.type_arena,
      .active_constant_arena = input.active_constant_arena,
      .symbol_table = input.symbol_table,
      .body_places = &per_instance_places,
      .design_places = &decls.design_places,
      .local_places = {},
      .design_place_cache = {},
      .body_events = nullptr,
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .temp_types = {},
      .temp_metadata = {},
      .builtin_types = input.builtin_types,
      .symbol_to_mir_function = nullptr,
      .design_functions = nullptr,
      .dpi_imports = nullptr,
      .cross_instance_places = nullptr,
      .generated_functions = nullptr,
      .return_slot = std::nullopt,
      .return_type = result_type,
      .design_slots = &decls.slots,
      .body_slots = nullptr,
      .cover_site_registry = nullptr,
      .deferred_assertion_site_registry = nullptr,
      .materialize_count = 0,
      .external_refs = nullptr,
      .provisional_targets = nullptr,
      .external_ref_cache = {},
  };

  MirBuilder builder(&body_arena, &ctx, nullptr);

  BlockIndex entry_idx = builder.CreateBlock();
  builder.SetCurrentBlock(entry_idx);

  auto expr_result = LowerExpression(expr_id, builder);
  if (!expr_result) {
    return std::unexpected(expr_result.error());
  }

  builder.EmitReturn(*expr_result);

  std::vector<mir::BasicBlock> blocks = builder.Finish();

  mir::Function func{
      .signature =
          mir::FunctionSignature{
              .return_type = result_type,
              .params = {},
              .return_policy = mir::ReturnPolicy::kDirect,
          },
      .entry = mir::BasicBlockId{entry_idx.value},
      .blocks = std::move(blocks),
      .local_types = std::move(ctx.local_types),
      .temp_types = std::move(ctx.temp_types),
      .temp_metadata = std::move(ctx.temp_metadata),
      .param_local_slots = {},
      .param_origins = {},
      .decision_sites = {},
      .abi_contract = {},
      .monitor_check_meta = std::nullopt,
      .monitor_setup_meta = std::nullopt,
  };

  mir::FunctionId func_id = body_arena.AddFunction(std::move(func));
  body_arena.MarkModuleScoped(func_id);
  return func_id;
}

}  // namespace lyra::lowering::hir_to_mir

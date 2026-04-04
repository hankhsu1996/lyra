#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/design_internal.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/connection_endpoint.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Extract the referenced SymbolId from a simple NameRef expression.
auto TryExtractNameRefSymbol(hir::ExpressionId expr_id, const hir::Arena& arena)
    -> std::optional<SymbolId> {
  const auto& expr = arena[expr_id];
  if (expr.kind != hir::ExpressionKind::kNameRef) {
    return std::nullopt;
  }
  const auto& data = std::get<hir::NameRefExpressionData>(expr.data);
  return data.symbol;
}

// Recursively find any NameRef symbol in an expression tree.
// Used to identify the parent module for non-kernelizable expressions.
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
    const std::vector<BodyLocalSlotEntry>& body_slots,
    const SymbolTable& symbol_table, mir::Arena& body_arena) -> PlaceMap {
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
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .temp_types = {},
      .temp_metadata = {},
      .builtin_types = input.builtin_types,
      .return_slot = std::nullopt,
      .return_type = result_type,
      .design_slots = &decls.slots,
  };

  MirBuilder builder(&body_arena, &ctx, nullptr, hir::kInvalidModuleBodyId);

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
  };

  return body_arena.AddFunction(std::move(func));
}

}  // namespace

auto CompileBindings(
    const ast_to_hir::DesignBindingPlan& plan,
    const InstanceSlotResolver& resolver, const LoweringInput& input,
    const DesignDeclarations& decls, ExprCompilationData& expr_data)
    -> Result<mir::ResolvedBindingPlan> {
  mir::ResolvedBindingPlan result;
  if (plan.bindings.empty()) {
    return result;
  }

  const auto& hir_arena = *input.hir_arena;

  for (const auto& binding : plan.bindings) {
    // Resolve child port endpoint. child_port_sym is a per-instance unique
    // symbol, so ResolveByVariable is sufficient (no instance key needed).
    auto child_endpoint = resolver.ResolveByVariable(binding.child_port_sym);

    switch (binding.kind) {
      case ast_to_hir::PortBinding::Kind::kDriveParentToChild: {
        auto parent_sym =
            TryExtractNameRefSymbol(binding.parent_rvalue, hir_arena);
        if (parent_sym) {
          // Kernelizable: simple variable-to-variable connection.
          auto parent_endpoint = resolver.ResolveByVariable(*parent_sym);
          const Symbol& child_sym =
              (*input.symbol_table)[binding.child_port_sym];

          result.kernel_bindings.push_back(
              mir::ResolvedKernelBinding{
                  .kind = mir::PortConnection::Kind::kDriveParentToChild,
                  .src = parent_endpoint,
                  .dst = child_endpoint,
                  .trigger = parent_endpoint,
                  .trigger_edge = common::EdgeKind::kAnyChange,
                  .trigger_observation = std::nullopt,
                  .value_type = child_sym.type,
                  .child_port_sym = binding.child_port_sym,
                  .parent_instance_sym = binding.parent_instance_sym,
              });
        } else {
          // Non-kernelizable: compile as body-local function.
          auto any_ref = FindAnyNameRef(binding.parent_rvalue, hir_arena);
          if (!any_ref) {
            throw common::InternalError(
                "CompileBindings",
                "non-kernelizable expression has no variable references");
          }

          auto parent_endpoint = resolver.ResolveByVariable(*any_ref);
          uint32_t parent_oi = parent_endpoint.object_index;
          const auto& parent_obj = expr_data.objects->at(parent_oi);
          uint32_t body_group = parent_obj.body_group;

          // Find representative module for this body.
          uint32_t rep_path = expr_data.body_to_representative->at(body_group);
          const auto& rep_mod = *expr_data.hir_modules->at(rep_path);
          const auto& inst_mod =
              *expr_data.hir_modules->at(parent_obj.path_index);
          auto& parent_body = expr_data.module_bodies->at(body_group);
          const auto& body_slots = expr_data.body_local_slots->at(body_group);

          // Build per-instance places for expression lowering.
          auto per_instance_places = BuildPerInstancePlaces(
              inst_mod, rep_mod, body_slots, *input.symbol_table,
              parent_body.arena);

          const Symbol& child_sym =
              (*input.symbol_table)[binding.child_port_sym];

          auto func_result = LowerExprAsBodyFunction(
              binding.parent_rvalue, child_sym.type, input, decls,
              parent_body.arena, per_instance_places);
          if (!func_result) {
            return std::unexpected(func_result.error());
          }

          parent_body.functions.push_back(*func_result);

          result.expr_bindings.push_back(
              mir::CompiledConnectionExpr{
                  .kind = mir::PortConnection::Kind::kDriveParentToChild,
                  .parent_body_id = mir::ModuleBodyId{body_group},
                  .expr_function = *func_result,
                  .parent_object_index = parent_oi,
                  .child_object_index = child_endpoint.object_index,
                  .child_local_slot = child_endpoint.local_slot,
                  .result_type = child_sym.type,
                  .trigger = parent_endpoint,
                  .trigger_edge = common::EdgeKind::kAnyChange,
                  .child_port_sym = binding.child_port_sym,
                  .parent_instance_sym = binding.parent_instance_sym,
              });
        }
        break;
      }
      case ast_to_hir::PortBinding::Kind::kDriveChildToParent: {
        auto parent_sym =
            TryExtractNameRefSymbol(binding.parent_lvalue, hir_arena);
        if (!parent_sym) {
          throw common::InternalError(
              "CompileBindings",
              std::format(
                  "non-kernelizable output port for child port sym {}; "
                  "projected output ports not yet supported",
                  binding.child_port_sym.value));
        }

        auto parent_endpoint = resolver.ResolveByVariable(*parent_sym);
        const Symbol& child_sym = (*input.symbol_table)[binding.child_port_sym];

        result.kernel_bindings.push_back(
            mir::ResolvedKernelBinding{
                .kind = mir::PortConnection::Kind::kDriveChildToParent,
                .src = child_endpoint,
                .dst = parent_endpoint,
                .trigger = child_endpoint,
                .trigger_edge = common::EdgeKind::kAnyChange,
                .trigger_observation = std::nullopt,
                .value_type = child_sym.type,
                .child_port_sym = binding.child_port_sym,
                .parent_instance_sym = binding.parent_instance_sym,
            });
        break;
      }
    }
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir

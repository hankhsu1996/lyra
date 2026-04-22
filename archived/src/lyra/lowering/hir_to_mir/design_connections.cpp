#include <algorithm>
#include <expected>
#include <format>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/overloaded.hpp"
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
#include "lyra/mir/connection_recipe.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/statement.hpp"

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

// Lower a parent->child binding as a body-local writeback function.
//
// The function reads parent values through kModuleSlot places and
// assigns the computed value to an ExternalRefId representing the
// child target slot. There is no return value -- the body performs
// the write itself, just like an ordinary always_comb body. The same
// typed-assign / write-dispatch / dirty-notify chain used by every
// other assignment in the pipeline carries this write.
//
// child_target_ref must already be registered in the parent body's
// external_refs vector (done by the caller before invoking this).
auto LowerExprAsBodyFunction(
    hir::ExpressionId expr_id, TypeId result_type,
    mir::ExternalRefId child_target_ref, const LoweringInput& input,
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
      .local_place_ids = {},
      .temp_types = {},
      .temp_metadata = {},
      .builtin_types = input.builtin_types,
      .symbol_to_mir_function = nullptr,
      .design_functions = nullptr,
      .dpi_imports = nullptr,
      .generated_functions = nullptr,
      .return_slot = std::nullopt,
      .return_type = input.builtin_types.void_type,
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

  // Emit the cross-object writeback: the child target is addressed
  // through the pre-registered ExternalRefId. The standard typed-assign
  // path (PlainAssign / CopyAssign / MoveAssign classification) carries
  // this statement into the existing write-dispatch pipeline.
  builder.EmitTypedAssign(
      mir::WriteTarget{child_target_ref}, mir::RightHandSide(*expr_result),
      result_type);

  // Void-returning body: no return value, just an ordinary return.
  builder.EmitTerminate(std::nullopt);

  std::vector<mir::BasicBlock> blocks = builder.Finish();

  mir::Function func{
      .signature =
          mir::FunctionSignature{
              .return_type = input.builtin_types.void_type,
              .params = {},
              .return_policy = mir::ReturnPolicy::kVoid,
          },
      .runtime_meta = {},
      .entry = mir::BasicBlockId{entry_idx.value},
      .blocks = std::move(blocks),
      .local_types = std::move(ctx.local_types),
      .temp_types = std::move(ctx.temp_types),
      .temp_metadata = std::move(ctx.temp_metadata),
      .param_local_slots = {},
      .param_origins = {},
      .decision_sites = {},
      .abi_contract = {},
  };

  mir::FunctionId func_id = body_arena.AddFunction(std::move(func));
  body_arena.MarkModuleScoped(func_id);
  return func_id;
}

auto CollectParentLocalReadSlotsFromExprFunction(
    const mir::Function& func, const mir::Arena& arena)
    -> std::vector<common::LocalSlotId> {
  std::vector<common::LocalSlotId> slots;
  auto collect = [&](const mir::Operand& op) {
    if (op.kind != mir::Operand::Kind::kUse) return;
    const auto& place = arena[std::get<mir::PlaceId>(op.payload)];
    switch (place.root.kind) {
      case mir::PlaceRoot::Kind::kModuleSlot:
        slots.push_back(
            common::LocalSlotId{static_cast<uint32_t>(place.root.id)});
        break;
      case mir::PlaceRoot::Kind::kLocal:
      case mir::PlaceRoot::Kind::kTemp:
        break;
      case mir::PlaceRoot::Kind::kDesignGlobal:
      case mir::PlaceRoot::Kind::kObjectLocal:
        throw common::InternalError(
            "CollectParentLocalReadSlotsFromExprFunction",
            std::format(
                "unsupported read root kind {} in expr connection source",
                static_cast<int>(place.root.kind)));
    }
  };
  auto collect_rhs = [&](const mir::RightHandSide& rhs) {
    std::visit(
        common::Overloaded{
            [&](const mir::Operand& op) { collect(op); },
            [&](const mir::Rvalue& rv) {
              for (const auto& op : rv.operands) collect(op);
            }},
        rhs);
  };
  for (const auto& block : func.blocks) {
    for (const auto& stmt : block.statements)
      std::visit(
          common::Overloaded{
              [&](const auto& a)
                requires(mir::kIsDirectAssign<std::decay_t<decltype(a)>>)
              { collect_rhs(a.rhs); },
              [&](const mir::DefineTemp& dt) { collect_rhs(dt.rhs); },
              [](const auto&) {}},
              stmt.data);
    std::visit(
        common::Overloaded{
            [&](const mir::Branch& b) { collect(b.condition); },
            [&](const mir::Jump& j) {
              for (const auto& a : j.args) collect(a);
            },
            [](const auto&) {}},
        block.terminator.data);
  }
  std::ranges::sort(slots, {}, &common::LocalSlotId::value);
  auto [first, last] =
      std::ranges::unique(slots, {}, &common::LocalSlotId::value);
  slots.erase(first, slots.end());
  return slots;
}

void BuildInstallableComputations(mir::ModuleBody& body) {
  if (!body.installable_computations.empty()) {
    throw common::InternalError(
        "BuildInstallableComputations",
        "installable_computations already populated");
  }

  // Every parent->child recipe is backed by one body-local writeback
  // function that writes its child target internally through a
  // pre-registered ExternalRefId. This step does exactly two things:
  // extract the dependency set from the function's read set, and
  // package the callable + deps into a runtime template.
  //
  // The seam checks below guard exactly what this function reads or
  // relies on: (a) source.function is the active union arm, and
  // (b) the ExternalRefId the callable body was built against exists
  // in body.external_refs (where finalize-time binding resolution
  // will look for it).
  //
  // kDriveChildToParent recipes belong to the memcpy pipeline and are
  // skipped here. kConstant-sourced parent->child recipes are applied
  // inline during child creation as ChildConstInit writes and carry no
  // installable computation.
  for (const auto& recipe : body.connection_recipes) {
    if (recipe.kind != mir::PortConnection::Kind::kDriveParentToChild) {
      continue;
    }
    if (recipe.source.kind == mir::ConnectionSourceRecipe::Kind::kConstant) {
      continue;
    }
    if (recipe.source.kind != mir::ConnectionSourceRecipe::Kind::kFunction) {
      throw common::InternalError(
          "BuildInstallableComputations",
          "parent->child recipe must carry a kFunction or kConstant source");
    }
    if (recipe.target_ref.value >= body.external_refs.size()) {
      throw common::InternalError(
          "BuildInstallableComputations",
          "parent->child recipe target_ref out of range; writeback body "
          "requires a registered ExternalAccessRecipe");
    }

    auto func_id = recipe.source.function;
    const auto& func = body.arena[func_id];
    auto deps = CollectParentLocalReadSlotsFromExprFunction(func, body.arena);

    body.installable_computations.push_back(
        mir::InstallableComputationTemplate{
            .callable = func_id,
            .deps = std::move(deps),
        });
  }
}

}  // namespace lyra::lowering::hir_to_mir

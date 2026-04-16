#include <algorithm>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/edge_kind.hpp"
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
#include "lyra/mir/connection_endpoint.hpp"
#include "lyra/mir/connection_recipe.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/signal_ref.hpp"
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
      .local_place_ids = {},
      .temp_types = {},
      .temp_metadata = {},
      .builtin_types = input.builtin_types,
      .symbol_to_mir_function = nullptr,
      .design_functions = nullptr,
      .dpi_imports = nullptr,
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

namespace {

// Extract the complete parent-local storage read set from a body-local
// expression function. This defines the trigger set contract for
// expression-connection synthesis: every parent-local slot read by the
// expression becomes a wake source.
//
// Rejects disallowed read roots (kBoundChildDest, kObjectLocal) that
// must not appear in expression connection source functions.
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
      case mir::PlaceRoot::Kind::kBoundChildDest:
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

// Synthesize a looping body-local process from an expression function.
// Returns a mir::Process struct (NOT a ProcessId -- caller adds to arena
// during the suffix append pass).
//
// Precondition: read_slots is non-empty. Expression connections with
// zero parent-local reads are rejected -- a looping wait with no
// triggers has no defined meaning.
auto SynthesizeExprConnectionProcess(
    mir::Arena& body_arena, mir::FunctionId func_id,
    common::LocalSlotId child_slot, TypeId result_type,
    std::span<const common::LocalSlotId> read_slots) -> mir::Process {
  if (read_slots.empty()) {
    throw common::InternalError(
        "SynthesizeExprConnectionProcess",
        "expression connection produced empty parent-local trigger set");
  }

  const auto& func = body_arena[func_id];
  std::vector<mir::BasicBlock> blocks;
  mir::BasicBlockId entry_id = func.entry;

  for (const auto& block : func.blocks) {
    mir::BasicBlock nb;
    nb.params = block.params;
    nb.statements = block.statements;

    if (const auto* ret = std::get_if<mir::Return>(&block.terminator.data)) {
      // Append child write before Wait. Only PlainAssign.dest may
      // receive kBoundChildDest -- enforced here by construction.
      if (ret->value) {
        auto dest = body_arena.AddPlace(
            mir::Place{
                .root =
                    mir::PlaceRoot{
                        .kind = mir::PlaceRoot::Kind::kBoundChildDest,
                        .id = static_cast<int>(child_slot.value),
                        .type = result_type},
                .projections = {}});
        nb.statements.push_back(
            mir::Statement{
                .data = mir::PlainAssign{.dest = dest, .rhs = *ret->value}});
      }
      // Replace Return with Wait on full parent-local read set.
      std::vector<mir::WaitTrigger> wt;
      for (const auto& slot : read_slots) {
        wt.push_back(
            mir::WaitTrigger{
                .signal =
                    mir::SignalRef{
                        .scope = mir::SignalRef::Scope::kModuleLocal,
                        .id = slot.value,
                        .object_index = 0},
                .edge = common::EdgeKind::kAnyChange,
                .observed_place = std::nullopt,
                .late_bound = std::nullopt,
                .unresolved_external_ref = std::nullopt});
      }
      nb.terminator.data =
          mir::Wait{.triggers = std::move(wt), .resume = entry_id};
    } else {
      nb.terminator = block.terminator;
    }
    blocks.push_back(std::move(nb));
  }

  return mir::Process{
      .kind = mir::ProcessKind::kLooping,
      .entry = entry_id,
      .blocks = std::move(blocks),
      .temp_metadata = func.temp_metadata,
      .decision_sites = {}};
}

// Check if a place has kBoundChildDest root.
auto IsBoundChildDestPlace(mir::PlaceId pid, const mir::Arena& arena) -> bool {
  return arena[pid].root.kind == mir::PlaceRoot::Kind::kBoundChildDest;
}

// Reject kBoundChildDest in any read-position operand.
void RejectBoundChildInOperand(
    const mir::Operand& op, const mir::Arena& arena) {
  if (op.kind != mir::Operand::Kind::kUse) return;
  if (IsBoundChildDestPlace(std::get<mir::PlaceId>(op.payload), arena)) {
    throw common::InternalError(
        "VerifyExprConnectionProcessShape",
        "kBoundChildDest found in read position");
  }
}

// Reject kBoundChildDest in a write destination (for non-PlainAssign
// statement forms that should never target a bound child).
void RejectBoundChildInDest(mir::WriteTarget dest, const mir::Arena& arena) {
  if (const auto* pid = std::get_if<mir::PlaceId>(&dest)) {
    if (IsBoundChildDestPlace(*pid, arena)) {
      throw common::InternalError(
          "VerifyExprConnectionProcessShape",
          "kBoundChildDest found in non-PlainAssign dest");
    }
  }
}

// Verify that a synthesized expression connection process uses
// kBoundChildDest only in exactly one place: PlainAssign.dest.
// Rejects kBoundChildDest in:
//   - any operand (read position)
//   - CopyAssign.dest or MoveAssign.dest
//   - branch conditions, jump args
//   - wait trigger signals
void VerifyExprConnectionProcessShape(
    const mir::Process& proc, const mir::Arena& arena) {
  auto check_rhs = [&](const mir::RightHandSide& rhs) {
    std::visit(
        common::Overloaded{
            [&](const mir::Operand& op) {
              RejectBoundChildInOperand(op, arena);
            },
            [&](const mir::Rvalue& rv) {
              for (const auto& op : rv.operands)
                RejectBoundChildInOperand(op, arena);
            }},
        rhs);
  };

  for (const auto& block : proc.blocks) {
    for (const auto& stmt : block.statements) {
      std::visit(
          common::Overloaded{
              [&](const mir::PlainAssign& a) {
                // PlainAssign.dest is the ONLY allowed kBoundChildDest site.
                check_rhs(a.rhs);
              },
              [&](const mir::CopyAssign& a) {
                RejectBoundChildInDest(a.dest, arena);
                check_rhs(a.rhs);
              },
              [&](const mir::MoveAssign& a) {
                RejectBoundChildInDest(a.dest, arena);
                check_rhs(a.rhs);
              },
              [&](const mir::DefineTemp& dt) { check_rhs(dt.rhs); },
              [](const auto&) {}},
          stmt.data);
    }
    std::visit(
        common::Overloaded{
            [&](const mir::Branch& b) {
              RejectBoundChildInOperand(b.condition, arena);
            },
            [&](const mir::Jump& j) {
              for (const auto& a : j.args) RejectBoundChildInOperand(a, arena);
            },
            [&](const mir::Wait& w) {
              for (const auto& t : w.triggers) {
                if (t.signal.scope == mir::SignalRef::Scope::kBoundChildDest) {
                  throw common::InternalError(
                      "VerifyExprConnectionProcessShape",
                      "kBoundChildDest found in wait trigger");
                }
              }
            },
            [](const auto&) {}},
        block.terminator.data);
  }
}

// Synthesize expression connection processes for all kFunction-sourced
// connection recipes on the body. Appends synthesized processes to the
// body.processes suffix and populates body.expr_connection_templates.
//
// This is the single owning function for expression-connection execution
// artifact creation. No later phase may append expression processes,
// mutate their MIR, change their trigger set, or change their child
// destination representation.
void LowerExprConnectionForBodyImpl(mir::ModuleBody& body) {
  if (!body.expr_connection_templates.empty()) {
    throw common::InternalError(
        "LowerExprConnectionForBody",
        "expr_connection_templates already populated");
  }
  const auto ordinary_count_before =
      static_cast<uint32_t>(body.processes.size());

  std::vector<mir::Process> pending_expr_processes;
  std::vector<mir::ExprConnectionTemplate> pending_templates;

  for (const auto& recipe : body.connection_recipes) {
    if (recipe.source.kind != mir::ConnectionSourceRecipe::Kind::kFunction)
      continue;

    auto func_id = recipe.source.function;
    auto read_slots = CollectParentLocalReadSlotsFromExprFunction(
        body.arena[func_id], body.arena);

    auto proc = SynthesizeExprConnectionProcess(
        body.arena, func_id, recipe.child_slot, recipe.result_type, read_slots);

    VerifyExprConnectionProcessShape(proc, body.arena);
    pending_expr_processes.push_back(std::move(proc));
    pending_templates.push_back(
        mir::ExprConnectionTemplate{
            .expr_process_suffix_ordinal = 0,
            .child_site = recipe.child_site,
            .child_slot = recipe.child_slot,
            .result_type = recipe.result_type,
        });
  }

  // Append all synthesized processes as contiguous suffix.
  for (uint32_t i = 0; i < pending_expr_processes.size(); ++i) {
    auto pid = body.arena.AddProcess(std::move(pending_expr_processes[i]));
    body.processes.push_back(pid);
    pending_templates[i].expr_process_suffix_ordinal = i;
  }

  body.expr_connection_templates = std::move(pending_templates);

  // Verify suffix invariant.
  if (body.processes.size() !=
      ordinary_count_before + body.expr_connection_templates.size()) {
    throw common::InternalError(
        "LowerExprConnectionForBody",
        std::format(
            "suffix invariant violated: processes={}, "
            "ordinary={}, templates={}",
            body.processes.size(), ordinary_count_before,
            body.expr_connection_templates.size()));
  }
}

}  // namespace

void MaterializeExprConnectionProcessSuffix(mir::ModuleBody& body) {
  LowerExprConnectionForBodyImpl(body);
}

}  // namespace lyra::lowering::hir_to_mir

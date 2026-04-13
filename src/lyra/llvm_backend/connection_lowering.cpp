#include "lyra/llvm_backend/connection_lowering.hpp"

#include <algorithm>
#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/common/overloaded.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/connection_endpoint.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerConnectionArtifacts(const LoweringInput& input)
    -> LoweredConnectionArtifacts {
  LoweredConnectionArtifacts result;

  auto to_flat_slot = [&](const mir::BoundEndpoint& ref) -> common::SlotId {
    const auto& obj = input.construction->objects.at(ref.object_index.value);
    return common::SlotId{obj.design_state_base_slot + ref.local_slot.value};
  };

  // Build kernel entries from bound_connections (recipe path).
  // BoundConnection does not carry trigger_observation by design:
  // the recipe-based path resolves only slot-based triggers, which
  // never require byte-level observation metadata.
  if (input.bound_connections != nullptr) {
    for (const auto& bc : *input.bound_connections) {
      bool is_p2c = bc.kind == mir::PortConnection::Kind::kDriveParentToChild;
      result.kernel_entries.push_back(
          ConnectionKernelEntry{
              .process_id = {},
              .src_slot =
                  to_flat_slot(is_p2c ? bc.parent_source : bc.child_target),
              .dst_slot =
                  to_flat_slot(is_p2c ? bc.child_target : bc.parent_source),
              .trigger_slot = to_flat_slot(bc.trigger),
              .trigger_edge = bc.trigger_edge,
              .trigger_observation = std::nullopt,
          });
    }
  }

  // Clone expr_connections as design-level processes.
  // Remaps kModuleSlot -> kObjectLocal preserving body-local slot identity.
  if (input.expr_connections != nullptr) {
    for (const auto& expr : *input.expr_connections) {
      uint32_t parent_oi = expr.parent_object_index.value;
      uint32_t child_oi = expr.child_object_index.value;
      const auto& func = (*expr.parent_arena)[expr.expr_function];

      auto remap_place = [&](mir::PlaceId pid,
                             const mir::Arena& src) -> mir::PlaceId {
        const auto& place = src[pid];
        mir::Place new_place = place;
        if (place.root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
          new_place.root.kind = mir::PlaceRoot::Kind::kObjectLocal;
          new_place.root.object_index = parent_oi;
        }
        return (*input.mir_arena).AddPlace(std::move(new_place));
      };
      auto remap_operand = [&](const mir::Operand& op,
                               const mir::Arena& src) -> mir::Operand {
        if (op.kind == mir::Operand::Kind::kUse)
          return mir::Operand::Use(
              remap_place(std::get<mir::PlaceId>(op.payload), src));
        return op;
      };
      auto remap_rhs = [&](const mir::RightHandSide& rhs,
                           const mir::Arena& src) -> mir::RightHandSide {
        return std::visit(
            common::Overloaded{
                [&](const mir::Operand& op) -> mir::RightHandSide {
                  return remap_operand(op, src);
                },
                [&](const mir::Rvalue& rv) -> mir::RightHandSide {
                  mir::Rvalue new_rv;
                  new_rv.info = rv.info;
                  new_rv.operands.reserve(rv.operands.size());
                  for (const auto& op : rv.operands)
                    new_rv.operands.push_back(remap_operand(op, src));
                  if (auto* gu =
                          std::get_if<mir::GuardedUseRvalueInfo>(&new_rv.info))
                    gu->place = remap_place(gu->place, src);
                  if (auto* bc =
                          std::get_if<mir::BuiltinCallRvalueInfo>(&new_rv.info))
                    if (bc->receiver)
                      bc->receiver = remap_place(*bc->receiver, src);
                  return new_rv;
                },
            },
            rhs);
      };

      mir::PlaceId child_place =
          (*input.mir_arena)
              .AddPlace(
                  mir::Place{
                      .root =
                          mir::PlaceRoot{
                              .kind = mir::PlaceRoot::Kind::kObjectLocal,
                              .id =
                                  static_cast<int>(expr.child_local_slot.value),
                              .type = expr.result_type,
                              .object_index = child_oi},
                      .projections = {}});

      struct TriggerSlot {
        uint32_t object_index;
        uint32_t local_slot;
        auto operator<=>(const TriggerSlot&) const = default;
      };
      std::vector<TriggerSlot> trigger_slots;
      auto collect_trigger = [&](const mir::Operand& op,
                                 const mir::Arena& src) {
        if (op.kind != mir::Operand::Kind::kUse) return;
        const auto& place = src[std::get<mir::PlaceId>(op.payload)];
        if (place.root.kind == mir::PlaceRoot::Kind::kModuleSlot)
          trigger_slots.push_back(
              {.object_index = parent_oi,
               .local_slot = static_cast<uint32_t>(place.root.id)});
      };
      auto collect_rhs_triggers = [&](const mir::RightHandSide& rhs) {
        std::visit(
            common::Overloaded{
                [&](const mir::Operand& op) {
                  collect_trigger(op, *expr.parent_arena);
                },
                [&](const mir::Rvalue& rv) {
                  for (const auto& op : rv.operands)
                    collect_trigger(op, *expr.parent_arena);
                },
            },
            rhs);
      };
      for (const auto& block : func.blocks) {
        for (const auto& stmt : block.statements)
          std::visit(
              common::Overloaded{
                  [&](const mir::Assign& a) { collect_rhs_triggers(a.rhs); },
                  [&](const mir::DefineTemp& dt) {
                    collect_rhs_triggers(dt.rhs);
                  },
                  [](const auto&) {},
              },
              stmt.data);
        std::visit(
            common::Overloaded{
                [&](const mir::Branch& b) {
                  collect_trigger(b.condition, *expr.parent_arena);
                },
                [&](const mir::Jump& j) {
                  for (const auto& a : j.args)
                    collect_trigger(a, *expr.parent_arena);
                },
                [](const auto&) {},
            },
            block.terminator.data);
      }

      std::vector<mir::BasicBlock> process_blocks;
      mir::BasicBlockId entry_id = func.entry;
      for (const auto& block : func.blocks) {
        mir::BasicBlock new_block;
        new_block.params = block.params;
        for (const auto& stmt : block.statements) {
          mir::Statement new_stmt;
          new_stmt.origin = stmt.origin;
          new_stmt.data = std::visit(
              common::Overloaded{
                  [&](const mir::Assign& a) -> mir::StatementData {
                    return mir::Assign{
                        .dest = remap_place(
                            mir::RequireLocalDest(a.dest, "ExprConnections"),
                            *expr.parent_arena),
                        .rhs = remap_rhs(a.rhs, *expr.parent_arena)};
                  },
                  [&](const mir::DefineTemp& dt) -> mir::StatementData {
                    return mir::DefineTemp{
                        .temp_id = dt.temp_id,
                        .type = dt.type,
                        .rhs = remap_rhs(dt.rhs, *expr.parent_arena)};
                  },
                  [&](const auto& other) -> mir::StatementData {
                    return other;
                  },
              },
              stmt.data);
          new_block.statements.push_back(std::move(new_stmt));
        }
        if (const auto* ret =
                std::get_if<mir::Return>(&block.terminator.data)) {
          if (ret->value) {
            new_block.statements.push_back(
                mir::Statement{
                    .data = mir::Assign{
                        .dest = child_place,
                        .rhs =
                            remap_operand(*ret->value, *expr.parent_arena)}});
          }
          std::vector<mir::WaitTrigger> triggers;
          std::ranges::sort(trigger_slots);
          auto last = std::ranges::unique(trigger_slots);
          trigger_slots.erase(last.begin(), last.end());
          for (const auto& ts : trigger_slots)
            triggers.push_back(
                mir::WaitTrigger{
                    .signal =
                        mir::SignalRef{
                            .scope = mir::SignalRef::Scope::kObjectLocal,
                            .id = ts.local_slot,
                            .object_index = ts.object_index},
                    .edge = common::EdgeKind::kAnyChange,
                    .observed_place = std::nullopt,
                    .late_bound = std::nullopt,
                    .unresolved_external_ref = std::nullopt});
          new_block.terminator.data =
              mir::Wait{.triggers = std::move(triggers), .resume = entry_id};
        } else {
          new_block.terminator.origin = block.terminator.origin;
          new_block.terminator.data = std::visit(
              common::Overloaded{
                  [&](const mir::Jump& j) -> mir::TerminatorData {
                    std::vector<mir::Operand> args;
                    for (const auto& a : j.args)
                      args.push_back(remap_operand(a, *expr.parent_arena));
                    return mir::Jump{
                        .target = j.target, .args = std::move(args)};
                  },
                  [&](const mir::Branch& b) -> mir::TerminatorData {
                    auto cond = remap_operand(b.condition, *expr.parent_arena);
                    std::vector<mir::Operand> then_args;
                    for (const auto& a : b.then_args)
                      then_args.push_back(remap_operand(a, *expr.parent_arena));
                    std::vector<mir::Operand> else_args;
                    for (const auto& a : b.else_args)
                      else_args.push_back(remap_operand(a, *expr.parent_arena));
                    return mir::Branch{
                        .condition = cond,
                        .then_target = b.then_target,
                        .then_args = std::move(then_args),
                        .else_target = b.else_target,
                        .else_args = std::move(else_args)};
                  },
                  [&](const mir::Switch& s) -> mir::TerminatorData {
                    return mir::Switch{
                        .selector =
                            remap_operand(s.selector, *expr.parent_arena),
                        .targets = s.targets};
                  },
                  [&](const auto& other) -> mir::TerminatorData {
                    return other;
                  },
              },
              block.terminator.data);
        }
        process_blocks.push_back(std::move(new_block));
      }
      auto pid = (*input.mir_arena)
                     .AddProcess(
                         mir::Process{
                             .kind = mir::ProcessKind::kLooping,
                             .entry = entry_id,
                             .blocks = std::move(process_blocks),
                             .temp_metadata = func.temp_metadata,
                             .decision_sites = {}});
      result.non_kernelized_processes.push_back(pid);
    }
  }

  return result;
}

}  // namespace lyra::lowering::mir_to_llvm

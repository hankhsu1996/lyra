#include "lyra/mir/verify.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string_view>
#include <unordered_set>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

namespace {

// Verify Return invariants in a list of basic blocks.
// is_void: true if the routine is void (no return value expected)
// routine_kind: "function" or "process" for error messages
void VerifyReturnInvariants(
    const std::vector<BasicBlock>& blocks, bool is_void,
    std::string_view routine_kind) {
  for (size_t i = 0; i < blocks.size(); ++i) {
    const auto& block = blocks[i];
    if (const auto* ret = std::get_if<Return>(&block.terminator.data)) {
      if (is_void) {
        if (ret->value.has_value()) {
          throw common::InternalError(
              "MIR verify", std::format(
                                "block {}: {} is void but Return has value", i,
                                routine_kind));
        }
      } else {
        if (!ret->value.has_value()) {
          throw common::InternalError(
              "MIR verify",
              std::format(
                  "block {}: {} is non-void but Return has no value", i,
                  routine_kind));
        }
      }
    }
  }
}

// Verify param_local_slots invariants for a function.
void VerifyParamLocalSlots(const Function& func) {
  if (func.param_local_slots.size() != func.signature.params.size()) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "param_local_slots.size() ({}) != signature.params.size() ({})",
            func.param_local_slots.size(), func.signature.params.size()));
  }

  std::unordered_set<uint32_t> seen_slots;
  for (size_t i = 0; i < func.param_local_slots.size(); ++i) {
    uint32_t slot = func.param_local_slots[i];

    if (slot >= func.local_types.size()) {
      throw common::InternalError(
          "MIR verify",
          std::format(
              "param_local_slots[{}] = {} is out of range (local_types.size() "
              "= {})",
              i, slot, func.local_types.size()));
    }

    if (seen_slots.contains(slot)) {
      throw common::InternalError(
          "MIR verify",
          std::format(
              "param_local_slots[{}] = {} is a duplicate (aliasing not "
              "allowed)",
              i, slot));
    }
    seen_slots.insert(slot);
  }
}

// Get the TypeId for an operand (for type checking edge args)
auto GetOperandType(
    const Operand& op, const std::vector<BasicBlock>& blocks,
    const Arena& arena, const TypeArena& types) -> TypeId {
  return std::visit(
      common::Overloaded{
          [&](const Constant& c) -> TypeId { return c.type; },
          [&](PlaceId place_id) -> TypeId {
            const auto& place = arena[place_id];
            return TypeOfPlace(types, place);
          },
          [&](TempId temp_id) -> TypeId {
            // Find the block param that defines this temp
            for (const auto& block : blocks) {
              for (const auto& param : block.params) {
                if (param.temp_id == temp_id.value) {
                  return param.type;
                }
              }
            }
            // Temp not found - this will be caught by VerifyUseTempDefined
            return TypeId{0};
          },
      },
      op.payload);
}

// Verify edge arg operand kind is allowed.
// Allowed kinds for edge args (Option B model):
// - kConst: constant values are always allowed
// - kUse (PlaceId): loads from places are allowed (emitted in predecessor)
// - kUseTemp: references to block param temps are allowed
// - kPoison: not allowed (should never reach lowering)
void VerifyEdgeArgKind(
    const Operand& arg, size_t arg_idx, size_t src_block_idx,
    size_t target_block_idx, std::string_view edge_desc) {
  if (arg.kind == Operand::Kind::kPoison) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "block {}: {} to block {}: arg[{}] is Poison (invalid operand)",
            src_block_idx, edge_desc, target_block_idx, arg_idx));
  }
  // kConst, kUse, kUseTemp are all allowed.
  // Note: kUse (PlaceId) may emit loads in predecessor block - this is legal.
}

// Verify edge args match target block params (count, types, and kinds)
void VerifyEdgeArgs(
    const std::vector<Operand>& args, size_t target_block_idx,
    const std::vector<BasicBlock>& blocks, const Arena& arena,
    const TypeArena& types, std::string_view edge_desc, size_t src_block_idx) {
  const auto& target_block = blocks[target_block_idx];
  const auto& params = target_block.params;

  if (args.size() != params.size()) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "block {}: {} to block {}: arg count ({}) != param count ({})",
            src_block_idx, edge_desc, target_block_idx, args.size(),
            params.size()));
  }

  for (size_t i = 0; i < args.size(); ++i) {
    // Verify operand kind is allowed
    VerifyEdgeArgKind(args[i], i, src_block_idx, target_block_idx, edge_desc);

    // Verify type matches
    TypeId arg_type = GetOperandType(args[i], blocks, arena, types);
    TypeId param_type = params[i].type;
    if (arg_type != param_type) {
      throw common::InternalError(
          "MIR verify",
          std::format(
              "block {}: {} to block {}: arg[{}] type ({}) != param type ({})",
              src_block_idx, edge_desc, target_block_idx, i, arg_type.value,
              param_type.value));
    }
  }
}

// Verify BlockParam temp_ids are unique across all blocks
void VerifyUniqueBlockParamTempIds(
    const std::vector<BasicBlock>& blocks, std::string_view routine_kind) {
  std::unordered_set<int> seen_temp_ids;
  for (size_t i = 0; i < blocks.size(); ++i) {
    for (const auto& param : blocks[i].params) {
      if (seen_temp_ids.contains(param.temp_id)) {
        throw common::InternalError(
            "MIR verify", std::format(
                              "{} block {}: duplicate block param temp_id {}",
                              routine_kind, i, param.temp_id));
      }
      seen_temp_ids.insert(param.temp_id);
    }
  }
}

// Verify all UseTemp operands reference defined temp_ids
void VerifyUseTempDefined(
    const Operand& op, const std::unordered_set<int>& defined_temps,
    size_t block_idx, std::string_view context) {
  if (const auto* temp_id = std::get_if<TempId>(&op.payload)) {
    if (!defined_temps.contains(temp_id->value)) {
      throw common::InternalError(
          "MIR verify",
          std::format(
              "block {}: {}: UseTemp({}) references undefined temp", block_idx,
              context, temp_id->value));
    }
  }
}

// Verify block params and edge args for a routine's blocks
void VerifyBlockParamsAndEdgeArgs(
    const std::vector<BasicBlock>& blocks, const Arena& arena,
    const TypeArena& types, std::string_view routine_kind) {
  // Collect all defined temp_ids from block params
  std::unordered_set<int> defined_temps;
  for (const auto& block : blocks) {
    for (const auto& param : block.params) {
      defined_temps.insert(param.temp_id);
    }
  }

  // Verify unique temp_ids
  VerifyUniqueBlockParamTempIds(blocks, routine_kind);

  // Verify each terminator's edge args
  for (size_t i = 0; i < blocks.size(); ++i) {
    const auto& block = blocks[i];

    std::visit(
        common::Overloaded{
            [&](const Jump& jump) {
              VerifyEdgeArgs(
                  jump.args, jump.target.value, blocks, arena, types, "Jump",
                  i);
              for (size_t j = 0; j < jump.args.size(); ++j) {
                VerifyUseTempDefined(
                    jump.args[j], defined_temps, i,
                    std::format("Jump.args[{}]", j));
              }
            },
            [&](const Branch& branch) {
              VerifyEdgeArgs(
                  branch.then_args, branch.then_target.value, blocks, arena,
                  types, "Branch.then", i);
              VerifyEdgeArgs(
                  branch.else_args, branch.else_target.value, blocks, arena,
                  types, "Branch.else", i);
              for (size_t j = 0; j < branch.then_args.size(); ++j) {
                VerifyUseTempDefined(
                    branch.then_args[j], defined_temps, i,
                    std::format("Branch.then_args[{}]", j));
              }
              for (size_t j = 0; j < branch.else_args.size(); ++j) {
                VerifyUseTempDefined(
                    branch.else_args[j], defined_temps, i,
                    std::format("Branch.else_args[{}]", j));
              }
            },
            [](const auto&) {
              // Other terminators don't have edge args
            },
        },
        block.terminator.data);
  }
}

}  // namespace

void VerifyFunction(
    const Function& func, const Arena& arena, const TypeArena& types) {
  VerifyParamLocalSlots(func);

  const Type& ret_type = types[func.signature.return_type];
  bool is_void = ret_type.Kind() == TypeKind::kVoid;
  VerifyReturnInvariants(func.blocks, is_void, "function");

  // Verify block params and edge args
  VerifyBlockParamsAndEdgeArgs(func.blocks, arena, types, "function");
}

void VerifyProcess(
    const Process& proc, const Arena& arena, const TypeArena& types) {
  VerifyReturnInvariants(proc.blocks, true, "process");

  // Verify block params and edge args
  VerifyBlockParamsAndEdgeArgs(proc.blocks, arena, types, "process");
}

}  // namespace lyra::mir

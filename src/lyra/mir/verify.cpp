#include "lyra/mir/verify.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/assoc_op.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/compiled_specialization.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

auto RequireExternalRefRecipe(
    const CompiledModuleBody& body, ExternalRefId id, const char* where)
    -> const ExternalAccessRecipe& {
  const auto* recipe = body.GetExternalAccessRecipe(id);
  if (recipe == nullptr || !(recipe->ref_id == id)) {
    throw common::InternalError(
        where, std::format("invalid ExternalRefId {}", id.value));
  }
  if (recipe->type.value == 0) {
    throw common::InternalError(
        where, std::format("ExternalRefId {} has invalid type", id.value));
  }
  return *recipe;
}

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

// Tracks locations where temps are defined (for error messages)
struct TempDefinition {
  int temp_id;
  TypeId type;
  size_t block_idx;
  bool is_block_param;       // true for BlockParam, false for DefineTemp
  size_t param_or_stmt_idx;  // param index if block_param, stmt index otherwise
};

auto FormatTempDefLocation(const TempDefinition& def) -> std::string {
  if (def.is_block_param) {
    return std::format(
        "block {} param {}", def.block_idx, def.param_or_stmt_idx);
  }
  return std::format("block {} stmt {}", def.block_idx, def.param_or_stmt_idx);
}

// Verify a ValueTemp definition (BlockParam or DefineTemp) against
// temp_metadata. Checks: bounds, kind==kValue, type match.
void VerifyValueTempMetadata(
    int temp_id, TypeId type, const std::vector<TempMetadata>& temp_metadata,
    std::string_view location, std::string_view routine_kind) {
  // Check bounds
  if (temp_id < 0 || static_cast<size_t>(temp_id) >= temp_metadata.size()) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: {}: temp_id {} out of temp_metadata range (size={})",
            routine_kind, location, temp_id, temp_metadata.size()));
  }

  const auto& meta = temp_metadata[static_cast<size_t>(temp_id)];

  // Check kind is kValue
  if (meta.kind != TempKind::kValue) {
    throw common::InternalError(
        "MIR verify", std::format(
                          "{}: {}: temp_id {} has kind kPlace in temp_metadata "
                          "(expected kValue for BlockParam/DefineTemp)",
                          routine_kind, location, temp_id));
  }

  // Check type match
  if (meta.type != type) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: {}: temp_id {} type mismatch: temp_metadata has {}, "
            "definition has {}",
            routine_kind, location, temp_id, meta.type.value, type.value));
  }
}

// Verify a PlaceRoot::kTemp against temp_metadata.
// Checks: bounds, kind==kPlace, type match.
void VerifyPlaceTempMetadata(
    const PlaceRoot& root, const std::vector<TempMetadata>& temp_metadata,
    std::string_view location, std::string_view routine_kind) {
  if (root.kind != PlaceRoot::Kind::kTemp) {
    return;
  }

  int temp_id = root.id;

  // Check bounds
  if (temp_id < 0 || static_cast<size_t>(temp_id) >= temp_metadata.size()) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: {}: PlaceRoot::kTemp id {} out of temp_metadata range "
            "(size={})",
            routine_kind, location, temp_id, temp_metadata.size()));
  }

  const auto& meta = temp_metadata[static_cast<size_t>(temp_id)];

  // Check kind is kPlace
  if (meta.kind != TempKind::kPlace) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: {}: PlaceRoot::kTemp id {} has kind kValue in temp_metadata "
            "(expected kPlace)",
            routine_kind, location, temp_id));
  }

  // Check type match
  if (meta.type != root.type) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: {}: PlaceRoot::kTemp id {} type mismatch: temp_metadata has "
            "{}, PlaceRoot has {}",
            routine_kind, location, temp_id, meta.type.value, root.type.value));
  }
}

// Verify a UseTemp operand against temp_metadata.
// Checks: bounds, kind==kValue.
void VerifyUseTempMetadata(
    int temp_id, const std::vector<TempMetadata>& temp_metadata,
    std::string_view location, std::string_view routine_kind) {
  // Check bounds
  if (temp_id < 0 || static_cast<size_t>(temp_id) >= temp_metadata.size()) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: {}: UseTemp({}) out of temp_metadata range (size={})",
            routine_kind, location, temp_id, temp_metadata.size()));
  }

  const auto& meta = temp_metadata[static_cast<size_t>(temp_id)];

  // Check kind is kValue
  if (meta.kind != TempKind::kValue) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: {}: UseTemp({}) references PlaceTemp (kind=kPlace); "
            "UseTemp may only reference ValueTemps",
            routine_kind, location, temp_id));
  }
}

// Collect all ValueTemp definitions from BlockParams and DefineTemp statements.
// Detects duplicates immediately, cross-checks against temp_metadata, and
// throws error on any inconsistency.
void CollectValueTempDefinitions(
    const std::vector<BasicBlock>& blocks,
    const std::vector<TempMetadata>& temp_metadata,
    std::string_view routine_kind,
    std::unordered_map<int, TempDefinition>& defined_temps) {
  for (size_t block_idx = 0; block_idx < blocks.size(); ++block_idx) {
    const auto& block = blocks[block_idx];

    // BlockParams define ValueTemps
    for (size_t param_idx = 0; param_idx < block.params.size(); ++param_idx) {
      const auto& param = block.params[param_idx];
      TempDefinition new_def{
          .temp_id = param.temp_id,
          .type = param.type,
          .block_idx = block_idx,
          .is_block_param = true,
          .param_or_stmt_idx = param_idx,
      };

      // Cross-check against temp_metadata
      std::string location =
          std::format("block {} param {}", block_idx, param_idx);
      VerifyValueTempMetadata(
          param.temp_id, param.type, temp_metadata, location, routine_kind);

      auto [it, inserted] = defined_temps.try_emplace(param.temp_id, new_def);
      if (!inserted) {
        throw common::InternalError(
            "MIR verify",
            std::format(
                "{}: ValueTemp {} defined at {} was already defined at {}",
                routine_kind, param.temp_id, FormatTempDefLocation(new_def),
                FormatTempDefLocation(it->second)));
      }
    }

    // DefineTemp statements define ValueTemps
    for (size_t stmt_idx = 0; stmt_idx < block.statements.size(); ++stmt_idx) {
      if (const auto* dt =
              std::get_if<DefineTemp>(&block.statements[stmt_idx].data)) {
        TempDefinition new_def{
            .temp_id = dt->temp_id,
            .type = dt->type,
            .block_idx = block_idx,
            .is_block_param = false,
            .param_or_stmt_idx = stmt_idx,
        };

        // Cross-check against temp_metadata
        std::string location =
            std::format("block {} stmt {}", block_idx, stmt_idx);
        VerifyValueTempMetadata(
            dt->temp_id, dt->type, temp_metadata, location, routine_kind);

        auto [it, inserted] = defined_temps.try_emplace(dt->temp_id, new_def);
        if (!inserted) {
          throw common::InternalError(
              "MIR verify",
              std::format(
                  "{}: ValueTemp {} defined at {} was already defined at {}",
                  routine_kind, dt->temp_id, FormatTempDefLocation(new_def),
                  FormatTempDefLocation(it->second)));
        }
      }
    }
  }
}

// Check if a type can be used as a branch/dispatch condition.
// Valid: packed types (Integral, PackedArray, PackedStruct, Enum) and real
// types. These types have well-defined "truthiness" semantics (non-zero =
// true).
auto IsConditionType(const Type& type) -> bool {
  switch (type.Kind()) {
    case TypeKind::kIntegral:
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum:
    case TypeKind::kReal:
    case TypeKind::kShortReal:
      return true;
    default:
      return false;
  }
}

// Check if a type can be used as a switch selector.
// Valid: packed integral types only (not real - can't switch on floats).
auto IsSelectorType(const Type& type) -> bool {
  switch (type.Kind()) {
    case TypeKind::kIntegral:
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum:
      return true;
    default:
      return false;
  }
}

// Get the TypeId for an operand (for type checking edge args).
// Phase-aware: in kPreBackend, ExternalRefId resolves through the body's
// external_refs table. In kBackendReady, ExternalRefId is illegal.
auto GetOperandType(
    const Operand& op, [[maybe_unused]] const std::vector<BasicBlock>& blocks,
    const Arena& arena, const VerifyContext& cx,
    const std::unordered_map<int, TempDefinition>& defined_temps) -> TypeId {
  return std::visit(
      common::Overloaded{
          [&](const Constant& c) -> TypeId { return c.type; },
          [&](PlaceId place_id) -> TypeId {
            const auto& place = arena[place_id];
            return TypeOfPlace(*cx.types, place);
          },
          [&](TempId temp_id) -> TypeId {
            // Look up in defined_temps map
            auto it = defined_temps.find(temp_id.value);
            if (it != defined_temps.end()) {
              return it->second.type;
            }
            // Temp not found - this will be caught by VerifyUseTempDefined
            return TypeId{0};
          },
          [&](ExternalRefId id) -> TypeId {
            if (cx.phase == VerifyContext::Phase::kPreBackend) {
              const auto& recipe =
                  RequireExternalRefRecipe(*cx.body, id, "GetOperandType");
              return recipe.type;
            }
            throw common::InternalError(
                "MIR verify", "ExternalRefId in backend-ready MIR");
          },
      },
      op.payload);
}

// Verify operand is not Poison.
// Poison operands are invalid and should never reach lowering.
void VerifyNotPoison(
    const Operand& op, size_t block_idx, std::string_view context,
    std::string_view routine_kind) {
  if (op.kind == Operand::Kind::kPoison) {
    throw common::InternalError(
        "MIR verify", std::format(
                          "{}: block {}: {}: Poison operand is invalid",
                          routine_kind, block_idx, context));
  }
}

// Verify operand type is valid for a branch/dispatch condition.
void VerifyConditionType(
    const Operand& op, const std::vector<BasicBlock>& blocks,
    const Arena& arena, const VerifyContext& cx,
    const std::unordered_map<int, TempDefinition>& defined_temps,
    size_t block_idx, std::string_view context, std::string_view routine_kind) {
  // Skip Poison operands - they'll be caught by VerifyNotPoison
  if (op.kind == Operand::Kind::kPoison) return;

  TypeId type_id = GetOperandType(op, blocks, arena, cx, defined_temps);
  if (!type_id) return;  // Invalid type - will be caught elsewhere

  const Type& type = (*cx.types)[type_id];
  if (!IsConditionType(type)) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: block {}: {}: invalid condition type {} (expected packed or "
            "real type)",
            routine_kind, block_idx, context, ToString(type)));
  }
}

// Verify operand type matches an expected TypeId exactly.
void VerifyOperandHasType(
    const Operand& op, TypeId expected_type,
    const std::vector<BasicBlock>& blocks, const Arena& arena,
    const VerifyContext& cx,
    const std::unordered_map<int, TempDefinition>& defined_temps,
    size_t block_idx, std::string_view context, std::string_view routine_kind) {
  if (op.kind == Operand::Kind::kPoison) return;

  TypeId actual_type = GetOperandType(op, blocks, arena, cx, defined_temps);
  if (!actual_type) return;

  if (actual_type != expected_type) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: block {}: {}: operand type mismatch (expected type id {}, "
            "got {})",
            routine_kind, block_idx, context, expected_type.value,
            actual_type.value));
  }
}

// Verify operand type is valid for a switch selector.
void VerifySelectorType(
    const Operand& op, const std::vector<BasicBlock>& blocks,
    const Arena& arena, const VerifyContext& cx,
    const std::unordered_map<int, TempDefinition>& defined_temps,
    size_t block_idx, std::string_view context, std::string_view routine_kind) {
  // Skip Poison operands - they'll be caught by VerifyNotPoison
  if (op.kind == Operand::Kind::kPoison) return;

  TypeId type_id = GetOperandType(op, blocks, arena, cx, defined_temps);
  if (!type_id) return;  // Invalid type - will be caught elsewhere

  const Type& type = (*cx.types)[type_id];
  if (!IsSelectorType(type)) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: block {}: {}: invalid selector type {} (expected packed "
            "integral type)",
            routine_kind, block_idx, context, ToString(type)));
  }
}

// Verify edge arg operand kind is allowed.
// Allowed kinds for edge args (Option B model):
// - kConst: constant values are always allowed
// - kUse (PlaceId): loads from places are allowed (emitted in predecessor)
// - kUseTemp: references to block param temps are allowed
// - kPoison: not allowed (should never reach lowering)
void VerifyEdgeArgKind(
    const Operand& arg, size_t arg_idx, size_t src_block_idx,
    size_t target_block_idx, std::string_view edge_desc,
    std::string_view routine_kind) {
  if (arg.kind == Operand::Kind::kPoison) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: block {}: {} to block {}: arg[{}] is Poison (invalid "
            "operand)",
            routine_kind, src_block_idx, edge_desc, target_block_idx, arg_idx));
  }
  // kConst, kUse, kUseTemp are all allowed.
  // Note: kUse (PlaceId) may emit loads in predecessor block - this is legal.
}

// Verify edge args match target block params (count, types, and kinds)
void VerifyEdgeArgs(
    const std::vector<Operand>& args, size_t target_block_idx,
    const std::vector<BasicBlock>& blocks, const Arena& arena,
    const VerifyContext& cx,
    const std::unordered_map<int, TempDefinition>& defined_temps,
    std::string_view edge_desc, size_t src_block_idx,
    std::string_view routine_kind) {
  const auto& target_block = blocks[target_block_idx];
  const auto& params = target_block.params;

  if (args.size() != params.size()) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "{}: block {}: {} to block {}: arg count ({}) != param count ({})",
            routine_kind, src_block_idx, edge_desc, target_block_idx,
            args.size(), params.size()));
  }

  for (size_t i = 0; i < args.size(); ++i) {
    // Verify operand kind is allowed
    VerifyEdgeArgKind(
        args[i], i, src_block_idx, target_block_idx, edge_desc, routine_kind);

    // Verify type matches
    TypeId arg_type = GetOperandType(args[i], blocks, arena, cx, defined_temps);
    TypeId param_type = params[i].type;
    if (arg_type != param_type) {
      throw common::InternalError(
          "MIR verify",
          std::format(
              "{}: block {}: {} to block {}: arg[{}] type ({}) != param type "
              "({})",
              routine_kind, src_block_idx, edge_desc, target_block_idx, i,
              arg_type.value, param_type.value));
    }
  }
}

// Verify all UseTemp operands reference defined temp_ids and have correct
// kind in temp_metadata.
void VerifyUseTempDefined(
    const Operand& op,
    const std::unordered_map<int, TempDefinition>& defined_temps,
    const std::vector<TempMetadata>& temp_metadata, size_t block_idx,
    std::string_view context, std::string_view routine_kind) {
  if (const auto* temp_id = std::get_if<TempId>(&op.payload)) {
    // Check it's defined somewhere
    if (!defined_temps.contains(temp_id->value)) {
      throw common::InternalError(
          "MIR verify",
          std::format(
              "{}: block {}: {}: UseTemp({}) references undefined temp",
              routine_kind, block_idx, context, temp_id->value));
    }

    // Cross-check against temp_metadata
    std::string location = std::format("block {}: {}", block_idx, context);
    VerifyUseTempMetadata(
        temp_id->value, temp_metadata, location, routine_kind);
  }
}

// Verify a Place's root against temp_metadata (for PlaceRoot::kTemp).
void VerifyPlaceRoot(
    const Place& place, const std::vector<TempMetadata>& temp_metadata,
    std::string_view location, std::string_view routine_kind) {
  VerifyPlaceTempMetadata(place.root, temp_metadata, location, routine_kind);
}

// Verify all operands in a statement for intra-block def-before-use.
// available_temps: set of temp_ids available at this point in the block
// (block params + DefineTemps seen so far).
void VerifyOperandDefBeforeUse(
    const Operand& op, const std::unordered_set<int>& available_temps,
    size_t block_idx, size_t stmt_idx, std::string_view context,
    std::string_view routine_kind) {
  if (const auto* temp_id = std::get_if<TempId>(&op.payload)) {
    if (!available_temps.contains(temp_id->value)) {
      std::string avail_str;
      for (int t : available_temps) {
        if (!avail_str.empty()) avail_str += ",";
        avail_str += std::to_string(t);
      }
      throw common::InternalError(
          "MIR verify",
          std::format(
              "{}: block {} stmt {}: {}: UseTemp({}) used before definition "
              "in same block (available: [{}])",
              routine_kind, block_idx, stmt_idx, context, temp_id->value,
              avail_str));
    }
  }
}

// Visit operands in RightHandSide for def-before-use checking.
void VerifyRhsDefBeforeUse(
    const RightHandSide& rhs, const std::unordered_set<int>& available_temps,
    size_t block_idx, size_t stmt_idx, std::string_view routine_kind) {
  std::visit(
      common::Overloaded{
          [&](const Operand& op) {
            VerifyOperandDefBeforeUse(
                op, available_temps, block_idx, stmt_idx, "rhs", routine_kind);
          },
          [&](const Rvalue& rv) {
            for (size_t i = 0; i < rv.operands.size(); ++i) {
              std::string ctx = std::format("rhs.operands[{}]", i);
              VerifyOperandDefBeforeUse(
                  rv.operands[i], available_temps, block_idx, stmt_idx, ctx,
                  routine_kind);
            }
          },
      },
      rhs);
}

// Verify PlaceRoot::kTemp in all places referenced by a statement.
// Also enforces ownership: all PlaceIds and FunctionIds must resolve
// within the owning arena. For WriteTarget fields, verifies the local
// PlaceId if present; additionally validates ExternalRefId in pre-backend
// mode.
void VerifyStatementOwnership(
    const Statement& stmt, const Arena& arena,
    const std::vector<TempMetadata>& temp_metadata, const VerifyContext& cx,
    size_t block_idx, size_t stmt_idx, std::string_view routine_kind) {
  auto verify_place = [&](PlaceId place_id, std::string_view ctx) {
    if (place_id.value >= arena.PlaceCount()) {
      throw common::InternalError(
          "MIR verify",
          std::format(
              "{}: block {} stmt {}: {}: PlaceId {} out of arena range "
              "(arena has {} places)",
              routine_kind, block_idx, stmt_idx, ctx, place_id.value,
              arena.PlaceCount()));
    }
    const auto& place = arena[place_id];
    std::string location =
        std::format("block {} stmt {}: {}", block_idx, stmt_idx, ctx);
    VerifyPlaceRoot(place, temp_metadata, location, routine_kind);
  };

  auto verify_operand_place = [&](const Operand& op, std::string_view ctx) {
    if (op.kind == Operand::Kind::kUse) {
      verify_place(std::get<PlaceId>(op.payload), ctx);
    }
  };

  auto verify_rhs_places = [&](const RightHandSide& rhs, std::string_view ctx) {
    std::visit(
        common::Overloaded{
            [&](const Operand& op) { verify_operand_place(op, ctx); },
            [&](const Rvalue& rv) {
              for (size_t i = 0; i < rv.operands.size(); ++i) {
                verify_operand_place(
                    rv.operands[i], std::format("{}.operands[{}]", ctx, i));
              }
            },
        },
        rhs);
  };

  auto verify_write_target = [&](const WriteTarget& target,
                                 std::string_view ctx) {
    if (const auto* pid = std::get_if<PlaceId>(&target)) {
      verify_place(*pid, ctx);
    } else if (cx.phase == VerifyContext::Phase::kPreBackend) {
      // Validate ExternalRefId resolves in pre-backend mode
      auto ext_id = std::get<ExternalRefId>(target);
      RequireExternalRefRecipe(*cx.body, ext_id, "VerifyStatementOwnership");
    }
  };

  std::visit(
      common::Overloaded{
          [&](const Assign& assign) {
            verify_write_target(assign.dest, "Assign.dest");
            verify_rhs_places(assign.rhs, "Assign.rhs");
          },
          [&](const GuardedAssign& ga) {
            verify_write_target(ga.dest, "GuardedAssign.dest");
            verify_rhs_places(ga.rhs, "GuardedAssign.rhs");
            verify_operand_place(ga.guard, "GuardedAssign.guard");
          },
          [&](const Effect&) {
            // Effects may contain operands but no PlaceRoot::kTemp
          },
          [&](const DeferredAssign& da) {
            verify_write_target(da.dest, "DeferredAssign.dest");
            verify_rhs_places(da.rhs, "DeferredAssign.rhs");
          },
          [&](const Call& call) {
            // Verify callee ownership: FunctionId must be arena-local.
            if (const auto* func_id = std::get_if<FunctionId>(&call.callee)) {
              if (func_id->value >= arena.FunctionCount()) {
                throw common::InternalError(
                    "MIR verify",
                    std::format(
                        "{}: block {} stmt {}: Call.callee FunctionId {} "
                        "out of arena range (arena has {} functions)",
                        routine_kind, block_idx, stmt_idx, func_id->value,
                        arena.FunctionCount()));
              }
            }
            for (size_t i = 0; i < call.in_args.size(); ++i) {
              verify_operand_place(
                  call.in_args[i], std::format("Call.in_args[{}]", i));
            }
            if (call.ret) {
              verify_place(call.ret->tmp, "Call.ret.tmp");
              if (call.ret->dest) {
                verify_place(*call.ret->dest, "Call.ret.dest");
              }
            }
            for (size_t i = 0; i < call.writebacks.size(); ++i) {
              const auto& wb = call.writebacks[i];
              if (wb.kind == WritebackKind::kStaged) {
                // kStaged: tmp must be present
                if (!wb.tmp.has_value()) {
                  throw common::InternalError(
                      "VerifyMir", std::format(
                                       "Call.writebacks[{}]: kStaged requires "
                                       "tmp to be present",
                                       i));
                }
                verify_place(
                    *wb.tmp, std::format("Call.writebacks[{}].tmp", i));
              } else {
                // kDirectToDest: tmp must be absent
                if (wb.tmp.has_value()) {
                  throw common::InternalError(
                      "VerifyMir", std::format(
                                       "Call.writebacks[{}]: kDirectToDest "
                                       "requires tmp to be nullopt",
                                       i));
                }
              }
              verify_place(wb.dest, std::format("Call.writebacks[{}].dest", i));
            }
          },
          [&](const DpiCall& dpi_call) {
            for (size_t i = 0; i < dpi_call.args.size(); ++i) {
              const auto& binding = dpi_call.args[i];
              if (binding.input_value) {
                verify_operand_place(
                    *binding.input_value,
                    std::format("DpiCall.args[{}].input_value", i));
              }
              if (binding.writeback_dest) {
                verify_place(
                    *binding.writeback_dest,
                    std::format("DpiCall.args[{}].writeback_dest", i));
              }
            }
            if (dpi_call.ret) {
              verify_place(dpi_call.ret->tmp, "DpiCall.ret.tmp");
              if (dpi_call.ret->dest) {
                verify_place(*dpi_call.ret->dest, "DpiCall.ret.dest");
              }
            }
          },
          [&](const BuiltinCall& bcall) {
            if (bcall.dest) {
              verify_place(*bcall.dest, "BuiltinCall.dest");
            }
            verify_place(bcall.receiver, "BuiltinCall.receiver");
            for (size_t i = 0; i < bcall.args.size(); ++i) {
              verify_operand_place(
                  bcall.args[i], std::format("BuiltinCall.args[{}]", i));
            }
          },
          [&](const DefineTemp& dt) {
            // DefineTemp defines a ValueTemp, verified by
            // CollectValueTempDefinitions
            verify_rhs_places(dt.rhs, "DefineTemp.rhs");
          },
          [&](const AssocOp& aop) {
            verify_place(aop.receiver, "AssocOp.receiver");
            std::visit(
                [&](const auto& op) {
                  if constexpr (requires { op.dest; }) {
                    verify_place(op.dest, "AssocOp.dest");
                  }
                  if constexpr (requires { op.key; }) {
                    verify_operand_place(op.key, "AssocOp.key");
                  }
                  if constexpr (requires { op.value; }) {
                    verify_operand_place(op.value, "AssocOp.value");
                  }
                  if constexpr (requires { op.out_key; }) {
                    verify_place(op.out_key, "AssocOp.out_key");
                  }
                  if constexpr (requires { op.dest_found; }) {
                    verify_place(op.dest_found, "AssocOp.dest_found");
                  }
                  if constexpr (requires { op.key_place; }) {
                    verify_place(op.key_place, "AssocOp.key_place");
                  }
                  if constexpr (requires { op.dest_keys; }) {
                    verify_place(op.dest_keys, "AssocOp.dest_keys");
                  }
                },
                aop.data);
          },
          [&](const TriggerEvent& te) {
            if (cx.num_events > 0 && te.event.value >= cx.num_events) {
              throw common::InternalError(
                  "MIR verify",
                  std::format(
                      "{}: block {} stmt {}: TriggerEvent.event {} out of "
                      "range (body has {} events)",
                      routine_kind, block_idx, stmt_idx, te.event.value,
                      cx.num_events));
            }
          },
      },
      stmt.data);
}

// Verify intra-block def-before-use for a single block.
// Returns set of temp_ids available at end of block (for cross-block analysis
// if needed).
void VerifyIntraBlockDefOrder(
    const BasicBlock& block, size_t block_idx,
    [[maybe_unused]] const std::vector<TempMetadata>& temp_metadata,
    std::string_view routine_kind) {
  // Initialize available temps with block params
  std::unordered_set<int> available_temps;
  for (const auto& param : block.params) {
    available_temps.insert(param.temp_id);
  }

  // Check each statement in order
  for (size_t stmt_idx = 0; stmt_idx < block.statements.size(); ++stmt_idx) {
    const auto& stmt = block.statements[stmt_idx];

    // Check all UseTemp operands in this statement are available
    std::visit(
        common::Overloaded{
            [&](const Assign& assign) {
              VerifyRhsDefBeforeUse(
                  assign.rhs, available_temps, block_idx, stmt_idx,
                  routine_kind);
            },
            [&](const GuardedAssign& ga) {
              VerifyRhsDefBeforeUse(
                  ga.rhs, available_temps, block_idx, stmt_idx, routine_kind);
              VerifyOperandDefBeforeUse(
                  ga.guard, available_temps, block_idx, stmt_idx, "guard",
                  routine_kind);
            },
            [&](const Effect& eff) {
              // Verify operand def-before-use for decision observation
              if (const auto* dyn =
                      std::get_if<RecordDecisionObservationDynamic>(&eff.op)) {
                VerifyOperandDefBeforeUse(
                    dyn->match_class, available_temps, block_idx, stmt_idx,
                    "RecordDecisionObservationDynamic.match_class",
                    routine_kind);
                VerifyOperandDefBeforeUse(
                    dyn->selected_kind, available_temps, block_idx, stmt_idx,
                    "RecordDecisionObservationDynamic.selected_kind",
                    routine_kind);
                VerifyOperandDefBeforeUse(
                    dyn->selected_arm, available_temps, block_idx, stmt_idx,
                    "RecordDecisionObservationDynamic.selected_arm",
                    routine_kind);
              }
            },
            [&](const DeferredAssign& da) {
              VerifyRhsDefBeforeUse(
                  da.rhs, available_temps, block_idx, stmt_idx, routine_kind);
            },
            [&](const Call& call) {
              for (size_t i = 0; i < call.in_args.size(); ++i) {
                std::string ctx = std::format("in_args[{}]", i);
                VerifyOperandDefBeforeUse(
                    call.in_args[i], available_temps, block_idx, stmt_idx, ctx,
                    routine_kind);
              }
            },
            [&](const DpiCall& dpi_call) {
              for (size_t i = 0; i < dpi_call.args.size(); ++i) {
                if (dpi_call.args[i].input_value) {
                  std::string ctx = std::format("dpi_args[{}]", i);
                  VerifyOperandDefBeforeUse(
                      *dpi_call.args[i].input_value, available_temps, block_idx,
                      stmt_idx, ctx, routine_kind);
                }
              }
            },
            [&](const BuiltinCall& bcall) {
              for (size_t i = 0; i < bcall.args.size(); ++i) {
                std::string ctx = std::format("args[{}]", i);
                VerifyOperandDefBeforeUse(
                    bcall.args[i], available_temps, block_idx, stmt_idx, ctx,
                    routine_kind);
              }
            },
            [&](const DefineTemp& dt) {
              // Check RHS first (before this temp is defined)
              VerifyRhsDefBeforeUse(
                  dt.rhs, available_temps, block_idx, stmt_idx, routine_kind);
              // Then add this temp to available set
              available_temps.insert(dt.temp_id);
            },
            [&](const AssocOp& aop) {
              std::visit(
                  [&](const auto& op) {
                    if constexpr (requires { op.key; }) {
                      VerifyOperandDefBeforeUse(
                          op.key, available_temps, block_idx, stmt_idx, "key",
                          routine_kind);
                    }
                    if constexpr (requires { op.value; }) {
                      VerifyOperandDefBeforeUse(
                          op.value, available_temps, block_idx, stmt_idx,
                          "value", routine_kind);
                    }
                  },
                  aop.data);
            },
            [](const TriggerEvent&) {},
        },
        stmt.data);
  }
}

// Verify block params and edge args for a routine's blocks
void VerifyBlockParamsAndEdgeArgs(
    const std::vector<BasicBlock>& blocks, const Arena& arena,
    const VerifyContext& cx, const std::vector<TempMetadata>& temp_metadata,
    std::string_view routine_kind) {
  // Collect all defined temp_ids from block params AND DefineTemp statements.
  // This also checks for duplicate definitions and cross-checks temp_metadata.
  std::unordered_map<int, TempDefinition> defined_temps;
  CollectValueTempDefinitions(
      blocks, temp_metadata, routine_kind, defined_temps);

  // Verify intra-block def-before-use and PlaceRoot::kTemp consistency
  for (size_t i = 0; i < blocks.size(); ++i) {
    const auto& block = blocks[i];

    // Verify intra-block ordering
    VerifyIntraBlockDefOrder(block, i, temp_metadata, routine_kind);

    // Verify PlaceRoot::kTemp in all statements
    for (size_t j = 0; j < block.statements.size(); ++j) {
      VerifyStatementOwnership(
          block.statements[j], arena, temp_metadata, cx, i, j, routine_kind);
    }

    // Verify decision-specific MIR invariants per statement.
    for (size_t j = 0; j < block.statements.size(); ++j) {
      const auto& stmt = block.statements[j];

      // SelectRvalueInfo: 3 operands, condition-typed cond, two-state result
      if (const auto* dt = std::get_if<DefineTemp>(&stmt.data)) {
        if (const auto* rv = std::get_if<Rvalue>(&dt->rhs)) {
          if (const auto* sel = std::get_if<SelectRvalueInfo>(&rv->info)) {
            if (rv->operands.size() != 3) {
              throw common::InternalError(
                  "MIR verify",
                  std::format(
                      "{}: block {} stmt {}: SelectRvalueInfo must have "
                      "exactly 3 operands, got {}",
                      routine_kind, i, j, rv->operands.size()));
            }
            VerifyConditionType(
                rv->operands[0], blocks, arena, cx, defined_temps, i,
                "Select.condition", routine_kind);
            VerifyOperandHasType(
                rv->operands[1], sel->result_type, blocks, arena, cx,
                defined_temps, i, "Select.true_value", routine_kind);
            VerifyOperandHasType(
                rv->operands[2], sel->result_type, blocks, arena, cx,
                defined_temps, i, "Select.false_value", routine_kind);
            const Type& result_ty = (*cx.types)[sel->result_type];
            if (result_ty.Kind() != TypeKind::kIntegral ||
                result_ty.AsIntegral().is_four_state) {
              throw common::InternalError(
                  "MIR verify",
                  std::format(
                      "{}: block {} stmt {}: SelectRvalueInfo result_type "
                      "must be two-state integral",
                      routine_kind, i, j));
            }
          }
        }
      }

      // RecordDecisionObservation: non-arm must have selected_arm == 0
      if (const auto* eff = std::get_if<Effect>(&stmt.data)) {
        if (const auto* obs =
                std::get_if<RecordDecisionObservation>(&eff->op)) {
          if (obs->selected_kind_const !=
                  semantic::DecisionSelectedKind::kArm &&
              obs->selected_arm_const.Index() != 0) {
            throw common::InternalError(
                "MIR verify",
                std::format(
                    "{}: block {} stmt {}: non-arm decision observation "
                    "must encode selected_arm = 0",
                    routine_kind, i, j));
          }
        }

        // RecordDecisionObservationDynamic: verify operand types
        if (const auto* dyn =
                std::get_if<RecordDecisionObservationDynamic>(&eff->op)) {
          auto verify_is_i8 = [&](const Operand& op, std::string_view ctx) {
            TypeId tid = GetOperandType(op, blocks, arena, cx, defined_temps);
            if (!tid) return;
            const Type& ty = (*cx.types)[tid];
            if (ty.Kind() != TypeKind::kIntegral ||
                ty.AsIntegral().bit_width != 8 ||
                ty.AsIntegral().is_four_state) {
              throw common::InternalError(
                  "MIR verify",
                  std::format(
                      "{}: block {} stmt {}: {}: expected two-state i8",
                      routine_kind, i, j, ctx));
            }
          };
          auto verify_is_i16 = [&](const Operand& op, std::string_view ctx) {
            TypeId tid = GetOperandType(op, blocks, arena, cx, defined_temps);
            if (!tid) return;
            const Type& ty = (*cx.types)[tid];
            if (ty.Kind() != TypeKind::kIntegral ||
                ty.AsIntegral().bit_width != 16 ||
                ty.AsIntegral().is_four_state) {
              throw common::InternalError(
                  "MIR verify",
                  std::format(
                      "{}: block {} stmt {}: {}: expected two-state i16",
                      routine_kind, i, j, ctx));
            }
          };
          verify_is_i8(
              dyn->match_class, "RecordDecisionObservationDynamic.match_class");
          verify_is_i8(
              dyn->selected_kind,
              "RecordDecisionObservationDynamic.selected_kind");
          verify_is_i16(
              dyn->selected_arm,
              "RecordDecisionObservationDynamic.selected_arm");
        }
      }
    }
  }

  // Verify each terminator's edge args
  for (size_t i = 0; i < blocks.size(); ++i) {
    const auto& block = blocks[i];

    std::visit(
        common::Overloaded{
            [&](const Jump& jump) {
              VerifyEdgeArgs(
                  jump.args, jump.target.value, blocks, arena, cx,
                  defined_temps, "Jump", i, routine_kind);
              for (size_t j = 0; j < jump.args.size(); ++j) {
                VerifyUseTempDefined(
                    jump.args[j], defined_temps, temp_metadata, i,
                    std::format("Jump.args[{}]", j), routine_kind);
              }
            },
            [&](const Branch& branch) {
              // Verify condition operand is not Poison, UseTemp is defined,
              // type is valid
              VerifyNotPoison(
                  branch.condition, i, "Branch.condition", routine_kind);
              VerifyUseTempDefined(
                  branch.condition, defined_temps, temp_metadata, i,
                  "Branch.condition", routine_kind);
              VerifyConditionType(
                  branch.condition, blocks, arena, cx, defined_temps, i,
                  "Branch.condition", routine_kind);
              VerifyEdgeArgs(
                  branch.then_args, branch.then_target.value, blocks, arena, cx,
                  defined_temps, "Branch.then", i, routine_kind);
              VerifyEdgeArgs(
                  branch.else_args, branch.else_target.value, blocks, arena, cx,
                  defined_temps, "Branch.else", i, routine_kind);
              for (size_t j = 0; j < branch.then_args.size(); ++j) {
                VerifyUseTempDefined(
                    branch.then_args[j], defined_temps, temp_metadata, i,
                    std::format("Branch.then_args[{}]", j), routine_kind);
              }
              for (size_t j = 0; j < branch.else_args.size(); ++j) {
                VerifyUseTempDefined(
                    branch.else_args[j], defined_temps, temp_metadata, i,
                    std::format("Branch.else_args[{}]", j), routine_kind);
              }
            },
            [&](const Switch& sw) {
              // Verify selector operand is not Poison, UseTemp is defined, type
              // is valid
              VerifyNotPoison(sw.selector, i, "Switch.selector", routine_kind);
              VerifyUseTempDefined(
                  sw.selector, defined_temps, temp_metadata, i,
                  "Switch.selector", routine_kind);
              VerifySelectorType(
                  sw.selector, blocks, arena, cx, defined_temps, i,
                  "Switch.selector", routine_kind);
            },
            [&](const WaitEvent& we) {
              if (cx.num_events > 0 && we.event.value >= cx.num_events) {
                throw common::InternalError(
                    "MIR verify",
                    std::format(
                        "{}: block {}: WaitEvent.event {} out of range "
                        "(body has {} events)",
                        routine_kind, i, we.event.value, cx.num_events));
              }
              if (we.resume.value >= blocks.size()) {
                throw common::InternalError(
                    "MIR verify",
                    std::format(
                        "{}: block {}: WaitEvent.resume {} out of range "
                        "(routine has {} blocks)",
                        routine_kind, i, we.resume.value, blocks.size()));
              }
            },
            [](const auto&) {
              // Delay, Wait, Return, Finish, Repeat: no operands needing
              // temp verification.
            },
        },
        block.terminator.data);
  }
}

}  // namespace

void VerifyFunction(
    const Function& func, const Arena& arena, const VerifyContext& cx,
    std::string_view label) {
  VerifyParamLocalSlots(func);

  const Type& ret_type = (*cx.types)[func.signature.return_type];
  bool is_void = ret_type.Kind() == TypeKind::kVoid;
  VerifyReturnInvariants(func.blocks, is_void, label);

  // Verify block params, edge args, temp_metadata consistency, and
  // def-before-use
  VerifyBlockParamsAndEdgeArgs(
      func.blocks, arena, cx, func.temp_metadata, label);
}

void VerifyProcess(
    const Process& proc, const Arena& arena, const VerifyContext& cx,
    std::string_view label) {
  VerifyReturnInvariants(proc.blocks, true, label);

  VerifyBlockParamsAndEdgeArgs(
      proc.blocks, arena, cx, proc.temp_metadata, label);
}

void VerifyPreBackendBody(
    const CompiledModuleBody& body, const TypeArena& types,
    std::string_view label) {
  VerifyContext cx{
      .body = &body,
      .types = &types,
      .phase = VerifyContext::Phase::kPreBackend,
  };

  // Verify external_refs table integrity.
  for (uint32_t i = 0; i < body.external_refs.size(); ++i) {
    RequireExternalRefRecipe(body, ExternalRefId{i}, "VerifyPreBackendBody");
  }

  // Walk all processes and functions through the common verifier.
  for (size_t i = 0; i < body.processes.size(); ++i) {
    VerifyProcess(
        body.arena[body.processes[i]], body.arena, cx,
        std::format("{}:proc[{}]", label, i));
  }
  for (size_t i = 0; i < body.functions.size(); ++i) {
    VerifyFunction(
        body.arena[body.functions[i]], body.arena, cx,
        std::format("{}:func[{}]", label, i));
  }

  // Verify all ExternalRefId operands and write targets reference valid
  // entries. Uses the generic walkers for complete coverage.
  auto check_blocks = [&](const std::vector<BasicBlock>& blocks,
                          std::string_view routine_label) {
    for (size_t bi = 0; bi < blocks.size(); ++bi) {
      auto block_ctx = std::format("{}:block[{}]", routine_label, bi);
      for (const auto& stmt : blocks[bi].statements) {
        ForEachOperand(stmt.data, [&](const Operand& op) {
          if (op.kind == Operand::Kind::kExternalRef) {
            auto id = std::get<ExternalRefId>(op.payload);
            RequireExternalRefRecipe(body, id, "VerifyPreBackendBody");
          }
        });
        ForEachWriteTarget(stmt.data, [&](const WriteTarget& t) {
          if (const auto* ext = std::get_if<ExternalRefId>(&t)) {
            RequireExternalRefRecipe(body, *ext, "VerifyPreBackendBody");
          }
        });
      }
    }
  };

  for (size_t i = 0; i < body.processes.size(); ++i) {
    const auto& proc = body.arena[body.processes[i]];
    check_blocks(proc.blocks, std::format("{}:proc[{}]", label, i));
  }
  for (size_t i = 0; i < body.functions.size(); ++i) {
    const auto& func = body.arena[body.functions[i]];
    check_blocks(func.blocks, std::format("{}:func[{}]", label, i));
  }
}

void VerifyBackendReadyBody(
    const CompiledModuleBody& body, std::string_view label) {
  // Backend-ready MIR must contain no ExternalRefId in any operand or
  // write target. Uses the generic walkers for complete coverage.
  auto check_blocks = [&](const std::vector<BasicBlock>& blocks,
                          std::string_view routine_label) {
    for (size_t bi = 0; bi < blocks.size(); ++bi) {
      for (const auto& stmt : blocks[bi].statements) {
        ForEachOperand(stmt.data, [&](const Operand& op) {
          if (op.kind == Operand::Kind::kExternalRef) {
            throw common::InternalError(
                "VerifyBackendReadyBody",
                std::format(
                    "{}: {}:block[{}]: ExternalRefId operand in "
                    "backend-ready MIR",
                    label, routine_label, bi));
          }
        });
        ForEachWriteTarget(stmt.data, [&](const WriteTarget& t) {
          if (IsExternalWrite(t)) {
            throw common::InternalError(
                "VerifyBackendReadyBody",
                std::format(
                    "{}: {}:block[{}]: ExternalRefId write target in "
                    "backend-ready MIR",
                    label, routine_label, bi));
          }
        });
      }
    }
  };

  for (size_t i = 0; i < body.processes.size(); ++i) {
    const auto& proc = body.arena[body.processes[i]];
    check_blocks(proc.blocks, std::format("proc[{}]", i));
  }
  for (size_t i = 0; i < body.functions.size(); ++i) {
    const auto& func = body.arena[body.functions[i]];
    check_blocks(func.blocks, std::format("func[{}]", i));
  }
}

}  // namespace lyra::mir

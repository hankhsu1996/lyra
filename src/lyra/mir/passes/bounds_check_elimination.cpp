#include "lyra/mir/passes/bounds_check_elimination.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/package.hpp"
#include "lyra/mir/passes/canonical_loop_analysis.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::mir::passes {

namespace {

auto GetUsePlaceId(const Operand& op) -> std::optional<PlaceId> {
  if (op.kind == Operand::Kind::kUse) {
    return std::get<PlaceId>(op.payload);
  }
  return std::nullopt;
}

auto ResolveOperandToTempId(const Operand& op, const Arena& arena)
    -> std::optional<int> {
  if (op.kind == Operand::Kind::kUseTemp) {
    return std::get<TempId>(op.payload).value;
  }
  auto pid = GetUsePlaceId(op);
  if (!pid) return std::nullopt;
  const auto& place = arena[*pid];
  if (place.root.kind == PlaceRoot::Kind::kTemp && place.projections.empty()) {
    return place.root.id;
  }
  return std::nullopt;
}

auto ResolvePlaceToTempId(PlaceId pid, const Arena& arena)
    -> std::optional<int> {
  const auto& place = arena[pid];
  if (place.root.kind == PlaceRoot::Kind::kTemp && place.projections.empty()) {
    return place.root.id;
  }
  return std::nullopt;
}

auto IsTempProvenTrue(const std::vector<bool>& proven_true, int temp_id)
    -> bool {
  if (temp_id < 0 || static_cast<size_t>(temp_id) >= proven_true.size()) {
    return false;
  }
  return proven_true[static_cast<size_t>(temp_id)];
}

auto IsOperandProvenTrue(
    const std::vector<bool>& proven_true, const Operand& op, const Arena& arena)
    -> bool {
  auto temp_id = ResolveOperandToTempId(op, arena);
  if (!temp_id) return false;
  return IsTempProvenTrue(proven_true, *temp_id);
}

auto MakeConstantTrue(TypeId bit_type) -> Operand {
  return Operand::Const(
      Constant{
          .type = bit_type,
          .value = IntegralConstant{.value = {1}, .unknown = {0}},
      });
}

void FindProvablyTrueBoundsTemps(
    const std::vector<BasicBlock>& blocks, const CanonicalInductionLoop& loop,
    std::vector<bool>& proven_true) {
  for (uint32_t bi : loop.body_blocks) {
    const auto& block = blocks[bi];
    for (const auto& stmt : block.statements) {
      const auto* dt = std::get_if<DefineTemp>(&stmt.data);
      if (dt == nullptr) continue;

      const auto* rv = std::get_if<Rvalue>(&dt->rhs);
      if (rv == nullptr) continue;

      const auto* ir_info = std::get_if<IndexInRangeRvalueInfo>(&rv->info);
      if (ir_info == nullptr) continue;

      if (rv->operands.size() != 1) continue;
      auto index_place = GetUsePlaceId(rv->operands[0]);
      if (!index_place || *index_place != loop.induction_var) continue;

      if (loop.init >= ir_info->lower_bound &&
          loop.max_value <= ir_info->upper_bound) {
        if (dt->temp_id >= 0 &&
            static_cast<size_t>(dt->temp_id) < proven_true.size()) {
          proven_true[static_cast<size_t>(dt->temp_id)] = true;
        }
      }
    }
  }
}

auto MarkTempProvenTrue(std::vector<bool>& proven_true, int temp_id) -> bool {
  if (temp_id < 0 || static_cast<size_t>(temp_id) >= proven_true.size()) {
    return false;
  }
  if (proven_true[static_cast<size_t>(temp_id)]) return false;
  proven_true[static_cast<size_t>(temp_id)] = true;
  return true;
}

auto IsRhsProvenTrue(
    const std::vector<bool>& proven_true, const RightHandSide& rhs,
    const Arena& arena) -> bool {
  const auto* op = std::get_if<Operand>(&rhs);
  if (op == nullptr) return false;
  return IsOperandProvenTrue(proven_true, *op, arena);
}

void PropagateProvenTrueTemps(
    const std::vector<BasicBlock>& blocks, const CanonicalInductionLoop& loop,
    std::vector<bool>& proven_true, const Arena& arena) {
  bool changed = true;
  while (changed) {
    changed = false;
    for (uint32_t bi : loop.body_blocks) {
      const auto& block = blocks[bi];
      for (const auto& stmt : block.statements) {
        if (const auto* dt = std::get_if<DefineTemp>(&stmt.data)) {
          if (IsTempProvenTrue(proven_true, dt->temp_id)) continue;
          if (IsRhsProvenTrue(proven_true, dt->rhs, arena)) {
            changed = MarkTempProvenTrue(proven_true, dt->temp_id) || changed;
          }
          continue;
        }

        if (auto ref = TryGetDirectAssign(stmt.data); ref) {
          const auto* dest_pid = std::get_if<PlaceId>(ref.dest);
          if (dest_pid == nullptr) continue;
          auto dest_temp = ResolvePlaceToTempId(*dest_pid, arena);
          if (!dest_temp) continue;
          if (IsTempProvenTrue(proven_true, *dest_temp)) continue;
          if (IsRhsProvenTrue(proven_true, *ref.rhs, arena)) {
            changed = MarkTempProvenTrue(proven_true, *dest_temp) || changed;
          }
        }
      }
    }
  }
}

auto SimplifyRhs(
    RightHandSide& rhs, const std::vector<bool>& proven_true,
    const Arena& arena) -> bool {
  auto* rv = std::get_if<Rvalue>(&rhs);
  if (rv == nullptr) return false;

  if (const auto* bin = std::get_if<BinaryRvalueInfo>(&rv->info)) {
    if (bin->op == BinaryOp::kLogicalAnd && rv->operands.size() == 2) {
      if (IsOperandProvenTrue(proven_true, rv->operands[0], arena)) {
        Operand other = std::move(rv->operands[1]);
        rhs = std::move(other);
        return true;
      }
      if (IsOperandProvenTrue(proven_true, rv->operands[1], arena)) {
        Operand other = std::move(rv->operands[0]);
        rhs = std::move(other);
        return true;
      }
    }
    return false;
  }

  if (const auto* gu = std::get_if<GuardedUseRvalueInfo>(&rv->info)) {
    if (rv->operands.size() == 1 &&
        IsOperandProvenTrue(proven_true, rv->operands[0], arena)) {
      rhs = Operand::Use(gu->place);
      return true;
    }
  }

  return false;
}

void SimplifyBoundsCheckedOps(
    std::vector<BasicBlock>& blocks, const CanonicalInductionLoop& loop,
    const std::vector<bool>& proven_true, const Arena& arena) {
  for (uint32_t bi : loop.body_blocks) {
    auto& block = blocks[bi];
    for (auto& stmt : block.statements) {
      if (auto* dt = std::get_if<DefineTemp>(&stmt.data)) {
        if (IsTempProvenTrue(proven_true, dt->temp_id)) {
          auto* rv = std::get_if<Rvalue>(&dt->rhs);
          if (rv != nullptr &&
              std::holds_alternative<IndexInRangeRvalueInfo>(rv->info)) {
            dt->rhs = MakeConstantTrue(dt->type);
          }
          continue;
        }
        SimplifyRhs(dt->rhs, proven_true, arena);
        continue;
      }

      if (auto ref = TryGetDirectAssign(stmt.data); ref) {
        SimplifyRhs(*ref.rhs, proven_true, arena);
        continue;
      }

      if (auto* ga = std::get_if<GuardedAssign>(&stmt.data)) {
        // Simplify the RHS of guarded assigns with proven-true guards.
        // We do NOT convert to a direct assign because we lack TypeArena
        // to classify PlainAssign vs CopyAssign/MoveAssign for managed
        // destinations. The backend handles the proven-true guard
        // efficiently (LLVM folds the constant branch).
        if (IsOperandProvenTrue(proven_true, ga->guard, arena)) {
          SimplifyRhs(ga->rhs, proven_true, arena);
        }
      }
    }
  }
}

void OptimizeRoutineBoundsChecks(
    std::vector<BasicBlock>& blocks, size_t temp_count, const Arena& arena) {
  auto loops = FindCanonicalInductionLoops(blocks, arena);

  // Each loop is processed independently with a fresh proven_true set.
  // For nested loops, body_blocks may overlap (the outer loop's body range
  // includes inner loop blocks). This is safe because: (a) proven_true is
  // reset per loop, (b) IndexInRange simplification is idempotent (already-
  // constant true stays true), and (c) each loop's IV is distinct so proofs
  // do not interfere.
  for (const auto& loop : loops) {
    std::vector<bool> proven_true(temp_count, false);
    FindProvablyTrueBoundsTemps(blocks, loop, proven_true);
    PropagateProvenTrueTemps(blocks, loop, proven_true, arena);
    SimplifyBoundsCheckedOps(blocks, loop, proven_true, arena);
  }
}

}  // namespace

void EliminateRedundantBoundsChecks(Design& design, Arena& design_arena) {
  auto optimize_process = [](ProcessId pid, Arena& arena) {
    auto& proc = arena.GetMutableProcess(pid);
    OptimizeRoutineBoundsChecks(proc.blocks, proc.temp_metadata.size(), arena);
  };

  auto optimize_function = [](FunctionId fid, Arena& arena) {
    auto& func = arena.GetMutableFunction(fid);
    OptimizeRoutineBoundsChecks(func.blocks, func.temp_metadata.size(), arena);
  };

  for (const auto& element : design.elements) {
    std::visit(
        common::Overloaded{
            [&](const Module& mod) {
              // Resolve to mutable body via pointer-to-index. mod.body is a
              // const view; this pass has non-const Design ownership and
              // needs mutable arena access.
              auto body_idx =
                  static_cast<size_t>(mod.body - design.module_bodies.data());
              auto& body = design.module_bodies[body_idx];
              for (auto fid : body.functions)
                optimize_function(fid, body.arena);
              for (auto pid : body.processes) optimize_process(pid, body.arena);
            },
            [&](const Package& pkg) {
              for (auto fid : pkg.functions)
                optimize_function(fid, design_arena);
            },
        },
        element);
  }

  for (auto fid : design.generated_functions)
    optimize_function(fid, design_arena);
  for (auto pid : design.init_processes) optimize_process(pid, design_arena);
}

}  // namespace lyra::mir::passes

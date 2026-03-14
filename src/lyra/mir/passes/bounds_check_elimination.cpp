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
#include "lyra/mir/place.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir::passes {

namespace {

// Phase-1 contract with LowerForLoop
//
// This pass recognizes the exact block layout emitted by LowerForLoop in
// HIR->MIR lowering:
//
//   preheader: init IV with constant, Jump(header)
//   header (cond_bb):       Branch(cmp(iv, const_bound), header+1, exit)
//   body blocks:            loop body (one or more blocks, IV not modified)
//   latch (step_bb):        iv = iv + 1, Jump(header)
//
// Structural guarantees this pass depends on:
//   - preheader is the unique non-back-edge predecessor of header
//   - header's then-target is header + 1
//   - blocks [header, latch] are contiguous and form the loop region
//   - IV is initialized in the preheader with a constant
//   - IV is incremented by +1 only in the latch
//   - IV is not modified in body blocks
//
// Value temps (DefineTemp) are verified single-def by MIR verification
// (CollectValueTempDefinitions). Place temps (Assign-to-kTemp) are
// single-def by lowering construction but not verifier-checked; this pass
// uses FindRvalueForTemp for place temps only in the latch block for step
// recognition, where the shape is constrained by the LowerForLoop contract.
//
// IndexInRange is emitted only for fixed unpacked array element access.
// The pass does not need to filter by array kind because the MIR op
// already encodes the proven bounds [lower_bound, upper_bound].
//
// Phase 1 processes each recognized canonical loop independently.
// Rewrites are local to proven temps within the recognized body region,
// so independent loops (including nested loops with distinct IVs) do not
// interfere with each other's analysis or rewriting.

struct CanonicalInductionLoop {
  uint32_t header;
  uint32_t latch;
  PlaceId induction_var;

  // Proven range of the IV within the loop body: [init, max_value].
  // For `i < bound`:  max_value = bound - 1
  // For `i <= bound`: max_value = bound
  int64_t init;
  int64_t max_value;

  // Body block indices: (header, latch) exclusive.
  // These are the blocks where bounds-check ops appear and where
  // optimization is applied. Header (condition) and latch (step) are
  // excluded since they contain loop control, not array access.
  std::vector<uint32_t> body_blocks;
};

struct BackEdge {
  uint32_t source;
  uint32_t target;
};

// Find back-edges: edges where target <= source.
// This is a generic CFG scan. Self-loops (target == source) are included
// for completeness but are not recognized as canonical induction loops
// by RecognizeCanonicalInductionLoop (which requires a multi-block shape).
auto FindBackEdges(const std::vector<BasicBlock>& blocks)
    -> std::vector<BackEdge> {
  std::vector<BackEdge> result;
  for (uint32_t i = 0; i < blocks.size(); ++i) {
    ForEachSuccessor(
        blocks[i].terminator, [&](const TerminatorSuccessor& succ) {
          if (succ.target.value <= i) {
            result.push_back({.source = i, .target = succ.target.value});
          }
        });
  }
  return result;
}

auto GetUsePlaceId(const Operand& op) -> std::optional<PlaceId> {
  if (op.kind == Operand::Kind::kUse) {
    return std::get<PlaceId>(op.payload);
  }
  return std::nullopt;
}

auto GetUseTempId(const Operand& op) -> std::optional<int> {
  if (op.kind == Operand::Kind::kUseTemp) {
    return std::get<TempId>(op.payload).value;
  }
  return std::nullopt;
}

// Resolve an operand to a temp_id. Handles both UseTemp(temp_id) and
// Use(PlaceId) where the place is a bare kTemp root (no projections).
auto ResolveOperandToTempId(const Operand& op, const Arena& arena)
    -> std::optional<int> {
  if (auto tid = GetUseTempId(op)) return tid;
  auto pid = GetUsePlaceId(op);
  if (!pid) return std::nullopt;
  const auto& place = arena[*pid];
  if (place.root.kind == PlaceRoot::Kind::kTemp && place.projections.empty()) {
    return place.root.id;
  }
  return std::nullopt;
}

// Resolve a PlaceId to a temp_id if it is a bare kTemp root.
auto ResolvePlaceToTempId(PlaceId pid, const Arena& arena)
    -> std::optional<int> {
  const auto& place = arena[pid];
  if (place.root.kind == PlaceRoot::Kind::kTemp && place.projections.empty()) {
    return place.root.id;
  }
  return std::nullopt;
}

// Find the Rvalue that defines a given temp_id in a block.
// Searches both DefineTemp and Assign-to-temp-place statements.
//
// Value temps are verified single-def by MIR verification. Place temps
// are single-def by construction in the latch block (the only context
// where this function is used for place temps in Phase 1).
auto FindRvalueForTemp(const BasicBlock& block, int temp_id, const Arena& arena)
    -> const Rvalue* {
  for (const auto& stmt : block.statements) {
    if (const auto* dt = std::get_if<DefineTemp>(&stmt.data)) {
      if (dt->temp_id == temp_id) {
        return std::get_if<Rvalue>(&dt->rhs);
      }
      continue;
    }
    if (const auto* assign = std::get_if<Assign>(&stmt.data)) {
      auto dest_temp = ResolvePlaceToTempId(assign->dest, arena);
      if (dest_temp && *dest_temp == temp_id) {
        return std::get_if<Rvalue>(&assign->rhs);
      }
    }
  }
  return nullptr;
}

// Resolve one level of temp-place copy indirection.
//
// If the operand is Use(temp_place) and that temp is defined as a simple
// copy Use(source_place) in the given block, returns source_place.
// Otherwise returns the operand's direct PlaceId (if any).
//
// This handles the specific LowerForLoop step pattern where the IV is
// copied to a temp before the add operation:
//   %t = use(_iv)
//   %u = binary(+), use(%t), const(1)
//
// Single-level only: does not chase through multiple copies.
auto ResolveOneUseCopy(
    const Operand& op, const BasicBlock& block, const Arena& arena)
    -> std::optional<PlaceId> {
  auto pid = GetUsePlaceId(op);
  if (!pid) return std::nullopt;

  auto temp_id = ResolvePlaceToTempId(*pid, arena);
  if (!temp_id) return pid;

  // Find the definition of this temp in the block.
  for (const auto& stmt : block.statements) {
    if (const auto* dt = std::get_if<DefineTemp>(&stmt.data)) {
      if (dt->temp_id == *temp_id) {
        const auto* rhs_op = std::get_if<Operand>(&dt->rhs);
        if (rhs_op != nullptr) return GetUsePlaceId(*rhs_op);
        return std::nullopt;
      }
      continue;
    }
    if (const auto* assign = std::get_if<Assign>(&stmt.data)) {
      auto dest_tid = ResolvePlaceToTempId(assign->dest, arena);
      if (dest_tid && *dest_tid == *temp_id) {
        const auto* rhs_op = std::get_if<Operand>(&assign->rhs);
        if (rhs_op != nullptr) return GetUsePlaceId(*rhs_op);
        return std::nullopt;
      }
    }
  }
  return std::nullopt;
}

// Extract a constant integer from an operand.
// Only supports values representable in a single 64-bit limb.
// Returns nullopt for multi-limb constants, unknown values, or non-constants.
auto GetConstantInt(const Operand& op) -> std::optional<int64_t> {
  if (op.kind != Operand::Kind::kConst) return std::nullopt;
  const auto& constant = std::get<Constant>(op.payload);
  const auto* ic = std::get_if<IntegralConstant>(&constant.value);
  if (ic == nullptr || !ic->IsKnown()) return std::nullopt;
  if (ic->value.size() != 1) return std::nullopt;
  return static_cast<int64_t>(ic->value[0]);
}

// Check whether any block in (header, latch) exclusive modifies the IV.
// If the IV is reassigned in the loop body, the induction range proof is
// invalid and the pass must reject this loop.
auto IsIvModifiedInBody(
    const std::vector<BasicBlock>& blocks, uint32_t header, uint32_t latch,
    PlaceId iv) -> bool {
  for (uint32_t bi = header + 1; bi < latch; ++bi) {
    for (const auto& stmt : blocks[bi].statements) {
      if (const auto* assign = std::get_if<Assign>(&stmt.data)) {
        if (assign->dest == iv) return true;
      }
      if (const auto* ga = std::get_if<GuardedAssign>(&stmt.data)) {
        if (ga->dest == iv) return true;
      }
    }
  }
  return false;
}

// Recognize a canonical induction loop from a back-edge.
// See Phase-1 contract comment at top of namespace for the expected shape.
auto RecognizeCanonicalInductionLoop(
    const std::vector<BasicBlock>& blocks, const BackEdge& edge,
    const Arena& arena) -> std::optional<CanonicalInductionLoop> {
  const auto& header = blocks[edge.target];
  const auto& latch = blocks[edge.source];

  // Header must have a Branch terminator.
  const auto* branch = std::get_if<Branch>(&header.terminator.data);
  if (branch == nullptr) return std::nullopt;

  // Latch must have a Jump back to header.
  const auto* jump = std::get_if<Jump>(&latch.terminator.data);
  if (jump == nullptr || jump->target.value != edge.target) return std::nullopt;

  // Multi-block shape required: header != latch (rejects self-loops).
  if (edge.target == edge.source) return std::nullopt;

  // Phase-1 shape: branch then-target must be header + 1.
  if (branch->then_target.value != edge.target + 1) return std::nullopt;

  // Resolve the branch condition to a temp_id.
  auto cond_temp_id = ResolveOperandToTempId(branch->condition, arena);
  if (!cond_temp_id) return std::nullopt;

  // Find the comparison DefineTemp in the header block.
  const BinaryRvalueInfo* cmp_info = nullptr;
  const Rvalue* cmp_rvalue = nullptr;

  for (const auto& stmt : header.statements) {
    const auto* dt = std::get_if<DefineTemp>(&stmt.data);
    if (dt == nullptr || dt->temp_id != *cond_temp_id) continue;

    const auto* rv = std::get_if<Rvalue>(&dt->rhs);
    if (rv == nullptr) continue;

    cmp_info = std::get_if<BinaryRvalueInfo>(&rv->info);
    if (cmp_info != nullptr) {
      cmp_rvalue = rv;
      break;
    }
  }

  if (cmp_info == nullptr || cmp_rvalue == nullptr) return std::nullopt;

  // Must be a less-than or less-than-equal comparison.
  bool is_strict = false;
  switch (cmp_info->op) {
    case BinaryOp::kLessThan:
    case BinaryOp::kLessThanSigned:
      is_strict = true;
      break;
    case BinaryOp::kLessThanEqual:
    case BinaryOp::kLessThanEqualSigned:
      is_strict = false;
      break;
    default:
      return std::nullopt;
  }

  // operands[0] must be Use(iv_place), operands[1] must be Const(bound).
  if (cmp_rvalue->operands.size() != 2) return std::nullopt;

  auto iv_place = GetUsePlaceId(cmp_rvalue->operands[0]);
  auto bound = GetConstantInt(cmp_rvalue->operands[1]);
  if (!iv_place || !bound) return std::nullopt;

  int64_t max_value = is_strict ? (*bound - 1) : *bound;

  // Find the preheader: the unique non-back-edge predecessor of the header.
  // In LowerForLoop's canonical shape, the preheader precedes the header in
  // block order, so searching [0, edge.target) finds it while naturally
  // excluding the latch (which is at edge.source >= edge.target).
  // Reject if zero or multiple non-back-edge predecessors.
  std::optional<uint32_t> preheader_idx;
  for (uint32_t bi = 0; bi < edge.target; ++bi) {
    bool jumps_to_header = false;
    ForEachSuccessor(
        blocks[bi].terminator, [&](const TerminatorSuccessor& succ) {
          if (succ.target.value == edge.target) jumps_to_header = true;
        });
    if (!jumps_to_header) continue;
    if (preheader_idx) return std::nullopt;
    preheader_idx = bi;
  }
  if (!preheader_idx) return std::nullopt;
  const auto& preheader = blocks[*preheader_idx];

  // Find the last assignment to IV in the preheader (last write wins).
  std::optional<int64_t> init_value;
  for (const auto& stmt : preheader.statements) {
    const auto* assign = std::get_if<Assign>(&stmt.data);
    if (assign == nullptr || assign->dest != *iv_place) continue;
    const auto* rhs_op = std::get_if<Operand>(&assign->rhs);
    init_value = rhs_op != nullptr ? GetConstantInt(*rhs_op) : std::nullopt;
  }

  if (!init_value) return std::nullopt;

  // Verify the latch has a +1 step for the IV.
  // Supports two LowerForLoop step patterns:
  //   (A) _iv = binary(+), use(_iv), const(1)
  //   (B) %t = use(_iv); %u = binary(+), use(%t), const(1); _iv = use(%u)
  bool has_unit_step = false;
  for (const auto& stmt : latch.statements) {
    const auto* assign = std::get_if<Assign>(&stmt.data);
    if (assign == nullptr || assign->dest != *iv_place) continue;

    // Pattern A: direct Rvalue in the assign.
    const Rvalue* step_rv = std::get_if<Rvalue>(&assign->rhs);

    // Pattern B: assign RHS is a temp reference, look up its definition.
    if (step_rv == nullptr) {
      const auto* rhs_op = std::get_if<Operand>(&assign->rhs);
      if (rhs_op != nullptr) {
        auto tid = ResolveOperandToTempId(*rhs_op, arena);
        if (tid) {
          step_rv = FindRvalueForTemp(latch, *tid, arena);
        }
      }
    }

    if (step_rv == nullptr) continue;

    const auto* bin = std::get_if<BinaryRvalueInfo>(&step_rv->info);
    if (bin == nullptr || bin->op != BinaryOp::kAdd) continue;

    if (step_rv->operands.size() != 2) continue;

    // The add's first operand must be the IV or a single copy of the IV.
    auto lhs_source = ResolveOneUseCopy(step_rv->operands[0], latch, arena);
    auto rhs_const = GetConstantInt(step_rv->operands[1]);
    if (lhs_source == *iv_place && rhs_const == 1) {
      has_unit_step = true;
      break;
    }
  }

  if (!has_unit_step) return std::nullopt;
  if (*init_value > max_value) return std::nullopt;

  // IV must not be modified in body blocks (between header and latch,
  // exclusive). The latch owns the step; any other write invalidates
  // the induction range proof.
  if (IsIvModifiedInBody(blocks, edge.target, edge.source, *iv_place)) {
    return std::nullopt;
  }

  // Collect body blocks: (header, latch) exclusive.
  // LowerForLoop always emits at least one body block between header and
  // latch. Reject if empty (defensive; should not happen for canonical for).
  if (edge.target + 1 >= edge.source) return std::nullopt;

  std::vector<uint32_t> body_blocks;
  for (uint32_t bi = edge.target + 1; bi < edge.source; ++bi) {
    body_blocks.push_back(bi);
  }

  return CanonicalInductionLoop{
      .header = edge.target,
      .latch = edge.source,
      .induction_var = *iv_place,
      .init = *init_value,
      .max_value = max_value,
      .body_blocks = std::move(body_blocks),
  };
}

auto IsTempProvenTrue(const std::vector<bool>& proven_true, int temp_id)
    -> bool {
  if (temp_id < 0 || static_cast<size_t>(temp_id) >= proven_true.size()) {
    return false;
  }
  return proven_true[static_cast<size_t>(temp_id)];
}

// Check if an operand references a proven-true temp, handling both
// UseTemp(temp_id) and Use(PlaceId) for bare kTemp places.
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

// Mark IndexInRange temps as provably-true when the IV range is within bounds.
// Scans only body blocks where array access ops appear.
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

// Try to mark a temp_id as proven-true. Returns true if newly marked.
auto MarkTempProvenTrue(std::vector<bool>& proven_true, int temp_id) -> bool {
  if (temp_id < 0 || static_cast<size_t>(temp_id) >= proven_true.size()) {
    return false;
  }
  if (proven_true[static_cast<size_t>(temp_id)]) return false;
  proven_true[static_cast<size_t>(temp_id)] = true;
  return true;
}

// Check if a RightHandSide references a proven-true value.
auto IsRhsProvenTrue(
    const std::vector<bool>& proven_true, const RightHandSide& rhs,
    const Arena& arena) -> bool {
  const auto* op = std::get_if<Operand>(&rhs);
  if (op == nullptr) return false;
  return IsOperandProvenTrue(proven_true, *op, arena);
}

// Propagate proven-true status through simple copies within body blocks.
// Handles both DefineTemp and Assign-to-temp-place patterns:
//   DefineTemp { t, rhs = UseTemp(proven) }
//   Assign { kTemp_place, rhs = UseTemp(proven) }
//
// The proven_true set is seeded only from IndexInRange temps and extended
// only through copy chains, so it cannot mark unrelated temps.
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

        if (const auto* assign = std::get_if<Assign>(&stmt.data)) {
          auto dest_temp = ResolvePlaceToTempId(assign->dest, arena);
          if (!dest_temp) continue;
          if (IsTempProvenTrue(proven_true, *dest_temp)) continue;
          if (IsRhsProvenTrue(proven_true, assign->rhs, arena)) {
            changed = MarkTempProvenTrue(proven_true, *dest_temp) || changed;
          }
        }
      }
    }
  }
}

// Simplify a RightHandSide in place based on proven-true temps.
// Returns true if a simplification was applied.
//
// The proven_true set is seeded from IndexInRange and propagated through
// copies only. LogicalAnd simplification therefore only fires when one
// operand is a proven bounds-check result (the 4-state validity chain:
// LogicalAnd(IndexInRange, IsKnown) -> IsKnown).
auto SimplifyRhs(
    RightHandSide& rhs, const std::vector<bool>& proven_true,
    const Arena& arena) -> bool {
  auto* rv = std::get_if<Rvalue>(&rhs);
  if (rv == nullptr) return false;

  // LogicalAnd(proven_true, x) -> x.
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

  // GuardedUse(proven_true, place) -> Use(place).
  if (const auto* gu = std::get_if<GuardedUseRvalueInfo>(&rv->info)) {
    if (rv->operands.size() == 1 &&
        IsOperandProvenTrue(proven_true, rv->operands[0], arena)) {
      rhs = Operand::Use(gu->place);
      return true;
    }
  }

  return false;
}

// Apply MIR simplifications for proven-true bounds checks within body
// blocks only (header and latch are excluded):
//   IndexInRange(proven-safe) -> constant true
//   LogicalAnd(true, x) -> x  (collapses 4-state validity chain)
//   GuardedUse(true, place) -> Use(place)
//   GuardedAssign(true, dest, rhs) -> Assign(dest, rhs)
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

      if (auto* assign = std::get_if<Assign>(&stmt.data)) {
        SimplifyRhs(assign->rhs, proven_true, arena);
        continue;
      }

      if (auto* ga = std::get_if<GuardedAssign>(&stmt.data)) {
        if (IsOperandProvenTrue(proven_true, ga->guard, arena)) {
          stmt.data = Assign{
              .dest = ga->dest,
              .rhs = std::move(ga->rhs),
          };
        }
      }
    }
  }
}

void OptimizeRoutineBoundsChecks(
    std::vector<BasicBlock>& blocks, size_t temp_count, const Arena& arena) {
  auto back_edges = FindBackEdges(blocks);
  if (back_edges.empty()) return;

  for (const auto& edge : back_edges) {
    auto loop = RecognizeCanonicalInductionLoop(blocks, edge, arena);
    if (!loop) continue;

    std::vector<bool> proven_true(temp_count, false);
    FindProvablyTrueBoundsTemps(blocks, *loop, proven_true);
    PropagateProvenTrueTemps(blocks, *loop, proven_true, arena);
    SimplifyBoundsCheckedOps(blocks, *loop, proven_true, arena);
  }
}

}  // namespace

void EliminateRedundantBoundsChecks(const Design& design, Arena& arena) {
  auto optimize_process = [&](ProcessId pid) {
    auto& proc = arena.GetMutableProcess(pid);
    OptimizeRoutineBoundsChecks(proc.blocks, proc.temp_metadata.size(), arena);
  };

  auto optimize_function = [&](FunctionId fid) {
    auto& func = arena.GetMutableFunction(fid);
    OptimizeRoutineBoundsChecks(func.blocks, func.temp_metadata.size(), arena);
  };

  for (const auto& element : design.elements) {
    std::visit(
        common::Overloaded{
            [&](const Module& mod) {
              const auto& body = GetModuleBody(design, mod);
              for (auto fid : body.functions) optimize_function(fid);
              for (auto pid : body.processes) optimize_process(pid);
            },
            [&](const Package& pkg) {
              for (auto fid : pkg.functions) optimize_function(fid);
            },
        },
        element);
  }

  for (auto fid : design.generated_functions) optimize_function(fid);
  for (auto pid : design.init_processes) optimize_process(pid);
}

}  // namespace lyra::mir::passes

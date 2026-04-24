#include "lyra/mir/passes/canonical_loop_analysis.hpp"

#include <cstdint>
#include <optional>
#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir::passes {

namespace {

struct BackEdge {
  uint32_t source;
  uint32_t target;
};

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

auto ResolvePlaceToTempId(PlaceId pid, const Arena& arena)
    -> std::optional<int> {
  const auto& place = arena[pid];
  if (place.root.kind == PlaceRoot::Kind::kTemp && place.projections.empty()) {
    return place.root.id;
  }
  return std::nullopt;
}

auto FindRvalueForTemp(const BasicBlock& block, int temp_id, const Arena& arena)
    -> const Rvalue* {
  for (const auto& stmt : block.statements) {
    if (const auto* dt = std::get_if<DefineTemp>(&stmt.data)) {
      if (dt->temp_id == temp_id) {
        return std::get_if<Rvalue>(&dt->rhs);
      }
      continue;
    }
    if (auto ref = TryGetDirectAssign(stmt.data); ref) {
      const auto* dest_pid = std::get_if<PlaceId>(ref.dest);
      if (dest_pid == nullptr) continue;
      auto dest_temp = ResolvePlaceToTempId(*dest_pid, arena);
      if (dest_temp && *dest_temp == temp_id) {
        return std::get_if<Rvalue>(ref.rhs);
      }
    }
  }
  return nullptr;
}

auto ResolveOneUseCopy(
    const Operand& op, const BasicBlock& block, const Arena& arena)
    -> std::optional<PlaceId> {
  auto pid = GetUsePlaceId(op);
  if (!pid) return std::nullopt;

  auto temp_id = ResolvePlaceToTempId(*pid, arena);
  if (!temp_id) return pid;

  for (const auto& stmt : block.statements) {
    if (const auto* dt = std::get_if<DefineTemp>(&stmt.data)) {
      if (dt->temp_id == *temp_id) {
        const auto* rhs_op = std::get_if<Operand>(&dt->rhs);
        if (rhs_op != nullptr) return GetUsePlaceId(*rhs_op);
        return std::nullopt;
      }
      continue;
    }
    if (auto ref = TryGetDirectAssign(stmt.data); ref) {
      const auto* dest_pid = std::get_if<PlaceId>(ref.dest);
      if (dest_pid == nullptr) continue;
      auto dest_tid = ResolvePlaceToTempId(*dest_pid, arena);
      if (dest_tid && *dest_tid == *temp_id) {
        const auto* rhs_op = std::get_if<Operand>(ref.rhs);
        if (rhs_op != nullptr) return GetUsePlaceId(*rhs_op);
        return std::nullopt;
      }
    }
  }
  return std::nullopt;
}

auto GetConstantInt(const Operand& op) -> std::optional<int64_t> {
  if (op.kind != Operand::Kind::kConst) return std::nullopt;
  const auto& constant = std::get<Constant>(op.payload);
  const auto* ic = std::get_if<IntegralConstant>(&constant.value);
  if (ic == nullptr || !ic->IsKnown()) return std::nullopt;
  if (ic->value.size() != 1) return std::nullopt;
  return static_cast<int64_t>(ic->value[0]);
}

auto IsIvModifiedInBody(
    const std::vector<BasicBlock>& blocks, uint32_t header, uint32_t latch,
    PlaceId iv) -> bool {
  for (uint32_t bi = header + 1; bi < latch; ++bi) {
    for (const auto& stmt : blocks[bi].statements) {
      if (auto ref = TryGetDirectAssign(stmt.data); ref) {
        if (const auto* pid = std::get_if<PlaceId>(ref.dest);
            pid != nullptr && *pid == iv)
          return true;
      }
      if (const auto* ga = std::get_if<GuardedAssign>(&stmt.data)) {
        if (RequireLocalDest(ga->dest, "CanonicalLoopAnalysis") == iv)
          return true;
      }
    }
  }
  return false;
}

auto RecognizeLoop(
    const std::vector<BasicBlock>& blocks, const BackEdge& edge,
    const Arena& arena) -> std::optional<CanonicalInductionLoop> {
  const auto& header = blocks[edge.target];
  const auto& latch = blocks[edge.source];

  const auto* branch = std::get_if<Branch>(&header.terminator.data);
  if (branch == nullptr) return std::nullopt;

  const auto* jump = std::get_if<Jump>(&latch.terminator.data);
  if (jump == nullptr || jump->target.value != edge.target) return std::nullopt;

  if (edge.target == edge.source) return std::nullopt;

  if (branch->then_target.value != edge.target + 1) return std::nullopt;

  auto cond_temp_id = ResolveOperandToTempId(branch->condition, arena);
  if (!cond_temp_id) return std::nullopt;

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

  if (cmp_rvalue->operands.size() != 2) return std::nullopt;

  auto iv_place = GetUsePlaceId(cmp_rvalue->operands[0]);
  auto bound = GetConstantInt(cmp_rvalue->operands[1]);
  if (!iv_place || !bound) return std::nullopt;

  int64_t max_value = is_strict ? (*bound - 1) : *bound;

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

  std::optional<int64_t> init_value;
  for (const auto& stmt : preheader.statements) {
    auto ref = TryGetDirectAssign(stmt.data);
    if (!ref) continue;
    const auto* dest_pid = std::get_if<PlaceId>(ref.dest);
    if (dest_pid == nullptr || *dest_pid != *iv_place) continue;
    const auto* rhs_op = std::get_if<Operand>(ref.rhs);
    init_value = rhs_op != nullptr ? GetConstantInt(*rhs_op) : std::nullopt;
  }

  if (!init_value) return std::nullopt;

  bool has_unit_step = false;
  for (const auto& stmt : latch.statements) {
    auto ref = TryGetDirectAssign(stmt.data);
    if (!ref) continue;
    const auto* dest_pid = std::get_if<PlaceId>(ref.dest);
    if (dest_pid == nullptr || *dest_pid != *iv_place) continue;

    const Rvalue* step_rv = std::get_if<Rvalue>(ref.rhs);

    if (step_rv == nullptr) {
      const auto* rhs_op = std::get_if<Operand>(ref.rhs);
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

    auto lhs_source = ResolveOneUseCopy(step_rv->operands[0], latch, arena);
    auto rhs_const = GetConstantInt(step_rv->operands[1]);
    if (lhs_source == *iv_place && rhs_const == 1) {
      has_unit_step = true;
      break;
    }
  }

  if (!has_unit_step) return std::nullopt;
  if (*init_value > max_value) return std::nullopt;

  if (IsIvModifiedInBody(blocks, edge.target, edge.source, *iv_place)) {
    return std::nullopt;
  }

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

// Qualify a recognized canonical loop as safe for back-edge guard removal.
//
// Returns true only when ALL of:
//   1. The IV type is a kIntegral packed scalar (not packed array/struct/enum)
//   2. The IV bit width is in [2, 63] (representable in our int64_t analysis)
//   3. The post-latch IV value (max_value + 1) is representable in the IV type
//
// These conditions ensure the loop terminates by monotonic progression
// without wraparound. Anything outside this narrow acceptance is rejected
// conservatively -- this is the proof gate for removing infinite-loop
// protection.
auto IsOverflowSafe(
    const CanonicalInductionLoop& loop, const Arena& arena,
    const TypeArena& types) -> bool {
  const auto& iv_place = arena[loop.induction_var];
  const Type& iv_type = types[iv_place.root.type];

  // Only accept kIntegral scalar types. Reject packed arrays, packed
  // structs, enums, and non-packed types -- we only trust overflow
  // analysis for plain integer-like scalars.
  if (iv_type.Kind() != TypeKind::kIntegral) return false;

  uint32_t bit_width = iv_type.AsIntegral().bit_width;

  // Reject bit_width < 2: a 1-bit signed integer has range [-1, 0],
  // which makes the type_max arithmetic degenerate. Also reject
  // bit_width > 63: our int64_t representation cannot reliably compare
  // max_value against the type maximum.
  if (bit_width < 2 || bit_width > 63) return false;

  bool is_signed = iv_type.AsIntegral().is_signed;
  if (is_signed) {
    auto type_max = (static_cast<int64_t>(1) << (bit_width - 1)) - 1;
    return loop.max_value < type_max;
  }
  auto type_max =
      static_cast<int64_t>((static_cast<uint64_t>(1) << bit_width) - 1);
  return loop.max_value < type_max;
}

}  // namespace

auto FindCanonicalInductionLoops(
    const std::vector<BasicBlock>& blocks, const Arena& arena)
    -> std::vector<CanonicalInductionLoop> {
  auto back_edges = FindBackEdges(blocks);
  if (back_edges.empty()) return {};

  std::vector<CanonicalInductionLoop> result;
  for (const auto& edge : back_edges) {
    auto loop = RecognizeLoop(blocks, edge, arena);
    if (loop) result.push_back(std::move(*loop));
  }
  return result;
}

// First-version scope: canonical increasing for-loops only.
// Does not recognize repeat(N), while, decrementing loops, or non-constant
// bounds. Extend by adding new recognizers to FindCanonicalInductionLoops
// and new overflow qualifiers alongside IsOverflowSafe.
auto FindBoundedBackEdges(
    const std::vector<BasicBlock>& blocks, const Arena& arena,
    const TypeArena& types) -> std::vector<BoundedBackEdge> {
  auto loops = FindCanonicalInductionLoops(blocks, arena);

  std::vector<BoundedBackEdge> result;
  for (const auto& loop : loops) {
    if (!IsOverflowSafe(loop, arena, types)) continue;

    int64_t max_iterations = loop.max_value - loop.init + 1;
    result.push_back({
        .latch_block = loop.latch,
        .header_block = loop.header,
        .max_iterations = max_iterations,
    });
  }
  return result;
}

}  // namespace lyra::mir::passes

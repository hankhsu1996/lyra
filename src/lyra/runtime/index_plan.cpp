#include "lyra/runtime/index_plan.hpp"

#include <cstdint>
#include <cstring>
#include <format>
#include <span>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/slot_meta.hpp"

namespace lyra::runtime {

auto EvaluateIndexPlan(
    const void* design_state_base, const SlotMetaRegistry& registry,
    std::span<const IndexPlanOp> plan, bool* has_xz) -> int64_t {
  if (plan.empty()) {
    throw common::InternalError("EvaluateIndexPlan", "empty plan");
  }
  if (plan.size() > kMaxPlanOps) {
    throw common::InternalError(
        "EvaluateIndexPlan",
        std::format(
            "plan too large: {} ops (max {})", plan.size(), kMaxPlanOps));
  }

  int64_t stack[kMaxPlanStackDepth];
  uint32_t sp = 0;

  for (const auto& op : plan) {
    switch (op.kind) {
      case IndexPlanOp::Kind::kReadSlot: {
        if (sp >= kMaxPlanStackDepth) {
          throw common::InternalError("EvaluateIndexPlan", "stack overflow");
        }
        const auto& meta = registry.Get(op.slot_id);
        // Check for X/Z bits.
        if (meta.kind == SlotStorageKind::kPacked4) {
          const auto* unk = static_cast<const uint8_t*>(design_state_base) +
                            meta.base_off + meta.planes.unk_off +
                            op.byte_offset;
          for (uint8_t i = 0; i < op.byte_size; ++i) {
            if (unk[i] != 0) {
              *has_xz = true;
              return 0;
            }
          }
        }
        // Read value.
        const auto* base =
            static_cast<const uint8_t*>(design_state_base) + meta.base_off;
        uint64_t v = 0;
        std::memcpy(&v, base + op.byte_offset, op.byte_size);
        // Mask to bit_width.
        if (op.bit_width > 0 && op.bit_width < 64) {
          v &= (uint64_t{1} << op.bit_width) - 1;
        }
        // Sign-extend if signed.
        if (op.is_signed != 0 && op.bit_width > 0 && op.bit_width < 64) {
          uint64_t sign_bit = uint64_t{1} << (op.bit_width - 1);
          v = (v ^ sign_bit) - sign_bit;
        }
        stack[sp++] = static_cast<int64_t>(v);
        break;
      }
      case IndexPlanOp::Kind::kConst: {
        if (sp >= kMaxPlanStackDepth) {
          throw common::InternalError("EvaluateIndexPlan", "stack overflow");
        }
        stack[sp++] = op.const_value;
        break;
      }
      case IndexPlanOp::Kind::kAdd: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kAdd");
        }
        int64_t rhs = stack[--sp];
        stack[sp - 1] += rhs;
        break;
      }
      case IndexPlanOp::Kind::kSub: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kSub");
        }
        int64_t rhs = stack[--sp];
        stack[sp - 1] -= rhs;
        break;
      }
      case IndexPlanOp::Kind::kMul: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kMul");
        }
        int64_t rhs = stack[--sp];
        stack[sp - 1] *= rhs;
        break;
      }
      case IndexPlanOp::Kind::kAnd: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kAnd");
        }
        int64_t rhs = stack[--sp];
        stack[sp - 1] &= rhs;
        break;
      }
      case IndexPlanOp::Kind::kOr: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kOr");
        }
        int64_t rhs = stack[--sp];
        stack[sp - 1] |= rhs;
        break;
      }
      case IndexPlanOp::Kind::kXor: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kXor");
        }
        int64_t rhs = stack[--sp];
        stack[sp - 1] ^= rhs;
        break;
      }
      case IndexPlanOp::Kind::kShl: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kShl");
        }
        int64_t rhs = stack[--sp];
        auto shift = static_cast<uint64_t>(rhs) & 63;
        stack[sp - 1] =
            static_cast<int64_t>(static_cast<uint64_t>(stack[sp - 1]) << shift);
        break;
      }
      case IndexPlanOp::Kind::kLShr: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kLShr");
        }
        int64_t rhs = stack[--sp];
        auto shift = static_cast<uint64_t>(rhs) & 63;
        stack[sp - 1] =
            static_cast<int64_t>(static_cast<uint64_t>(stack[sp - 1]) >> shift);
        break;
      }
      case IndexPlanOp::Kind::kAShr: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kAShr");
        }
        int64_t rhs = stack[--sp];
        auto shift = static_cast<uint64_t>(rhs) & 63;
        stack[sp - 1] >>= static_cast<int>(shift);
        break;
      }
    }
  }

  if (sp != 1) {
    throw common::InternalError(
        "EvaluateIndexPlan",
        std::format("stack has {} elements after evaluation (expected 1)", sp));
  }
  return stack[0];
}

}  // namespace lyra::runtime

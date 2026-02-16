#include "lyra/runtime/index_plan.hpp"

#include <array>
#include <cstdint>
#include <cstring>
#include <format>
#include <span>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/slot_meta.hpp"

namespace lyra::runtime {

auto EvaluateIndexPlan(
    const void* design_state_base, const SlotMetaRegistry& registry,
    std::span<const IndexPlanOp> plan, bool* should_deactivate) -> int64_t {
  if (plan.empty()) {
    throw common::InternalError("EvaluateIndexPlan", "empty plan");
  }
  if (plan.size() > kMaxPlanOps) {
    throw common::InternalError(
        "EvaluateIndexPlan",
        std::format(
            "plan too large: {} ops (max {})", plan.size(), kMaxPlanOps));
  }

  std::array<int64_t, kMaxPlanStackDepth> stack{};
  uint32_t sp = 0;

  auto ds = std::span(
      static_cast<const uint8_t*>(design_state_base),
      // Upper bound: we only access via meta.base_off + byte_offset + byte_size
      // which is always within design state. Use a large sentinel.
      std::numeric_limits<size_t>::max());

  for (const auto& op : plan) {
    switch (op.kind) {
      case IndexPlanOp::Kind::kReadSlot: {
        if (sp >= kMaxPlanStackDepth) {
          throw common::InternalError("EvaluateIndexPlan", "stack overflow");
        }
        const auto& meta = registry.Get(op.slot_id);
        // Check for X/Z bits.
        if (meta.kind == SlotStorageKind::kPacked4) {
          auto unk =
              ds.subspan(meta.base_off + meta.planes.unk_off + op.byte_offset);
          for (uint8_t i = 0; i < op.byte_size; ++i) {
            if (unk[i] != 0) {
              *should_deactivate = true;
              return 0;
            }
          }
        }
        // Read value.
        auto base = ds.subspan(meta.base_off);
        uint64_t v = 0;
        std::memcpy(&v, &base[op.byte_offset], op.byte_size);
        // Mask to bit_width.
        if (op.bit_width > 0 && op.bit_width < 64) {
          v &= (uint64_t{1} << op.bit_width) - 1;
        }
        // Sign-extend if signed.
        if (op.is_signed != 0 && op.bit_width > 0 && op.bit_width < 64) {
          uint64_t sign_bit = uint64_t{1} << (op.bit_width - 1);
          v = (v ^ sign_bit) - sign_bit;
        }
        stack.at(sp++) = static_cast<int64_t>(v);
        break;
      }
      case IndexPlanOp::Kind::kConst: {
        if (sp >= kMaxPlanStackDepth) {
          throw common::InternalError("EvaluateIndexPlan", "stack overflow");
        }
        stack.at(sp++) = op.const_value;
        break;
      }
      case IndexPlanOp::Kind::kAdd: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kAdd");
        }
        int64_t rhs = stack.at(--sp);
        stack.at(sp - 1) += rhs;
        break;
      }
      case IndexPlanOp::Kind::kSub: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kSub");
        }
        int64_t rhs = stack.at(--sp);
        stack.at(sp - 1) -= rhs;
        break;
      }
      case IndexPlanOp::Kind::kMul: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kMul");
        }
        int64_t rhs = stack.at(--sp);
        stack.at(sp - 1) *= rhs;
        break;
      }
      case IndexPlanOp::Kind::kAnd: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kAnd");
        }
        int64_t rhs = stack.at(--sp);
        stack.at(sp - 1) &= rhs;
        break;
      }
      case IndexPlanOp::Kind::kOr: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kOr");
        }
        int64_t rhs = stack.at(--sp);
        stack.at(sp - 1) |= rhs;
        break;
      }
      case IndexPlanOp::Kind::kXor: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kXor");
        }
        int64_t rhs = stack.at(--sp);
        stack.at(sp - 1) ^= rhs;
        break;
      }
      case IndexPlanOp::Kind::kShl: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kShl");
        }
        int64_t rhs = stack.at(--sp);
        auto shift = static_cast<uint64_t>(rhs) & 63;
        stack.at(sp - 1) = static_cast<int64_t>(
            static_cast<uint64_t>(stack.at(sp - 1)) << shift);
        break;
      }
      case IndexPlanOp::Kind::kLShr: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kLShr");
        }
        int64_t rhs = stack.at(--sp);
        auto shift = static_cast<uint64_t>(rhs) & 63;
        stack.at(sp - 1) = static_cast<int64_t>(
            static_cast<uint64_t>(stack.at(sp - 1)) >> shift);
        break;
      }
      case IndexPlanOp::Kind::kAShr: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kAShr");
        }
        int64_t rhs = stack.at(--sp);
        auto shift = static_cast<uint64_t>(rhs) & 63;
        stack.at(sp - 1) >>= static_cast<int>(shift);
        break;
      }
      case IndexPlanOp::Kind::kDivS: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kDivS");
        }
        int64_t rhs = stack.at(--sp);
        if (rhs == 0) {
          *should_deactivate = true;
          return 0;
        }
        stack.at(sp - 1) /= rhs;
        break;
      }
      case IndexPlanOp::Kind::kDivU: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kDivU");
        }
        int64_t rhs = stack.at(--sp);
        if (rhs == 0) {
          *should_deactivate = true;
          return 0;
        }
        stack.at(sp - 1) = static_cast<int64_t>(
            static_cast<uint64_t>(stack.at(sp - 1)) /
            static_cast<uint64_t>(rhs));
        break;
      }
      case IndexPlanOp::Kind::kModS: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kModS");
        }
        int64_t rhs = stack.at(--sp);
        if (rhs == 0) {
          *should_deactivate = true;
          return 0;
        }
        stack.at(sp - 1) %= rhs;
        break;
      }
      case IndexPlanOp::Kind::kModU: {
        if (sp < 2) {
          throw common::InternalError(
              "EvaluateIndexPlan", "stack underflow on kModU");
        }
        int64_t rhs = stack.at(--sp);
        if (rhs == 0) {
          *should_deactivate = true;
          return 0;
        }
        stack.at(sp - 1) = static_cast<int64_t>(
            static_cast<uint64_t>(stack.at(sp - 1)) %
            static_cast<uint64_t>(rhs));
        break;
      }
    }
  }

  if (sp != 1) {
    throw common::InternalError(
        "EvaluateIndexPlan",
        std::format("stack has {} elements after evaluation (expected 1)", sp));
  }
  return stack.at(0);
}

}  // namespace lyra::runtime

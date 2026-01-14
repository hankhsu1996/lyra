#include "lyra/interpreter/instruction/type.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit.hpp"
#include "lyra/common/wide_bit_ops.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/system_call/format.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

namespace {

/// Create a WideBit from an int64 value, with optional sign extension.
/// Masks the final word to the specified bit width for correct behavior.
auto CreateWideFromInt64(int64_t value, size_t bit_width, bool sign_extend)
    -> common::WideBit {
  auto num_words = common::wide_ops::WordsForBits(bit_width);
  common::WideBit wide(num_words);
  wide.SetWord(0, static_cast<uint64_t>(value));

  if (sign_extend && value < 0) {
    for (size_t i = 1; i < num_words; ++i) {
      wide.SetWord(i, ~uint64_t{0});
    }
  }

  // Mask final word to bit width (matches SDK behavior)
  common::wide_ops::MaskToWidth(wide.Words(), bit_width);
  return wide;
}

}  // namespace

auto HandleTypeOps(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  assert(instr.result.has_value());

  switch (instr.kind) {
    case lir::InstructionKind::kConversion: {
      const auto& src = ctx.GetTemp(instr.operands[0].AsTempRef());
      const auto& target_type = ctx.GetTempType(*instr.result);

      // Handle string to string and real to real as no-op conversions.
      if ((src.type == common::Type::String() &&
           target_type == common::Type::String()) ||
          (src.type == common::Type::Real() &&
           target_type == common::Type::Real())) {
        ctx.WriteTemp(instr.result.value(), src);
        return InstructionResult::Continue();
      }

      if (src.type.IsBitvector() && target_type.IsBitvector()) {
        // Get bit width and signedness for the target type
        size_t target_width = target_type.GetBitWidth();
        bool target_signed = target_type.IsSigned();

        // Get source signedness for sign extension
        bool src_signed = src.type.IsSigned();

        // Wide target type - create WideBit and sign-extend if needed
        if (target_width > 64) {
          common::WideBit wide = src.IsWide() ? src.AsWideBit() : [&]() {
            return CreateWideFromInt64(
                src.AsNarrow().AsInt64(), target_width, src_signed);
          }();

          RuntimeValue result = RuntimeValue::IntegralWide(
              std::move(wide), target_width, target_signed);
          ctx.WriteTemp(instr.result.value(), result);
          return InstructionResult::Continue();
        }

        // Narrow target type - extract value (may be from wide source)
        int64_t raw_value = ExtractInt64FromSource(src);

        // Apply sign/bitwidth conversion
        RuntimeValue result =
            target_signed
                ? RuntimeValue::IntegralSigned(raw_value, target_width)
                : RuntimeValue::IntegralUnsigned(
                      static_cast<uint64_t>(raw_value), target_width);

        ctx.WriteTemp(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (src.type.IsBitvector() &&
          target_type.kind == common::Type::Kind::kReal) {
        bool src_signed = src.type.IsSigned();
        double real_value = 0.0;
        if (src.IsWide()) {
          real_value = src.AsWideBit().ToDouble();
        } else {
          int64_t raw_value = src.AsNarrow().AsInt64();
          real_value =
              src_signed
                  ? static_cast<double>(raw_value)
                  : static_cast<double>(static_cast<uint64_t>(raw_value));
        }
        ctx.WriteTemp(instr.result.value(), RuntimeValue::Real(real_value));
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kReal &&
          target_type.IsBitvector()) {
        size_t target_width = target_type.GetBitWidth();
        bool target_signed = target_type.IsSigned();
        auto raw_value = static_cast<int64_t>(src.AsDouble());

        // Wide target type
        if (target_width > 64) {
          common::WideBit wide = CreateWideFromInt64(
              raw_value, target_width, /*sign_extend=*/true);
          RuntimeValue result = RuntimeValue::IntegralWide(
              std::move(wide), target_width, target_signed);
          ctx.WriteTemp(instr.result.value(), result);
          return InstructionResult::Continue();
        }

        // Narrow target type
        RuntimeValue result =
            target_signed
                ? RuntimeValue::IntegralSigned(raw_value, target_width)
                : RuntimeValue::IntegralUnsigned(
                      static_cast<uint64_t>(raw_value), target_width);

        ctx.WriteTemp(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      // Shortreal conversions
      if (src.type.IsBitvector() &&
          target_type.kind == common::Type::Kind::kShortReal) {
        bool src_signed = src.type.IsSigned();
        float float_value = 0.0F;
        if (src.IsWide()) {
          float_value = static_cast<float>(src.AsWideBit().ToDouble());
        } else {
          int64_t raw_value = src.AsNarrow().AsInt64();
          float_value =
              src_signed ? static_cast<float>(raw_value)
                         : static_cast<float>(static_cast<uint64_t>(raw_value));
        }
        ctx.WriteTemp(
            instr.result.value(), RuntimeValue::ShortReal(float_value));
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kShortReal &&
          target_type.IsBitvector()) {
        size_t target_width = target_type.GetBitWidth();
        bool target_signed = target_type.IsSigned();
        auto raw_value = static_cast<int64_t>(src.AsFloat());

        // Wide target type
        if (target_width > 64) {
          common::WideBit wide = CreateWideFromInt64(
              raw_value, target_width, /*sign_extend=*/true);
          RuntimeValue result = RuntimeValue::IntegralWide(
              std::move(wide), target_width, target_signed);
          ctx.WriteTemp(instr.result.value(), result);
          return InstructionResult::Continue();
        }

        // Narrow target type
        RuntimeValue result =
            target_signed
                ? RuntimeValue::IntegralSigned(raw_value, target_width)
                : RuntimeValue::IntegralUnsigned(
                      static_cast<uint64_t>(raw_value), target_width);

        ctx.WriteTemp(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kReal &&
          target_type.kind == common::Type::Kind::kShortReal) {
        ctx.WriteTemp(
            instr.result.value(),
            RuntimeValue::ShortReal(static_cast<float>(src.AsDouble())));
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kShortReal &&
          target_type.kind == common::Type::Kind::kReal) {
        ctx.WriteTemp(
            instr.result.value(),
            RuntimeValue::Real(static_cast<double>(src.AsFloat())));
        return InstructionResult::Continue();
      }

      // Integral to string conversion (LRM 6.16)
      if (src.type.IsBitvector() &&
          target_type.kind == common::Type::Kind::kString) {
        ctx.WriteTemp(
            instr.result.value(), RuntimeValue::String(IntegralToString(src)));
        return InstructionResult::Continue();
      }

      // Dynamic array to dynamic array: pass through unchanged
      if (src.type.IsDynamicArray() && target_type.IsDynamicArray()) {
        ctx.WriteTemp(instr.result.value(), src);
        return InstructionResult::Continue();
      }

      throw std::runtime_error(
          fmt::format(
              "conversion only supports two-state/real/shortreal/string "
              "types, got: {} -> {}",
              src.type, target_type));
    }

    case lir::InstructionKind::kConcatenation: {
      const auto& result_type = ctx.GetTempType(*instr.result);

      // String concatenation: collect strings and join
      if (result_type.kind == common::Type::Kind::kString) {
        std::string result;
        for (const auto& operand : instr.temp_operands) {
          const auto& val = ctx.GetTemp(operand);
          if (val.IsString()) {
            result += val.AsString();
          } else {
            // Integral operand - convert per LRM (8 bits = 1 char)
            result += IntegralToString(val);
          }
        }
        ctx.WriteTemp(instr.result.value(), RuntimeValue::String(result));
        return InstructionResult::Continue();
      }

      // Integral concatenation
      size_t result_width = result_type.GetBitWidth();

      // Collect operand values
      std::vector<RuntimeValue> operand_values;
      operand_values.reserve(instr.temp_operands.size());
      for (const auto& operand : instr.temp_operands) {
        operand_values.push_back(ctx.GetTemp(operand));
      }

      // RuntimeValue::Concatenate handles narrow/wide dispatch internally
      ctx.WriteTemp(
          instr.result.value(),
          RuntimeValue::Concatenate(operand_values, result_width));
      return InstructionResult::Continue();
    }

    default:
      throw common::InternalError(
          "interpreter", "unhandled instruction kind in type handler");
  }
}

}  // namespace lyra::interpreter

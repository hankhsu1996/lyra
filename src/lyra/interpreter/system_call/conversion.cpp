#include "lyra/interpreter/system_call/conversion.hpp"

#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>

#include "lyra/common/type.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/system_call/format.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

auto HandleConversionCalls(
    const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  // $signed: reinterpret signedness, preserving bit pattern
  if (instr.system_call_name == "$signed") {
    assert(instr.result.has_value());
    assert(instr.result_type.has_value());
    assert(instr.operands.size() == 1);
    const auto& src = ctx.GetOperandValue(instr.operands[0]);
    size_t target_width = instr.result_type->GetBitWidth();
    auto result =
        RuntimeValue::IntegralSigned(ExtractInt64FromSource(src), target_width);
    ctx.GetTempTable().Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $unsigned: reinterpret bits as unsigned
  if (instr.system_call_name == "$unsigned") {
    assert(instr.result.has_value());
    assert(instr.result_type.has_value());
    assert(instr.operands.size() == 1);
    const auto& src = ctx.GetOperandValue(instr.operands[0]);
    size_t target_width = instr.result_type->GetBitWidth();
    auto result = RuntimeValue::IntegralUnsigned(
        static_cast<uint64_t>(ExtractInt64FromSource(src)), target_width);
    ctx.GetTempTable().Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $itor: convert integer to real
  if (instr.system_call_name == "$itor") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = ctx.GetOperandValue(instr.operands[0]);
    bool src_signed = src.type.IsSigned();
    double real_value =
        src_signed ? static_cast<double>(ExtractInt64FromSource(src))
                   : static_cast<double>(
                         static_cast<uint64_t>(ExtractInt64FromSource(src)));
    ctx.GetTempTable().Write(
        instr.result.value(), RuntimeValue::Real(real_value));
    return InstructionResult::Continue();
  }

  // $rtoi: convert real to integer by truncation toward zero
  if (instr.system_call_name == "$rtoi") {
    assert(instr.result.has_value());
    assert(instr.result_type.has_value());
    assert(!instr.operands.empty());
    const auto& src = ctx.GetOperandValue(instr.operands[0]);
    size_t target_width = instr.result_type->GetBitWidth();
    bool target_signed = instr.result_type->IsSigned();
    auto raw_value = static_cast<int64_t>(src.AsDouble());
    auto result = target_signed
                      ? RuntimeValue::IntegralSigned(raw_value, target_width)
                      : RuntimeValue::IntegralUnsigned(
                            static_cast<uint64_t>(raw_value), target_width);
    ctx.GetTempTable().Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $realtobits: real -> 64-bit IEEE 754 representation
  if (instr.system_call_name == "$realtobits") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = ctx.GetOperandValue(instr.operands[0]);
    auto bits = std::bit_cast<uint64_t>(src.AsDouble());
    ctx.GetTempTable().Write(
        instr.result.value(), RuntimeValue::IntegralUnsigned(bits, 64));
    return InstructionResult::Continue();
  }

  // $bitstoreal: 64-bit vector -> real (IEEE 754)
  if (instr.system_call_name == "$bitstoreal") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = ctx.GetOperandValue(instr.operands[0]);
    auto real_value = std::bit_cast<double>(src.AsNarrow().AsUInt64());
    ctx.GetTempTable().Write(
        instr.result.value(), RuntimeValue::Real(real_value));
    return InstructionResult::Continue();
  }

  // $shortrealtobits: shortreal -> 32-bit IEEE 754 representation
  if (instr.system_call_name == "$shortrealtobits") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = ctx.GetOperandValue(instr.operands[0]);
    auto bits = std::bit_cast<uint32_t>(src.AsFloat());
    ctx.GetTempTable().Write(
        instr.result.value(), RuntimeValue::IntegralUnsigned(bits, 32));
    return InstructionResult::Continue();
  }

  // $bitstoshortreal: 32-bit vector -> shortreal (IEEE 754)
  if (instr.system_call_name == "$bitstoshortreal") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = ctx.GetOperandValue(instr.operands[0]);
    auto bits = static_cast<uint32_t>(src.AsNarrow().AsUInt64());
    auto shortreal_value = std::bit_cast<float>(bits);
    ctx.GetTempTable().Write(
        instr.result.value(), RuntimeValue::ShortReal(shortreal_value));
    return InstructionResult::Continue();
  }

  // $clog2: ceiling of log base 2 (arg treated as unsigned, 0 -> 0)
  if (instr.system_call_name == "$clog2") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = ctx.GetOperandValue(instr.operands[0]);
    uint64_t n = src.AsNarrow().AsUInt64();
    int result = (n == 0) ? 0 : std::bit_width(n - 1);
    ctx.GetTempTable().Write(
        instr.result.value(), RuntimeValue::IntegralSigned(result, 32));
    return InstructionResult::Continue();
  }

  std::unreachable();
}

}  // namespace lyra::interpreter

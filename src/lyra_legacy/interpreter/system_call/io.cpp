#include "lyra/interpreter/system_call/io.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <fstream>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/mem_io.hpp"
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

struct MemTargetInfo {
  bool is_unpacked = false;
  size_t element_width = 0;
  size_t element_count = 0;
  int32_t lower_bound = 0;
};

auto GetMemTargetInfo(const RuntimeValue& value, const common::Type& type)
    -> MemTargetInfo {
  if (type.kind == common::Type::Kind::kUnpackedArray) {
    const auto& array_data = std::get<common::UnpackedArrayData>(type.data);
    if (array_data.element_type->kind != common::Type::Kind::kIntegral) {
      throw common::InternalError(
          "interpreter", "readmem/writemem target element must be integral");
    }
    return {
        .is_unpacked = true,
        .element_width = array_data.element_type->GetBitWidth(),
        .element_count = value.AsArray().size(),
        .lower_bound = array_data.lower_bound,
    };
  }

  if (type.kind == common::Type::Kind::kIntegral) {
    const auto& integral = std::get<common::IntegralData>(type.data);
    if (integral.element_type != nullptr) {
      return {
          .is_unpacked = false,
          .element_width = integral.element_type->GetBitWidth(),
          .element_count = integral.element_count,
          .lower_bound = integral.element_lower,
      };
    }
    return {
        .is_unpacked = false,
        .element_width = integral.bit_width,
        .element_count = 1,
        .lower_bound = 0,
    };
  }

  throw common::InternalError(
      "interpreter", "readmem/writemem target must be an array");
}

auto ParseMemTokenToValue(std::string_view token, size_t bit_width, bool is_hex)
    -> RuntimeValue {
  auto words_result =
      common::mem_io::ParseMemTokenToWords(token, bit_width, is_hex);
  if (!words_result) {
    throw std::runtime_error(words_result.error());
  }
  auto words = std::move(*words_result);

  if (bit_width <= 64) {
    return RuntimeValue::IntegralUnsigned(
        words.empty() ? 0 : words[0], bit_width);
  }
  common::WideBit wide(std::move(words));
  return RuntimeValue::IntegralWide(std::move(wide), bit_width);
}

auto ExtractPackedElement(
    const RuntimeValue& value, size_t index, size_t element_width)
    -> RuntimeValue {
  size_t start_bit = index * element_width;
  if (element_width <= 64) {
    uint64_t mask = common::MakeBitMask(static_cast<uint32_t>(element_width));
    uint64_t extracted = 0;
    if (value.IsWide()) {
      auto shifted = value.AsWideBit().ShiftRightLogical(start_bit);
      extracted = shifted.GetWord(0) & mask;
    } else {
      extracted = (value.AsNarrow().AsUInt64() >> start_bit) & mask;
    }
    return RuntimeValue::IntegralUnsigned(extracted, element_width);
  }

  auto wide_value = value.IsWide() ? value.AsWideBit()
                                   : common::WideBit::FromUInt64(
                                         value.AsNarrow().AsUInt64(), 2);
  auto extracted = wide_value.ExtractSlice(start_bit, element_width);
  return RuntimeValue::IntegralWide(std::move(extracted), element_width);
}

auto StorePackedElement(
    const RuntimeValue& current, size_t index, size_t element_width,
    const RuntimeValue& new_value) -> RuntimeValue {
  size_t storage_width = current.type.GetBitWidth();
  bool storage_is_signed = current.type.IsSigned();
  bool storage_is_wide = current.IsWide();
  bool element_is_wide = element_width > 64;
  size_t shift = index * element_width;

  if (!storage_is_wide && !element_is_wide) {
    uint64_t elem_mask =
        common::MakeBitMask(static_cast<uint32_t>(element_width));
    uint64_t clear_mask = ~(elem_mask << shift);
    uint64_t merged = (current.AsNarrow().AsUInt64() & clear_mask) |
                      ((new_value.AsNarrow().AsUInt64() & elem_mask) << shift);
    return storage_is_signed
               ? RuntimeValue::IntegralSigned(
                     static_cast<int64_t>(merged), storage_width)
               : RuntimeValue::IntegralUnsigned(merged, storage_width);
  }

  size_t storage_words = common::wide_ops::WordsForBits(storage_width);
  auto current_wide = storage_is_wide
                          ? current.AsWideBit()
                          : common::WideBit::FromUInt64(
                                current.AsNarrow().AsUInt64(), storage_words);
  auto value_wide = element_is_wide
                        ? new_value.AsWideBit()
                        : common::WideBit::FromUInt64(
                              new_value.AsNarrow().AsUInt64(), storage_words);
  auto merged = current_wide.InsertSlice(value_wide, shift, element_width);
  return RuntimeValue::IntegralWide(
      std::move(merged), storage_width, storage_is_signed);
}

auto FormatMemValue(const RuntimeValue& value, size_t bit_width, bool is_hex)
    -> std::string {
  auto word_count = common::wide_ops::WordsForBits(bit_width);
  std::vector<uint64_t> words(word_count, 0);
  value.MatchIntegral(
      [&](RuntimeValue::NarrowIntegral n) {
        if (!words.empty()) {
          words[0] = n.AsUInt64();
        }
      },
      [&](const common::WideBit& w) {
        for (size_t i = 0; i < words.size(); ++i) {
          words[i] = w.GetWord(i);
        }
      });
  return common::mem_io::FormatMemWords(words, bit_width, is_hex);
}

/// Handles $readmem* and $writemem* system tasks.
auto HandleMemIO(
    const lir::Instruction& instr, bool is_read, bool is_hex,
    InstructionContext& ctx) -> InstructionResult {
  std::string_view task_name = is_read ? "$readmem" : "$writemem";
  // Operands: filename + optional start/end addresses (1-3 operands)
  // Target array is in output_targets
  if (instr.operands.empty() || instr.operands.size() > 3) {
    throw common::InternalError(
        "interpreter", std::format("{} expects 1-3 operands", task_name));
  }
  bool filename_is_string_literal = instr.format_string_is_literal;
  const auto filename_value = ctx.GetOperandValue(instr.operands[0]);
  std::string filename;
  if (filename_value.IsString()) {
    filename = filename_value.AsString();
  } else if (filename_is_string_literal && filename_value.IsTwoState()) {
    filename = IntegralToString(filename_value);
  } else {
    throw common::InternalError(
        "interpreter", std::format("{} filename must be a string", task_name));
  }
  // Target array is in output_targets (write target), not operands
  if (instr.output_targets.empty()) {
    throw common::InternalError(
        "interpreter",
        std::format(
            "{} target must be specified in output_targets", task_name));
  }
  auto target_symbol = instr.output_targets[0];
  auto target_value = ctx.ReadVariable(target_symbol);
  auto info = GetMemTargetInfo(target_value, target_value.type);

  // Optional start/end addresses in operands[1] and operands[2]
  std::optional<int64_t> start_addr;
  std::optional<int64_t> end_addr;
  if (instr.operands.size() >= 2) {
    auto start_val = ctx.GetOperandValue(instr.operands[1]);
    if (!start_val.IsTwoState() || start_val.IsWide()) {
      throw common::InternalError(
          "interpreter",
          std::format("{} start address must be narrow integral", task_name));
    }
    start_addr = start_val.AsNarrow().AsInt64();
  }
  if (instr.operands.size() == 3) {
    auto end_val = ctx.GetOperandValue(instr.operands[2]);
    if (!end_val.IsTwoState() || end_val.IsWide()) {
      throw common::InternalError(
          "interpreter",
          std::format("{} end address must be narrow integral", task_name));
    }
    end_addr = end_val.AsNarrow().AsInt64();
  }

  if (info.element_count == 0 || info.element_width == 0) {
    throw common::InternalError(
        "interpreter", std::format("{} target has zero size", task_name));
  }

  int64_t min_addr = info.lower_bound;
  int64_t max_addr =
      info.lower_bound + static_cast<int64_t>(info.element_count) - 1;
  int64_t current_addr = start_addr.value_or(min_addr);
  int64_t final_addr = end_addr.value_or(max_addr);

  if (current_addr < min_addr || current_addr > max_addr) {
    throw common::InternalError(
        "interpreter",
        std::format("{} start address out of bounds", task_name));
  }
  if (final_addr < min_addr || final_addr > max_addr) {
    throw common::InternalError(
        "interpreter", std::format("{} end address out of bounds", task_name));
  }

  auto path = common::mem_io::ResolveMemPath(filename);
  auto handle_read_mem = [&]() -> InstructionResult {
    std::ifstream in(path);
    if (!in) {
      throw common::InternalError(
          "interpreter",
          std::format("failed to open memory file: {}", path.string()));
    }

    std::string content(
        (std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());

    RuntimeValue packed_value = target_value;
    auto write_value = [&](int64_t addr, const RuntimeValue& value) {
      if (addr < min_addr || addr > max_addr) {
        throw common::InternalError(
            "interpreter", std::format("{} address out of bounds", task_name));
      }
      auto index = static_cast<size_t>(addr - min_addr);
      if (info.is_unpacked) {
        target_value.AsArray()[index] = value;
      } else {
        packed_value =
            StorePackedElement(packed_value, index, info.element_width, value);
      }
    };

    auto parse_result = common::mem_io::ParseMemFile(
        content, is_hex, min_addr, max_addr, current_addr, final_addr,
        task_name, [&](std::string_view token, int64_t addr) {
          auto value = ParseMemTokenToValue(token, info.element_width, is_hex);
          write_value(addr, value);
        });
    if (!parse_result.success) {
      throw std::runtime_error(parse_result.error);
    }

    if (info.is_unpacked) {
      ctx.StoreVariable(target_symbol, target_value, false);
    } else {
      ctx.StoreVariable(target_symbol, packed_value, false);
    }
    return InstructionResult::Continue();
  };

  auto handle_write_mem = [&]() -> InstructionResult {
    std::ofstream out(path);
    if (!out) {
      throw common::InternalError(
          "interpreter",
          std::format(
              "failed to open memory file for write: {}", path.string()));
    }

    if (start_addr.has_value()) {
      out << "@"
          << common::mem_io::FormatMemAddress(
                 static_cast<uint64_t>(*start_addr), is_hex)
          << "\n";
    }

    for (int64_t addr = current_addr; addr <= final_addr; ++addr) {
      auto index = static_cast<size_t>(addr - min_addr);
      RuntimeValue element;
      if (info.is_unpacked) {
        element = target_value.AsArray()[index];
      } else {
        element = ExtractPackedElement(target_value, index, info.element_width);
      }
      out << FormatMemValue(element, info.element_width, is_hex) << "\n";
    }
    return InstructionResult::Continue();
  };

  return is_read ? handle_read_mem() : handle_write_mem();
}

}  // namespace

auto HandleMemIoCalls(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  if (instr.system_call_name == "$readmemh" ||
      instr.system_call_name == "$readmemb") {
    bool is_hex = instr.system_call_name == "$readmemh";
    return HandleMemIO(instr, true, is_hex, ctx);
  }

  if (instr.system_call_name == "$writememh" ||
      instr.system_call_name == "$writememb") {
    bool is_hex = instr.system_call_name == "$writememh";
    return HandleMemIO(instr, false, is_hex, ctx);
  }

  std::unreachable();
}

}  // namespace lyra::interpreter

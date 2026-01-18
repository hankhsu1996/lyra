#include "lyra/mir/interp/runtime_value.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <ranges>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"

namespace lyra::mir::interp {

namespace {

constexpr uint32_t kBitsPerWord = 64;

auto WordsNeeded(uint32_t bit_width) -> size_t {
  return (bit_width + kBitsPerWord - 1) / kBitsPerWord;
}

auto GetMask(uint32_t bit_width) -> uint64_t {
  if (bit_width == 0 || bit_width >= kBitsPerWord) {
    return ~uint64_t{0};
  }
  return (uint64_t{1} << bit_width) - 1;
}

// Mask the top word to the actual bit width
void MaskTopWord(std::vector<uint64_t>& words, uint32_t bit_width) {
  if (words.empty() || bit_width == 0) {
    return;
  }
  uint32_t top_bits = bit_width % kBitsPerWord;
  if (top_bits != 0) {
    words.back() &= GetMask(top_bits);
  }
}

}  // namespace

auto RuntimeIntegral::IsZero() const -> bool {
  if (!IsKnown()) {
    return false;
  }
  return std::ranges::all_of(value, [](uint64_t w) { return w == 0; });
}

auto RuntimeIntegral::IsX() const -> bool {
  return std::ranges::any_of(x_mask, [](uint64_t w) { return w != 0; });
}

auto RuntimeIntegral::IsZ() const -> bool {
  return std::ranges::any_of(z_mask, [](uint64_t w) { return w != 0; });
}

auto RuntimeIntegral::IsKnown() const -> bool {
  return !IsX() && !IsZ();
}

auto RuntimeIntegral::IsAllOnes() const -> bool {
  if (!IsKnown()) {
    return false;
  }
  if (value.empty()) {
    return false;
  }

  // Check all but the last word
  for (size_t i = 0; i + 1 < value.size(); ++i) {
    if (value[i] != ~uint64_t{0}) {
      return false;
    }
  }

  // Check the last word with mask
  uint32_t top_bits = bit_width % kBitsPerWord;
  uint64_t mask = (top_bits == 0) ? ~uint64_t{0} : GetMask(top_bits);
  return value.back() == mask;
}

auto MakeIntegral(uint64_t val, uint32_t bit_width) -> RuntimeValue {
  size_t num_words = WordsNeeded(bit_width);
  RuntimeIntegral result;
  result.bit_width = bit_width;
  result.value.resize(num_words, 0);
  result.x_mask.resize(num_words, 0);
  result.z_mask.resize(num_words, 0);
  if (!result.value.empty()) {
    result.value[0] = val;
    MaskTopWord(result.value, bit_width);
  }
  return result;
}

auto MakeIntegralX(uint32_t bit_width) -> RuntimeValue {
  size_t num_words = WordsNeeded(bit_width);
  RuntimeIntegral result;
  result.bit_width = bit_width;
  result.value.resize(num_words, 0);
  result.x_mask.resize(num_words, ~uint64_t{0});  // All bits X
  result.z_mask.resize(num_words, 0);
  MaskTopWord(result.x_mask, bit_width);
  return result;
}

auto MakeIntegralFromConstant(const IntegralConstant& c, uint32_t bit_width)
    -> RuntimeValue {
  RuntimeIntegral result;
  result.bit_width = bit_width;
  result.value = c.value;
  result.x_mask = c.x_mask;
  result.z_mask = c.z_mask;

  // Ensure proper size
  size_t num_words = WordsNeeded(bit_width);
  result.value.resize(num_words, 0);
  result.x_mask.resize(num_words, 0);
  result.z_mask.resize(num_words, 0);
  MaskTopWord(result.value, bit_width);
  MaskTopWord(result.x_mask, bit_width);
  MaskTopWord(result.z_mask, bit_width);

  return result;
}

auto MakeString(std::string val) -> RuntimeValue {
  return RuntimeString{.value = std::move(val)};
}

auto MakeReal(double val) -> RuntimeValue {
  return RuntimeReal{.value = val};
}

auto MakeStruct(TypeId type, std::vector<RuntimeValue> fields) -> RuntimeValue {
  auto s = std::make_unique<RuntimeStruct>();
  s->type = type;
  s->fields = std::move(fields);
  return s;
}

auto MakeArray(std::vector<RuntimeValue> elements) -> RuntimeValue {
  auto a = std::make_unique<RuntimeArray>();
  a->elements = std::move(elements);
  return a;
}

auto Clone(const RuntimeValue& v) -> RuntimeValue {
  return std::visit(
      [](const auto& val) -> RuntimeValue {
        using T = std::decay_t<decltype(val)>;
        if constexpr (std::is_same_v<T, std::monostate>) {
          return std::monostate{};
        } else if constexpr (std::is_same_v<T, RuntimeIntegral>) {
          return val;
        } else if constexpr (std::is_same_v<T, RuntimeString>) {
          return val;
        } else if constexpr (std::is_same_v<T, RuntimeReal>) {
          return val;  // Trivial copy
        } else if constexpr (std::is_same_v<
                                 T, std::unique_ptr<RuntimeStruct>>) {
          auto copy = std::make_unique<RuntimeStruct>();
          copy->type = val->type;
          copy->fields.reserve(val->fields.size());
          for (const auto& f : val->fields) {
            copy->fields.push_back(Clone(f));
          }
          return copy;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<RuntimeArray>>) {
          auto copy = std::make_unique<RuntimeArray>();
          copy->elements.reserve(val->elements.size());
          for (const auto& e : val->elements) {
            copy->elements.push_back(Clone(e));
          }
          return copy;
        }
      },
      v);
}

auto IsIntegral(const RuntimeValue& v) -> bool {
  return std::holds_alternative<RuntimeIntegral>(v);
}

auto IsString(const RuntimeValue& v) -> bool {
  return std::holds_alternative<RuntimeString>(v);
}

auto IsReal(const RuntimeValue& v) -> bool {
  return std::holds_alternative<RuntimeReal>(v);
}

auto IsStruct(const RuntimeValue& v) -> bool {
  return std::holds_alternative<std::unique_ptr<RuntimeStruct>>(v);
}

auto IsArray(const RuntimeValue& v) -> bool {
  return std::holds_alternative<std::unique_ptr<RuntimeArray>>(v);
}

auto AsIntegral(RuntimeValue& v) -> RuntimeIntegral& {
  return std::get<RuntimeIntegral>(v);
}

auto AsIntegral(const RuntimeValue& v) -> const RuntimeIntegral& {
  return std::get<RuntimeIntegral>(v);
}

auto AsString(RuntimeValue& v) -> RuntimeString& {
  return std::get<RuntimeString>(v);
}

auto AsString(const RuntimeValue& v) -> const RuntimeString& {
  return std::get<RuntimeString>(v);
}

auto AsReal(RuntimeValue& v) -> RuntimeReal& {
  return std::get<RuntimeReal>(v);
}

auto AsReal(const RuntimeValue& v) -> const RuntimeReal& {
  return std::get<RuntimeReal>(v);
}

auto AsStruct(RuntimeValue& v) -> RuntimeStruct& {
  return *std::get<std::unique_ptr<RuntimeStruct>>(v);
}

auto AsStruct(const RuntimeValue& v) -> const RuntimeStruct& {
  return *std::get<std::unique_ptr<RuntimeStruct>>(v);
}

auto AsArray(RuntimeValue& v) -> RuntimeArray& {
  return *std::get<std::unique_ptr<RuntimeArray>>(v);
}

auto AsArray(const RuntimeValue& v) -> const RuntimeArray& {
  return *std::get<std::unique_ptr<RuntimeArray>>(v);
}

auto ToString(const RuntimeValue& v) -> std::string {
  return std::visit(
      [](const auto& val) -> std::string {
        using T = std::decay_t<decltype(val)>;
        if constexpr (std::is_same_v<T, std::monostate>) {
          return "<void>";
        } else if constexpr (std::is_same_v<T, RuntimeIntegral>) {
          return ToDecimalString(val, false);
        } else if constexpr (std::is_same_v<T, RuntimeString>) {
          return val.value;
        } else if constexpr (std::is_same_v<T, RuntimeReal>) {
          return std::format("{:g}", val.value);
        } else if constexpr (std::is_same_v<
                                 T, std::unique_ptr<RuntimeStruct>>) {
          std::string result = "'{";
          for (size_t i = 0; i < val->fields.size(); ++i) {
            if (i > 0) {
              result += ", ";
            }
            result += ToString(val->fields[i]);
          }
          result += "}";
          return result;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<RuntimeArray>>) {
          std::string result = "[";
          for (size_t i = 0; i < val->elements.size(); ++i) {
            if (i > 0) {
              result += ", ";
            }
            result += ToString(val->elements[i]);
          }
          result += "]";
          return result;
        }
      },
      v);
}

auto ToDecimalString(const RuntimeIntegral& v, bool is_signed) -> std::string {
  if (v.IsX()) {
    return "x";
  }
  if (v.IsZ()) {
    return "z";
  }

  if (v.bit_width <= 64) {
    uint64_t val = v.value.empty() ? 0 : v.value[0];
    if (is_signed) {
      auto sval = static_cast<int64_t>(val);
      // Sign extend if needed
      if (v.bit_width < 64) {
        uint64_t sign_bit = uint64_t{1} << (v.bit_width - 1);
        if ((val & sign_bit) != 0) {
          sval =
              static_cast<int64_t>(val | ~((uint64_t{1} << v.bit_width) - 1));
        }
      }
      return std::format("{}", sval);
    }
    return std::format("{}", val);
  }

  // Multi-word: just show hex for now
  std::string result = "0x";
  for (uint64_t word : std::ranges::reverse_view(v.value)) {
    result += std::format("{:016x}", word);
  }
  return result;
}

auto ToHexString(const RuntimeIntegral& v) -> std::string {
  if (v.IsX()) {
    return "x";
  }
  if (v.IsZ()) {
    return "z";
  }

  if (v.bit_width <= 64) {
    uint64_t val = v.value.empty() ? 0 : v.value[0];
    return std::format("{:x}", val);
  }

  std::string result;
  for (uint64_t word : std::ranges::reverse_view(v.value)) {
    if (result.empty()) {
      result = std::format("{:x}", word);
    } else {
      result += std::format("{:016x}", word);
    }
  }
  return result;
}

auto ToBinaryString(const RuntimeIntegral& v) -> std::string {
  if (v.IsX()) {
    return "x";
  }
  if (v.IsZ()) {
    return "z";
  }

  if (v.bit_width <= 64) {
    uint64_t val = v.value.empty() ? 0 : v.value[0];
    return std::format("{:b}", val);
  }

  std::string result;
  for (uint64_t word : std::ranges::reverse_view(v.value)) {
    if (result.empty()) {
      result = std::format("{:b}", word);
    } else {
      result += std::format("{:064b}", word);
    }
  }
  return result;
}

auto ToOctalString(const RuntimeIntegral& v) -> std::string {
  if (v.IsX()) {
    return "x";
  }
  if (v.IsZ()) {
    return "z";
  }

  if (v.bit_width <= 64) {
    uint64_t val = v.value.empty() ? 0 : v.value[0];
    return std::format("{:o}", val);
  }

  // Multi-word: convert to decimal for simplicity
  return ToDecimalString(v, false);
}

}  // namespace lyra::mir::interp

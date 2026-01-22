#include "lyra/semantic/value.hpp"

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

namespace lyra::semantic {

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

auto RuntimeIntegral::IsAllX() const -> bool {
  if (x_mask.empty() || bit_width == 0) {
    return false;
  }
  // Check all but the last word are all 1s
  for (size_t i = 0; i + 1 < x_mask.size(); ++i) {
    if (x_mask[i] != ~uint64_t{0}) {
      return false;
    }
  }
  // Check the last word with mask for actual bit width
  uint32_t top_bits = bit_width % kBitsPerWord;
  uint64_t mask = (top_bits == 0) ? ~uint64_t{0} : GetMask(top_bits);
  return x_mask.back() == mask;
}

auto RuntimeIntegral::IsAllZ() const -> bool {
  if (z_mask.empty() || bit_width == 0) {
    return false;
  }
  // Check all but the last word are all 1s
  for (size_t i = 0; i + 1 < z_mask.size(); ++i) {
    if (z_mask[i] != ~uint64_t{0}) {
      return false;
    }
  }
  // Check the last word with mask for actual bit width
  uint32_t top_bits = bit_width % kBitsPerWord;
  uint64_t mask = (top_bits == 0) ? ~uint64_t{0} : GetMask(top_bits);
  return z_mask.back() == mask;
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

auto MakeIntegralSigned(int64_t val, uint32_t bit_width) -> RuntimeValue {
  size_t num_words = WordsNeeded(bit_width);
  RuntimeIntegral result;
  result.bit_width = bit_width;
  result.x_mask.resize(num_words, 0);
  result.z_mask.resize(num_words, 0);

  // Sign-extend: if negative and bit_width > 64, fill high words with 1s
  if (val < 0 && num_words > 1) {
    result.value.resize(num_words, ~uint64_t{0});  // All 1s for sign extension
  } else {
    result.value.resize(num_words, 0);
  }
  if (!result.value.empty()) {
    result.value[0] = static_cast<uint64_t>(val);
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

auto MakeShortReal(float val) -> RuntimeValue {
  return RuntimeShortReal{.value = val};
}

auto MakeStruct(std::vector<RuntimeValue> fields) -> RuntimeValue {
  auto s = std::make_unique<RuntimeStruct>();
  s->fields = std::move(fields);
  return s;
}

auto MakeArray(std::vector<RuntimeValue> elements) -> RuntimeValue {
  auto a = std::make_unique<RuntimeArray>();
  a->elements = std::move(elements);
  return a;
}

auto MakeUnion(RuntimeIntegral storage_bits) -> RuntimeValue {
  auto u = std::make_unique<RuntimeUnion>();
  u->storage_bits = std::move(storage_bits);
  return u;
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
        } else if constexpr (std::is_same_v<T, RuntimeShortReal>) {
          return val;  // Trivial copy
        } else if constexpr (std::is_same_v<
                                 T, std::unique_ptr<RuntimeStruct>>) {
          auto copy = std::make_unique<RuntimeStruct>();
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
        } else if constexpr (std::is_same_v<T, std::unique_ptr<RuntimeUnion>>) {
          auto copy = std::make_unique<RuntimeUnion>();
          copy->storage_bits = val->storage_bits;
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

auto IsShortReal(const RuntimeValue& v) -> bool {
  return std::holds_alternative<RuntimeShortReal>(v);
}

auto IsStruct(const RuntimeValue& v) -> bool {
  return std::holds_alternative<std::unique_ptr<RuntimeStruct>>(v);
}

auto IsArray(const RuntimeValue& v) -> bool {
  return std::holds_alternative<std::unique_ptr<RuntimeArray>>(v);
}

auto IsUnion(const RuntimeValue& v) -> bool {
  return std::holds_alternative<std::unique_ptr<RuntimeUnion>>(v);
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

auto AsShortReal(RuntimeValue& v) -> RuntimeShortReal& {
  return std::get<RuntimeShortReal>(v);
}

auto AsShortReal(const RuntimeValue& v) -> const RuntimeShortReal& {
  return std::get<RuntimeShortReal>(v);
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

auto AsUnion(RuntimeValue& v) -> RuntimeUnion& {
  return *std::get<std::unique_ptr<RuntimeUnion>>(v);
}

auto AsUnion(const RuntimeValue& v) -> const RuntimeUnion& {
  return *std::get<std::unique_ptr<RuntimeUnion>>(v);
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
        } else if constexpr (std::is_same_v<T, RuntimeShortReal>) {
          return std::format("{:g}", static_cast<double>(val.value));
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
        } else if constexpr (std::is_same_v<T, std::unique_ptr<RuntimeUnion>>) {
          return std::format("<union[{}]>", ToHexString(val->storage_bits));
        }
      },
      v);
}

auto ToDecimalString(const RuntimeIntegral& v, bool is_signed) -> std::string {
  // LRM 21.2.1.3: lowercase x/z when ALL bits are X/Z, uppercase when partial
  if (v.IsX()) {
    return v.IsAllX() ? "x" : "X";
  }
  if (v.IsZ()) {
    return v.IsAllZ() ? "z" : "Z";
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

  // Multi-word: convert to decimal by dividing by 10^19 and emitting chunks.
  // 10^19 is the largest power of 10 that fits in uint64_t.
  constexpr uint64_t kChunkDivisor = 10000000000000000000ULL;  // 10^19

  std::vector<uint64_t> words = v.value;
  size_t num_words = WordsNeeded(v.bit_width);
  words.resize(num_words, 0);
  MaskTopWord(words, v.bit_width);

  // Check for negative signed value (sign bit set)
  bool is_negative = false;
  if (is_signed && v.bit_width > 0) {
    uint32_t sign_bit_pos = (v.bit_width - 1) % kBitsPerWord;
    size_t sign_word_idx = (v.bit_width - 1) / kBitsPerWord;
    if (sign_word_idx < words.size() &&
        (words[sign_word_idx] & (uint64_t{1} << sign_bit_pos)) != 0) {
      is_negative = true;
      // Two's complement: ~value + 1
      uint64_t carry = 1;
      for (uint64_t& word : words) {
        word = ~word;
        uint64_t sum = word + carry;
        carry = (sum < word) ? 1 : 0;
        word = sum;
      }
      MaskTopWord(words, v.bit_width);
    }
  }

  // Remove leading zero words
  while (words.size() > 1 && words.back() == 0) {
    words.pop_back();
  }

  if (words.size() == 1 && words[0] == 0) {
    return "0";
  }

  // Extract chunks by repeatedly dividing by 10^19
  std::vector<uint64_t> chunks;
  while (words.size() > 1 || words[0] != 0) {
    uint64_t remainder = 0;
    for (size_t i = words.size(); i > 0; --i) {
      auto combined =
          (static_cast<__uint128_t>(remainder) << 64) | words[i - 1];
      words[i - 1] = static_cast<uint64_t>(combined / kChunkDivisor);
      remainder = static_cast<uint64_t>(combined % kChunkDivisor);
    }
    chunks.push_back(remainder);

    while (words.size() > 1 && words.back() == 0) {
      words.pop_back();
    }
  }

  // Build result: most significant chunk without padding, rest zero-padded
  std::string result = is_negative ? "-" : "";
  result += std::format("{}", chunks.back());
  for (size_t i = chunks.size() - 1; i > 0; --i) {
    result += std::format("{:019}", chunks[i - 1]);
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

}  // namespace lyra::semantic

#include "lyra/semantic/value.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"

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
  // X: unknown=1, value=0
  for (size_t i = 0; i < unknown.size(); ++i) {
    uint64_t vi = (i < value.size()) ? value[i] : 0;
    if ((unknown[i] & ~vi) != 0) {
      return true;
    }
  }
  return false;
}

auto RuntimeIntegral::IsZ() const -> bool {
  // Z: unknown=1, value=1
  for (size_t i = 0; i < unknown.size(); ++i) {
    uint64_t vi = (i < value.size()) ? value[i] : 0;
    if ((unknown[i] & vi) != 0) {
      return true;
    }
  }
  return false;
}

auto RuntimeIntegral::IsAllX() const -> bool {
  if (unknown.empty() || bit_width == 0) {
    return false;
  }
  // All X: value=0, unknown=all-ones (within bit_width)
  uint32_t top_bits = bit_width % kBitsPerWord;
  uint64_t top_mask = (top_bits == 0) ? ~uint64_t{0} : GetMask(top_bits);
  for (size_t i = 0; i < unknown.size(); ++i) {
    uint64_t vi = (i < value.size()) ? value[i] : 0;
    uint64_t word_mask = (i + 1 == unknown.size()) ? top_mask : ~uint64_t{0};
    if (vi != 0 || unknown[i] != word_mask) {
      return false;
    }
  }
  return true;
}

auto RuntimeIntegral::IsAllZ() const -> bool {
  if (unknown.empty() || bit_width == 0) {
    return false;
  }
  // All Z: value=all-ones, unknown=all-ones (within bit_width)
  uint32_t top_bits = bit_width % kBitsPerWord;
  uint64_t top_mask = (top_bits == 0) ? ~uint64_t{0} : GetMask(top_bits);
  for (size_t i = 0; i < unknown.size(); ++i) {
    uint64_t vi = (i < value.size()) ? value[i] : 0;
    uint64_t word_mask = (i + 1 == unknown.size()) ? top_mask : ~uint64_t{0};
    if (vi != word_mask || unknown[i] != word_mask) {
      return false;
    }
  }
  return true;
}

auto RuntimeIntegral::IsKnown() const -> bool {
  return std::ranges::all_of(unknown, [](uint64_t w) { return w == 0; });
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
  result.unknown.resize(num_words, 0);
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
  result.unknown.resize(num_words, 0);

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
  result.unknown.resize(num_words, ~uint64_t{0});
  MaskTopWord(result.unknown, bit_width);
  return result;
}

auto MakeIntegralWide(
    const uint64_t* words, size_t num_words, uint32_t bit_width)
    -> RuntimeValue {
  if (words == nullptr) {
    throw lyra::common::InternalError("MakeIntegralWide", "words is null");
  }

  size_t expected = WordsNeeded(bit_width);
  if (num_words != expected) {
    throw lyra::common::InternalError(
        "MakeIntegralWide",
        std::format(
            "word count mismatch: got {}, expected {}", num_words, expected));
  }

  // Wrap in span to avoid pointer arithmetic
  std::span<const uint64_t> words_span(words, num_words);

  // Validate top word has no garbage padding bits (caller must mask)
  uint32_t top_bits = bit_width % kBitsPerWord;
  if (top_bits != 0) {
    uint64_t top_mask = (uint64_t{1} << top_bits) - 1;
    if ((words_span.back() & ~top_mask) != 0) {
      throw lyra::common::InternalError(
          "MakeIntegralWide",
          "top word has nonzero padding bits (caller must mask)");
    }
  }

  RuntimeIntegral result;
  result.bit_width = bit_width;
  result.value.assign(words_span.begin(), words_span.end());
  result.unknown.resize(num_words, 0);  // 2-state: all known
  return RuntimeValue{std::move(result)};
}

auto MakeIntegralWide(
    const uint64_t* value_words, const uint64_t* unknown_words,
    size_t num_words, uint32_t bit_width) -> RuntimeValue {
  if (value_words == nullptr) {
    throw lyra::common::InternalError(
        "MakeIntegralWide (4-state)", "value_words is null");
  }
  if (unknown_words == nullptr) {
    throw lyra::common::InternalError(
        "MakeIntegralWide (4-state)", "unknown_words is null");
  }

  size_t expected = WordsNeeded(bit_width);
  if (num_words != expected) {
    throw lyra::common::InternalError(
        "MakeIntegralWide (4-state)",
        std::format(
            "word count mismatch: got {}, expected {}", num_words, expected));
  }

  std::span<const uint64_t> value_span(value_words, num_words);
  std::span<const uint64_t> unknown_span(unknown_words, num_words);

  // Validate top word has no garbage padding bits (caller must mask)
  uint32_t top_bits = bit_width % kBitsPerWord;
  if (top_bits != 0) {
    uint64_t top_mask = (uint64_t{1} << top_bits) - 1;
    if ((value_span.back() & ~top_mask) != 0) {
      throw lyra::common::InternalError(
          "MakeIntegralWide (4-state)",
          "value top word has nonzero padding bits (caller must mask)");
    }
    if ((unknown_span.back() & ~top_mask) != 0) {
      throw lyra::common::InternalError(
          "MakeIntegralWide (4-state)",
          "unknown top word has nonzero padding bits (caller must mask)");
    }
  }

  RuntimeIntegral result;
  result.bit_width = bit_width;
  result.value.assign(value_span.begin(), value_span.end());
  result.unknown.assign(unknown_span.begin(), unknown_span.end());
  return RuntimeValue{std::move(result)};
}

auto MakeIntegralFromConstant(const IntegralConstant& c, uint32_t bit_width)
    -> RuntimeValue {
  RuntimeIntegral result;
  result.bit_width = bit_width;
  result.value = c.value;
  result.unknown = c.unknown;

  // Ensure proper size
  size_t num_words = WordsNeeded(bit_width);
  result.value.resize(num_words, 0);
  result.unknown.resize(num_words, 0);
  MaskTopWord(result.value, bit_width);
  MaskTopWord(result.unknown, bit_width);

  return result;
}

auto MakeIntegralFilled(uint32_t bit_width, bool value_bit, bool unknown_bit)
    -> RuntimeValue {
  // Contract: creates an integral with all bits set to the same 4-state value
  // value_bit=0, unknown_bit=0 → all 0
  // value_bit=1, unknown_bit=0 → all 1
  // value_bit=0, unknown_bit=1 → all X
  // value_bit=1, unknown_bit=1 → all Z
  if (bit_width == 0) {
    throw lyra::common::InternalError(
        "MakeIntegralFilled", "bit_width must be > 0");
  }

  size_t num_words = WordsNeeded(bit_width);
  uint64_t value_pattern = value_bit ? ~uint64_t{0} : uint64_t{0};
  uint64_t unknown_pattern = unknown_bit ? ~uint64_t{0} : uint64_t{0};

  RuntimeIntegral result;
  result.bit_width = bit_width;
  result.value.assign(num_words, value_pattern);
  result.unknown.assign(num_words, unknown_pattern);
  MaskTopWord(result.value, bit_width);
  MaskTopWord(result.unknown, bit_width);

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

// Helper to get bit at position from multi-word vector
auto GetBit(const std::vector<uint64_t>& words, uint32_t bit_pos) -> bool {
  size_t word_idx = bit_pos / 64;
  uint32_t bit_in_word = bit_pos % 64;
  if (word_idx >= words.size()) {
    return false;
  }
  return ((words[word_idx] >> bit_in_word) & 1) != 0;
}

auto ToHexString(const RuntimeIntegral& v) -> std::string {
  if (v.bit_width == 0) {
    return "0";
  }

  // Shortcut: if no unknown bits, use fast path
  if (v.IsKnown()) {
    if (v.bit_width <= 64) {
      uint64_t val = v.value.empty() ? 0 : v.value[0];
      return std::format("{:x}", val);
    }
    // Multi-word: skip leading zero words, keep at least one
    std::string result;
    bool started = false;
    for (uint64_t word : std::ranges::reverse_view(v.value)) {
      if (!started && word == 0) {
        continue;  // Skip leading zero words
      }
      if (!started) {
        result = std::format("{:x}", word);
        started = true;
      } else {
        result += std::format("{:016x}", word);
      }
    }
    return result.empty() ? "0" : result;
  }

  // 4-state path: process nibble by nibble (LRM 21.2.1.3)
  // All-X nibble → 'x', all-Z nibble → 'z', mixed unknown → 'X'
  std::string result;
  uint32_t num_nibbles = (v.bit_width + 3) / 4;

  for (uint32_t n = num_nibbles; n > 0; --n) {
    uint32_t nibble_start = (n - 1) * 4;
    uint32_t nibble_bits = std::min(4U, v.bit_width - nibble_start);

    uint32_t nibble_val = 0;
    uint32_t nibble_unk = 0;
    for (uint32_t b = 0; b < nibble_bits; ++b) {
      if (GetBit(v.value, nibble_start + b)) {
        nibble_val |= (1U << b);
      }
      if (GetBit(v.unknown, nibble_start + b)) {
        nibble_unk |= (1U << b);
      }
    }

    // Mask for valid bits in this nibble
    uint32_t nibble_mask = (1U << nibble_bits) - 1;

    if (nibble_unk == 0) {
      // Known: output hex digit
      constexpr std::string_view kHexDigits = "0123456789abcdef";
      result += kHexDigits[nibble_val];
    } else if (nibble_unk == nibble_mask && nibble_val == 0) {
      // All X (unk=1, val=0 for all bits)
      result += 'x';
    } else if (nibble_unk == nibble_mask && nibble_val == nibble_mask) {
      // All Z (unk=1, val=1 for all bits)
      result += 'z';
    } else {
      // Mixed unknown: uppercase X
      result += 'X';
    }
  }

  // Strip leading zeros (but keep at least one digit)
  auto first_nonzero = result.find_first_not_of('0');
  if (first_nonzero != std::string::npos && first_nonzero > 0) {
    result = result.substr(first_nonzero);
  }
  return result.empty() ? "0" : result;
}

auto ToBinaryString(const RuntimeIntegral& v) -> std::string {
  if (v.bit_width == 0) {
    return "0";
  }

  // Shortcut: if no unknown bits, use fast path
  if (v.IsKnown()) {
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

  // 4-state path: process bit by bit
  std::string result;
  result.reserve(v.bit_width);

  for (uint32_t i = v.bit_width; i > 0; --i) {
    uint32_t bit_pos = i - 1;
    bool val_bit = GetBit(v.value, bit_pos);
    bool unk_bit = GetBit(v.unknown, bit_pos);

    if (!unk_bit) {
      result += val_bit ? '1' : '0';
    } else {
      result += val_bit ? 'z' : 'x';
    }
  }

  // Strip leading zeros (but keep at least one digit)
  auto first_nonzero = result.find_first_not_of('0');
  if (first_nonzero != std::string::npos && first_nonzero > 0) {
    result = result.substr(first_nonzero);
  }
  return result.empty() ? "0" : result;
}

auto ToOctalString(const RuntimeIntegral& v) -> std::string {
  if (v.bit_width == 0) {
    return "0";
  }

  // Shortcut: if no unknown bits and fits in 64 bits, use fast path
  if (v.IsKnown() && v.bit_width <= 64) {
    uint64_t val = v.value.empty() ? 0 : v.value[0];
    return std::format("{:o}", val);
  }

  // Process 3 bits at a time (octal digit)
  // For known values, octet_unk is always 0
  // For 4-state: All-X → 'x', all-Z → 'z', mixed unknown → 'X'
  std::string result;
  uint32_t num_octets = (v.bit_width + 2) / 3;

  for (uint32_t n = num_octets; n > 0; --n) {
    uint32_t octet_start = (n - 1) * 3;
    uint32_t octet_bits = std::min(3U, v.bit_width - octet_start);

    uint32_t octet_val = 0;
    uint32_t octet_unk = 0;
    for (uint32_t b = 0; b < octet_bits; ++b) {
      if (GetBit(v.value, octet_start + b)) {
        octet_val |= (1U << b);
      }
      if (GetBit(v.unknown, octet_start + b)) {
        octet_unk |= (1U << b);
      }
    }

    // Mask for valid bits in this octet
    uint32_t octet_mask = (1U << octet_bits) - 1;

    if (octet_unk == 0) {
      // Known: output octal digit
      result += static_cast<char>('0' + octet_val);
    } else if (octet_unk == octet_mask && octet_val == 0) {
      // All X
      result += 'x';
    } else if (octet_unk == octet_mask && octet_val == octet_mask) {
      // All Z
      result += 'z';
    } else {
      // Mixed unknown: uppercase X
      result += 'X';
    }
  }

  // Strip leading zeros (but keep at least one digit)
  auto first_nonzero = result.find_first_not_of('0');
  if (first_nonzero != std::string::npos && first_nonzero > 0) {
    result = result.substr(first_nonzero);
  }
  return result.empty() ? "0" : result;
}

}  // namespace lyra::semantic

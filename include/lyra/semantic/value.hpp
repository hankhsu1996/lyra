#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/assoc_array_handle.hpp"
#include "lyra/common/constant.hpp"

namespace lyra::semantic {

struct RuntimeIntegral {
  std::vector<uint64_t> value;    // Value/Z bits
  std::vector<uint64_t> unknown;  // Unknown bits (0 = known)
  uint32_t bit_width;

  [[nodiscard]] auto IsZero() const -> bool;
  [[nodiscard]] auto IsX() const -> bool;
  [[nodiscard]] auto IsZ() const -> bool;
  [[nodiscard]] auto IsAllX() const -> bool;
  [[nodiscard]] auto IsAllZ() const -> bool;
  [[nodiscard]] auto IsKnown() const -> bool;
  [[nodiscard]] auto IsAllOnes() const -> bool;
};

struct RuntimeString {
  std::string value;
};

struct RuntimeReal {
  double value;
};

struct RuntimeShortReal {
  float value;
};

// Forward declarations for recursive types
struct RuntimeStruct;
struct RuntimeArray;
struct RuntimeUnion;

using RuntimeValue = std::variant<
    std::monostate, RuntimeIntegral, RuntimeString, RuntimeReal,
    RuntimeShortReal, std::unique_ptr<RuntimeStruct>,
    std::unique_ptr<RuntimeArray>, std::unique_ptr<RuntimeUnion>,
    runtime::AssocArrayHandle>;

struct RuntimeStruct {
  std::vector<RuntimeValue> fields;
};

struct RuntimeArray {
  std::vector<RuntimeValue> elements;
};

struct RuntimeUnion {
  RuntimeIntegral
      storage_bits;  // Single storage blob, width = max member width
};

// Factory functions
auto MakeIntegral(uint64_t value, uint32_t bit_width) -> RuntimeValue;
auto MakeIntegralSigned(int64_t value, uint32_t bit_width)
    -> RuntimeValue;  // Sign-extends for wide values
auto MakeIntegralX(uint32_t bit_width) -> RuntimeValue;  // All bits unknown (X)
auto MakeIntegralWide(
    const uint64_t* words, size_t num_words, uint32_t bit_width)
    -> RuntimeValue;  // From multi-word array (little-endian: word[0] = bits
                      // 0-63), 2-state (unknown defaults to zero)
auto MakeIntegralWide(
    const uint64_t* value_words, const uint64_t* unknown_words,
    size_t num_words, uint32_t bit_width)
    -> RuntimeValue;  // 4-state: explicit value and unknown planes
auto MakeIntegralFromConstant(const IntegralConstant& c, uint32_t bit_width)
    -> RuntimeValue;
// Create an integral with all bits set to the same 4-state value.
// value_bit=0, unknown_bit=0 -> all 0; value_bit=1, unknown_bit=0 -> all 1
// value_bit=0, unknown_bit=1 -> all X; value_bit=1, unknown_bit=1 -> all Z
auto MakeIntegralFilled(uint32_t bit_width, bool value_bit, bool unknown_bit)
    -> RuntimeValue;
auto MakeString(std::string value) -> RuntimeValue;
auto MakeReal(double value) -> RuntimeValue;
auto MakeShortReal(float value) -> RuntimeValue;
auto MakeStruct(std::vector<RuntimeValue> fields) -> RuntimeValue;
auto MakeArray(std::vector<RuntimeValue> elements) -> RuntimeValue;
auto MakeUnion(RuntimeIntegral storage_bits) -> RuntimeValue;
auto MakeAssocHandle(runtime::AssocArrayHandle handle) -> RuntimeValue;

// Deep copy
auto Clone(const RuntimeValue& v) -> RuntimeValue;

// Type checks
auto IsIntegral(const RuntimeValue& v) -> bool;
auto IsString(const RuntimeValue& v) -> bool;
auto IsReal(const RuntimeValue& v) -> bool;
auto IsShortReal(const RuntimeValue& v) -> bool;
auto IsStruct(const RuntimeValue& v) -> bool;
auto IsArray(const RuntimeValue& v) -> bool;
auto IsUnion(const RuntimeValue& v) -> bool;
auto IsAssocArray(const RuntimeValue& v) -> bool;

// Accessors (assert correct type)
auto AsIntegral(RuntimeValue& v) -> RuntimeIntegral&;
auto AsIntegral(const RuntimeValue& v) -> const RuntimeIntegral&;
auto AsString(RuntimeValue& v) -> RuntimeString&;
auto AsString(const RuntimeValue& v) -> const RuntimeString&;
auto AsReal(RuntimeValue& v) -> RuntimeReal&;
auto AsReal(const RuntimeValue& v) -> const RuntimeReal&;
auto AsShortReal(RuntimeValue& v) -> RuntimeShortReal&;
auto AsShortReal(const RuntimeValue& v) -> const RuntimeShortReal&;
auto AsStruct(RuntimeValue& v) -> RuntimeStruct&;
auto AsStruct(const RuntimeValue& v) -> const RuntimeStruct&;
auto AsArray(RuntimeValue& v) -> RuntimeArray&;
auto AsArray(const RuntimeValue& v) -> const RuntimeArray&;
auto AsUnion(RuntimeValue& v) -> RuntimeUnion&;
auto AsUnion(const RuntimeValue& v) -> const RuntimeUnion&;
auto AsAssocHandle(const RuntimeValue& v) -> runtime::AssocArrayHandle;

// Conversion to printable string (for $display)
auto ToString(const RuntimeValue& v) -> std::string;
auto ToDecimalString(const RuntimeIntegral& v, bool is_signed) -> std::string;
auto ToHexString(const RuntimeIntegral& v) -> std::string;
auto ToBinaryString(const RuntimeIntegral& v) -> std::string;
auto ToOctalString(const RuntimeIntegral& v) -> std::string;

}  // namespace lyra::semantic

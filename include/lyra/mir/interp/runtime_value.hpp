#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"

namespace lyra::mir::interp {

struct RuntimeIntegral {
  std::vector<uint64_t> value;
  std::vector<uint64_t> x_mask;  // 4-state: X (unknown)
  std::vector<uint64_t> z_mask;  // 4-state: Z (high-impedance)
  uint32_t bit_width;

  [[nodiscard]] auto IsZero() const -> bool;
  [[nodiscard]] auto IsX() const -> bool;     // Any bit is X
  [[nodiscard]] auto IsZ() const -> bool;     // Any bit is Z
  [[nodiscard]] auto IsAllX() const -> bool;  // All bits are X
  [[nodiscard]] auto IsAllZ() const -> bool;  // All bits are Z
  [[nodiscard]] auto IsKnown() const -> bool;
  [[nodiscard]] auto IsAllOnes() const -> bool;
};

struct RuntimeString {
  std::string value;
};

struct RuntimeReal {
  double value;
};

// Forward declarations for recursive types
struct RuntimeStruct;
struct RuntimeArray;

using RuntimeValue = std::variant<
    std::monostate, RuntimeIntegral, RuntimeString, RuntimeReal,
    std::unique_ptr<RuntimeStruct>, std::unique_ptr<RuntimeArray>>;

struct RuntimeStruct {
  std::vector<RuntimeValue> fields;
};

struct RuntimeArray {
  std::vector<RuntimeValue> elements;
};

// Factory functions
auto MakeIntegral(uint64_t value, uint32_t bit_width) -> RuntimeValue;
auto MakeIntegralSigned(int64_t value, uint32_t bit_width)
    -> RuntimeValue;  // Sign-extends for wide values
auto MakeIntegralX(uint32_t bit_width) -> RuntimeValue;  // All bits unknown (X)
auto MakeIntegralFromConstant(const IntegralConstant& c, uint32_t bit_width)
    -> RuntimeValue;
auto MakeString(std::string value) -> RuntimeValue;
auto MakeReal(double value) -> RuntimeValue;
auto MakeStruct(std::vector<RuntimeValue> fields) -> RuntimeValue;
auto MakeArray(std::vector<RuntimeValue> elements) -> RuntimeValue;

// Deep copy
auto Clone(const RuntimeValue& v) -> RuntimeValue;

// Type checks
auto IsIntegral(const RuntimeValue& v) -> bool;
auto IsString(const RuntimeValue& v) -> bool;
auto IsReal(const RuntimeValue& v) -> bool;
auto IsStruct(const RuntimeValue& v) -> bool;
auto IsArray(const RuntimeValue& v) -> bool;

// Accessors (assert correct type)
auto AsIntegral(RuntimeValue& v) -> RuntimeIntegral&;
auto AsIntegral(const RuntimeValue& v) -> const RuntimeIntegral&;
auto AsString(RuntimeValue& v) -> RuntimeString&;
auto AsString(const RuntimeValue& v) -> const RuntimeString&;
auto AsReal(RuntimeValue& v) -> RuntimeReal&;
auto AsReal(const RuntimeValue& v) -> const RuntimeReal&;
auto AsStruct(RuntimeValue& v) -> RuntimeStruct&;
auto AsStruct(const RuntimeValue& v) -> const RuntimeStruct&;
auto AsArray(RuntimeValue& v) -> RuntimeArray&;
auto AsArray(const RuntimeValue& v) -> const RuntimeArray&;

// Conversion to printable string (for $display)
auto ToString(const RuntimeValue& v) -> std::string;
auto ToDecimalString(const RuntimeIntegral& v, bool is_signed) -> std::string;
auto ToHexString(const RuntimeIntegral& v) -> std::string;
auto ToBinaryString(const RuntimeIntegral& v) -> std::string;
auto ToOctalString(const RuntimeIntegral& v) -> std::string;

}  // namespace lyra::mir::interp

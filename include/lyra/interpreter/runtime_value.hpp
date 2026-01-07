#pragma once

#include <cassert>
#include <cstdint>
#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/context.hpp"

namespace lyra {

struct RuntimeValue;

// Array storage uses shared_ptr to allow value semantics while supporting
// recursive RuntimeValue definition
using ArrayStorage = std::shared_ptr<std::vector<RuntimeValue>>;
using ValueVariant = std::variant<common::ValueStorage, ArrayStorage>;

struct RuntimeValue {
 public:
  common::Type type;
  ValueVariant value;

  static auto FromLiteral(lir::LiteralRef literal) -> RuntimeValue {
    return FromLiteral(*literal.ptr);
  }

  static auto FromLiteral(const common::Literal& literal) -> RuntimeValue {
    return RuntimeValue{
        .type = literal.type, .value = ValueVariant(literal.value)};
  }

  static auto DefaultValueForType(const common::Type& type) -> RuntimeValue;

  static auto Array(common::Type type, std::vector<RuntimeValue> elements)
      -> RuntimeValue {
    return RuntimeValue{
        .type = std::move(type),
        .value =
            std::make_shared<std::vector<RuntimeValue>>(std::move(elements))};
  }

  static auto IntegralSigned(int64_t value, size_t bit_width) -> RuntimeValue {
    uint64_t mask = common::MakeBitMask(bit_width);
    uint64_t truncated = static_cast<uint64_t>(value) & mask;
    int64_t sign_extended = common::SignExtend(truncated, bit_width);

    return RuntimeValue{
        .type = common::Type::IntegralSigned(bit_width),
        .value = ValueVariant(common::ValueStorage(sign_extended))};
  }

  static auto IntegralUnsigned(uint64_t value, size_t bit_width)
      -> RuntimeValue {
    uint64_t masked = value & common::MakeBitMask(bit_width);
    return RuntimeValue{
        .type = common::Type::IntegralUnsigned(bit_width),
        .value =
            ValueVariant(common::ValueStorage(static_cast<int64_t>(masked)))};
  }

  static auto Bool(bool value) -> RuntimeValue {
    return IntegralUnsigned(value ? 1 : 0, 1);
  }

  static auto String(std::string value) -> RuntimeValue {
    return RuntimeValue{
        .type = common::Type::String(),
        .value = ValueVariant(common::ValueStorage(std::move(value)))};
  }

  static auto Real(double value) -> RuntimeValue {
    return RuntimeValue{
        .type = common::Type::Real(),
        .value = ValueVariant(common::ValueStorage(value))};
  }

  static auto ShortReal(float value) -> RuntimeValue {
    return RuntimeValue{
        .type = common::Type::ShortReal(),
        .value = ValueVariant(common::ValueStorage(value))};
  }

  // Scalar accessors - assumes value holds ValueStorage
  [[nodiscard]] auto AsScalar() const -> const common::ValueStorage& {
    return std::get<common::ValueStorage>(value);
  }

  [[nodiscard]] auto AsInt64() const -> int64_t {
    return AsScalar().AsInt64();
  }

  [[nodiscard]] auto AsDouble() const -> double {
    return AsScalar().AsDouble();
  }

  [[nodiscard]] auto AsFloat() const -> float {
    return AsScalar().AsFloat();
  }

  [[nodiscard]] auto AsUInt64() const -> uint64_t {
    return static_cast<uint64_t>(AsScalar().AsInt64()) &
           common::MakeBitMask(GetBitWidth());
  }

  [[nodiscard]] auto AsBool() const -> bool {
    return AsScalar().AsInt64() != 0;
  }

  [[nodiscard]] auto AsString() const -> const std::string& {
    return AsScalar().AsString();
  }

  // Array accessors - assumes value holds ArrayStorage
  [[nodiscard]] auto IsArray() const -> bool {
    return type.kind == common::Type::Kind::kUnpackedArray;
  }

  [[nodiscard]] auto AsArray() const -> const std::vector<RuntimeValue>& {
    return *std::get<ArrayStorage>(value);
  }

  [[nodiscard]] auto AsArray() -> std::vector<RuntimeValue>& {
    return *std::get<ArrayStorage>(value);
  }

  [[nodiscard]] auto GetElement(size_t index) const -> const RuntimeValue& {
    return AsArray()[index];
  }

  auto SetElement(size_t index, RuntimeValue element) -> void {
    AsArray()[index] = std::move(element);
  }

  [[nodiscard]] auto ToString() const -> std::string {
    if (IsArray()) {
      std::string result = "{";
      const auto& arr = AsArray();
      for (size_t i = 0; i < arr.size(); ++i) {
        if (i > 0) {
          result += ", ";
        }
        result += arr[i].ToString();
      }
      result += "}";
      return result;
    }
    return fmt::format("{}", AsScalar().ToString());
  }

  [[nodiscard]] auto IsTwoState() const -> bool {
    return type.kind == common::Type::Kind::kIntegral;
  }

  [[nodiscard]] auto IsString() const -> bool {
    return type.kind == common::Type::Kind::kString;
  }

  [[nodiscard]] auto IsReal() const -> bool {
    return type.kind == common::Type::Kind::kReal;
  }

  [[nodiscard]] auto IsShortReal() const -> bool {
    return type.kind == common::Type::Kind::kShortReal;
  }

  [[nodiscard]] auto GetBitWidth() const -> uint32_t {
    if (type.kind != common::Type::Kind::kIntegral) {
      return 0;
    }
    return std::get<common::IntegralData>(type.data).bit_width;
  }

  // Note: Can't use default comparison because ArrayStorage is shared_ptr
  // and we want value equality for arrays
  [[nodiscard]] auto operator==(const RuntimeValue& rhs) const -> bool {
    if (type != rhs.type) {
      return false;
    }
    if (IsArray()) {
      return AsArray() == rhs.AsArray();
    }
    return AsScalar() == rhs.AsScalar();
  }
};

inline auto RuntimeValue::DefaultValueForType(const common::Type& type)
    -> RuntimeValue {
  switch (type.kind) {
    case common::Type::Kind::kVoid:
      return {};
    case common::Type::Kind::kIntegral: {
      auto data = std::get<common::IntegralData>(type.data);
      if (data.is_signed) {
        return IntegralSigned(0, data.bit_width);
      }
      return IntegralUnsigned(0, data.bit_width);
    }
    case common::Type::Kind::kReal:
      return Real(0.0);
    case common::Type::Kind::kShortReal:
      return ShortReal(0.0F);
    case common::Type::Kind::kString:
      return String("");
    case common::Type::Kind::kUnpackedArray: {
      auto array_data = std::get<common::UnpackedArrayData>(type.data);
      std::vector<RuntimeValue> elements;
      elements.reserve(array_data.size);
      auto element_default = DefaultValueForType(*array_data.element_type);
      for (size_t i = 0; i < array_data.size; ++i) {
        elements.push_back(element_default);
      }
      return Array(type, std::move(elements));
    }
  }
}

inline auto operator<<(std::ostream& os, const RuntimeValue& value)
    -> std::ostream& {
  return os << value.ToString();
}

}  // namespace lyra

template <>
struct fmt::formatter<lyra::RuntimeValue> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::RuntimeValue& value, FormatContext& ctx) {
    return fmt::format_to(ctx.out(), "{}", value.ToString());
  }
};

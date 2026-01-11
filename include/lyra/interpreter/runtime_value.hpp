#pragma once

#include <cassert>
#include <cstdint>
#include <memory>
#include <ranges>
#include <span>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit.hpp"
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

  // Factory for wide integral values (>64 bits)
  static auto IntegralWide(
      common::WideBit value, size_t bit_width, bool is_signed = false)
      -> RuntimeValue {
    value.MaskToWidth(bit_width);
    return RuntimeValue{
        .type = is_signed ? common::Type::IntegralSigned(bit_width)
                          : common::Type::IntegralUnsigned(bit_width),
        .value = ValueVariant(common::ValueStorage(std::move(value)))};
  }

  // Concatenate multiple operands into a single result.
  // Operands are MSB to LSB order (first operand is most significant).
  // Result is always unsigned.
  static auto Concatenate(
      std::span<const RuntimeValue> operands, size_t result_width)
      -> RuntimeValue {
    if (result_width <= 64) {
      // Narrow result: combine using shift-and-or
      uint64_t result = 0;
      size_t shift = 0;

      // Process from LSB to MSB (reverse order)
      for (const auto& operand : std::views::reverse(operands)) {
        size_t width = operand.type.GetBitWidth();
        uint64_t bits = operand.IsWide() ? operand.AsWideBit().GetWord(0)
                                         : operand.AsNarrow().AsUInt64();
        // Mask to operand width to avoid sign-extension issues
        bits &= common::MakeBitMask(static_cast<uint32_t>(width));
        result |= (bits << shift);
        shift += width;
      }
      return IntegralUnsigned(result, result_width);
    }

    // Wide result: use WideBit operations
    size_t num_words = common::wide_ops::WordsForBits(result_width);
    common::WideBit result(num_words);
    size_t bit_offset = 0;

    // Process from LSB to MSB (reverse order)
    for (const auto& operand : std::views::reverse(operands)) {
      size_t width = operand.type.GetBitWidth();

      // Get operand as WideBit (or convert from narrow)
      common::WideBit operand_wide =
          operand.IsWide()
              ? operand.AsWideBit()
              : common::WideBit::FromUInt64(operand.AsNarrow().AsUInt64(), 2);

      // Insert operand bits at current offset
      result = result.InsertSlice(operand_wide, bit_offset, width);
      bit_offset += width;
    }

    // Mask to result width
    common::wide_ops::MaskToWidth(result.Words(), result_width);
    return IntegralWide(std::move(result), result_width);
  }

  // Scalar accessors - assumes value holds ValueStorage
  [[nodiscard]] auto AsScalar() const -> const common::ValueStorage& {
    return std::get<common::ValueStorage>(value);
  }

  [[nodiscard]] auto AsDouble() const -> double {
    return AsScalar().AsDouble();
  }

  [[nodiscard]] auto AsFloat() const -> float {
    return AsScalar().AsFloat();
  }

  [[nodiscard]] auto AsString() const -> const std::string& {
    return AsScalar().AsString();
  }

  // Wide bit accessors
  [[nodiscard]] auto IsWide() const -> bool {
    return IsTwoState() && GetBitWidth() > 64;
  }

  [[nodiscard]] auto AsWideBit() const -> const common::WideBit& {
    return AsScalar().AsWideBit();
  }

  // Array accessors - assumes value holds ArrayStorage
  [[nodiscard]] auto IsArray() const -> bool {
    return type.kind == common::Type::Kind::kUnpackedArray ||
           type.kind == common::Type::Kind::kDynamicArray;
  }

  [[nodiscard]] auto IsUnpackedStruct() const -> bool {
    return type.kind == common::Type::Kind::kUnpackedStruct;
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

  // Struct field accessors - unpacked structs use ArrayStorage indexed by field
  // position
  [[nodiscard]] auto GetField(size_t field_index) const -> const RuntimeValue& {
    assert(IsUnpackedStruct() && "GetField requires unpacked struct");
    return (*std::get<ArrayStorage>(value))[field_index];
  }

  auto SetField(size_t field_index, RuntimeValue field_value) -> void {
    assert(IsUnpackedStruct() && "SetField requires unpacked struct");
    (*std::get<ArrayStorage>(value))[field_index] = std::move(field_value);
  }

  [[nodiscard]] auto ToString() const -> std::string {
    if (IsArray() || IsUnpackedStruct()) {
      std::string result = "{";
      const auto& arr = *std::get<ArrayStorage>(value);
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

  [[nodiscard]] auto IsSigned() const -> bool {
    if (type.kind != common::Type::Kind::kIntegral) {
      return false;
    }
    return std::get<common::IntegralData>(type.data).is_signed;
  }

  // Safe narrow integral value wrapper.
  // Use this instead of AsInt64()/AsUInt64() for type-safe access.
  struct NarrowIntegral {
    int64_t raw;
    uint32_t bit_width;
    bool is_signed;

    [[nodiscard]] auto AsInt64() const -> int64_t {
      return raw;
    }

    [[nodiscard]] auto AsUInt64() const -> uint64_t {
      return static_cast<uint64_t>(raw) & common::MakeBitMask(bit_width);
    }
  };

  // Safe accessor for narrow (<=64 bit) integral values.
  // Asserts that the value is not wide - use MatchIntegral for wide-safe
  // access.
  [[nodiscard]] auto AsNarrow() const -> NarrowIntegral {
    assert(
        IsTwoState() && !IsWide() && "Use MatchIntegral for wide-safe access");
    return {
        .raw = AsScalar().AsInt64(),
        .bit_width = GetBitWidth(),
        .is_signed = IsSigned()};
  }

  // Pattern matching for integral values - forces handling both narrow and wide
  // cases. This is the preferred way to access integral values as it prevents
  // crashes from forgetting to check IsWide().
  //
  // Example:
  //   auto result = value.MatchIntegral(
  //       [](NarrowIntegral n) { return n.AsInt64() == 0; },
  //       [](const common::WideBit& w) { return w.IsZero(); });
  template <typename NarrowFn, typename WideFn>
  auto MatchIntegral(NarrowFn narrow_fn, WideFn wide_fn) const {
    assert(IsTwoState() && "MatchIntegral requires integral type");
    if (IsWide()) {
      return wide_fn(AsWideBit());
    }
    return narrow_fn(AsNarrow());
  }

  // Note: Can't use default comparison because ArrayStorage is shared_ptr
  // and we want value equality for arrays and structs
  [[nodiscard]] auto operator==(const RuntimeValue& rhs) const -> bool {
    if (type != rhs.type) {
      return false;
    }
    if (IsArray() || IsUnpackedStruct()) {
      return *std::get<ArrayStorage>(value) ==
             *std::get<ArrayStorage>(rhs.value);
    }
    return AsScalar() == rhs.AsScalar();
  }

  // Deep copy for value semantics - recursively copies nested arrays.
  // Uses std::visit for exhaustive handling: when a new storage type is added
  // to ValueVariant, the compiler will error here, forcing explicit handling.
  [[nodiscard]] auto DeepCopy() const -> RuntimeValue {
    auto copied_value = std::visit(
        [](const auto& v) -> ValueVariant {
          using T = std::decay_t<decltype(v)>;
          if constexpr (std::is_same_v<T, common::ValueStorage>) {
            // Value semantics - shallow copy is sufficient
            return v;
          } else if constexpr (std::is_same_v<T, ArrayStorage>) {
            // Reference semantics - must deep copy
            std::vector<RuntimeValue> copied;
            copied.reserve(v->size());
            for (const auto& elem : *v) {
              copied.push_back(elem.DeepCopy());
            }
            return std::make_shared<std::vector<RuntimeValue>>(
                std::move(copied));
          }
        },
        value);
    return RuntimeValue{.type = type, .value = std::move(copied_value)};
  }
};

inline auto RuntimeValue::DefaultValueForType(const common::Type& type)
    -> RuntimeValue {
  switch (type.kind) {
    case common::Type::Kind::kVoid:
      return {};
    case common::Type::Kind::kIntegral: {
      auto data = std::get<common::IntegralData>(type.data);
      if (data.bit_width > 64) {
        // Wide value - use WideBit storage
        return IntegralWide(
            common::WideBit::FromBitWidth(data.bit_width), data.bit_width,
            data.is_signed);
      }
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
      // Must create a NEW default value for each element to avoid shared
      // ArrayStorage when element_type is itself an array
      for (size_t i = 0; i < array_data.size; ++i) {
        elements.push_back(DefaultValueForType(*array_data.element_type));
      }
      return Array(type, std::move(elements));
    }
    case common::Type::Kind::kDynamicArray:
      // Dynamic arrays default to empty (size 0)
      return Array(type, {});
    case common::Type::Kind::kPackedStruct: {
      // Packed structs are bitvectors - initialize to all zeros
      auto data = std::get<common::PackedStructData>(type.data);
      if (data.bit_width > 64) {
        return IntegralWide(
            common::WideBit::FromBitWidth(data.bit_width), data.bit_width,
            data.is_signed);
      }
      if (data.is_signed) {
        return IntegralSigned(0, data.bit_width);
      }
      return IntegralUnsigned(0, data.bit_width);
    }
    case common::Type::Kind::kUnpackedStruct: {
      // Unpacked structs use ArrayStorage indexed by field position
      const auto& struct_data = std::get<common::UnpackedStructData>(type.data);
      std::vector<RuntimeValue> fields;
      fields.reserve(struct_data.fields.size());
      for (const auto& field : struct_data.fields) {
        fields.push_back(DefaultValueForType(*field.field_type));
      }
      return RuntimeValue{
          .type = type,
          .value =
              std::make_shared<std::vector<RuntimeValue>>(std::move(fields))};
    }
  }
  throw common::InternalError(
      "DefaultValueForType",
      fmt::format("unhandled type kind: {}", type.ToString()));
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

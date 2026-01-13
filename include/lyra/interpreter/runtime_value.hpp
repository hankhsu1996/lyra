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
#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/sdk/bounded_queue.hpp"

namespace lyra {

struct RuntimeValue;
struct PointerValue;

// Pointer value for address-of operations (ResolveVar, ResolveIndex, etc.)
// Forms a linked chain where Field/Index/Slice have a base pointer.
using PointerStorage = std::shared_ptr<PointerValue>;

// Pointer to a variable (root of pointer chain)
struct VarPointer {
  common::SymbolRef symbol;
};

// Pointer to a struct field
struct FieldPointer {
  PointerStorage base;
  size_t field_id;
};

// Pointer to an array element
struct IndexPointer {
  PointerStorage base;
  size_t index;
};

// Pointer to a bit slice
struct SlicePointer {
  PointerStorage base;
  size_t offset;
  size_t width;
};

// Runtime representation of Pointer<T> typed SSA values.
// Used by kResolveVar, kResolveField, kResolveIndex, kResolveSlice
// instructions.
struct PointerValue {
  std::variant<VarPointer, FieldPointer, IndexPointer, SlicePointer> data;

  [[nodiscard]] auto IsVar() const -> bool {
    return std::holds_alternative<VarPointer>(data);
  }

  [[nodiscard]] auto IsField() const -> bool {
    return std::holds_alternative<FieldPointer>(data);
  }

  [[nodiscard]] auto IsIndex() const -> bool {
    return std::holds_alternative<IndexPointer>(data);
  }

  [[nodiscard]] auto IsSlice() const -> bool {
    return std::holds_alternative<SlicePointer>(data);
  }

  [[nodiscard]] auto AsVar() const -> const VarPointer& {
    return std::get<VarPointer>(data);
  }

  [[nodiscard]] auto AsField() const -> const FieldPointer& {
    return std::get<FieldPointer>(data);
  }

  [[nodiscard]] auto AsIndex() const -> const IndexPointer& {
    return std::get<IndexPointer>(data);
  }

  [[nodiscard]] auto AsSlice() const -> const SlicePointer& {
    return std::get<SlicePointer>(data);
  }

  // Get the root symbol by walking up the pointer chain
  [[nodiscard]] auto GetRootSymbol() const -> common::SymbolRef {
    const PointerValue* current = this;
    while (!current->IsVar()) {
      if (current->IsField()) {
        current = current->AsField().base.get();
      } else if (current->IsIndex()) {
        current = current->AsIndex().base.get();
      } else {
        current = current->AsSlice().base.get();
      }
    }
    return current->AsVar().symbol;
  }

  // Factory methods
  static auto Var(common::SymbolRef symbol) -> PointerStorage {
    return std::make_shared<PointerValue>(
        PointerValue{.data = VarPointer{.symbol = symbol}});
  }

  static auto Field(PointerStorage base, size_t field_id) -> PointerStorage {
    return std::make_shared<PointerValue>(PointerValue{
        .data = FieldPointer{.base = std::move(base), .field_id = field_id}});
  }

  static auto Index(PointerStorage base, size_t index) -> PointerStorage {
    return std::make_shared<PointerValue>(PointerValue{
        .data = IndexPointer{.base = std::move(base), .index = index}});
  }

  static auto Slice(PointerStorage base, size_t offset, size_t width)
      -> PointerStorage {
    return std::make_shared<PointerValue>(PointerValue{
        .data = SlicePointer{
            .base = std::move(base), .offset = offset, .width = width}});
  }
};

// Storage types use shared_ptr to allow value semantics while supporting
// recursive RuntimeValue definition
using ArrayStorage = std::shared_ptr<std::vector<RuntimeValue>>;
using QueueStorage = std::shared_ptr<sdk::BoundedQueue<RuntimeValue>>;
using ValueVariant = std::variant<
    common::ValueStorage, ArrayStorage, QueueStorage, PointerStorage>;

struct RuntimeValue {
 public:
  common::Type type;
  ValueVariant value;

  static auto FromConstant(lir::ConstantRef constant) -> RuntimeValue {
    return FromConstant(*constant.ptr);
  }

  static auto FromConstant(const common::Constant& constant) -> RuntimeValue {
    return RuntimeValue{
        .type = constant.type, .value = ValueVariant(constant.value)};
  }

  static auto DefaultValueForType(const common::Type& type) -> RuntimeValue;

  static auto Array(common::Type type, std::vector<RuntimeValue> elements)
      -> RuntimeValue {
    return RuntimeValue{
        .type = std::move(type),
        .value =
            std::make_shared<std::vector<RuntimeValue>>(std::move(elements))};
  }

  static auto Queue(common::Type type, size_t max_bound = 0) -> RuntimeValue {
    return RuntimeValue{
        .type = std::move(type),
        .value = std::make_shared<sdk::BoundedQueue<RuntimeValue>>(max_bound)};
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

  // Factory for pointer values (used by kResolveVar, kResolveIndex, etc.)
  static auto Pointer(common::Type pointer_type, PointerStorage ptr)
      -> RuntimeValue {
    return RuntimeValue{
        .type = std::move(pointer_type), .value = std::move(ptr)};
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

  // Array accessors - assumes value holds ArrayStorage (vector)
  [[nodiscard]] auto IsArray() const -> bool {
    return type.kind == common::Type::Kind::kUnpackedArray ||
           type.kind == common::Type::Kind::kDynamicArray;
  }

  // Queue accessor - assumes value holds QueueStorage (BoundedQueue)
  [[nodiscard]] auto IsQueue() const -> bool {
    return type.kind == common::Type::Kind::kQueue;
  }

  [[nodiscard]] auto IsUnpackedStruct() const -> bool {
    return type.kind == common::Type::Kind::kUnpackedStruct;
  }

  [[nodiscard]] auto IsUnpackedUnion() const -> bool {
    return type.kind == common::Type::Kind::kUnpackedUnion;
  }

  [[nodiscard]] auto IsPointer() const -> bool {
    return type.kind == common::Type::Kind::kPointer;
  }

  [[nodiscard]] auto IsSliceRef() const -> bool {
    return type.kind == common::Type::Kind::kSliceRef;
  }

  [[nodiscard]] auto AsPointer() const -> const PointerValue& {
    return *std::get<PointerStorage>(value);
  }

  [[nodiscard]] auto AsPointerStorage() const -> PointerStorage {
    return std::get<PointerStorage>(value);
  }

  [[nodiscard]] auto AsArray() const -> const std::vector<RuntimeValue>& {
    return *std::get<ArrayStorage>(value);
  }

  [[nodiscard]] auto AsArray() -> std::vector<RuntimeValue>& {
    return *std::get<ArrayStorage>(value);
  }

  [[nodiscard]] auto AsQueue() const -> const sdk::BoundedQueue<RuntimeValue>& {
    return *std::get<QueueStorage>(value);
  }

  [[nodiscard]] auto AsQueue() -> sdk::BoundedQueue<RuntimeValue>& {
    return *std::get<QueueStorage>(value);
  }

  [[nodiscard]] auto GetElement(size_t index) const -> const RuntimeValue& {
    if (IsQueue()) {
      return AsQueue()[index];
    }
    return AsArray()[index];
  }

  auto SetElement(size_t index, RuntimeValue element) -> void {
    if (IsQueue()) {
      AsQueue()[index] = std::move(element);
    } else {
      AsArray()[index] = std::move(element);
    }
  }

  // Struct/union field accessors - use ArrayStorage indexed by position
  // Structs: N slots (one per field); Unions: 1 slot (shared storage)
  [[nodiscard]] auto GetField(size_t field_index) const -> const RuntimeValue& {
    assert(
        (IsUnpackedStruct() || IsUnpackedUnion()) &&
        "GetField requires unpacked struct/union");
    return (*std::get<ArrayStorage>(value))[field_index];
  }

  auto SetField(size_t field_index, RuntimeValue field_value) -> void {
    assert(
        (IsUnpackedStruct() || IsUnpackedUnion()) &&
        "SetField requires unpacked struct/union");
    (*std::get<ArrayStorage>(value))[field_index] = std::move(field_value);
  }

  [[nodiscard]] auto ToString() const -> std::string {
    if (IsArray() || IsUnpackedStruct() || IsUnpackedUnion()) {
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
    if (IsQueue()) {
      std::string result = "{";
      const auto& queue = *std::get<QueueStorage>(value);
      for (size_t i = 0; i < queue.size(); ++i) {
        if (i > 0) {
          result += ", ";
        }
        result += queue[i].ToString();
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
    if (!type.IsBitvector()) {
      return 0;
    }
    return type.GetBitWidth();
  }

  [[nodiscard]] auto IsSigned() const -> bool {
    return type.IsSigned();
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

  // Note: Can't use default comparison because ArrayStorage/QueueStorage are
  // shared_ptr and we want value equality for arrays, queues, structs, and
  // unions
  [[nodiscard]] auto operator==(const RuntimeValue& rhs) const -> bool {
    if (type != rhs.type) {
      return false;
    }
    if (IsArray() || IsUnpackedStruct() || IsUnpackedUnion()) {
      return *std::get<ArrayStorage>(value) ==
             *std::get<ArrayStorage>(rhs.value);
    }
    if (IsQueue()) {
      return *std::get<QueueStorage>(value) ==
             *std::get<QueueStorage>(rhs.value);
    }
    return AsScalar() == rhs.AsScalar();
  }

  // Deep copy for value semantics - recursively copies nested arrays/queues.
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
          } else if constexpr (std::is_same_v<T, QueueStorage>) {
            // Reference semantics - must deep copy (preserve max_bound)
            auto copied = std::make_shared<sdk::BoundedQueue<RuntimeValue>>(
                v->max_bound());
            for (const auto& elem : *v) {
              copied->push_back(elem.DeepCopy());
            }
            return copied;
          } else {
            static_assert(std::is_same_v<T, PointerStorage>);
            // Pointer values are immutable - shallow copy is sufficient
            return v;
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
    case common::Type::Kind::kQueue: {
      // Queues default to empty (size 0) with bound from type
      const auto& queue_data = std::get<common::QueueData>(type.data);
      return Queue(type, queue_data.max_bound);
    }
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
    case common::Type::Kind::kUnpackedUnion: {
      // Unpacked unions use ArrayStorage with 1 element (shared storage)
      // Initialize to first member's default value
      const auto& union_data = std::get<common::UnpackedUnionData>(type.data);
      std::vector<RuntimeValue> storage;
      storage.push_back(DefaultValueForType(*union_data.fields[0].field_type));
      return RuntimeValue{
          .type = type,
          .value =
              std::make_shared<std::vector<RuntimeValue>>(std::move(storage))};
    }
    case common::Type::Kind::kEnum: {
      // Enums are bitvectors - initialize to 0
      const auto& data = std::get<common::EnumData>(type.data);
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
    case common::Type::Kind::kPointer:
      throw common::InternalError(
          "DefaultValueForType", "Pointer<T> is an internal LIR type");
    case common::Type::Kind::kSliceRef:
      throw common::InternalError(
          "DefaultValueForType", "SliceRef<T> is an internal LIR type");
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

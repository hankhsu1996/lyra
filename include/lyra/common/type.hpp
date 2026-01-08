#pragma once

#include <functional>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <variant>

#include <fmt/core.h>
#include <slang/ast/types/Type.h>

#include "lyra/common/meta_util.hpp"

namespace lyra::common {

struct Type;  // Forward declaration for UnpackedArrayData

struct IntegralData {
  size_t bit_width;  // Total flat storage width
  bool is_signed = false;
  bool is_four_state = false;  // true for logic/reg, false for bit/int

  // Packed array dimension (nullptr for scalars like int, bit)
  std::shared_ptr<Type> element_type;
  size_t element_count = 0;   // range.width() - number of elements
  int32_t element_lower = 0;  // range.lower() - index lower bound

  auto operator==(const IntegralData& other) const -> bool;
  [[nodiscard]] auto Hash() const -> std::size_t;
};

struct UnpackedArrayData {
  std::shared_ptr<Type> element_type;
  size_t size;
  int32_t lower_bound;  // For [2:5] style ranges, lower_bound=2

  auto operator==(const UnpackedArrayData& other) const -> bool;
  [[nodiscard]] auto Hash() const -> std::size_t;
};

struct Type {
  enum class Kind {
    kVoid,
    kIntegral,
    kReal,
    kShortReal,
    kString,
    kUnpackedArray
  };

  Kind kind{};
  std::variant<std::monostate, IntegralData, UnpackedArrayData> data{};

  // Optional type alias name for typedef'd types (e.g., "Byte" for typedef
  // bit[7:0] Byte). This is metadata for codegen readability, not part of
  // semantic type equality. Also used for enum typedefs.
  std::optional<std::string> alias_name;

  static auto FromSlang(const slang::ast::Type& type) -> Type {
    if (type.isString()) {
      return Type{.kind = Kind::kString};
    }
    if (type.isFloating()) {
      return Type::Real();
    }
    if (type.isIntegral()) {
      if (type.isSigned()) {
        return Type::IntegralSigned(type.getBitWidth());
      }
      return Type::IntegralUnsigned(type.getBitWidth());
    }
    throw std::runtime_error(
        fmt::format("Unsupported type: {}", type.toString()));
  }

  static auto Void() -> Type {
    return Type{.kind = Kind::kVoid};
  }

  static auto Integral(
      size_t bit_width, bool is_signed, bool is_four_state = false,
      int32_t element_lower = 0) -> Type {
    return Type{
        .kind = Kind::kIntegral,
        .data = IntegralData{
            .bit_width = bit_width,
            .is_signed = is_signed,
            .is_four_state = is_four_state,
            .element_type = nullptr,
            .element_count = 0,
            .element_lower = element_lower}};
  }

  static auto IntegralSigned(size_t bit_width) -> Type {
    return Integral(bit_width, true);
  }

  static auto IntegralUnsigned(size_t bit_width) -> Type {
    return Integral(bit_width, false);
  }

  // Create a packed array type (e.g., bit [3:0][7:0])
  static auto PackedArray(
      Type element_type, size_t element_count, int32_t element_lower = 0,
      bool is_signed = false, bool is_four_state = false) -> Type {
    // Validate: element type must be integral
    if (element_type.kind != Kind::kIntegral) {
      throw std::runtime_error("Packed array element must be integral type");
    }

    // Compute total bit width from element type and count
    const auto& elem_data = std::get<IntegralData>(element_type.data);
    size_t total_width = elem_data.bit_width * element_count;

    return Type{
        .kind = Kind::kIntegral,
        .data = IntegralData{
            .bit_width = total_width,
            .is_signed = is_signed,
            .is_four_state = is_four_state,
            .element_type = std::make_shared<Type>(std::move(element_type)),
            .element_count = element_count,
            .element_lower = element_lower}};
  }

  static auto Int() -> Type {
    return IntegralSigned(32);
  }

  static auto UInt() -> Type {
    return IntegralUnsigned(32);
  }

  static auto LongInt() -> Type {
    return IntegralSigned(64);
  }

  static auto ULongInt() -> Type {
    return IntegralUnsigned(64);
  }

  static auto Bool() -> Type {
    return IntegralUnsigned(1);
  }

  static auto String() -> Type {
    return Type{.kind = Kind::kString};
  }

  static auto Real() -> Type {
    return Type{.kind = Kind::kReal};
  }

  static auto ShortReal() -> Type {
    return Type{.kind = Kind::kShortReal};
  }

  static auto UnpackedArray(Type element, size_t size, int32_t lower_bound = 0)
      -> Type {
    return Type{
        .kind = Kind::kUnpackedArray,
        .data = UnpackedArrayData{
            .element_type = std::make_shared<Type>(element),
            .size = size,
            .lower_bound = lower_bound}};
  }

  // Is this a scalar integral (no packed array structure)?
  [[nodiscard]] auto IsScalar() const -> bool {
    if (kind != Kind::kIntegral) {
      return false;
    }
    return std::get<IntegralData>(data).element_type == nullptr;
  }

  // Is this a packed array (has element structure)?
  [[nodiscard]] auto IsPackedArray() const -> bool {
    if (kind != Kind::kIntegral) {
      return false;
    }
    return std::get<IntegralData>(data).element_type != nullptr;
  }

  // Get element type for indexing (works for both packed and unpacked arrays)
  [[nodiscard]] auto GetElementType() const -> const Type& {
    if (kind == Kind::kIntegral) {
      const auto& integral = std::get<IntegralData>(data);
      if (integral.element_type) {
        return *integral.element_type;
      }
      throw std::runtime_error("Scalar integral type has no element type");
    }
    if (kind == Kind::kUnpackedArray) {
      return *std::get<UnpackedArrayData>(data).element_type;
    }
    throw std::runtime_error("Type is not indexable");
  }

  // Get element count for this dimension
  [[nodiscard]] auto GetElementCount() const -> size_t {
    if (kind == Kind::kIntegral) {
      const auto& integral = std::get<IntegralData>(data);
      if (integral.element_type) {
        return integral.element_count;
      }
      throw std::runtime_error("Scalar integral type has no element count");
    }
    if (kind == Kind::kUnpackedArray) {
      return std::get<UnpackedArrayData>(data).size;
    }
    throw std::runtime_error("Type is not indexable");
  }

  // Get element lower bound for indexing
  [[nodiscard]] auto GetElementLower() const -> int32_t {
    if (kind == Kind::kIntegral) {
      // Works for both scalars and packed arrays
      return std::get<IntegralData>(data).element_lower;
    }
    if (kind == Kind::kUnpackedArray) {
      return std::get<UnpackedArrayData>(data).lower_bound;
    }
    throw std::runtime_error("Type is not indexable");
  }

  // Get element width for bit/element indexing operations
  // Returns 1 for scalars (bit selection), element's bit_width for packed
  // arrays
  [[nodiscard]] auto GetElementWidth() const -> size_t {
    if (kind == Kind::kIntegral) {
      const auto& integral = std::get<IntegralData>(data);
      if (integral.element_type) {
        return std::get<IntegralData>(integral.element_type->data).bit_width;
      }
      return 1;  // Scalar bit selection
    }
    if (kind == Kind::kUnpackedArray) {
      const auto& elem = *std::get<UnpackedArrayData>(data).element_type;
      if (elem.kind == Kind::kIntegral) {
        return std::get<IntegralData>(elem.data).bit_width;
      }
      throw std::runtime_error("Unpacked array element is not integral");
    }
    throw std::runtime_error("Type is not indexable");
  }

  // Get total bit width for integral types
  [[nodiscard]] auto GetBitWidth() const -> size_t {
    if (kind != Kind::kIntegral) {
      throw std::runtime_error("Type is not integral");
    }
    return std::get<IntegralData>(data).bit_width;
  }

  // Explicit operator== that ignores alias_name (it's display metadata, not
  // semantic)
  auto operator==(const Type& other) const -> bool {
    return kind == other.kind && data == other.data;
  }

  [[nodiscard]] auto Hash() const -> std::size_t {
    std::size_t h = 0;

    std::size_t kind_hash = std::hash<int>{}(static_cast<int>(kind));
    h ^= kind_hash + 0x9e3779b9 + (h << 6) + (h >> 2);

    std::size_t data_hash = std::visit(
        [](const auto& val) -> std::size_t {
          using T = std::decay_t<decltype(val)>;
          if constexpr (std::is_same_v<T, std::monostate>) {
            return 0;
          } else if constexpr (requires { val.Hash(); }) {
            return val.Hash();
          } else {
            static_assert(
                kAlwaysFalse<T>, "Unhandled variant type in Type::Hash()");
            return 0;
          }
        },
        data);

    h ^= data_hash + 0x9e3779b9 + (h << 6) + (h >> 2);

    return h;
  }

  [[nodiscard]] auto ToString() const -> std::string {
    std::string structural_str;
    switch (kind) {
      case Kind::kVoid:
        structural_str = "void";
        break;
      case Kind::kIntegral: {
        const auto& ts = std::get<IntegralData>(data);
        std::string base_name = ts.is_four_state ? "logic" : "bit";
        const auto* sign_str = ts.is_signed ? " signed" : "";

        if (ts.element_type) {
          // Packed array: show dimensions
          auto msb =
              static_cast<int32_t>(ts.element_count) + ts.element_lower - 1;
          structural_str = fmt::format(
              "{}[{}:{}]{}", ts.element_type->ToString(), msb, ts.element_lower,
              sign_str);
        } else {
          // Scalar: just show bit width
          structural_str =
              fmt::format("{}[{}]{}", base_name, ts.bit_width, sign_str);
        }
        break;
      }
      case Kind::kReal:
        structural_str = "real";
        break;
      case Kind::kShortReal:
        structural_str = "shortreal";
        break;
      case Kind::kString:
        structural_str = "string";
        break;
      case Kind::kUnpackedArray: {
        const auto& arr = std::get<UnpackedArrayData>(data);
        structural_str =
            fmt::format("{}[{}]", arr.element_type->ToString(), arr.size);
        break;
      }
    }

    // Include alias name if present
    if (alias_name) {
      return fmt::format("{} ({})", *alias_name, structural_str);
    }
    return structural_str;
  }
};

inline auto IntegralData::operator==(const IntegralData& other) const -> bool {
  if (bit_width != other.bit_width || is_signed != other.is_signed ||
      is_four_state != other.is_four_state ||
      element_count != other.element_count ||
      element_lower != other.element_lower) {
    return false;
  }
  // Compare element_type (handle nullptr cases)
  if (element_type == nullptr && other.element_type == nullptr) {
    return true;
  }
  if (element_type == nullptr || other.element_type == nullptr) {
    return false;
  }
  return *element_type == *other.element_type;
}

inline auto IntegralData::Hash() const -> std::size_t {
  std::size_t h = 0;
  h ^= std::hash<size_t>{}(bit_width) + 0x9e3779b9 + (h << 6) + (h >> 2);
  h ^= std::hash<bool>{}(is_signed) + 0x9e3779b9 + (h << 6) + (h >> 2);
  h ^= std::hash<bool>{}(is_four_state) + 0x9e3779b9 + (h << 6) + (h >> 2);
  if (element_type) {
    h ^= element_type->Hash() + 0x9e3779b9 + (h << 6) + (h >> 2);
  }
  h ^= std::hash<size_t>{}(element_count) + 0x9e3779b9 + (h << 6) + (h >> 2);
  h ^= std::hash<int32_t>{}(element_lower) + 0x9e3779b9 + (h << 6) + (h >> 2);
  return h;
}

inline auto UnpackedArrayData::operator==(const UnpackedArrayData& other) const
    -> bool {
  return size == other.size && lower_bound == other.lower_bound &&
         *element_type == *other.element_type;
}

inline auto UnpackedArrayData::Hash() const -> std::size_t {
  std::size_t h = 0;
  h ^= element_type->Hash() + 0x9e3779b9 + (h << 6) + (h >> 2);
  h ^= std::hash<size_t>{}(size) + 0x9e3779b9 + (h << 6) + (h >> 2);
  h ^= std::hash<int32_t>{}(lower_bound) + 0x9e3779b9 + (h << 6) + (h >> 2);
  return h;
}

inline auto ToString(Type::Kind kind) -> std::string {
  switch (kind) {
    case Type::Kind::kVoid:
      return "void";
    case Type::Kind::kIntegral:
      return "integral";
    case Type::Kind::kReal:
      return "real";
    case Type::Kind::kShortReal:
      return "shortreal";
    case Type::Kind::kString:
      return "string";
    case Type::Kind::kUnpackedArray:
      return "unpacked_array";
  }
}

inline auto operator<<(std::ostream& os, Type::Kind kind) -> std::ostream& {
  return os << ToString(kind);
}

inline auto operator<<(std::ostream& os, Type type) -> std::ostream& {
  return os << type.ToString();
}

// Convert bit width and signedness to a C++ fixed-width integer type string.
// Used by codegen for enum base types and other contexts requiring C++ types.
inline auto ToCppIntType(size_t width, bool is_signed) -> std::string {
  if (width <= 8) {
    return is_signed ? "int8_t" : "uint8_t";
  }
  if (width <= 16) {
    return is_signed ? "int16_t" : "uint16_t";
  }
  if (width <= 32) {
    return is_signed ? "int32_t" : "uint32_t";
  }
  return is_signed ? "int64_t" : "uint64_t";
}

}  // namespace lyra::common

template <>
struct fmt::formatter<lyra::common::Type::Kind> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::common::Type::Kind& type, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", lyra::common::ToString(type));
  }
};

template <>
struct fmt::formatter<lyra::common::Type> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::common::Type& type, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", type.ToString());
  }
};

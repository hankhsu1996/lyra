#include "lyra/common/type.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <string>

#include "lyra/common/internal_error.hpp"
#include "lyra/compiler/codegen/codegen.hpp"
#include "lyra/compiler/codegen/type.hpp"

namespace lyra::compiler {

auto Codegen::IsSigned(const common::Type& type) -> bool {
  return type.IsSigned();
}

auto Codegen::ToCppType(const common::Type& type) -> std::string {
  // Handle user-defined type aliases (typedef)
  if (type.alias_name) {
    const auto& name = *type.alias_name;

    // Get the C++ definition for this alias by computing it without the alias
    // name
    common::Type underlying = type;
    underlying.alias_name = std::nullopt;
    std::string cpp_def = ToCppType(underlying);

    // Register the alias for later emission (only if not already registered)
    if (user_type_names_.insert(name).second) {
      user_type_aliases_.emplace_back(name, cpp_def);
    }

    return name;
  }

  switch (type.kind) {
    case common::Type::Kind::kVoid:
      return "void";
    case common::Type::Kind::kReal:
      used_type_aliases_ |= TypeAlias::kReal;
      return "Real";
    case common::Type::Kind::kShortReal:
      used_type_aliases_ |= TypeAlias::kShortReal;
      return "ShortReal";
    case common::Type::Kind::kString:
      return "std::string";
    case common::Type::Kind::kIntegral: {
      auto data = std::get<common::IntegralData>(type.data);
      size_t width = data.bit_width;
      bool is_signed = data.is_signed;

      if (codegen::IsWideWidth(width)) {
        // Wide integers use lyra::sdk::WideBit<N>
        if (is_signed) {
          return std::format("lyra::sdk::WideBit<{}, true>", width);
        }
        return std::format("lyra::sdk::WideBit<{}>", width);
      }

      // Use LRM-aligned type aliases for standard signed integer types
      if (is_signed) {
        if (width == 8) {
          used_type_aliases_ |= TypeAlias::kByte;
          return "Byte";
        }
        if (width == 16) {
          used_type_aliases_ |= TypeAlias::kShortInt;
          return "ShortInt";
        }
        if (width == 32) {
          used_type_aliases_ |= TypeAlias::kInt;
          return "Int";
        }
        if (width == 64) {
          used_type_aliases_ |= TypeAlias::kLongInt;
          return "LongInt";
        }
        // Non-standard signed widths use Bit<N, true>
        used_type_aliases_ |= TypeAlias::kBit;
        return std::format("Bit<{}, true>", width);
      }

      // Unsigned types use Bit<N>
      used_type_aliases_ |= TypeAlias::kBit;
      return std::format("Bit<{}>", width);
    }
    case common::Type::Kind::kUnpackedArray: {
      const auto& array_data = std::get<common::UnpackedArrayData>(type.data);
      return std::format(
          "std::array<{}, {}>", ToCppType(*array_data.element_type),
          array_data.size);
    }
    case common::Type::Kind::kDynamicArray: {
      const auto& array_data = std::get<common::DynamicArrayData>(type.data);
      return std::format(
          "std::vector<{}>", ToCppType(*array_data.element_type));
    }
    case common::Type::Kind::kQueue: {
      const auto& queue_data = std::get<common::QueueData>(type.data);
      auto element_type = ToCppType(*queue_data.element_type);
      if (queue_data.max_bound > 0) {
        return std::format(
            "lyra::sdk::BoundedQueue<{}, {}>", element_type,
            queue_data.max_bound);
      }
      return std::format("std::deque<{}>", element_type);
    }
    case common::Type::Kind::kPackedStruct: {
      // Packed structs are bitvectors - same as kIntegral
      auto data = std::get<common::PackedStructData>(type.data);
      size_t width = data.bit_width;
      bool is_signed = data.is_signed;

      if (codegen::IsWideWidth(width)) {
        if (is_signed) {
          return std::format("lyra::sdk::WideBit<{}, true>", width);
        }
        return std::format("lyra::sdk::WideBit<{}>", width);
      }

      used_type_aliases_ |= TypeAlias::kBit;
      if (is_signed) {
        return std::format("Bit<{}, true>", width);
      }
      return std::format("Bit<{}>", width);
    }
    case common::Type::Kind::kUnpackedStruct: {
      // Unpacked structs are emitted as C++ structs
      // Generate a unique struct type name based on the field signature
      const auto& struct_data = std::get<common::UnpackedStructData>(type.data);

      // Build field list as part of the struct definition
      std::string struct_def = "struct { ";
      for (const auto& field : struct_data.fields) {
        struct_def +=
            std::format("{} {}; ", ToCppType(*field.field_type), field.name);
      }
      struct_def += "}";
      return struct_def;
    }
    case common::Type::Kind::kUnpackedUnion: {
      // Unpacked unions are emitted as C++ unions
      const auto& union_data = std::get<common::UnpackedUnionData>(type.data);

      std::string union_def = "union { ";
      for (const auto& field : union_data.fields) {
        union_def +=
            std::format("{} {}; ", ToCppType(*field.field_type), field.name);
      }
      union_def += "}";
      return union_def;
    }
    case common::Type::Kind::kEnum: {
      // Enums are integral types - use the underlying type representation
      // Note: The alias_name check at the top of the function handles named
      // enums. This case handles enum types without alias_name (shouldn't
      // happen in practice).
      const auto& enum_data = std::get<common::EnumData>(type.data);
      size_t width = enum_data.bit_width;
      bool is_signed = enum_data.is_signed;

      if (codegen::IsWideWidth(width)) {
        if (is_signed) {
          return std::format("lyra::sdk::WideBit<{}, true>", width);
        }
        return std::format("lyra::sdk::WideBit<{}>", width);
      }

      used_type_aliases_ |= TypeAlias::kBit;
      if (is_signed) {
        return std::format("Bit<{}, true>", width);
      }
      return std::format("Bit<{}>", width);
    }
  }
  throw common::InternalError(
      "ToCppType", std::format("unhandled type kind: {}", type.ToString()));
}

auto Codegen::ToCppRawType(const common::Type& type) -> std::string {
  // Returns C++ structural types suitable for template parameters.
  // C++20 requires non-type template parameters to be "structural types"
  // (public members only). std::string doesn't qualify, so we use FixedString.
  switch (type.kind) {
    case common::Type::Kind::kVoid:
      return "void";
    case common::Type::Kind::kReal:
      return "double";
    case common::Type::Kind::kShortReal:
      return "float";
    case common::Type::Kind::kString:
      // FixedString is a structural type; size is deduced from the literal
      return "lyra::sdk::FixedString";
    case common::Type::Kind::kIntegral: {
      auto data = std::get<common::IntegralData>(type.data);
      size_t width = data.bit_width;
      bool is_signed = data.is_signed;

      // Map to standard C++ integer types
      if (is_signed) {
        if (width <= 8) {
          return "int8_t";
        }
        if (width <= 16) {
          return "int16_t";
        }
        if (width <= 32) {
          return "int32_t";
        }
        if (width <= 64) {
          return "int64_t";
        }
      } else {
        if (width <= 8) {
          return "uint8_t";
        }
        if (width <= 16) {
          return "uint16_t";
        }
        if (width <= 32) {
          return "uint32_t";
        }
        if (width <= 64) {
          return "uint64_t";
        }
      }
      // Fall back to SDK type for wide types
      return ToCppType(type);
    }
    default:
      return ToCppType(type);
  }
}

auto Codegen::ToCppUnsignedType(const common::Type& type) -> std::string {
  if (type.kind != common::Type::Kind::kIntegral) {
    return "/* unknown type */";
  }
  auto data = std::get<common::IntegralData>(type.data);
  size_t width = data.bit_width;
  if (codegen::IsWideWidth(width)) {
    // Always return unsigned lyra::sdk::WideBit<N>
    return std::format("lyra::sdk::WideBit<{}>", width);
  }
  // Always return unsigned Bit<N> regardless of original signedness
  used_type_aliases_ |= TypeAlias::kBit;
  return std::format("Bit<{}>", width);
}

}  // namespace lyra::compiler

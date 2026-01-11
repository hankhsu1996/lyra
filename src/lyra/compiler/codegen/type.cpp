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
  if (type.kind != common::Type::Kind::kIntegral) {
    return false;
  }
  return std::get<common::IntegralData>(type.data).is_signed;
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

    // Register the alias for later emission
    user_type_aliases_[name] = cpp_def;

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
  }
  throw common::InternalError(
      "ToCppType", std::format("unhandled type kind: {}", type.ToString()));
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

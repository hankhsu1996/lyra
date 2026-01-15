#pragma once

#include <array>
#include <cstdint>
#include <string_view>
#include <variant>

namespace lyra {

enum class SystemFunctionReturnType : uint8_t {
  kVoid,
  kString,
};

enum class PrintRadix : uint8_t {
  kDecimal,
  kBinary,
  kOctal,
  kHex,
};

struct DisplayFunctionInfo {
  PrintRadix radix;
  bool append_newline;
};

using CategoryPayload = std::variant<DisplayFunctionInfo>;

struct SystemFunctionInfo {
  std::string_view name;
  uint8_t min_args;
  uint8_t max_args;
  SystemFunctionReturnType return_type;
  CategoryPayload payload;
};

using Ret = SystemFunctionReturnType;

// clang-format off
inline constexpr std::array kSystemFunctions = std::to_array<SystemFunctionInfo>({
  // $display family (stdout, with newline)
  {.name = "$display",  .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kDecimal, .append_newline = true}},
  {.name = "$displayb", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kBinary,  .append_newline = true}},
  {.name = "$displayo", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kOctal,   .append_newline = true}},
  {.name = "$displayh", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kHex,     .append_newline = true}},

  // $write family (stdout, no newline)
  {.name = "$write",  .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kDecimal, .append_newline = false}},
  {.name = "$writeb", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kBinary,  .append_newline = false}},
  {.name = "$writeo", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kOctal,   .append_newline = false}},
  {.name = "$writeh", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kHex,     .append_newline = false}},
});
// clang-format on

constexpr auto FindSystemFunction(std::string_view name)
    -> const SystemFunctionInfo* {
  for (const auto& info : kSystemFunctions) {
    if (info.name == name) {
      return &info;
    }
  }
  return nullptr;
}

constexpr auto IsSystemFunctionSupported(std::string_view name) -> bool {
  return FindSystemFunction(name) != nullptr;
}

}  // namespace lyra

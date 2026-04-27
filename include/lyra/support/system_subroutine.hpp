#pragma once

#include <array>
#include <compare>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>
#include <variant>

#include "lyra/support/internal_error.hpp"

namespace lyra::support {

struct SystemSubroutineId {
  std::uint16_t value;

  auto operator<=>(const SystemSubroutineId&) const
      -> std::strong_ordering = default;
};

enum class SystemSubroutineOrigin : std::uint8_t {
  kLanguageBuiltin,
  kLyraExtension,
  kExternalExtension,
};

enum class SystemSubroutineKind : std::uint8_t {
  kTask,
  kFunction,
};

enum class ReturnConvention : std::uint8_t {
  kVoid,
};

struct ArgCountPolicy {
  std::uint16_t min_args;
  std::uint16_t max_args;

  [[nodiscard]] constexpr auto Accepts(std::size_t count) const -> bool {
    return count >= min_args && count <= max_args;
  }
};

enum class PrintRadix : std::uint8_t {
  kDecimal,
  kBinary,
  kOctal,
  kHex,
};

enum class PrintSinkKind : std::uint8_t {
  kStdout,
  kFile,
};

struct PrintSystemSubroutineInfo {
  PrintRadix radix;
  bool append_newline;
  bool is_strobe;
  PrintSinkKind sink_kind;
};

using SystemSubroutineSemantic = std::variant<PrintSystemSubroutineInfo>;

struct SystemSubroutineDesc {
  SystemSubroutineId id;
  std::string_view name;
  SystemSubroutineOrigin origin;
  SystemSubroutineKind kind;
  ReturnConvention result_conv;
  ArgCountPolicy arg_policy;
  SystemSubroutineSemantic semantic;
};

namespace detail {

inline constexpr std::array kSystemSubroutines = {
    SystemSubroutineDesc{
        .id = SystemSubroutineId{0},
        .name = "$display",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .append_newline = true,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{1},
        .name = "$write",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 0, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .append_newline = false,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kStdout},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{2},
        .name = "$fdisplay",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .append_newline = true,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kFile},
    },
    SystemSubroutineDesc{
        .id = SystemSubroutineId{3},
        .name = "$fwrite",
        .origin = SystemSubroutineOrigin::kLanguageBuiltin,
        .kind = SystemSubroutineKind::kTask,
        .result_conv = ReturnConvention::kVoid,
        .arg_policy = ArgCountPolicy{.min_args = 1, .max_args = 255},
        .semantic =
            PrintSystemSubroutineInfo{
                .radix = PrintRadix::kDecimal,
                .append_newline = false,
                .is_strobe = false,
                .sink_kind = PrintSinkKind::kFile},
    },
};

}  // namespace detail

[[nodiscard]] inline auto FindSystemSubroutine(std::string_view name)
    -> const SystemSubroutineDesc* {
  for (const auto& desc : detail::kSystemSubroutines) {
    if (desc.name == name) {
      return &desc;
    }
  }
  return nullptr;
}

[[nodiscard]] inline auto LookupSystemSubroutine(SystemSubroutineId id)
    -> const SystemSubroutineDesc& {
  const std::span<const SystemSubroutineDesc> view{detail::kSystemSubroutines};
  if (id.value >= view.size()) {
    throw InternalError(
        "LookupSystemSubroutine: SystemSubroutineId out of range");
  }
  return view[id.value];
}

}  // namespace lyra::support

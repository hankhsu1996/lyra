#include "lyra/common/system_tf.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <optional>
#include <string_view>
#include <utility>

#include "lyra/common/internal_error.hpp"

namespace lyra {

namespace {

// Metadata table indexed by opcode. Must be kept in sync with SystemTfOpcode.
// clang-format off
constexpr std::array kMetadataTable = std::to_array<SystemTfMetadata>({
  // kFopen: $fopen(filename [, mode]) -> int32 fd
  {
    .opcode = SystemTfOpcode::kFopen,
    .name = "$fopen",
    .family = SystemTfFamily::kFileIO,
    .role = SystemTfRole::kMixed,  // Returns fd, but also has side effect
    .result_conv = ResultConvention::kFdHandle,
    .min_args = 1,
    .max_args = 2,
    .out_arg_index = -1,
  },
  // kFclose: $fclose(fd)
  {
    .opcode = SystemTfOpcode::kFclose,
    .name = "$fclose",
    .family = SystemTfFamily::kFileIO,
    .role = SystemTfRole::kEffect,
    .result_conv = ResultConvention::kNone,
    .min_args = 1,
    .max_args = 1,
    .out_arg_index = -1,
  },
  // kFflush: $fflush() or $fflush(fd)
  {
    .opcode = SystemTfOpcode::kFflush,
    .name = "$fflush",
    .family = SystemTfFamily::kFileIO,
    .role = SystemTfRole::kEffect,
    .result_conv = ResultConvention::kNone,
    .min_args = 0,
    .max_args = 1,
    .out_arg_index = -1,
  },
});
// clang-format on

// Sorted name->opcode lookup table (compile-time sorted for binary search).
// clang-format off
constexpr std::array kNameTable = std::to_array<std::pair<std::string_view, SystemTfOpcode>>({
  {"$fclose", SystemTfOpcode::kFclose},
  {"$fflush", SystemTfOpcode::kFflush},
  {"$fopen", SystemTfOpcode::kFopen},
});
// clang-format on

}  // namespace

auto GetSystemTfMetadata(SystemTfOpcode op) -> const SystemTfMetadata& {
  auto index = static_cast<size_t>(op);
  if (index >= kMetadataTable.size()) {
    throw common::InternalError(
        "GetSystemTfMetadata", "invalid SystemTfOpcode");
  }
  return kMetadataTable.at(index);
}

auto LookupSystemTfOpcode(std::string_view name)
    -> std::optional<SystemTfOpcode> {
  const auto* it = std::ranges::lower_bound(
      kNameTable, name, {},
      &std::pair<std::string_view, SystemTfOpcode>::first);
  if (it != kNameTable.end() && it->first == name) {
    return it->second;
  }
  return std::nullopt;
}

auto ToString(SystemTfOpcode op) -> const char* {
  switch (op) {
    case SystemTfOpcode::kFopen:
      return "$fopen";
    case SystemTfOpcode::kFclose:
      return "$fclose";
    case SystemTfOpcode::kFflush:
      return "$fflush";
  }
  return "?";
}

}  // namespace lyra

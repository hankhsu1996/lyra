#include "lyra/common/display_variant.hpp"

#include <algorithm>
#include <array>

namespace lyra::common {

namespace {

struct DisplayVariantEntry {
  std::string_view name;
  DisplayVariantProps props;
};

// Table of all display-like system calls and their properties.
// clang-format off
constexpr auto kDisplayVariants = std::to_array<DisplayVariantEntry>({
    // $write variants (no newline)
    {.name = "$write",   .props = {.radix = DisplayRadix::kDecimal, .append_newline = false}},
    {.name = "$writeb",  .props = {.radix = DisplayRadix::kBinary,  .append_newline = false}},
    {.name = "$writeo",  .props = {.radix = DisplayRadix::kOctal,   .append_newline = false}},
    {.name = "$writeh",  .props = {.radix = DisplayRadix::kHex,     .append_newline = false}},

    // $fwrite variants (no newline, file descriptor)
    {.name = "$fwrite",  .props = {.radix = DisplayRadix::kDecimal, .append_newline = false, .has_file_descriptor = true}},
    {.name = "$fwriteb", .props = {.radix = DisplayRadix::kBinary,  .append_newline = false, .has_file_descriptor = true}},
    {.name = "$fwriteo", .props = {.radix = DisplayRadix::kOctal,   .append_newline = false, .has_file_descriptor = true}},
    {.name = "$fwriteh", .props = {.radix = DisplayRadix::kHex,     .append_newline = false, .has_file_descriptor = true}},

    // $sformatf (returns string)
    {.name = "$sformatf", .props = {.radix = DisplayRadix::kDecimal, .append_newline = false, .returns_string = true}},

    // $sformat/$swrite variants (output target)
    {.name = "$sformat", .props = {.radix = DisplayRadix::kDecimal, .append_newline = false, .has_output_target = true}},
    {.name = "$swrite",  .props = {.radix = DisplayRadix::kDecimal, .append_newline = false, .has_output_target = true}},
    {.name = "$swriteb", .props = {.radix = DisplayRadix::kBinary,  .append_newline = false, .has_output_target = true}},
    {.name = "$swriteo", .props = {.radix = DisplayRadix::kOctal,   .append_newline = false, .has_output_target = true}},
    {.name = "$swriteh", .props = {.radix = DisplayRadix::kHex,     .append_newline = false, .has_output_target = true}},

    // $display variants (with newline)
    {.name = "$display",  .props = {.radix = DisplayRadix::kDecimal}},
    {.name = "$displayb", .props = {.radix = DisplayRadix::kBinary}},
    {.name = "$displayo", .props = {.radix = DisplayRadix::kOctal}},
    {.name = "$displayh", .props = {.radix = DisplayRadix::kHex}},

    // $fdisplay variants (with newline, file descriptor)
    {.name = "$fdisplay",  .props = {.radix = DisplayRadix::kDecimal, .has_file_descriptor = true}},
    {.name = "$fdisplayb", .props = {.radix = DisplayRadix::kBinary,  .has_file_descriptor = true}},
    {.name = "$fdisplayo", .props = {.radix = DisplayRadix::kOctal,   .has_file_descriptor = true}},
    {.name = "$fdisplayh", .props = {.radix = DisplayRadix::kHex,     .has_file_descriptor = true}},

    // $strobe variants (with newline)
    {.name = "$strobe",  .props = {.radix = DisplayRadix::kDecimal}},
    {.name = "$strobeb", .props = {.radix = DisplayRadix::kBinary}},
    {.name = "$strobeo", .props = {.radix = DisplayRadix::kOctal}},
    {.name = "$strobeh", .props = {.radix = DisplayRadix::kHex}},

    // $fstrobe variants (with newline, file descriptor)
    {.name = "$fstrobe",  .props = {.radix = DisplayRadix::kDecimal, .has_file_descriptor = true}},
    {.name = "$fstrobeb", .props = {.radix = DisplayRadix::kBinary,  .has_file_descriptor = true}},
    {.name = "$fstrobeo", .props = {.radix = DisplayRadix::kOctal,   .has_file_descriptor = true}},
    {.name = "$fstrobeh", .props = {.radix = DisplayRadix::kHex,     .has_file_descriptor = true}},

    // $monitor variants (with newline)
    {.name = "$monitor",  .props = {.radix = DisplayRadix::kDecimal}},
    {.name = "$monitorb", .props = {.radix = DisplayRadix::kBinary}},
    {.name = "$monitoro", .props = {.radix = DisplayRadix::kOctal}},
    {.name = "$monitorh", .props = {.radix = DisplayRadix::kHex}},

    // $fmonitor variants (with newline, file descriptor)
    {.name = "$fmonitor",  .props = {.radix = DisplayRadix::kDecimal, .has_file_descriptor = true}},
    {.name = "$fmonitorb", .props = {.radix = DisplayRadix::kBinary,  .has_file_descriptor = true}},
    {.name = "$fmonitoro", .props = {.radix = DisplayRadix::kOctal,   .has_file_descriptor = true}},
    {.name = "$fmonitorh", .props = {.radix = DisplayRadix::kHex,     .has_file_descriptor = true}},
});
// clang-format on

}  // namespace

auto GetDisplayVariantProps(std::string_view name)
    -> std::optional<DisplayVariantProps> {
  const auto* it = std::ranges::find_if(
      kDisplayVariants, [&](const auto& entry) { return entry.name == name; });
  if (it != kDisplayVariants.end()) {
    return it->props;
  }
  return std::nullopt;
}

auto IsDisplayLikeCall(std::string_view name) -> bool {
  return GetDisplayVariantProps(name).has_value();
}

auto GetSeverityLevel(std::string_view name) -> std::optional<SeverityLevel> {
  if (name == "$info") {
    return SeverityLevel::kInfo;
  }
  if (name == "$warning") {
    return SeverityLevel::kWarning;
  }
  if (name == "$error") {
    return SeverityLevel::kError;
  }
  if (name == "$fatal") {
    return SeverityLevel::kFatal;
  }
  return std::nullopt;
}

}  // namespace lyra::common

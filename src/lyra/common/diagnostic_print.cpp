#include <cstdio>
#include <string_view>

#include <fmt/color.h>
#include <fmt/core.h>

#include "lyra/common/diagnostic/print.hpp"

namespace lyra {

namespace {

constexpr auto kToolStyle =
    fmt::fg(fmt::terminal_color::white) | fmt::emphasis::bold;

}  // namespace

void PrintError(std::string_view message) {
  fmt::print(
      stderr, "{}: {} {}\n", fmt::styled("lyra", kToolStyle),
      fmt::styled(
          "error:",
          fmt::fg(fmt::terminal_color::bright_red) | fmt::emphasis::bold),
      fmt::styled(message, fmt::emphasis::bold));
}

void PrintWarning(std::string_view message) {
  fmt::print(
      stderr, "{}: {} {}\n", fmt::styled("lyra", kToolStyle),
      fmt::styled(
          "warning:",
          fmt::fg(fmt::terminal_color::bright_yellow) | fmt::emphasis::bold),
      fmt::styled(message, fmt::emphasis::bold));
}

}  // namespace lyra

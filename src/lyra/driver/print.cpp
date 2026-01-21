#include "print.hpp"

#include <fmt/color.h>
#include <fmt/core.h>

namespace lyra::driver {

namespace {

constexpr auto kToolColor = fmt::terminal_color::white;
constexpr auto kToolStyle = fmt::fg(kToolColor) | fmt::emphasis::bold;

}  // namespace

void PrintError(const std::string& message) {
  fmt::print(
      stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
      fmt::styled("error", fmt::fg(fmt::terminal_color::bright_red)),
      fmt::styled(message, fmt::emphasis::bold));
}

void PrintWarning(const std::string& message) {
  fmt::print(
      stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
      fmt::styled("warning", fmt::fg(fmt::terminal_color::bright_yellow)),
      fmt::styled(message, fmt::emphasis::bold));
}

void PrintDiagnostics(const DiagnosticSink& sink) {
  for (const auto& diag : sink.GetDiagnostics()) {
    const char* severity_str = nullptr;
    fmt::text_style severity_style;

    switch (diag.severity) {
      case DiagnosticSeverity::kError:
        severity_str = "error";
        severity_style = fmt::fg(fmt::terminal_color::bright_red);
        break;
      case DiagnosticSeverity::kWarning:
        severity_str = "warning";
        severity_style = fmt::fg(fmt::terminal_color::bright_yellow);
        break;
      case DiagnosticSeverity::kNote:
        severity_str = "note";
        severity_style = fmt::fg(fmt::terminal_color::bright_cyan);
        break;
    }

    fmt::print(
        stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
        fmt::styled(severity_str, severity_style),
        fmt::styled(diag.message, fmt::emphasis::bold));
  }
}

auto FormatDiagnostics(const DiagnosticSink& sink) -> std::string {
  std::string result;
  for (const auto& diag : sink.GetDiagnostics()) {
    const char* severity_str = nullptr;
    switch (diag.severity) {
      case DiagnosticSeverity::kError:
        severity_str = "error";
        break;
      case DiagnosticSeverity::kWarning:
        severity_str = "warning";
        break;
      case DiagnosticSeverity::kNote:
        severity_str = "note";
        break;
    }
    result += fmt::format("{}: {}\n", severity_str, diag.message);
  }
  return result;
}

}  // namespace lyra::driver

#include "lyra/common/diagnostic.hpp"

#include <algorithm>
#include <cstddef>
#include <print>
#include <string>
#include <string_view>

#include <fmt/color.h>

namespace lyra {

namespace {

auto GetSeverityString(DiagnosticSeverity severity) -> std::string_view {
  switch (severity) {
    case DiagnosticSeverity::kNote:
      return "note";
    case DiagnosticSeverity::kWarning:
      return "warning";
    case DiagnosticSeverity::kError:
      return "error";
  }
  return "unknown";
}

auto GetSeverityColor(DiagnosticSeverity severity) -> fmt::terminal_color {
  switch (severity) {
    case DiagnosticSeverity::kNote:
      return fmt::terminal_color::bright_black;
    case DiagnosticSeverity::kWarning:
      return fmt::terminal_color::bright_yellow;
    case DiagnosticSeverity::kError:
      return fmt::terminal_color::bright_red;
  }
  return fmt::terminal_color::white;
}

auto GetSourceLine(
    const slang::SourceManager& sm, slang::SourceLocation loc, size_t col)
    -> std::string_view {
  std::string_view text = sm.getSourceText(loc.buffer());
  if (text.empty()) {
    return "";
  }

  // Extract line starting at (offset - col + 1)
  std::size_t line_start = loc.offset() - (col - 1);
  std::string_view from_line_start = text.substr(line_start);

  // Find end of line
  const auto* line_end = std::ranges::find_if(
      from_line_start, [](char chr) { return chr == '\n' || chr == '\r'; });

  return from_line_start.substr(
      0, static_cast<std::size_t>(line_end - from_line_start.begin()));
}

void PrintHighlightLine(
    size_t col, size_t range_length, fmt::terminal_color highlight_color,
    bool colors) {
  std::string highlight(col - 1, ' ');
  highlight += '^';
  if (range_length > 1) {
    highlight += std::string(range_length - 1, '~');
  }

  if (colors) {
    fmt::print(
        stderr, "{}\n", fmt::styled(highlight, fmt::fg(highlight_color)));
  } else {
    std::print(stderr, "{}\n", highlight);
  }
}

}  // namespace

void PrintDiagnostic(
    const Diagnostic& diag, const slang::SourceManager& sm, bool colors) {
  auto loc = diag.location.start();
  bool has_location =
      loc.buffer() != slang::SourceLocation::NoLocation.buffer();

  constexpr auto kFilenameColor = fmt::terminal_color::cyan;
  constexpr auto kLocationColor = fmt::terminal_color::bright_cyan;
  constexpr auto kHighlightColor = fmt::terminal_color::bright_green;

  if (has_location) {
    auto filename = sm.getFileName(loc);
    auto line = sm.getLineNumber(loc);
    auto col = sm.getColumnNumber(loc);

    if (colors) {
      fmt::print(
          stderr, "{}:{}:{}: ", fmt::styled(filename, fmt::fg(kFilenameColor)),
          fmt::styled(line, fmt::fg(kLocationColor)),
          fmt::styled(col, fmt::fg(kLocationColor)));
    } else {
      std::print(stderr, "{}:{}:{}: ", filename, line, col);
    }
  }

  auto severity_color = GetSeverityColor(diag.severity);
  auto severity_string = GetSeverityString(diag.severity);

  if (colors) {
    fmt::print(
        stderr, "{}: {}\n",
        fmt::styled(severity_string, fmt::fg(severity_color)),
        fmt::styled(diag.message, fmt::emphasis::bold));
  } else {
    std::print(stderr, "{}: {}\n", severity_string, diag.message);
  }

  if (has_location) {
    auto col = sm.getColumnNumber(loc);
    auto source_line = GetSourceLine(sm, loc, col);

    if (!source_line.empty()) {
      std::print(stderr, "{}\n", source_line);

      size_t range_length = 1;
      if (diag.location.end().offset() > diag.location.start().offset()) {
        range_length =
            diag.location.end().offset() - diag.location.start().offset();
      }

      PrintHighlightLine(col, range_length, kHighlightColor, colors);
    }
  }
}

void PrintDiagnostic(const Diagnostic& diag, bool colors) {
  auto severity_color = GetSeverityColor(diag.severity);
  auto severity_string = GetSeverityString(diag.severity);

  if (colors) {
    fmt::print(
        stderr, "{}: {}\n",
        fmt::styled(severity_string, fmt::fg(severity_color)),
        fmt::styled(diag.message, fmt::emphasis::bold));
  } else {
    std::print(stderr, "{}: {}\n", severity_string, diag.message);
  }
}

}  // namespace lyra

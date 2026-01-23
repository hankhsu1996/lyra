#include "print.hpp"

#include <algorithm>

#include <fmt/color.h>
#include <fmt/core.h>

#include "lyra/common/source_span.hpp"
#include "pipeline.hpp"

namespace lyra::driver {

namespace {

constexpr auto kToolColor = fmt::terminal_color::white;
constexpr auto kToolStyle = fmt::fg(kToolColor) | fmt::emphasis::bold;

}  // namespace

void PrintError(const std::string& message) {
  fmt::print(
      stderr, "{}: {} {}\n", fmt::styled("lyra", kToolStyle),
      fmt::styled(
          "error:",
          fmt::fg(fmt::terminal_color::bright_red) | fmt::emphasis::bold),
      fmt::styled(message, fmt::emphasis::bold));
}

void PrintWarning(const std::string& message) {
  fmt::print(
      stderr, "{}: {} {}\n", fmt::styled("lyra", kToolStyle),
      fmt::styled(
          "warning:",
          fmt::fg(fmt::terminal_color::bright_yellow) | fmt::emphasis::bold),
      fmt::styled(message, fmt::emphasis::bold));
}

void PrintDiagnostics(
    const DiagnosticSink& sink, const SourceManager* source_manager) {
  uint32_t error_count = 0;
  uint32_t warning_count = 0;

  for (const auto& diag : sink.GetDiagnostics()) {
    const char* severity_str = nullptr;
    fmt::text_style severity_style;

    switch (diag.severity) {
      case DiagnosticSeverity::kError:
        severity_str = "error:";
        severity_style =
            fmt::fg(fmt::terminal_color::bright_red) | fmt::emphasis::bold;
        ++error_count;
        break;
      case DiagnosticSeverity::kWarning:
        severity_str = "warning:";
        severity_style =
            fmt::fg(fmt::terminal_color::bright_magenta) | fmt::emphasis::bold;
        ++warning_count;
        break;
      case DiagnosticSeverity::kNote:
        severity_str = "note:";
        severity_style =
            fmt::fg(fmt::terminal_color::bright_cyan) | fmt::emphasis::bold;
        break;
    }

    std::string location;
    if (source_manager != nullptr) {
      location = FormatSourceLocation(diag.location, *source_manager);
    }

    if (!location.empty()) {
      fmt::print(
          stderr, "{}: {} {}\n", fmt::styled(location, fmt::emphasis::bold),
          fmt::styled(severity_str, severity_style),
          fmt::styled(diag.message, fmt::emphasis::bold));
    } else {
      fmt::print(
          stderr, "{}: {} {}\n", fmt::styled("lyra", kToolStyle),
          fmt::styled(severity_str, severity_style),
          fmt::styled(diag.message, fmt::emphasis::bold));
    }

    // Print source line and caret marker
    const SourceSpan& span = diag.location;
    if (source_manager != nullptr && span.file_id) {
      const FileInfo* file = source_manager->GetFile(span.file_id);
      if (file != nullptr) {
        const std::string& content = file->content;

        // Find line boundaries and compute line number
        uint32_t line_start = 0;
        uint32_t line_num = 1;
        for (uint32_t i = 0; i < span.begin && i < content.size(); ++i) {
          if (content[i] == '\n') {
            line_start = i + 1;
            ++line_num;
          }
        }

        size_t line_end_pos = content.find('\n', span.begin);
        uint32_t line_end = (line_end_pos == std::string::npos)
                                ? static_cast<uint32_t>(content.size())
                                : static_cast<uint32_t>(line_end_pos);

        uint32_t col = span.begin - line_start;
        std::string source_line =
            content.substr(line_start, line_end - line_start);
        std::string line_num_str = std::to_string(line_num);
        size_t field_width = std::max(line_num_str.size(), size_t{4});
        std::string num_field(field_width - line_num_str.size(), ' ');
        num_field += line_num_str;
        std::string blank_field(field_width, ' ');

        constexpr auto kGutterStyle = fmt::fg(fmt::terminal_color::white);

        fmt::print(
            stderr, " {} {}\n", fmt::styled(num_field + " |", kGutterStyle),
            source_line);

        // Compute marker width: clamp span to current line
        uint32_t span_end = (span.end > span.begin) ? span.end : span.begin + 1;
        span_end = std::min(span_end, line_end);
        uint32_t span_width = span_end - span.begin;
        if (span_width == 0) {
          span_width = 1;
        }

        std::string marker = "^" + std::string(span_width - 1, '~');
        constexpr auto kMarkerStyle = fmt::fg(fmt::terminal_color::green);

        fmt::print(
            stderr, " {} {}{}\n", fmt::styled(blank_field + " |", kGutterStyle),
            std::string(col, ' '), fmt::styled(marker, kMarkerStyle));
      }
    }
  }

  if (warning_count > 0 || error_count > 0) {
    std::string summary;
    if (warning_count > 0) {
      summary += fmt::format(
          "{} warning{}", warning_count, warning_count == 1 ? "" : "s");
    }
    if (warning_count > 0 && error_count > 0) {
      summary += " and ";
    }
    if (error_count > 0) {
      summary +=
          fmt::format("{} error{}", error_count, error_count == 1 ? "" : "s");
    }
    fmt::print(stderr, "{} generated.\n", summary);
  }
}

auto CompilationError::FromDiagnostics(
    DiagnosticSink sink, std::unique_ptr<SourceManager> mgr)
    -> CompilationError {
  return CompilationError{
      .diagnostics = std::move(sink),
      .source_manager = std::move(mgr),
      .message = {},
  };
}

auto CompilationError::Simple(std::string msg) -> CompilationError {
  return CompilationError{
      .diagnostics = {},
      .source_manager = nullptr,
      .message = std::move(msg),
  };
}

void CompilationError::Print() const {
  if (diagnostics.HasErrors()) {
    PrintDiagnostics(diagnostics, source_manager.get());
  } else {
    PrintError(message);
  }
}

}  // namespace lyra::driver

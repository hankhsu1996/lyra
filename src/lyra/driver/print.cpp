#include "print.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include <fmt/color.h>
#include <fmt/core.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/common/source_span.hpp"
#include "pipeline.hpp"

namespace lyra::driver {

namespace {

constexpr auto kToolColor = fmt::terminal_color::white;
constexpr auto kToolStyle = fmt::fg(kToolColor) | fmt::emphasis::bold;

auto DiagKindToString(DiagKind kind) -> const char* {
  switch (kind) {
    case DiagKind::kError:
      return "error:";
    case DiagKind::kUnsupported:
      return "unsupported:";
    case DiagKind::kHostError:
      return "error:";
    case DiagKind::kWarning:
      return "warning:";
    case DiagKind::kNote:
      return "note:";
  }
  return "error:";
}

auto DiagKindToStyle(DiagKind kind) -> fmt::text_style {
  switch (kind) {
    case DiagKind::kError:
    case DiagKind::kUnsupported:
    case DiagKind::kHostError:
      return fmt::fg(fmt::terminal_color::bright_red) | fmt::emphasis::bold;
    case DiagKind::kWarning:
      return fmt::fg(fmt::terminal_color::bright_magenta) | fmt::emphasis::bold;
    case DiagKind::kNote:
      return fmt::fg(fmt::terminal_color::bright_cyan) | fmt::emphasis::bold;
  }
  return fmt::fg(fmt::terminal_color::bright_red) | fmt::emphasis::bold;
}

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

// Print a single DiagItem with optional source context
void PrintDiagItem(
    const DiagItem& item, const SourceManager* source_manager,
    bool is_primary) {
  const char* kind_str = DiagKindToString(item.kind);
  fmt::text_style kind_style = DiagKindToStyle(item.kind);

  std::string location;
  std::optional<SourceSpan> span_opt;

  std::visit(
      common::Overloaded{
          [&](const SourceSpan& span) {
            span_opt = span;
            if (source_manager != nullptr) {
              location = FormatSourceLocation(span, *source_manager);
            }
          },
          [&](UnknownSpan) {
            // No location available
          },
      },
      item.span);

  if (!location.empty()) {
    fmt::print(
        stderr, "{}: {} {}\n", fmt::styled(location, fmt::emphasis::bold),
        fmt::styled(kind_str, kind_style),
        fmt::styled(
            item.message,
            is_primary ? fmt::emphasis::bold : fmt::text_style{}));
  } else {
    fmt::print(
        stderr, "{}: {} {}\n", fmt::styled("lyra", kToolStyle),
        fmt::styled(kind_str, kind_style),
        fmt::styled(
            item.message,
            is_primary ? fmt::emphasis::bold : fmt::text_style{}));
  }

  // Print source line and caret marker (only for primary messages with span)
  if (is_primary && span_opt && source_manager != nullptr &&
      span_opt->file_id) {
    const SourceSpan& span = *span_opt;
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

void PrintDiagnostic(const Diagnostic& diag) {
  PrintDiagItem(diag.primary, nullptr, true);
  for (const auto& note : diag.notes) {
    PrintDiagItem(note, nullptr, false);
  }
}

void PrintDiagnostic(
    const Diagnostic& diag, const SourceManager& source_manager) {
  PrintDiagItem(diag.primary, &source_manager, true);
  for (const auto& note : diag.notes) {
    PrintDiagItem(note, &source_manager, false);
  }
}

void PrintDiagnostics(
    const DiagnosticSink& sink, const SourceManager* source_manager) {
  uint32_t error_count = 0;
  uint32_t warning_count = 0;

  for (const auto& diag : sink.GetDiagnostics()) {
    switch (diag.primary.kind) {
      case DiagKind::kError:
      case DiagKind::kUnsupported:
      case DiagKind::kHostError:
        ++error_count;
        break;
      case DiagKind::kWarning:
        ++warning_count;
        break;
      case DiagKind::kNote:
        break;
    }

    PrintDiagItem(diag.primary, source_manager, true);

    for (const auto& note : diag.notes) {
      PrintDiagItem(note, source_manager, false);
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
